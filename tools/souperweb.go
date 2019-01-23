// Copyright 2014 The Souper Authors. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package main

import (
	"bytes"
	"crypto/sha1"
	"encoding/json"
	"html"
	"io"
	"math/rand"
	"net"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"sort"
	"strings"
	"syscall"
	"text/template"
	"time"
	"fmt"

	"github.com/gomodule/redigo/redis"
)

type solverQuery struct {
	IsIR bool
	Req  string
}

type solverReq struct {
	solverQuery
	dest   io.Writer
	result chan<- solverResp
}

type solverResp struct {
	result string
	err    error
}

type solveHTMLResp struct {
	HTML string
	Key  string
}

type friendlyError struct {
	msg string
	err error
}

func (e friendlyError) Error() string {
	s := e.msg
	if e.err != nil {
		s += " (" + e.err.Error() + ")"
	}
	return s
}

type rateReq struct {
	ip string
	ok chan<- bool
}

type context struct {
	homepage *template.Template
	solverch chan solverReq
	ratech   chan rateReq
	pool     redis.Pool
}

func (ctx *context) init() {
	ctx.solverch = make(chan solverReq)
	ctx.ratech = make(chan rateReq)
	ctx.pool = redis.Pool{
		MaxIdle:     3,
		IdleTimeout: 240 * time.Second,
		Dial: func() (redis.Conn, error) {
			return redis.Dial("tcp", "127.0.0.1:6379")
		},
		TestOnBorrow: func(c redis.Conn, t time.Time) error {
			_, err := c.Do("PING")
			return err
		},
	}

	go ctx.solverWorker()
	go ctx.solverWorker()
	go ctx.rateWorker()

	ctx.homepage = ctx.buildHomePage()
}

func randFromQuery(query string) *rand.Rand {
	hash := sha1.Sum([]byte(query))
	var hash8 int64
	for i, h := range hash[0:8] {
		hash8 |= int64(h) << (8 * uint(i))
	}
	source := rand.NewSource(hash8)
	return rand.New(source)
}

// avoid English words and transcription errors by excluding 01AEIOU
const keyChars = "23456789BCDFGHJKLMNPQRSTVWXYZ"

func keyFromRand(r *rand.Rand) string {
	var key string
	for i := 0; i != 6; i++ {
		key += string(keyChars[r.Int31n(int32(len(keyChars)))])
	}
	return key
}

var keyRE = regexp.MustCompile("^[" + keyChars + "]{6}$")

func isKey(k string) bool {
	return keyRE.MatchString(k)
}

func (ctx *context) lookupByKey(conn redis.Conn, key string) (solverQuery, error) {
	_, err := conn.Do("SELECT", 0)
	if err != nil {
		return solverQuery{}, err
	}

	jsonquery, err := redis.Bytes(conn.Do("GET", key))
	if err != nil {
		return solverQuery{}, friendlyError{"Invalid key", err}
	}

	var query solverQuery
	err = json.Unmarshal(jsonquery, &query)
	return query, err
}

func (ctx *context) getKey(conn redis.Conn, query solverQuery) (string, error) {
	json, err := json.Marshal(query)
	if err != nil {
		return "", err
	}

	rand := randFromQuery(query.Req)

	_, err = conn.Do("SELECT", 0)
	if err != nil {
		return "", err
	}

	for {
		_, err := conn.Do("WATCH", "key:"+string(json))
		if err != nil {
			return "", err
		}

		key, err := redis.String(conn.Do("GET", "key:"+string(json)))
		if err != nil && err != redis.ErrNil {
			return "", err
		}
		if key != "" {
			_, err := conn.Do("UNWATCH")
			if err != nil {
				return "", err
			}
			return key, nil
		}

		_, err = conn.Do("WATCH", key)
		if err != nil {
			return "", err
		}

		key = keyFromRand(rand)
		len, err := redis.Int(conn.Do("STRLEN", key))
		if len != 0 {
			_, err := conn.Do("UNWATCH")
			if err != nil {
				return "", err
			}
			continue
		}

		_, err = conn.Do("MULTI")
		if err != nil {
			return "", err
		}

		_, err = conn.Do("SET", key, json)
		if err != nil {
			return "", err
		}

		_, err = conn.Do("SET", "key:"+string(json), key)
		if err != nil {
			return "", err
		}

		exec, err := conn.Do("EXEC")
		if err != nil {
			return "", err
		}
		if exec == nil {
			continue
		}

		return key, nil
	}
}

func (ctx *context) solverWorker() {
	conn := ctx.pool.Get()
	defer conn.Close()

	for r := range ctx.solverch {
		var arg string
		if r.IsIR {
			arg = "-action=ir"
		} else {
			arg = "-action=inst"
		}
		cmd := exec.Command(os.Args[0]+"-backend", arg, os.Args[1])

		cmd.Stdin = strings.NewReader(r.Req)

		var outb, errb bytes.Buffer
		cmd.Stdout = &outb
		cmd.Stderr = &errb

		var sys syscall.SysProcAttr
		sys.Setpgid = true
		cmd.SysProcAttr = &sys

		err := cmd.Start()
		if err != nil {
			os.Stdout.Write([]byte("Error invoking solver: " + err.Error() + "\n"))
			r.result <- solverResp{"", friendlyError{"Error invoking solver", err}}
			continue
		}

		timeout := false
		timer := time.AfterFunc(10*time.Second, func() {
			syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
			timeout = true
		})

		err = cmd.Wait()
		timer.Stop()
		if timeout {
			os.Stdout.Write([]byte("Solver timeout\n"))
			r.result <- solverResp{"", friendlyError{"Solver timeout", nil}}
		} else if err != nil {
			r.result <- solverResp{"", friendlyError{"Error invoking solver", err}}
		} else if errb.Len() != 0 {
			r.result <- solverResp{"", friendlyError{errb.String(), nil}}
		} else {
			r.result <- solverResp{outb.String(), nil}
		}
	}
}

func (ctx *context) rateWorker() {
	visits := make(map[string][3]int64)

	daily := make(chan bool)
	go func() {
		for {
			time.Sleep(86400 * time.Second)
			daily <- true
		}
	}()

	for {
		select {
		case r := <-ctx.ratech:
			now := time.Now().Unix()
			vs := visits[r.ip]
			ok := vs[2] <= now-10
			vs[0], vs[1], vs[2] = now, vs[0], vs[1]
			visits[r.ip] = vs
			r.ok <- ok

		case <-daily:
			visits = make(map[string][3]int64)
		}
	}
}

func (ctx *context) buildHomePage() *template.Template {
	conn := ctx.pool.Get()
	defer conn.Close()

	examples := map[string]solverQuery{
		"addnsw (IR)": {true, `define i32 @foo(i32 %x) {
entry:
  %add = add nsw i32 %x, 1
  %cmp = icmp sgt i32 %add, %x
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
`},
		"addnsw (inst)": {false, `%x:i32 = var
%add = addnsw %x, 1
%cmp = slt %x, %add
cand %cmp 1
`},
		"instcombine1": {false, `%a:i1 = var
%b:i32 = var
%ax:i32 = zext %a
%c = add %ax, %b

%b1 = add %b, 1
%c2 = select %a, %b1, %b

cand %c %c2
`},
		"simple": {false, `%a:i32 = var
%aa = add %a, %a
%2a = mul %a, 2
cand %aa %2a
`},
		"simple-pc": {false, `%x:i32 = var
%2lx = slt 2, %x
pc %2lx 1
%1lx = slt 1, %x
cand %1lx 1
`},
		"simple-pc-invalid": {false, `%x:i32 = var
%2lx = slt 2, %x
pc %2lx 1
%3lx = slt 3, %x
cand %3lx 1
`},
	}

	exampleNames := make([]string, len(examples))
	i := 0
	for name, _ := range examples {
		exampleNames[i] = name
		i++
	}
	sort.Strings(exampleNames)

	var b bytes.Buffer
	b.WriteString(`
<html>
<head>
<script>
function solve() {
	var xhr = new XMLHttpRequest();
	xhr.open("POST", "/solve/json", true);
	xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
	xhr.responseType = "json";

	var req = "type=";
	if (document.getElementById("typeInst").checked) {
		req += "inst";
	} else {
		req += "ir";
	}
	req += "&data=";
	req += escape(document.getElementById("data").value);

	xhr.onload = function() {
		var resp = xhr.response;
		document.getElementById("result").innerHTML = resp.HTML;
		if (resp.Key != "") {
			history.pushState(null, "", "/"+resp.Key);
		}
	}
	xhr.onerror = function() {
		document.getElementById("result").innerHTML = "<pre>Internal error</pre>";
	}
	xhr.send(req);
}

function lookup(key, push) {
	var xhr = new XMLHttpRequest();
	xhr.open("GET", "/"+key+"/json", true);
	xhr.responseType = "json";

	xhr.onload = function() {
		var resp = xhr.response;
		document.getElementById("result").innerHTML = resp.resp.HTML;
		if (resp.resp.Key != "") {
			if (resp.query.IsIR) {
				document.getElementById("typeIR").checked = true;
			} else {
				document.getElementById("typeInst").checked = true;
			}
			document.getElementById("data").value = resp.query.Req;
			if (push) {
				history.pushState(null, "", "/"+key);
			}
		}
	}
	xhr.onerror = function() {
		document.getElementById("result").innerHTML = "<pre>Internal error</pre>";
	}
	xhr.send(null);
}

function init() {
	window.onpopstate = function() {
		if (location.pathname.length > 1) {
			lookup(location.pathname.substring(1), false);
		} else {
			document.getElementById("typeInst").checked = true;
			document.getElementById("data").value = "";
			document.getElementById("result").innerHTML = "";
		}
	};

	var links = document.getElementsByTagName("a");
	for (var i = 0; i != links.length; i++) {
		if (links[i].className == "example") {
			links[i].onclick = function() {
				var key = this.pathname.substring(1);
				lookup(key, true);
				return false;
			};
		}
	}

	document.getElementById("theForm").onsubmit = function() {
		solve();
		return false;
	}
}

if (history.pushState) {
	window.onload = init;
}
</script>
<style>
pre {
	margin: 5pt;
}
</style>
</head>
<body style="font-family: sans-serif">
<h1>souperweb</h1>
Enter <a href="https://github.com/google/souper/blob/master/docs/InstRef.rst">Souper instructions</a> or <a href="http://llvm.org/docs/LangRef.html">LLVM IR</a> into the box below and hit Submit.<br>
`)

	b.WriteString(`
<form id="theForm" action="/solve" method="post">
<input type="radio" name="type" value="inst"{{if not .query.IsIR}} checked{{end}} id="typeInst">Souper inst
<input type="radio" name="type" value="ir"{{if .query.IsIR}} checked{{end}} id="typeIR">LLVM IR<br>
<table>
<tr>
<td>
<textarea name="data" id="data" style="width: 900px; height: 300px">{{html .query.Req}}</textarea><br>
</td>
<td valign="top">
Examples:<br>
`)
	for _, n := range exampleNames {
		key, err := ctx.getKey(conn, examples[n])
		if err != nil {
			panic(err.Error())
		}
		b.WriteString(`<a class="example" href="/`)
		b.WriteString(key)
		b.WriteString(`">`)
		b.WriteString(n)
		b.WriteString(`</a><br>`)
	}
	b.WriteString(`
</td>
</tr>
</table>
<input type="submit">
</form>

<div name="result" id="result" style="width: 900px; height: 300px; overflow-y: scroll; border: 1px solid black">{{.resp.HTML}}</div>
</body>
</html>
`)
	return template.Must(template.New("homepage").Parse(b.String()))
}

func (ctx *context) rootHandler(w http.ResponseWriter, r *http.Request) {
	if len(r.URL.Path) > 1 {
		key := r.URL.Path[1:]
		isjson := false
		if strings.HasSuffix(key, "/json") {
			key = key[:len(key)-5]
			isjson = true
		}

		if !isKey(key) {
			w.WriteHeader(http.StatusNotFound)
			w.Write([]byte("Not Found\n"))
			return
		}

		conn := ctx.pool.Get()
		defer conn.Close()

		query, resp := ctx.buildLookupResp(conn, key, r)
		if isjson {
			json, _ := json.Marshal(map[string]interface{}{"query": query, "resp": resp})
			w.Write(json)
		} else {
			ctx.writeHomePage(w, query, resp)
		}
	} else {
		ctx.writeHomePage(w, solverQuery{}, solveHTMLResp{})
	}
}

func (ctx *context) getErrorResp(err error) (resp solveHTMLResp) {
	var w bytes.Buffer
	w.Write([]byte(`<font color="red"><pre>`))
	if ferr, ok := err.(friendlyError); ok {
		if ferr.err != nil {
			os.Stdout.Write([]byte(ferr.err.Error() + "\n"))
		}
		w.Write([]byte(html.EscapeString(ferr.msg)))
	} else {
		os.Stdout.Write([]byte(err.Error() + "\n"))
		w.Write([]byte("Internal error"))
	}
	w.Write([]byte(`</pre></font>`))
	resp.HTML = w.String()
	return resp
}

func (ctx *context) solve(conn redis.Conn, w io.Writer, query solverQuery, r *http.Request) error {
	_, err := conn.Do("SELECT", 1)
	if err != nil {
		return err
	}

	json, err := json.Marshal(query)
	if err != nil {
		return err
	}

	result, err := redis.String(conn.Do("GET", json))
	if err != nil && err != redis.ErrNil {
		return err
	}
	if err != redis.ErrNil {
		w.Write([]byte("<pre>"))
		w.Write([]byte(html.EscapeString(result)))
		w.Write([]byte("</pre>"))
		return nil
	}

	os.Stdout.WriteString(time.Now().String() + " " + r.RemoteAddr + "\n")

	ch := make(chan bool)

	host, _, err := net.SplitHostPort(r.RemoteAddr)
	if err != nil {
		return err
	}

	ctx.ratech <- rateReq{host, ch}

	if <-ch {
		respch := make(chan solverResp)
		ctx.solverch <- solverReq{query, w, respch}
		resp := <-respch

		if resp.err != nil {
			return resp.err
		}

		_, err := conn.Do("SET", json, resp.result)
		if err != nil {
			return err
		}

		w.Write([]byte("<pre>"))
		w.Write([]byte(html.EscapeString(resp.result)))
		w.Write([]byte("</pre>"))

		return nil
	} else {
		return friendlyError{"Rate limit exceeded", nil}
	}

	return nil
}

func (ctx *context) buildLookupResp(conn redis.Conn, key string, r *http.Request) (solverQuery, solveHTMLResp) {
	query, err := ctx.lookupByKey(conn, key)
	if err != nil {
		return solverQuery{}, ctx.getErrorResp(err)
	}

	var html bytes.Buffer
	err = ctx.solve(conn, &html, query, r)
	if err != nil {
		return query, ctx.getErrorResp(err)
	}

	var resp solveHTMLResp
	resp.HTML = html.String()
	resp.Key = key
	return query, resp
}

func (ctx *context) buildSolveResp(conn redis.Conn, query solverQuery, r *http.Request) solveHTMLResp {
	var html bytes.Buffer
	err := ctx.solve(conn, &html, query, r)
	if err != nil {
		return ctx.getErrorResp(err)
	}

	key, err := ctx.getKey(conn, query)
	if err != nil {
		return ctx.getErrorResp(err)
	}

	var resp solveHTMLResp
	resp.HTML = html.String()
	resp.Key = key
	return resp
}

func (ctx *context) solveHandler(w http.ResponseWriter, r *http.Request) {
	conn := ctx.pool.Get()
	defer conn.Close()

	r.ParseForm()

	typeForm := r.Form["type"]
	if len(typeForm) == 0 {
		return
	}

	dataForm := r.Form["data"]
	if len(dataForm) == 0 {
		return
	}

	data := strings.Replace(dataForm[0], "\r\n", "\n", -1)
	query := solverQuery{typeForm[0] == "ir", data}

	resp := ctx.buildSolveResp(conn, query, r)

	if strings.HasSuffix(r.URL.Path, "/json") {
		json, _ := json.Marshal(resp)
		w.Write(json)
	} else if resp.Key != "" {
		http.Redirect(w, r, "/"+resp.Key, http.StatusFound)
	} else {
		ctx.writeHomePage(w, query, resp)
	}
}

func (ctx *context) writeHomePage(w io.Writer, query solverQuery, resp solveHTMLResp) {
	ctx.homepage.Execute(w, map[string]interface{}{"query": query, "resp": resp})
}

func main() {
	var ctx context
	ctx.init()

	http.HandleFunc("/", ctx.rootHandler)
	http.HandleFunc("/solve", ctx.solveHandler)
	http.HandleFunc("/solve/json", ctx.solveHandler)

	fmt.Println("Listening on port :8080")
	http.ListenAndServe(":8080", nil)
}

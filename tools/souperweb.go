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
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/exec"
	"strings"
	"time"
)

type solverReq struct {
	isIR bool
	req  string
	dest io.Writer
	done chan<- bool
}

type rateReq struct {
	ip string
	ok chan<- bool
}

type context struct {
	solverch chan solverReq
	ratech   chan rateReq
}

func (ctx *context) init() {
	ctx.solverch = make(chan solverReq)
	ctx.ratech = make(chan rateReq)
	go ctx.solverWorker()
	go ctx.solverWorker()
	go ctx.rateWorker()
}

func (ctx *context) solverWorker() {
	for r := range ctx.solverch {
		var arg string
		if r.isIR {
			arg = "ir"
		} else {
			arg = "inst"
		}
		cmd := exec.Command(os.Args[0] + "-backend", arg)

		cmd.Stdin = strings.NewReader(r.req)
		cmd.Stdout = r.dest
		cmd.Stderr = r.dest

		err := cmd.Run()
		if err != nil {
			os.Stdout.Write([]byte("Error invoking solver: " + err.Error() + "\n"))
			r.dest.Write([]byte("Error invoking solver"))
		}

		r.done <- true
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
		case r := <- ctx.ratech:
			now := time.Now().Unix()
			vs := visits[r.ip]
			ok := vs[2] <= now - 10
			vs[0], vs[1], vs[2] = now, vs[0], vs[1]
			visits[r.ip] = vs
			r.ok <- ok

		case <-daily:
			visits = make(map[string][3]int64)
		}
	}
}

func (*context) rootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, `
<h1>souperweb</h1>
Enter Souper instructions or LLVM IR into the box below and hit Submit.<br>
<form action="/solve" method="post" target="result">
<input type="radio" name="type" value="inst" checked>Souper inst
<input type="radio" name="type" value="ir">LLVM IR<br>
<textarea name="data" style="width: 900px; height: 300px"></textarea><br>
<input type="submit">
</form>

<iframe name="result" src="about:blank" style="width: 900px; height: 300px"></iframe>
`)
}

func (ctx *context) solveHandler(w http.ResponseWriter, r *http.Request) {
	r.ParseForm()

	typeForm := r.Form["type"]
	if len(typeForm) == 0 {
		return
	}

	dataForm := r.Form["data"]
	if len(dataForm) == 0 {
		return
	}

	os.Stdout.WriteString(time.Now().String() + " " + r.RemoteAddr + "\n")

	w.Header().Set("Content-Type", "text/plain")

	ch := make(chan bool)

	host, _, err := net.SplitHostPort(r.RemoteAddr)
	if err != nil {
		os.Stdout.WriteString(r.RemoteAddr + ": " + err.Error() + "\n")
		w.Write([]byte("Bad address"))
	}

	ctx.ratech <- rateReq{host, ch}

	if <-ch {
		ctx.solverch <- solverReq{typeForm[0] == "ir", dataForm[0], w, ch}
		<-ch
	} else {
		w.Write([]byte("Rate limit exceeded"))
	}
}

func main() {
	var ctx context
	ctx.init()

	http.HandleFunc("/", ctx.rootHandler)
	http.HandleFunc("/solve", ctx.solveHandler)

	http.ListenAndServe(":8080", nil)
}

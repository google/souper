// REQUIRES: solver

// RUN: %clang++ -Xclang -load -Xclang %pass -O2 -std=c++11 -mllvm %solver -emit-llvm -S -o - %s

// Regression test for pull request #296
void *a;
class c {
public:
  virtual bool aq(void *);
};
class d {
  friend class e;
  template <typename> void aq() { at()->aq(a); }
  c *at();
};
class e {
public:
  static d au() {
    d b;
    b.aq<e>();
  }
};
void f() { e::au(); }

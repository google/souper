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

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "souper/SMTLIB2/Solver.h"
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

using namespace llvm;
using namespace souper;

SMTLIBSolver::~SMTLIBSolver() {}

namespace {

class ProcessSMTLIBSolver : public SMTLIBSolver {
  std::string Name;
  SolverProgram Prog;
  std::vector<std::string> Args;
  std::vector<const char *> ArgPtrs;

public:
  ProcessSMTLIBSolver(std::string Name, SolverProgram Prog,
                      const std::vector<std::string> &Args)
      : Name(Name), Prog(Prog), Args(Args) {
    std::transform(Args.begin(), Args.end(), std::back_inserter(ArgPtrs),
                   [](const std::string &Arg) { return Arg.c_str(); });
    ArgPtrs.push_back(0);
  }

  std::string getName() const {
    return Name;
  }

  error_code isSatisfiable(StringRef Query, bool &Result,
                           unsigned Timeout) override {
    int InputFD;
    SmallString<64> InputPath;
    if (error_code EC =
            sys::fs::createTemporaryFile("input", "smt2", InputFD, InputPath))
      return EC;

    raw_fd_ostream InputFile(InputFD, true, /*unbuffered=*/true);
    InputFile << Query;
    InputFile.close();

    int OutputFD;
    SmallString<64> OutputPath;
    if (error_code EC =
            sys::fs::createTemporaryFile("output", "out", OutputFD, OutputPath))
      return EC;
    ::close(OutputFD);

    int ExitCode = Prog(Args, InputPath, OutputPath, Timeout);
    ::remove(InputPath.c_str());

    switch (ExitCode) {
    case -2:
      ::remove(OutputPath.c_str());
      return make_error_code(errc::timed_out);

    case -1:
      ::remove(OutputPath.c_str());
      return make_error_code(errc::executable_format_error);

    default: {
      OwningPtr<MemoryBuffer> MB;
      if (error_code EC = MemoryBuffer::getFile(OutputPath.str(), MB)) {
        ::remove(OutputPath.c_str());
        return EC;
      }

      if (MB->getBuffer() == "sat\n") {
        ::remove(OutputPath.c_str());
        Result = true;
        return error_code();
      } else if (MB->getBuffer() == "unsat\n") {
        ::remove(OutputPath.c_str());
        Result = false;
        return error_code();
      } else {
        ::remove(OutputPath.c_str());
        return make_error_code(errc::protocol_error);
      }
    }
    }
  }

};

}

SolverProgram souper::makeExternalSolverProgram(StringRef Path) {
  std::string PathStr = Path;
  return [PathStr](const std::vector<std::string> &Args, StringRef RedirectIn,
                   StringRef RedirectOut, unsigned Timeout) {
    std::vector<const char *> ArgPtrs;
    ArgPtrs.push_back(PathStr.c_str());
    std::transform(Args.begin(), Args.end(), std::back_inserter(ArgPtrs),
                   [](const std::string &Arg) { return Arg.c_str(); });
    ArgPtrs.push_back(0);

    const StringRef *Redirects[] = {&RedirectIn, &RedirectOut, &RedirectOut};
    return sys::ExecuteAndWait(PathStr, ArgPtrs.data(), 0, Redirects, Timeout);
  };
}

SolverProgram souper::makeInternalSolverProgram(int MainPtr(int argc,
                                                            char **argv)) {
  return [MainPtr](const std::vector<std::string> &Args, StringRef RedirectIn,
                   StringRef RedirectOut, unsigned Timeout) {
    int pid = fork();
    if (pid == 0) {
      int InFD = open(RedirectIn.str().c_str(), O_RDONLY);
      if (InFD == -1) _exit(1);
      int OutFD = open(RedirectOut.str().c_str(), O_WRONLY);
      if (OutFD == -1) _exit(1);

      close(STDIN_FILENO);
      close(STDOUT_FILENO);
      close(STDERR_FILENO);

      if (dup2(InFD, STDIN_FILENO) == -1) _exit(1);
      if (dup2(OutFD, STDOUT_FILENO) == -1) _exit(1);
      if (dup2(OutFD, STDERR_FILENO) == -1) _exit(1);

      rlimit rlim;
      if (getrlimit(RLIMIT_NOFILE, &rlim) == -1) _exit(1);

      for (unsigned fd = 3; fd != rlim.rlim_cur; ++fd) {
        close(fd);
      }

      std::vector<const char *> ArgPtrs;
      ArgPtrs.push_back("solver");
      std::transform(Args.begin(), Args.end(), std::back_inserter(ArgPtrs),
                     [](const std::string &Arg) { return Arg.c_str(); });

      int rv = MainPtr(ArgPtrs.size(), const_cast<char **>(ArgPtrs.data()));
      fflush(stdout);
      fflush(stderr);
      _exit(rv);
    } else {
      sys::ProcessInfo PI;
      PI.Pid = pid;
      PI = sys::Wait(PI, Timeout, /*WaitUntilTerminates=*/Timeout == 0);
      return PI.ReturnCode;
    }
  };
}

std::unique_ptr<SMTLIBSolver> souper::createBoolectorSolver(SolverProgram Prog) {
  return std::unique_ptr<SMTLIBSolver>(
      new ProcessSMTLIBSolver("Boolector", Prog, {"--smt2"}));
}

std::unique_ptr<SMTLIBSolver> souper::createCVC4Solver(SolverProgram Prog) {
  return std::unique_ptr<SMTLIBSolver>(
      new ProcessSMTLIBSolver("CVC4", Prog, {"--lang=smt"}));
}

std::unique_ptr<SMTLIBSolver> souper::createSTPSolver(SolverProgram Prog) {
  return std::unique_ptr<SMTLIBSolver>(
      new ProcessSMTLIBSolver("STP", Prog, {"--SMTLIB2"}));
}

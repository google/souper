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

#define DEBUG_TYPE "souper"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/SMTLIB2/Solver.h"
#include <fcntl.h>
#include <stdio.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <system_error>

using namespace llvm;
using namespace souper;

STATISTIC(Errors, "Number of SMT solver errors");
STATISTIC(Sats, "Number of satisfiable SMT queries");
STATISTIC(Timeouts, "Number of SMT solver timeouts");
STATISTIC(Unsats, "Number of unsatisfiable SMT queries");

SMTLIBSolver::~SMTLIBSolver() {}

namespace {

// Bare bones SMT-LIB parser; enough to parse a get-value response.
struct SMTLIBParser {
  const char *Begin, *End;

  char next() {
    while (Begin != End && (*Begin == ' ' || *Begin == '\n' || *Begin == '\r' ||
                            *Begin == '\t'))
      ++Begin;
    if (Begin == End)
      return 0;
    return *Begin++;
  }

  bool consumeExpected(char C, std::string &ErrStr) {
    char N = next();
    if (N != C) {
      ErrStr = std::string("expected '") + C + "'";
      return false;
    }
    return true;
  }

  // Begin is before a '('. Consume until (and including) matching ')'.
  bool consumeSExpr(std::string &ErrStr) {
    if (!consumeExpected('(', ErrStr))
      return false;

    unsigned Level = 1;
    while (Begin != End) {
      if (*Begin == '(')
        ++Level;
      if (*Begin == ')') {
        --Level;
        if (Level == 0) {
          ++Begin;
          return true;
        }
      }
      ++Begin;
    }

    ErrStr = "unexpected EOF";
    return false;
  }

  bool consumeBvName(std::string &ErrStr) {
    while (Begin != End && (*Begin != ' ' && *Begin != '\n' && *Begin != '\r' &&
                            *Begin != '\t'))
      ++Begin;
    if (Begin == End) {
      ErrStr = "unexpected EOF";
      return false;
    }
    return true;
  }

  APInt parseBinaryBitVector(std::string &ErrStr) {
    ErrStr.clear();
    const char *NumBegin = Begin;
    while (Begin != End && (*Begin == '0' || *Begin == '1'))
      ++Begin;
    const char *NumEnd = Begin;
    unsigned Width = NumEnd - NumBegin;

    return APInt(Width, StringRef(NumBegin, Width), 2);
  }

  APInt parseDecimalBitVector(std::string &ErrStr) {
    ErrStr.clear();
    if (!consumeExpected('_', ErrStr))
      return APInt();
    if (!consumeExpected('b', ErrStr))
      return APInt();
    if (!consumeExpected('v', ErrStr))
      return APInt();
    if (Begin == End || *Begin < '0' || *Begin > '9') {
      ErrStr = "expected integer";
      return APInt();
    }
    const char *NumBegin = Begin;
    while (Begin != End && *Begin >= '0' && *Begin <= '9')
      ++Begin;
    const char *NumEnd = Begin;

    char WidthChar = next();
    if (WidthChar < '0' || WidthChar > '9') {
      ErrStr = "expected integer width";
      return APInt();
    }

    unsigned Width = WidthChar - '0';
    while (Begin != End && *Begin >= '0' && *Begin <= '9')
      Width = Width*10 + (*Begin++ - '0');

    if (!consumeExpected(')', ErrStr))
      return APInt();

    return APInt(Width, StringRef(NumBegin, NumEnd - NumBegin), 10);
  }

  APInt parseHexBitVector(std::string &ErrStr) {
    ErrStr.clear();
    const char *NumBegin = Begin;
    while (Begin != End && ((*Begin >= '0' && *Begin <= '9') ||
           (*Begin >= 'a' && *Begin <= 'f') ||
           (*Begin >= 'A' && *Begin <= 'F')))
      ++Begin;
    const char *NumEnd = Begin;
    unsigned Width = NumEnd - NumBegin;

    return APInt(Width*4, StringRef(NumBegin, Width), 16);
  }

  APInt parseModel(std::string &ErrStr) {
    APInt Res;

    if (!consumeExpected('(', ErrStr))
      return Res;
    if (!consumeExpected('(', ErrStr))
      return Res;
    if (!consumeSExpr(ErrStr))
      if (!consumeBvName(ErrStr))
        return Res;
    if (consumeExpected('#', ErrStr)) {
      if (consumeExpected('x', ErrStr))
        Res = parseHexBitVector(ErrStr);
      else
        Res = parseBinaryBitVector(ErrStr);
    } else {
      Res = parseDecimalBitVector(ErrStr);
    }
    if (!consumeExpected(')', ErrStr))
      return Res;
    if (!consumeExpected(')', ErrStr))
      return Res;

    return Res;
  }
};

std::vector<APInt> ParseModels(StringRef Models, unsigned NumModels,
                               std::string &ErrStr) {
  SMTLIBParser P{Models.data(), Models.data() + Models.size()};

  std::vector<APInt> ModelVals;
  for (unsigned I = 0; I != NumModels; ++I) {
    ModelVals.push_back(P.parseModel(ErrStr));
    if (!ErrStr.empty())
      return ModelVals;
  }

  if (P.next() != 0)
    ErrStr = "unexpected extra input";

  return ModelVals;
}

class ProcessSMTLIBSolver : public SMTLIBSolver {
  std::string Name;
  bool Keep;
  SolverProgram Prog;
  bool SupportsModels;
  std::vector<std::string> Args;
  std::vector<const char *> ArgPtrs;

public:
  ProcessSMTLIBSolver(std::string Name, bool Keep, SolverProgram Prog,
                      bool SupportsModels, const std::vector<std::string> &Args)
      : Name(Name), Keep(Keep), Prog(Prog), SupportsModels(SupportsModels),
        Args(Args) {
    std::transform(Args.begin(), Args.end(), std::back_inserter(ArgPtrs),
                   [](const std::string &Arg) { return Arg.c_str(); });
    ArgPtrs.push_back(0);
  }

  std::string getName() const override {
    return Name;
  }

  bool supportsModels() const override {
    return SupportsModels;
  }

  std::error_code isSatisfiable(StringRef Query, bool &Result,
                                unsigned NumModels, std::vector<APInt> *Models,
                                unsigned Timeout) override {
    int InputFD;
    SmallString<64> InputPath;
    if (std::error_code EC =
            sys::fs::createTemporaryFile("input", "smt2", InputFD, InputPath)) {
      ++Errors;
      return EC;
    }

    raw_fd_ostream InputFile(InputFD, true, /*unbuffered=*/true);
    InputFile << Query;
    InputFile.close();

    int OutputFD;
    SmallString<64> OutputPath;
    if (std::error_code EC =
            sys::fs::createTemporaryFile("output", "out", OutputFD,
                                         OutputPath)) {
      ++Errors;
      return EC;
    }
    ::close(OutputFD);

    int ExitCode =
        Prog(Args, InputPath, OutputPath, /*ErrorPath=*/"/dev/null", Timeout);

    if (Keep) {
      llvm::errs() << "Solver input saved to " << InputPath << '\n';
    } else {
      ::remove(InputPath.c_str());
    }

    switch (ExitCode) {
    case -2:
      ::remove(OutputPath.c_str());
      ++Timeouts;
      return std::make_error_code(std::errc::timed_out);

    case -1:
      ::remove(OutputPath.c_str());
      ++Errors;
      return std::make_error_code(std::errc::executable_format_error);

    default: {
      llvm::ErrorOr<std::unique_ptr<MemoryBuffer>> MB =
          MemoryBuffer::getFile(OutputPath.str());
      if (std::error_code EC = MB.getError()) {
        ::remove(OutputPath.c_str());
        ++Errors;
        return EC;
      }

      if ((*MB)->getBuffer().startswith("sat\n")) {
        ::remove(OutputPath.c_str());
        Result = true;
        ++Sats;
        std::string ErrStr;
        if (Models) {
          *Models = ParseModels((*MB)->getBuffer().slice(4, StringRef::npos),
                                NumModels, ErrStr);
        }
        if (!ErrStr.empty())
          return std::make_error_code(std::errc::protocol_error);
        return std::error_code();
      } else if ((*MB)->getBuffer().startswith("unsat\n")) {
        ::remove(OutputPath.c_str());
        Result = false;
        ++Unsats;
        return std::error_code();
      } else {
        ::remove(OutputPath.c_str());
        ++Errors;
        return std::make_error_code(std::errc::protocol_error);
      }
    }
    }
  }

};

}

SolverProgram souper::makeExternalSolverProgram(StringRef Path) {
  std::string PathStr = Path;
  return [PathStr](const std::vector<std::string> &Args, StringRef RedirectIn,
                   StringRef RedirectOut, StringRef RedirectErr,
                   unsigned Timeout) {
    std::vector<const char *> ArgPtrs;
    ArgPtrs.push_back(PathStr.c_str());
    std::transform(Args.begin(), Args.end(), std::back_inserter(ArgPtrs),
                   [](const std::string &Arg) { return Arg.c_str(); });
    ArgPtrs.push_back(0);

    const StringRef *Redirects[] = {&RedirectIn, &RedirectOut, &RedirectErr};
    return sys::ExecuteAndWait(PathStr, ArgPtrs.data(), 0, Redirects, Timeout);
  };
}

SolverProgram souper::makeInternalSolverProgram(int MainPtr(int argc,
                                                            char **argv)) {
  return [MainPtr](const std::vector<std::string> &Args, StringRef RedirectIn,
                   StringRef RedirectOut, StringRef RedirectErr,
                   unsigned Timeout) {
    int pid = fork();
    if (pid == 0) {
      int InFD = open(RedirectIn.str().c_str(), O_RDONLY);
      if (InFD == -1) _exit(1);
      int OutFD = open(RedirectOut.str().c_str(), O_WRONLY);
      if (OutFD == -1) _exit(1);
      int ErrFD = open(RedirectErr.str().c_str(), O_WRONLY);
      if (ErrFD == -1) _exit(1);

      close(STDIN_FILENO);
      close(STDOUT_FILENO);
      close(STDERR_FILENO);

      if (dup2(InFD, STDIN_FILENO) == -1) _exit(1);
      if (dup2(OutFD, STDOUT_FILENO) == -1) _exit(1);
      if (dup2(ErrFD, STDERR_FILENO) == -1) _exit(1);

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

std::unique_ptr<SMTLIBSolver> souper::createBoolectorSolver(SolverProgram Prog,
                                                            bool Keep) {
  return std::unique_ptr<SMTLIBSolver>(
      new ProcessSMTLIBSolver("Boolector", Keep, Prog, false, {"--smt2"}));
}

std::unique_ptr<SMTLIBSolver> souper::createCVC4Solver(SolverProgram Prog,
                                                       bool Keep) {
  return std::unique_ptr<SMTLIBSolver>(
      new ProcessSMTLIBSolver("CVC4", Keep, Prog, true, {"--lang=smt"}));
}

std::unique_ptr<SMTLIBSolver> souper::createSTPSolver(SolverProgram Prog,
                                                      bool Keep) {
  return std::unique_ptr<SMTLIBSolver>(
      new ProcessSMTLIBSolver("STP", Keep, Prog, false, {"--SMTLIB2"}));
}

std::unique_ptr<SMTLIBSolver> souper::createZ3Solver(SolverProgram Prog,
                                                     bool Keep) {
  return std::unique_ptr<SMTLIBSolver>(
      new ProcessSMTLIBSolver("Z3", Keep, Prog, true, {"-smt2", "-in"}));
}

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

#include "souper/KVStore/KVImplSQLite.h"
#include "llvm/Support/CommandLine.h"
#include <sqlite3.h>

using namespace llvm;
using namespace souper;

static cl::opt<std::string> SQLiteDB("souper-sqlite", cl::init(""),
    cl::desc("SQLite database file"));

namespace souper {

const int DatabaseSchemaVersion = 1;

KVImplSQLite::KVImplSQLite() {
  int err = ::sqlite3_open(SQLiteDB.c_str(), &DB);
  if (err != SQLITE_OK) {
    llvm::report_fatal_error((llvm::StringRef)"SQLite connection error: " +
                             sqlite3_errstr(err) + "\n");
  }

  // TODO: Create the tables if they doesn't exist
  // TODO: Check the metadata table for the schema version
}

KVImplSQLite::~KVImplSQLite() {
  int err = ::sqlite3_close(DB);
  if (err != SQLITE_OK) {
    llvm::report_fatal_error(
        (llvm::StringRef)"Failed to close SQLite database: " +
        sqlite3_errstr(err) + "\n");
  }
}

void KVImplSQLite::hIncrBy(llvm::StringRef Key, llvm::StringRef Field,
                              int Incr) {
  //pass
}

bool KVImplSQLite::hGet(llvm::StringRef Key, llvm::StringRef Field,
                           std::string &Value) {
  return false;
}

void KVImplSQLite::hSet(llvm::StringRef Key, llvm::StringRef Field,
                              llvm::StringRef Value) {
  //pass
}

}

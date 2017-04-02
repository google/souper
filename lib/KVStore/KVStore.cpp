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

#include "souper/KVStore/KVStore.h"

#if WITH_REDIS
#include "souper/KVStore/KVImplRedis.h"
#endif
#if WITH_SQLITE
#include "souper/KVStore/KVImplSQLite.h"
#endif

#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace souper;

enum StoreKinds {
#if WITH_REDIS
  SKRedis,
#endif
#if WITH_SQLITE
  SKSQLite
#endif
};

cl::opt<StoreKinds> StoreKind("store-kind",
    cl::desc("Persistent storage provider"),
    cl::values(
#if WITH_REDIS
       clEnumValN(SKRedis, "redis", "use Redis"),
#endif
#if WITH_SQLITE
       clEnumValN(SKSQLite, "sqlite", "use SQLite"),
#endif
      clEnumValEnd));

namespace souper {

KVStore::KVStore() {
  switch (StoreKind) {
#if WITH_REDIS
  SKRedis:
    Impl = std::unique_ptr<KVImpl>(new KVImplRedis());
    break;
#endif
#if WITH_SQLITE
  SKSQLite:
    Impl = std::unique_ptr<KVImpl>(new KVImplSQLite());
    break;
#endif
  default:
    llvm::report_fatal_error("unsupported persistent storage provider");
  }
}

KVStore::~KVStore() {}

void KVStore::hIncrBy(llvm::StringRef Key, llvm::StringRef Field, int Incr) {
  Impl->hIncrBy(Key, Field, Incr);
}

bool KVStore::hGet(llvm::StringRef Key, llvm::StringRef Field,
                   std::string &Value) {
  return Impl->hGet(Key, Field, Value);
}

void KVStore::hSet(llvm::StringRef Key, llvm::StringRef Field,
                   llvm::StringRef Value) {
  Impl->hSet(Key, Field, Value);
}

}

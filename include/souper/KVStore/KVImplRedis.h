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

#ifndef SOUPER_KVSTORE_KVIMPLREDIS_H
#define SOUPER_KVSTORE_KVIMPLREDIS_H

#if !WITH_REDIS
#error Redis support is disabled
#endif

#include "souper/KVStore/KVStore.h"
#include "llvm/ADT/StringRef.h"
#include "hiredis.h"

namespace souper {

class KVImplRedis : public KVImpl {
  redisContext *Ctx;
public:
  KVImplRedis();
  ~KVImplRedis();
  void hIncrBy(llvm::StringRef Key, llvm::StringRef Field, int Incr);
  bool hGet(llvm::StringRef Key, llvm::StringRef Field, std::string &Value);
  void hSet(llvm::StringRef Key, llvm::StringRef Field, llvm::StringRef Value);
};

}

#endif  // SOUPER_KVSTORE_KVIMPLREDIS_H

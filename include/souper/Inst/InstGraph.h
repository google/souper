// Copyright 2019 The Souper Authors. All rights reserved.
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

#ifndef SOUPER_INSTGRAPH_H
#define SOUPER_INSTGRAPH_H

#include <string>
#include <set>

#include "souper/Inst/Inst.h"

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/DOTGraphTraits.h"

template<> struct llvm::GraphTraits<souper::Inst*> {
  using NodeRef = souper::Inst*;

  static NodeRef getEntryNode(souper::Inst* instr) { return instr; }

  using ChildIteratorType = std::vector<NodeRef>::iterator;

  static ChildIteratorType child_begin(NodeRef N) {
    return N->Ops.begin();
  }

  static ChildIteratorType child_end(NodeRef N) {
    return N->Ops.end();
  }

  using nodes_iterator = llvm::df_iterator<NodeRef>;

  static nodes_iterator nodes_begin(souper::Inst* I) {
    return nodes_iterator::begin(getEntryNode(I));
  }

  static nodes_iterator nodes_end(souper::Inst* I) {
    return nodes_iterator::end(getEntryNode(I));
  }
};

template<> struct llvm::DOTGraphTraits<souper::Inst*> : public llvm::DefaultDOTGraphTraits {
  DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string getGraphName(souper::Inst* instr) { return "Souper IR graph"; }

  std::string getNodeLabel(souper::Inst* instr, souper::Inst* root) {
    switch(instr->K) {
    case souper::Inst::Kind::ReservedConst:
      return "ReservedConst";
    case souper::Inst::Kind::ReservedInst:
      return "ReservedInst";
    case souper::Inst::Kind::Var:
      return "Var " + instr->Name;
    case souper::Inst::Kind::Const:
      return instr->Val.toString(10, false);
    default:
      return std::string(souper::Inst::getKindName(instr->K));
    }
  }

  static std::string getNodeAttributes(const souper::Inst* instr, const souper::Inst* root) {
    if (instr == root)
      return "style=bold";

    return "";
  }

  static bool renderGraphFromBottomUp() { return true; }

  static std::string getNodeIdentifierLabel(souper::Inst* instr, souper::Inst* root) {
    return "\"" + std::to_string(reinterpret_cast<intptr_t>(instr)) + "\"";
  }
};

#endif

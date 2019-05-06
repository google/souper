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

#ifndef SOUPER_INST_INST_H
#define SOUPER_INST_INST_H

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/IR/ConstantRange.h"
#include "llvm/IR/Value.h"

#include "souper/SMTLIB2/Solver.h"

#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

namespace souper {

enum class HarvestType { HarvestedFromDef, HarvestedFromUse };

const unsigned MaxPreds = 100000;
const std::string ReservedConstPrefix = "reservedconst_";
const std::string ReservedInstPrefix = "reservedinst";

struct Inst;

struct Block {
  std::string Name;
  unsigned Preds;
  unsigned Number;
  std::vector<Inst *> PredVars;
};

struct Inst : llvm::FoldingSetNode {
  typedef enum {
    Const,
    UntypedConst,
    Var,
    Phi,
    Hole,

    Add,
    AddNSW,
    AddNUW,
    AddNW,
    Sub,
    SubNSW,
    SubNUW,
    SubNW,
    Mul,
    MulNSW,
    MulNUW,
    MulNW,
    UDiv,
    SDiv,
    UDivExact,
    SDivExact,
    URem,
    SRem,
    And,
    Or,
    Xor,
    Shl,
    ShlNSW,
    ShlNUW,
    ShlNW,
    LShr,
    LShrExact,
    AShr,
    AShrExact,
    Select,
    ZExt,
    SExt,
    Trunc,
    Eq,
    Ne,
    Ult,
    Slt,
    Ule,
    Sle,
    CtPop,
    BSwap,
    Cttz,
    Ctlz,
    BitReverse,
    FShl,
    FShr,
    ExtractValue,
    SAddWithOverflow,
    SAddO,
    UAddWithOverflow,
    UAddO,
    SSubWithOverflow,
    SSubO,
    USubWithOverflow,
    USubO,
    SMulWithOverflow,
    SMulO,
    UMulWithOverflow,
    UMulO,
    SAddSat,
    UAddSat,
    SSubSat,
    USubSat,

    ReservedConst,
    ReservedInst,

    None,
} Kind;

  Kind K;
  unsigned Number;
  unsigned Width;
  Block *B;
  bool Available = true;
  llvm::APInt Val;
  std::string Name;
  std::unordered_set<Inst *> DepsWithExternalUses;
  std::vector<Inst *> Ops;
  mutable std::vector<Inst *> OrderedOps;
  std::vector<llvm::Value *> Origins;

  bool operator<(const Inst &I) const;
  const std::vector<Inst *> &orderedOps() const;
  bool hasOrigin(llvm::Value *V) const;

  void Profile(llvm::FoldingSetNodeID &ID) const;

  static const char *getKindName(Kind K);
  static std::string getKnownBitsString(llvm::APInt Zero, llvm::APInt One);
  static std::string getMoreKnownBitsString(bool NonZero, bool NonNegative,
                                            bool PowOfTwo, bool Negative);
  static std::string getDemandedBitsString(llvm::APInt DBVal);
  static Kind getKind(std::string Name);

  static bool isAssociative(Kind K);
  static bool isCmp(Kind K);
  static bool isCommutative(Kind K);
  static bool isShift(Kind K);
  static int getCost(Kind K);
  llvm::APInt KnownZeros;
  llvm::APInt KnownOnes;
  bool NonZero;
  bool NonNegative;
  bool PowOfTwo;
  bool Negative;
  unsigned NumSignBits;
  llvm::APInt DemandedBits;
  unsigned SynthesisConstID;
  HarvestType HarvestKind;
  llvm::BasicBlock* HarvestFrom;
  llvm::ConstantRange Range=llvm::ConstantRange(1);
};

/// A mapping from an Inst to a replacement. This may either represent a
/// path condition or a candidate replacement.
struct InstMapping {
  InstMapping() : LHS(0), RHS(0) {}
  InstMapping(Inst *LHS, Inst *RHS)
      : LHS(LHS), RHS(RHS) {}

  Inst *LHS, *RHS;
};

struct BlockPCMapping {
  BlockPCMapping() : B(0) {}
  BlockPCMapping(Block *B, unsigned I, InstMapping PC)
      : B(B), PredIdx(I), PC(PC) {}

  Block *B;
  unsigned PredIdx;
  InstMapping PC;
};

typedef std::vector<BlockPCMapping> BlockPCs;

class ReplacementContext {
  llvm::DenseMap<Inst *, std::string> InstNames;
  llvm::DenseMap<Block *, std::string> BlockNames;
  std::map<std::string, Inst *> NameToInst;
  std::map<std::string, Block *> NameToBlock;
  std::string printInstImpl(Inst *I, llvm::raw_ostream &Out, bool printNames, Inst *OrigI);

public:
  void printPCs(const std::vector<InstMapping> &PCs,
                llvm::raw_ostream &Out, bool printNames);
  void printBlockPCs(const BlockPCs &BPCs,
                     llvm::raw_ostream &Out, bool printNames);
  std::string printInst(Inst *I, llvm::raw_ostream &Out, bool printNames);
  std::string printBlock(Block *B, llvm::raw_ostream &Out);
  Inst *getInst(llvm::StringRef Name);
  void setInst(llvm::StringRef Name, Inst *I);
  Block *getBlock(llvm::StringRef Name);
  void setBlock(llvm::StringRef Name, Block *B);
  void clear();
  bool empty();
};

class InstContext {
  typedef llvm::DenseMap<unsigned, std::vector<std::unique_ptr<Block>>>
      BlockMap;
  BlockMap BlocksByPreds;

  typedef llvm::DenseMap<unsigned, std::vector<std::unique_ptr<Inst>>> InstMap;
  InstMap VarInstsByWidth;

  std::vector<std::unique_ptr<Inst>> Insts;
  llvm::FoldingSet<Inst> InstSet;
  unsigned ReservedConstCounter = 0;

public:
  Inst *getConst(const llvm::APInt &I);
  Inst *getUntypedConst(const llvm::APInt &I);
  Inst *getReservedConst();
  Inst *getReservedInst();

  Inst *createHole(unsigned Width);
  Inst *createSynthesisConstant(unsigned Width, unsigned SynthesisConstID);
  Inst *createVar(unsigned Width, llvm::StringRef Name);
  Inst *createVar(unsigned Width, llvm::StringRef Name,
                  llvm::ConstantRange Range,
                  llvm::APInt Zero, llvm::APInt One,
                  bool NonZero, bool NonNegative, bool PowOfTwo,
                  bool Negative, unsigned NumSignBits, unsigned SynthesisConstID);
  Block *createBlock(unsigned Preds);

  Inst *getPhi(Block *B, const std::vector<Inst *> &Ops);
  Inst *getPhi(Block *B, const std::vector<Inst *> &Ops, llvm::APInt Demandedbits);

  Inst *getInst(Inst::Kind K, unsigned Width, const std::vector<Inst *> &Ops,
                bool Available=true);
  Inst *getInst(Inst::Kind K, unsigned Width, const std::vector<Inst *> &Ops,
                llvm::APInt DemandedBits, bool Available);
};

struct SynthesisContext {
  InstContext &IC;
  SMTLIBSolver *SMTSolver;
  Inst *LHS;
  Inst *LHSUB;
  const std::vector<InstMapping> &PCs;
  const BlockPCs &BPCs;
  unsigned Timeout;
};

int cost(Inst *I, bool IgnoreDepsWithExternalUses = false);
int instCount(Inst *I);
int benefit(Inst *LHS, Inst *RHS);

void PrintReplacement(llvm::raw_ostream &Out, const BlockPCs &BPCs,
                      const std::vector<InstMapping> &PCs, InstMapping Mapping,
                      bool printNames = false);
std::string GetReplacementString(const BlockPCs &BPCs,
                                 const std::vector<InstMapping> &PCs,
                                 InstMapping Mapping,
                                 bool printNames = false);
void PrintReplacementLHS(llvm::raw_ostream &Out, const BlockPCs &BPCs,
                         const std::vector<InstMapping> &PCs,
                         Inst *LHS, ReplacementContext &Context,
                         bool printNames = false);
std::string GetReplacementLHSString(const BlockPCs &BPCs,
                                    const std::vector<InstMapping> &PCs,
                                    Inst *LHS, ReplacementContext &Context,
                                    bool printNames = false);
void PrintReplacementRHS(llvm::raw_ostream &Out, Inst *RHS,
                         ReplacementContext &Context,
                         bool printNames = false);
std::string GetReplacementRHSString(Inst *RHS, ReplacementContext &Context,
                                    bool printNames = false);

void findCands(Inst *Root, std::vector<Inst *> &Guesses,
               bool WidthMustMatch, bool FilterVars, int Max);

Inst *getInstCopy(Inst *I, InstContext &IC,
                  std::map<Inst *, Inst *> &InstCache,
                  std::map<Block *, Block *> &BlockCache,
                  std::map<Inst *, llvm::APInt> *ConstMap,
                  bool CloneVars);

Inst *instJoin(Inst *I, Inst *Reserved, Inst *NewInst, InstContext &IC);

void findVars(Inst *Root, std::vector<Inst *> &Vars);

bool hasGivenInst(Inst *Root, std::function<bool(Inst*)> InstTester);
void getHoles(Inst *Root, std::vector<Inst *> &Holes);
void getConstants(Inst *Root, std::set<Inst *> &ConstSet);

void getReservedInsts(Inst *Root, std::vector<Inst *> &ReservedInsts);

void separateBlockPCs(const BlockPCs &BPCs, BlockPCs &BPCsCopy,
                      std::map<Inst *, Inst *> &InstCache,
                      std::map<Block *, Block *> &BlockCache,
                      InstContext &IC,
                      std::map<Inst *, llvm::APInt> *ConstMap,
                      bool CloneVars);

void separatePCs(const std::vector<InstMapping> &PCs,
                 std::vector<InstMapping> &PCsCopy,
                 std::map<Inst *, Inst *> &InstCache,
                 std::map<Block *, Block *> &BlockCache,
                 InstContext &IC,
                 std::map<Inst *, llvm::APInt> *ConstMap,
                 bool CloneVars);

}

#endif  // SOUPER_INST_INST_H

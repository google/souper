// Copyright 2015 The Souper Authors. All rights reserved.
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

#ifndef SOUPER_INST_SYNTHESIS_H
#define SOUPER_INST_SYNTHESIS_H

#include "souper/Inst/Inst.h"
#include "souper/SMTLIB2/Solver.h"

#include <map>
#include <set>
#include <vector>
#include <utility>

/// This class is based on the "Synthesis of Loop-free Programs" approach
/// by Gulwani et al. presented at PLDI'11.
/// Paper:  http://dl.acm.org/citation.cfm?id=1993506
/// Slides: http://www.eecs.berkeley.edu/~jha/pres/pldi11.pdf
/// The implementation is derived from the description in the paper, but it
/// also contains additional steps that were necessary to apply this synthesis
/// method in Souper.

namespace souper {

/// A location variable identifies an input variable, the output variable, or
/// input/output of a library component. The encoding has the following
/// semantics: If the first integer is 0, it's an input var or a constant
/// where the second integer describes its number. If the second integer is 0,
/// it's an output var and the first integer describes the number of a
/// component. The output variable is encoded by second integer being 0 and
/// the first integer being equal to the number of library components plus one.
/// Otherwise, it's a component input where the first integer describes the
/// number of a component and the second integer describes it's input number.
/// Note that constant components (no inputs, one output) are not treated as
/// ordinary library components, but rather as inputs.
/// For example:
/// 0_1, 0_2, 0_3 describe the input variables 1, 2, and 3.
/// 1_0, 2_0, 3_0 describe the outputs of components 1, 2, and 3.
/// 1_1, 2_2, 3_1 describe the first input of component one, the second input of
/// component two, and the first input of component three.
/// 4_0 describes the final output (assuming there are only three components)
typedef std::pair<unsigned, unsigned> LocVar;

/// A mapping from a location variable to its concrete value (Inst::Const).
/// The values of location variables decide the interconnections between the
/// various components. To describe a program, we have to determine which
/// component goes on which location (line-number), and from which location
/// (line-number of program input) does it get its input arguments. E.g.,
/// the location mappings
/// 0_1 -> 0
/// 1_1 -> 0
/// 1_0 -> 1
/// 2_0 -> 1
/// describe a program with one input (0_1) and one component with one
/// input (1_1) located at line-number 0, producing output (1_0) at line-number 1
/// which is the output of the program (2_0).
/// Initially, the values of location variables are symbolic and it is the job
/// of the SMT solver to figure out a valid wiring representing a valid program
typedef std::pair<LocVar, Inst *> LocInst;

/// A component is a fixed-width instruction kind
struct Component {
  Inst::Kind Kind;
  unsigned Width;
  std::vector<unsigned> OpWidths;
};

/// Unsupported components kinds
static const std::set<Inst::Kind> UnsupportedCompKinds = {
  Inst::Var, Inst::UntypedConst,
  // Not implemented
  Inst::Phi,
  // Disabled due to scalability challenges
  Inst::AddNSW, Inst::AddNUW, Inst::AddNW,
  Inst::SubNSW, Inst::SubNUW, Inst::SubNW,
  Inst::MulNSW, Inst::MulNUW, Inst::MulNW,
  Inst::ShlNSW, Inst::ShlNUW, Inst::ShlNW,
};

/// Supported component library.
/// The width of most of the instructions is unknown in advance, therefore, we
/// indicate this by setting the Width to 0. Per each input of distinct width,
/// a component of that width is instantiated.
/// Again, note that constants are treated as ordinary inputs
static const std::vector<Component> CompLibrary = {
  Component{Inst::Add, 0, {0,0}},
  Component{Inst::Sub, 0, {0,0}},
  Component{Inst::Mul, 0, {0,0}},
  Component{Inst::UDiv, 0, {0,0}},
  Component{Inst::SDiv, 0, {0,0}},
  Component{Inst::UDivExact, 0, {0,0}},
  Component{Inst::SDivExact, 0, {0,0}},
  Component{Inst::URem, 0, {0,0}},
  Component{Inst::SRem, 0, {0,0}},
  Component{Inst::And, 0, {0,0}},
  Component{Inst::Or, 0, {0,0}},
  Component{Inst::Xor, 0, {0,0}},
  Component{Inst::Shl, 0, {0,0}},
  Component{Inst::LShr, 0, {0,0}},
  Component{Inst::LShrExact, 0, {0,0}},
  Component{Inst::AShr, 0, {0,0}},
  Component{Inst::AShrExact, 0, {0,0}},
  Component{Inst::Select, 0, {1,0,0}},
  Component{Inst::Eq, 1, {0,0}},
  Component{Inst::Ne, 1, {0,0}},
  Component{Inst::Ult, 1, {0,0}},
  Component{Inst::Slt, 1, {0,0}},
  Component{Inst::Ule, 1, {0,0}},
  Component{Inst::Sle, 1, {0,0}},
  //
  Component{Inst::CtPop, 0, {0}},
  Component{Inst::BSwap, 0, {0}},
  Component{Inst::Cttz, 0, {0}},
  Component{Inst::Ctlz, 0, {0}}
};

class InstSynthesis {
public:
  // Synthesize an instruction from the specification in LHS
  std::error_code synthesize(SMTLIBSolver *SMTSolver,
                             const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *TargetLHS, Inst *&RHS,
                             InstContext &IC, unsigned Timeout);

private:
  /// LHS to synthesize
  Inst *LHS;
  /// Replacement context for inst printing
  ReplacementContext Context;
  /// Components to be used
  std::vector<Component> Comps;
  /// Constant components
  std::vector<Component> ConstComps;
  /// Program inputs
  std::vector<Inst *> Inputs;
  /// Default component width
  unsigned DefaultWidth = 0;
  /// Input location set I
  std::vector<LocInst> I;
  /// Component input location set P
  std::vector<LocInst> P;
  /// Component output location set R
  std::vector<LocInst> R;
  /// Output location O
  LocInst O;
  /// Location variables set L (I \cup P \cup R \cup {O}).
  std::vector<LocInst> L;
  /// Number of input variables
  unsigned N;
  /// Number of components + N
  unsigned M;
  /// A mapping from a location variable to a concrete component instance,
  /// namely created instruction
  std::map<LocVar, Inst *> CompInstMap;
  /// Location variable's width (increase for support of >256 comps+inputs)
  const unsigned LocInstWidth = 8;
  /// A mapping from a location variable's string representation to its location.
  /// Required during model parsing
  std::map<std::string, LocInst> LocInstMap;
  /// Invalid wirings
  std::set<std::pair<LocVar, LocVar>> InvalidWirings;

  /// Initialize components to be used during synthesis
  void setCompLibrary();

  /// Initalize input variable locations
  void initInputVars(InstContext &IC);

  /// Initalize components' input locations, output locations,
  /// and components' concrete instruction instances
  void initComponents(InstContext &IC);

  /// Initalize constant components
  void initConstComponents(InstContext &IC);

  /// Initalize output location
  void initOutput(InstContext &IC);

  /// Initalize locations L
  void initLocations();

  /// Print some debug info on initial synthesis settings
  void printInitInfo();

  /// Set particular wirings to be invalid (e.g. width mismatches).
  /// These invalid wirings are not encoded as constraints, they are
  /// simply skipped during connectivity constraint creation
  void setInvalidWirings();

  /// Every line in the program has at most one component.
  /// Don't wire semantically invalid variable locations,
  /// e.g., a component's output to its inputs
  Inst *getConsistencyConstraint(InstContext &IC);

  /// In a well-formed program, every variable is initialized
  /// before it is used. For every component, if x is an input
  /// of that component and y is an output of that component,
  /// then the location l_x where the input is defined, should
  /// be earlier than the location l_y where the component is
  /// positioned and its output is defined
  Inst *getAcyclicityConstraint(InstContext &IC);

  /// phi_P: Forall x in P \cup I: 0 <= l_x <= M-1
  /// phi_R: Forall x in R: |N| <= l_x <= M-1
  Inst *getLocVarConstraint(InstContext &IC);

  /// Begin <= O < End
  Inst *getOutputLocVarConstraint(int Begin, int End, InstContext &IC);

  /// Each component's input should be wired either to an input
  /// or to a component's output
  Inst *getInputDefinednessConstraint(InstContext &IC);

  /// Output must be wired to either a component's output or input(s)
  Inst *getOutputDefinednessConstraint(InstContext &IC);

  /// phi_conn := Forall x,y \in P \cup R \cup I \cup {O}: (l_x = l_y) => x = y.
  /// Given an interconnection among components specified by values of location
  /// variables L, we can relate the input/output variables of the components
  /// and the program
  Inst *getConnectivityConstraint(InstContext &IC);

  /// Create a copy of instruction I replacing its input vars with vars
  /// in Replacements
  Inst *getInstCopy(Inst *I, InstContext &IC,
                    const std::map<Inst *, Inst *> &Replacements);

  /// A mapping from program locations (line numbers) to a set of component
  /// location variables
  typedef std::map<unsigned, std::set<LocVar>> LineLocVarMap;

  typedef std::vector<std::pair<LocInst, LocInst>> ProgramWiring;

  typedef std::pair<const std::vector<Inst *>&,
                    const std::vector<llvm::APInt>&> SolverSolution;

  /// Create a program from a solver model
  Inst *createInstFromModel(const SolverSolution &Solution,
                            ProgramWiring &CandWiring,
                            std::map<LocVar, llvm::APInt> &ConstValMap,
                            InstContext &IC);

  /// Recursive instruction creation from a given program wiring
  Inst *createInstFromWiring(const LocVar &OutLoc,
                             const std::vector<LocVar> &OpLocs,
                             const LineLocVarMap &LineWiring,
                             const std::map<LocVar, llvm::APInt> &ConstValMap,
                             ProgramWiring &CandWiring,
                             InstContext &IC);

  /// Parse wiring models extracting concrete values for location variables
  /// and constants. Return location variable that matches the output
  LocVar parseWiringModel(const SolverSolution &Solution,
                          LineLocVarMap &LineWiring,
                          std::map<LocVar, llvm::APInt> &ConstValMap);

  /// Find a wiring input for given location variable Loc.
  /// The result is either an input, a constant, or a component output
  LocVar getWiringLocVar(const LocVar &Loc, const LineLocVarMap &LineWiring);

  /// Create a junk-free inst. E.g., return %0 if inst is of type and %0, %0
  Inst *createJunkFreeInst(Inst::Kind Kind, unsigned Width,
                           std::vector<Inst *> &Ops, InstContext &IC);

  /// Helper functions
  void filterFixedWidthIntrinsicComps();
  void getInputVars(Inst *I, std::vector<Inst *> &InputVars);
  std::string getLocVarStr(const LocVar &Loc, const std::string Prefix="");
  LocVar getLocVarFromStr(const std::string &Str);
  std::vector<LocVar> getOpLocs(const LocVar &Loc);
  std::vector<std::string> splitString(const char *S, char Del=',');
  bool isWiringInvalid(const LocVar &Left, const LocVar &Right);
  void forbidInvalidCandWiring(const ProgramWiring &CandWiring,
                               std::vector<InstMapping> &LoopPCs,
                               InstContext &IC);
  int costHelper(Inst *I, std::set<Inst *> &Visited);
  int cost(Inst *I);
  bool hasConst(Inst *I);

};

}

#endif  // SOUPER_INST_SYNTHESIS_H

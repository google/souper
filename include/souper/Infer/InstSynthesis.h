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

#ifndef SOUPER_INST_SYNTHESIS_H
#define SOUPER_INST_SYNTHESIS_H

#include "souper/Inst/Inst.h"
#include "souper/SMTLIB2/Solver.h"

#include <map>
#include <set>
#include <vector>
#include <utility>

/*
  This class is based on the "Synthesis of Loop-free Programs" approach
  by Gulwani et al. presented at PLDI'11.
  Paper:  http://dl.acm.org/citation.cfm?id=1993506
  Slides: http://www.eecs.berkeley.edu/~jha/pres/pldi11.pdf

  The implementation is derived from the description in the paper, but it
  also contains additional steps that were necessary to apply this synthesis
  method in Souper.
*/

namespace souper {

/// A location variable identifies an input variable, the output variable, or
/// input/output of a library component. The encoding has the following
/// semantics: If the first integer is 0, it's an input var where the second
/// integer describes the number of an input variable. If the second integer
/// is 0, it's an output var and the first integer describes the number of a
/// component. The output variable is encoded by second integer being 0 and
/// the first integer being equal to the number of library components plus one.
/// Otherwise, it's a component input where the first integer describes the
/// number of a component and the second integer describes it's input number.
/// For example:
/// 0_1, 0_2, 0_3 describe the input variables 1, 2, and 3.
/// 1_0, 2_0, 3_0 describe the outputs of components 1, 2, and 3.
/// 1_1, 2_2, 3_1 describe the first input of component one, the second input of
/// component two, and the first input of component three.
/// 4_0 describes the final output (assuming there are only three components)
typedef std::pair<unsigned, unsigned> LocVar;

/// A mapping from a location variable to it's concrete value (Inst::Const).
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
  unsigned OpNum;
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
  Inst::AddNSW, Inst::AddNUW, Inst::AddNW,
};

/// Supported component library.
/// The width of most of the instructions is unknown in advance, therefore, we
/// indicate this by setting the Width to 0. During initialization, the width
/// will be set to the DefaultInstWidth (maximum width of the input vars).
static const std::vector<Component> CompLibrary = {
  // Reserved: Set the const width to the output width (constant synthesis)
  Component{Inst::Const, static_cast<unsigned>(~0), 0},
  Component{Inst::Const, 0, 0},
  //
  Component{Inst::Add, 0, 2},
  Component{Inst::Sub, 0, 2},
  Component{Inst::Mul, 0, 2},
  Component{Inst::UDiv, 0, 2},
  Component{Inst::SDiv, 0, 2},
  Component{Inst::UDivExact, 0, 2},
  Component{Inst::SDivExact, 0, 2},
  Component{Inst::URem, 0, 2},
  Component{Inst::SRem, 0, 2},
  Component{Inst::And, 0, 2},
  Component{Inst::Or, 0, 2},
  Component{Inst::Xor, 0, 2},
  Component{Inst::Shl, 0, 2},
  Component{Inst::LShr, 0, 2},
  Component{Inst::LShrExact, 0, 2},
  Component{Inst::AShr, 0, 2},
  Component{Inst::AShrExact, 0, 2},
  Component{Inst::Select, 0, 3},
  //
  Component{Inst::ZExt, 0, 1},
  Component{Inst::SExt, 0, 1},
  //
  Component{Inst::Trunc, 1, 1},
  //
  Component{Inst::Eq, 1, 2},
  Component{Inst::Ne, 1, 2},
  Component{Inst::Ult, 1, 2},
  Component{Inst::Slt, 1, 2},
  Component{Inst::Ule, 1, 2},
  Component{Inst::Sle, 1, 2},
  //
  Component{Inst::CtPop, 0, 1},
  Component{Inst::BSwap, 0, 1},
  Component{Inst::Cttz, 0, 1},
  Component{Inst::Ctlz, 0, 1}
};

class InstSynthesis {
public:
  // If Comps != 0 and MaxCompNum < 0, use only component kinds
  // from Comps during synthesis. If MaxCompNum == 0, inputs are wired
  // directly to the output (nop synthesis)
  InstSynthesis(const std::vector<Inst::Kind> *Comps=0, int MaxCompNum=-1);

  // Synthesize an instruction for the specification in LHS
  std::error_code synthesize(SMTLIBSolver *SMTSolver,
                             const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *LHS, Inst *&RHS, InstContext &IC,
                             unsigned Timeout);

private:
  /// Components to be used
  std::vector<Component> Comps;
  /// Max number of components in the synthesized result.
  ///   <0 -> use all available components in the library.
  ///    0 -> do nop synthesis (no components, use just inputs).
  ///   >0 -> use the number of specified components.
  /// Note that Const inst comprises several components with different widths
  int MaxCompNum;
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
  unsigned N = 0;
  /// Number of components + N
  unsigned M = 0;
  /// A mapping from a location variable to a concrete component instance,
  /// namely created instruction
  std::map<LocVar, Inst *> CompInstMap;
  /// Default component inst width
  unsigned DefaultInstWidth = 0;
  /// LocInst width is fixed
  unsigned const LocInstWidth = 32;
  /// A mapping from a location variable's string representation to it's location.
  /// Required during model parsing
  std::map<std::string, LocInst> LocInstMap;
  /// Invalid wirings
  std::set<std::pair<LocInst, LocInst>> InvalidWirings;

  /// Initialize components to be used during synthesis
  void setCompLibrary(const std::vector<Inst::Kind> *UserCompKinds);

  /// Get input variables
  void getInputVars(Inst *I, std::set<Inst *> &InputVars);

  /// Initalize input variable locations
  void initInputVars(Inst *LHS, InstContext &IC);

  /// Initalize components' input locations, output locations,
  /// and components' concrete instruction instances
  void initComponents(InstContext &IC);

  /// Initalize output location
  void initOutput(Inst *LHS, InstContext &IC);

  /// Initalize locations L
  void initLocations();

  /// Print some debug info on initial synthesis settings
  void printInitInfo();

  /// Create a fresh location variable
  Inst *createLocVarInst(const LocVar &Loc, InstContext &IC);

  /// Add all synthesis constraints as path conditions
  void addConstraints(std::vector<InstMapping> &PCs, InstContext &IC);

  /// Don't wire location variables if components' widths don't match
  Inst *getWidthConstraint(InstContext &IC);

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
  /// phi_R: Forall x in R \cup {O}: |N| <= l_x <= M-1
  /// If MaxCompNum >= 0, then
  /// phi_R: Forall x in R \cup {O}: |N| <= l_x <= |N|+MaxCompNum
  Inst *getLocVarConstraint(InstContext &IC);

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

  /// Create a copy of instruction I replacing it's input vars with vars
  /// in Replacements
  Inst *getInstCopy(Inst *I, InstContext &IC,
                    const std::map<Inst *, Inst *> &Replacements);

  /// Create a program from a solver model
  Inst *createInstFromModel(const std::vector<Inst *> &ModelInsts,
                            const std::vector<llvm::APInt> &ModelVals,
                            InstContext &IC);

  /// A mapping from program locations (line numbers) to a set of component
  /// location variables
  typedef std::map<unsigned, std::set<LocVar>> LineLocVarMap;

  /// Recursive instruction creation from a given program wiring
  Inst *createInstFromWiring(const LocVar &OutLoc,
                             const std::vector<LocVar> &OpLocs,
                             const LineLocVarMap &ProgramWiring,
                             const std::map<LocVar, llvm::APInt> &ConstValMap,
                             InstContext &IC);

  /// Parse wiring models extracting concrete values for location variables
  /// and constants. Return location variable that matches the output
  LocVar parseWiringModel(const std::vector<Inst *> &ModelInsts,
                          const std::vector<llvm::APInt> &ModelVals,
                          LineLocVarMap &ProgramWiring,
                          std::map<LocVar, llvm::APInt> &ConstValMap);

  /// Find a wiring input for given location variable Loc.
  /// The result is either an input or a component output
  LocVar getWiringLocVar(const LocVar &Loc, const LineLocVarMap &ProgramWiring);

  /// Helper functions
  std::string getLocVarStr(const LocVar &Loc, const std::string Prefix="");
  LocVar getLocVarFromStr(const std::string &Str);
  std::vector<LocVar> getOpLocs(const LocVar &Loc);
  std::vector<std::string> splitString(const char *S, char Del=',');

};

}

#endif  // SOUPER_INST_SYNTHESIS_H

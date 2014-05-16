cmake_minimum_required(VERSION 2.8)
project(Souper)

find_program(LLVM_CONFIG_EXECUTABLE HINTS ${CMAKE_SOURCE_DIR}/third_party/llvm/${CMAKE_BUILD_TYPE}/bin
                                    NAMES llvm-config DOC "llvm-config executable")
if(LLVM_CONFIG_EXECUTABLE STREQUAL "LLVM_CONFIG_EXECUTABLE-NOTFOUND")
  message(FATAL_ERROR "llvm-config could not be found!")
endif()

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --cppflags
  OUTPUT_VARIABLE LLVM_CXXFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -fno-exceptions -fno-rtti")

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --libs bitreader bitwriter instrumentation ipo irreader linker mc mcparser objcarcopts option scalaropts transformutils
  OUTPUT_VARIABLE LLVM_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --system-libs
  OUTPUT_VARIABLE LLVM_SYSTEM_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
set(LLVM_LIBS "${LLVM_LIBS} ${LLVM_SYSTEM_LIBS}")

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --ldflags
  OUTPUT_VARIABLE LLVM_LDFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --bindir
  OUTPUT_VARIABLE LLVM_BINDIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --src-root
  OUTPUT_VARIABLE LLVM_SRC
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --obj-root
  OUTPUT_VARIABLE LLVM_BUILD
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(CLANG_CXXFLAGS "-I\"${LLVM_SRC}/tools/clang/include\" -I\"${LLVM_BUILD}/tools/clang/include\"")
set(CLANG_LIBS "-lclangCodeGen -lclangTooling -lclangRewriteCore -lclangFrontend -lclangAnalysis -lclangParse -lclangSerialization -lclangSema -lclangEdit -lclangAnalysis -lclangAST -lclangDriver -lclangLex -lclangBasic")

set(GTEST_CXXFLAGS "-I\"${LLVM_SRC}/utils/unittest/googletest/include\" -DGTEST_HAS_RTTI=0")
set(GTEST_LIBS "-lgtest_main -lgtest")

if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -std=c++0x")
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LANGUAGE_STANDARD "c++11")
  set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -std=c++11")
endif()

include_directories(include third_party/klee/include)

add_library(kleeExpr STATIC
  third_party/klee/lib/Expr/Constraints.cpp
  third_party/klee/lib/Expr/ExprBuilder.cpp
  third_party/klee/lib/Expr/Expr.cpp
  third_party/klee/lib/Expr/ExprEvaluator.cpp
  third_party/klee/lib/Expr/ExprPPrinter.cpp
  third_party/klee/lib/Expr/ExprSMTLIBLetPrinter.cpp
  third_party/klee/lib/Expr/ExprSMTLIBPrinter.cpp
  third_party/klee/lib/Expr/ExprUtil.cpp
  third_party/klee/lib/Expr/ExprVisitor.cpp
  third_party/klee/lib/Expr/Lexer.cpp
  third_party/klee/lib/Expr/Parser.cpp
  third_party/klee/lib/Expr/Updates.cpp
)

add_library(souperClangTool STATIC
  lib/ClangTool/Actions.cpp
)

add_library(souperExtractor STATIC
  lib/Extractor/CandidateMap.cpp
  lib/Extractor/Candidates.cpp
  lib/Extractor/KLEEBuilder.cpp
)

add_library(souperInst STATIC
  lib/Inst/Inst.cpp
)

add_library(souperSMTLIB2 STATIC
  lib/SMTLIB2/Solver.cpp
)

add_library(souperTool STATIC
  lib/Tool/CandidateMapUtils.cpp
)

add_executable(clang-souper
  tools/clang-souper.cpp
)

add_executable(souper
  tools/souper.cpp
)

add_executable(internal-solver-test
  tools/internal-solver-test.cpp
)

add_executable(extractor_tests
  unittests/Extractor/ExtractorTests.cpp
)

add_executable(inst_tests
  unittests/Inst/InstTests.cpp
)

set_target_properties(souper internal-solver-test souperExtractor souperInst souperSMTLIB2 souperTool kleeExpr
  PROPERTIES COMPILE_FLAGS "${LLVM_CXXFLAGS} -Wno-covered-switch-default")
set_target_properties(souperClangTool clang-souper
  PROPERTIES COMPILE_FLAGS "${CLANG_CXXFLAGS} ${LLVM_CXXFLAGS} -Wno-covered-switch-default")
set_target_properties(extractor_tests inst_tests
  PROPERTIES COMPILE_FLAGS "${GTEST_CXXFLAGS} ${LLVM_CXXFLAGS} -Wno-covered-switch-default")

# Executables
target_link_libraries(souper souperTool souperExtractor souperInst souperSMTLIB2 kleeExpr ${LLVM_LDFLAGS} ${LLVM_LIBS})
target_link_libraries(clang-souper souperClangTool souperTool souperExtractor souperInst souperSMTLIB2 kleeExpr ${CLANG_LIBS} ${LLVM_LIBS} ${LLVM_LDFLAGS})

# Tests
target_link_libraries(internal-solver-test souperSMTLIB2 ${LLVM_LDFLAGS} ${LLVM_LIBS})
target_link_libraries(extractor_tests souperExtractor souperInst kleeExpr ${GTEST_LIBS} ${LLVM_LDFLAGS} ${LLVM_LIBS})
target_link_libraries(inst_tests souperInst ${GTEST_LIBS} ${LLVM_LDFLAGS} ${LLVM_LIBS})

# Custom targets
add_custom_target(run_extractor_tests COMMAND ./extractor_tests DEPENDS extractor_tests)
add_custom_target(run_inst_tests COMMAND ./inst_tests DEPENDS inst_tests)

add_custom_target(check DEPENDS run_extractor_tests run_inst_tests)

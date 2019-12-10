cmake_minimum_required(VERSION 2.8)
project(Souper)

set(CMAKE_MACOSX_RPATH NEW)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif()

if (NOT LLVM_BUILD_TYPE)
  set(LLVM_BUILD_TYPE ${CMAKE_BUILD_TYPE})
endif()
set(LLVM_CONFIG_EXECUTABLE ${CMAKE_SOURCE_DIR}/third_party/llvm/${LLVM_BUILD_TYPE}/bin/llvm-config)
if(NOT EXISTS ${LLVM_CONFIG_EXECUTABLE})
  message(FATAL_ERROR "llvm-config could not be found!")
endif()

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --includedir
  OUTPUT_VARIABLE LLVM_INCLUDEDIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --cppflags
  OUTPUT_VARIABLE LLVM_CXXFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -fno-exceptions -fno-rtti")

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --libs
  OUTPUT_VARIABLE LLVM_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --system-libs
  OUTPUT_VARIABLE LLVM_SYSTEM_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
set(LLVM_LIBS "${LLVM_LIBS} ${LLVM_SYSTEM_LIBS}")
string(STRIP ${LLVM_LIBS} LLVM_LIBS)

execute_process(
  COMMAND ${LLVM_CONFIG_EXECUTABLE} --ldflags
  OUTPUT_VARIABLE LLVM_LDFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(PASS_LDFLAGS "${LLVM_LDFLAGS}")

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

set(CLANG_CXXFLAGS "")
set(CLANG_INCLUDEDIR "${LLVM_SRC}/tools/clang/include" "${LLVM_BUILD}/tools/clang/include")

set(ALIVE_DIR "${CMAKE_SOURCE_DIR}/third_party/alive2")
set(ALIVE_LDFLAGS "-L ${ALIVE_DIR}/build -lalive2")

set(CLANG_LIBS "-lclangCodeGen -lclangTooling -lclangRewrite -lclangFrontend -lclangAnalysis -lclangParse -lclangSerialization -lclangSema -lclangEdit -lclangAnalysis -lclangAST -lclangDriver -lclangLex -lclangBasic -lclangASTMatchers")

set(GTEST_CXXFLAGS "-DGTEST_HAS_RTTI=0")
set(GTEST_INCLUDEDIR "${LLVM_SRC}/utils/unittest/googletest/include")
set(GTEST_LIBS "-lgtest_main -lgtest")

set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -std=c++17")

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LANGUAGE_STANDARD "c++17")
  set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -fvisibility-inlines-hidden")
  set(PASS_LDFLAGS "-Wl,-undefined,dynamic_lookup")
endif()

include_directories(include third_party/klee/include)
include_directories(include "${ALIVE_DIR}")
include_directories(include "${ALIVE_DIR}/alive2")

set(KLEE_EXPR_FILES
  third_party/klee/lib/Expr/Constraints.cpp
  third_party/klee/lib/Expr/ExprBuilder.cpp
  third_party/klee/lib/Expr/Expr.cpp
  third_party/klee/lib/Expr/ExprEvaluator.cpp
  third_party/klee/lib/Expr/ExprPPrinter.cpp
  third_party/klee/lib/Expr/ExprSMTLIBPrinter.cpp
  third_party/klee/lib/Expr/ExprUtil.cpp
  third_party/klee/lib/Expr/ExprVisitor.cpp
  third_party/klee/lib/Expr/Lexer.cpp
  third_party/klee/lib/Expr/Parser.cpp
  third_party/klee/lib/Expr/Updates.cpp
)

add_library(kleeExpr STATIC
  ${KLEE_EXPR_FILES}
)

find_path(HIREDIS_INCLUDE_DIR
  NAMES
  hiredis/hiredis.h
  PATHS
  third_party/hiredis/install/include
  NO_DEFAULT_PATH)

include_directories(${HIREDIS_INCLUDE_DIR}/hiredis)

find_library(HIREDIS_LIBRARY
  NAMES
  hiredis
  PATHS
  third_party/hiredis/install/lib
  NO_DEFAULT_PATH)

find_library(ALIVE_LIBRARY alive2 PATHS "${ALIVE_DIR}/build" NO_DEFAULT_PATH)

find_path(Z3_INCLUDE_DIRECTORY
  NAMES
  z3.h
  z3++.h
  PATHS
  third_party/z3-install/include
  NO_DEFAULT_PATH)

include_directories(${Z3_INCLUDE_DIRECTORY})

find_library(Z3_LIBRARY
  NAMES
  libz3.a
  PATHS
  third_party/z3-install/lib
  NO_DEFAULT_PATH)

find_program(Z3
  NAMES
  z3
  PATHS
  third_party/z3-install/bin
  NO_DEFAULT_PATH)

set(TEST_SOLVER "-z3-path=${Z3}")

set(SOUPER_CLANG_TOOL_FILES
  lib/ClangTool/Actions.cpp
  include/souper/ClangTool/Actions.h
)

add_library(souperClangTool STATIC
  ${SOUPER_CLANG_TOOL_FILES}
)

set(SOUPER_EXTRACTOR_FILES
  lib/Extractor/Candidates.cpp
  lib/Extractor/ExprBuilder.cpp
  lib/Extractor/KLEEBuilder.cpp
  lib/Extractor/Solver.cpp
  include/souper/Extractor/Candidates.h
  include/souper/Extractor/ExprBuilder.h
  include/souper/Extractor/Solver.h
)

add_library(souperExtractor STATIC
  ${SOUPER_EXTRACTOR_FILES}
)

set(SOUPER_KVSTORE_FILES
  lib/KVStore/KVStore.cpp
  include/souper/KVStore/KVStore.h
)

add_library(souperKVStore STATIC
  ${SOUPER_KVSTORE_FILES}
)

set(SOUPER_INFER_FILES
  lib/Infer/InstSynthesis.cpp
  include/souper/Infer/InstSynthesis.h
  lib/Infer/ConstantSynthesis.cpp
  include/souper/Infer/ConstantSynthesis.h
  lib/Infer/EnumerativeSynthesis.cpp
  include/souper/Infer/EnumerativeSynthesis.h
  lib/Infer/AliveDriver.cpp
  include/souper/Infer/AliveDriver.h
  lib/Infer/Pruning.cpp
  include/souper/Infer/Pruning.h
  lib/Infer/Interpreter.cpp
  lib/Infer/AbstractInterpreter.cpp
  include/souper/Infer/Interpreter.h
)

add_library(souperInfer STATIC
  ${SOUPER_INFER_FILES}
)

set(SOUPER_INST_FILES
  lib/Inst/Inst.cpp
  include/souper/Inst/Inst.h
  include/souper/Inst/InstGraph.h
)

add_library(souperInst STATIC
  ${SOUPER_INST_FILES}
)

set(SOUPER_PARSER_FILES
  lib/Parser/Parser.cpp
  include/souper/Parser/Parser.h
)

add_library(souperParser STATIC
  ${SOUPER_PARSER_FILES}
)

set(SOUPER_SMTLIB2_FILES
  lib/SMTLIB2/Solver.cpp
  include/souper/SMTLIB2/Solver.h
)

add_library(souperSMTLIB2 STATIC
  ${SOUPER_SMTLIB2_FILES}
)

set(SOUPER_TOOL_FILES
  lib/Tool/CandidateMapUtils.cpp
  include/souper/Tool/CandidateMapUtils.h
  include/souper/Tool/GetSolverFromArgs.h
)

add_library(souperTool STATIC
  ${SOUPER_TOOL_FILES}
)

set(SOUPER_SOURCES
  ${SOUPER_EXTRACTOR_FILES}
  ${SOUPER_INST_FILES}
  ${SOUPER_KVSTORE_FILES}
  ${SOUPER_PARSER_FILES}
  ${SOUPER_SMTLIB2_FILES}
  ${SOUPER_TOOL_FILES}
  ${SOUPER_INFER_FILES})

set(SOUPER_CODEGEN_FILES
  lib/Codegen/Codegen.cpp
  include/souper/Codegen/Codegen.h
)

add_library(souperPass SHARED
  ${KLEE_EXPR_FILES}
  ${SOUPER_SOURCES}
  lib/Pass/Pass.cpp
)

add_library(souperPassProfileAll SHARED
  ${KLEE_EXPR_FILES}
  ${SOUPER_SOURCES}
  lib/Pass/Pass.cpp
)

add_library(souperCodegen SHARED
  ${SOUPER_CODEGEN_FILES}
)

target_compile_definitions(souperPassProfileAll PRIVATE DYNAMIC_PROFILE_ALL=1)

add_executable(clang-souper
  tools/clang-souper.cpp
)

add_executable(souper
  tools/souper.cpp
)

add_executable(internal-solver-test
  tools/internal-solver-test.cpp
)

add_executable(lexer-test
  tools/lexer-test.cpp
)

add_executable(parser-test
  tools/parser-test.cpp
)

add_executable(souper-check
  tools/souper-check.cpp
)

add_executable(souper-interpret
  tools/souper-interpret.cpp
)

add_executable(count-insts
  tools/count-insts.cpp
)

add_executable(souper2llvm
  tools/souper2llvm.cpp
)

add_executable(extractor_tests
  unittests/Extractor/ExtractorTests.cpp
)

add_executable(inst_tests
  unittests/Inst/InstTests.cpp
)

add_executable(parser_tests
  unittests/Parser/ParserTests.cpp
)

add_executable(interpreter_tests
  unittests/Interpreter/InterpreterInfra.cpp
  unittests/Interpreter/InterpreterTests.cpp)

set(LLVM_LDFLAGS "${LLVM_LDFLAGS} ${ALIVE_LDFLAGS}")
foreach(target souper internal-solver-test lexer-test parser-test souper-check count-insts
               souper2llvm souper-interpret
               souperExtractor souperInfer souperInst souperKVStore souperParser
               souperSMTLIB2 souperTool souperPass souperPassProfileAll kleeExpr
               souperCodegen)
  set_target_properties(${target} PROPERTIES COMPILE_FLAGS "${LLVM_CXXFLAGS}")
  target_include_directories(${target} PRIVATE "${LLVM_INCLUDEDIR}")
endforeach()
foreach(target souperClangTool clang-souper)
  set_target_properties(${target} PROPERTIES COMPILE_FLAGS "${CLANG_CXXFLAGS} ${LLVM_CXXFLAGS}")
  target_include_directories(${target} PRIVATE "${LLVM_INCLUDEDIR}" ${CLANG_INCLUDEDIR})
endforeach()
foreach(target extractor_tests inst_tests parser_tests interpreter_tests)
  set_target_properties(${target} PROPERTIES COMPILE_FLAGS "${GTEST_CXXFLAGS} ${LLVM_CXXFLAGS}")
  target_include_directories(${target} PRIVATE "${LLVM_INCLUDEDIR}" "${GTEST_INCLUDEDIR}")
endforeach()

add_library(z3 STATIC IMPORTED)
set_target_properties(z3 PROPERTIES IMPORTED_LOCATION ${Z3_LIBRARY})
set_target_properties(z3 PROPERTIES IMPORTED_IMPLIB ${Z3_LIBRARY})

# static
target_link_libraries(kleeExpr ${LLVM_LIBS} ${LLVM_LDFLAGS})
target_link_libraries(souperClangTool souperExtractor souperTool ${CLANG_LIBS} ${LLVM_LIBS} ${LLVM_LDFLAGS})
target_link_libraries(souperExtractor souperParser souperKVStore souperInfer souperInst kleeExpr)
target_link_libraries(souperInfer souperExtractor ${LLVM_LIBS} ${LLVM_LDFLAGS} z3)
target_link_libraries(souperInst ${LLVM_LIBS} ${LLVM_LDFLAGS})
target_link_libraries(souperKVStore ${HIREDIS_LIBRARY} ${LLVM_LIBS} ${LLVM_LDFLAGS})
target_link_libraries(souperParser souperInst ${LLVM_LIBS} ${LLVM_LDFLAGS} ${ALIVE_LIBRARY})
target_link_libraries(souperSMTLIB2 ${LLVM_LIBS} ${LLVM_LDFLAGS})
target_link_libraries(souperTool souperExtractor souperSMTLIB2)

# dynamic
target_link_libraries(souperCodegen ${PASS_LDFLAGS})
target_link_libraries(souperPass souperCodegen ${PASS_LDFLAGS} ${HIREDIS_LIBRARY} ${ALIVE_LIBRARY} z3)
target_link_libraries(souperPassProfileAll souperCodegen ${PASS_LDFLAGS} ${HIREDIS_LIBRARY} ${ALIVE_LIBRARY} z3)

# executables
target_link_libraries(souper souperExtractor souperKVStore souperParser souperSMTLIB2 souperTool kleeExpr ${HIREDIS_LIBRARY} ${ALIVE_LIBRARY} z3)
target_link_libraries(internal-solver-test souperSMTLIB2)
target_link_libraries(lexer-test souperParser)
target_link_libraries(parser-test souperParser)
target_link_libraries(souper-check souperTool souperExtractor souperKVStore souperSMTLIB2 souperParser ${HIREDIS_LIBRARY} ${ALIVE_LIBRARY} z3)
target_link_libraries(souper-interpret souperTool souperExtractor souperKVStore souperSMTLIB2 souperParser ${HIREDIS_LIBRARY} ${ALIVE_LIBRARY} z3)
target_link_libraries(clang-souper souperClangTool souperExtractor souperKVStore souperParser souperSMTLIB2 souperTool kleeExpr ${CLANG_LIBS} ${LLVM_LIBS} ${LLVM_LDFLAGS} ${HIREDIS_LIBRARY} ${ALIVE_LIBRARY} z3)
target_link_libraries(count-insts souperParser)
target_link_libraries(souper2llvm souperParser souperCodegen)
target_link_libraries(extractor_tests souperExtractor souperParser ${GTEST_LIBS} ${ALIVE_LIBRARY})
target_link_libraries(inst_tests souperInfer souperInst souperExtractor ${GTEST_LIBS} ${ALIVE_LIBRARY})
target_link_libraries(parser_tests souperParser ${GTEST_LIBS} ${ALIVE_LIBRARY})
target_link_libraries(interpreter_tests souperInfer souperInst ${GTEST_LIBS} ${ALIVE_LIBRARY})

SET(BUILD_CLANG_TOOL 1 CACHE BOOL "Build the Souper Clang tool")
if (NOT BUILD_CLANG_TOOL)
  message(STATUS "Excluding Clang tool from build")
  set_target_properties(souperClangTool clang-souper PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)
endif()

set(TEST_SYNTHESIS "ON" CACHE STRING "Enable additional, computationally intensive synthesis tests")
set(TEST_LONG_DURATION_SYNTHESIS "" CACHE STRING "Enable long duration (> 10 min) synthesis tests")

configure_file(
  ${CMAKE_SOURCE_DIR}/test/lit.site.cfg.in
  ${CMAKE_BINARY_DIR}/test/lit.site.cfg
)

configure_file(
  ${CMAKE_SOURCE_DIR}/utils/run_lit.in
  ${CMAKE_BINARY_DIR}/run_lit
)

configure_file(
  ${CMAKE_SOURCE_DIR}/tools/gdbprinter.py
  ${CMAKE_BINARY_DIR}/souper-check-gdb.py
)

add_custom_target(check
  COMMAND ${CMAKE_BINARY_DIR}/run_lit
  DEPENDS extractor_tests inst_tests parser-test parser_tests profileRuntime souper souper-check souper-interpret souperPass souperPassProfileAll count-insts interpreter_tests
  USES_TERMINAL)

find_program(GO_EXECUTABLE NAMES go DOC "go executable")
if(NOT GO_EXECUTABLE STREQUAL "GO_EXECUTABLE-NOTFOUND")
  add_executable(souperweb-backend
    tools/souperweb-backend.cpp
  )
  set_target_properties(souperweb-backend
    PROPERTIES COMPILE_FLAGS "${LLVM_CXXFLAGS}")
  target_include_directories(souperweb-backend PRIVATE "${LLVM_INCLUDEDIR}")

  target_link_libraries(souperweb-backend souperTool souperExtractor souperKVStore souperSMTLIB2 souperParser souperInst ${HIREDIS_LIBRARY})

  add_custom_target(souperweb ALL COMMAND ${GO_EXECUTABLE} build -o ${CMAKE_BINARY_DIR}/souperweb ${CMAKE_SOURCE_DIR}/tools/souperweb.go
                                  COMMENT "Building souperweb")
endif()

add_library(profileRuntime STATIC
  runtime/souperPassProfile.c)

set(SOUPER_PASS ${CMAKE_BINARY_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}souperPass${CMAKE_SHARED_LIBRARY_SUFFIX})
set(SOUPER_PASS_PROFILE_ALL ${CMAKE_BINARY_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}souperPassProfileAll${CMAKE_SHARED_LIBRARY_SUFFIX})
set(PROFILE_LIBRARY ${CMAKE_BINARY_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}profileRuntime${CMAKE_STATIC_LIBRARY_SUFFIX})
configure_file(${CMAKE_SOURCE_DIR}/utils/reduce.in ${CMAKE_BINARY_DIR}/reduce @ONLY)
configure_file(${CMAKE_SOURCE_DIR}/utils/cache_dump.in ${CMAKE_BINARY_DIR}/cache_dump @ONLY)
configure_file(${CMAKE_SOURCE_DIR}/utils/cache_dfa.in ${CMAKE_BINARY_DIR}/cache_dfa @ONLY)
configure_file(${CMAKE_SOURCE_DIR}/utils/cache_import.in ${CMAKE_BINARY_DIR}/cache_import @ONLY)
configure_file(${CMAKE_SOURCE_DIR}/utils/cache_infer.in ${CMAKE_BINARY_DIR}/cache_infer @ONLY)
configure_file(${CMAKE_SOURCE_DIR}/utils/py_souper2llvm.in ${CMAKE_BINARY_DIR}/py_souper2llvm @ONLY)

if (BUILD_CLANG_TOOL)
  configure_file(${CMAKE_SOURCE_DIR}/utils/sclang.in ${CMAKE_BINARY_DIR}/sclang @ONLY)
  configure_file(${CMAKE_SOURCE_DIR}/utils/sclang.in ${CMAKE_BINARY_DIR}/sclang++ @ONLY)
endif()

add_subdirectory(docs)

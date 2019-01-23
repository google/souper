# add a target to generate API documentation with Doxygen
find_package(Doxygen)
option(BUILD_DOCUMENTATION "Create and install the HTML based API documentation (requires Doxygen)" ${DOXYGEN_FOUND})

if(BUILD_DOCUMENTATION)
  set(doxyfile_in ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
  set(doxyfile ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

  configure_file(${doxyfile_in} ${doxyfile} @ONLY)

  # Convert file paths to absolute paths
  set(SOUPER_SOURCES_ABS "")
  foreach(file ${SOUPER_SOURCES})
    list(APPEND SOUPER_SOURCES_ABS "${CMAKE_SOURCE_DIR}/${file}")
  endforeach(file)

  add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
		     COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
		     DEPENDS ${SOUPER_SOURCES_ABS})

  add_custom_target(docs ALL
                    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/html/index.html)

  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html DESTINATION share/doc)
endif()

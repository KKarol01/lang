cmake_minimum_required(VERSION 3.29)
project(lang)
add_executable(main main.cpp "lexer.cpp" "interpreter.cpp" "parser.cpp" "app.cpp")
target_compile_features(main PRIVATE cxx_std_23)

target_compile_definitions(main
	PRIVATE
      $<$<CONFIG:Debug>:DEBUG_PRINT_INFO>
)

add_custom_target(copy_script ALL
	COMMAND ${CMAKE_COMMAND} -E copy_if_different
		"${CMAKE_SOURCE_DIR}/assets/script.lang"
		"$<TARGET_FILE_DIR:main>/script.lang"
)
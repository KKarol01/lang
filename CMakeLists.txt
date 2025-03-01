cmake_minimum_required(VERSION 3.29)
project(lang)
add_subdirectory(parser)
add_executable(main main.cpp)
target_compile_features(main PRIVATE cxx_std_23)
target_link_libraries(main PRIVATE parser)

add_custom_command(TARGET main POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory
		"$<TARGET_FILE_DIR:parser>"
		"$<TARGET_FILE_DIR:main>"
)

add_custom_target(copy_script ALL
	COMMAND ${CMAKE_COMMAND} -E copy_if_different
		"${CMAKE_SOURCE_DIR}/assets/script.lang"
		"$<TARGET_FILE_DIR:main>/script.lang"
)
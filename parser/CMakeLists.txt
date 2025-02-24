add_library(parser SHARED parser.cpp)
target_compile_features(parser PRIVATE cxx_std_23)
target_include_directories(parser PUBLIC ${CMAKE_SOURCE_DIR})
target_compile_definitions(parser PRIVATE LANG_BUILD_AS_DLL)
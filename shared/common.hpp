#pragma once

#ifdef LANG_BUILD_AS_DLL
#define LANG_DLL_FUNC extern "C" __declspec(dllexport)
#else 
#define LANG_DLL_FUNC extern "C" __declspec(dllimport)
#endif
﻿if (WIN32)
	if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
		if(MINGW)
			set(ENV{FREETYPE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/win64/mingw")
		elseif(MSVC)
			set(ENV{FREETYPE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/win64/msvc")
		endif()
	else()
		if(MINGW)
			set(ENV{FREETYPE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/win32/mingw")
		elseif(MSVC)
			set(ENV{FREETYPE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/win32/msvc")
		endif()
	endif()
endif()

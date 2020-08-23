file(GLOB_RECURSE libsrc *.h *.cpp *.hpp *.c *.cc)

list(FILTER libsrc EXCLUDE REGEX "${CMAKE_CURRENT_SOURCE_DIR}/freetype/")
list(FILTER libsrc EXCLUDE REGEX "${CMAKE_CURRENT_SOURCE_DIR}/stb/")

set(target_name fontpp)


set(TTF_LOADER "FREETYPE" CACHE STRING "Backend type chosen by the user at CMake configure time")
set_property(CACHE TTF_LOADER PROPERTY STRINGS FREETYPE STB)

if("${TTF_LOADER}" STREQUAL "FREETYPE")
    message( STATUS "FREETYPE TTF loader selected." )

    find_package(Freetype REQUIRED)

    if(WIN32 AND CMAKE_RUNTIME_OUTPUT_DIRECTORY)
        add_custom_target(copy_freetype_binaries ALL
			COMMAND ${CMAKE_COMMAND} -E copy ${FREETYPE_LIBRARIES} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    endif()

    file(GLOB LIBTTF_libsrc "${CMAKE_CURRENT_SOURCE_DIR}/freetype/*.h"
                             "${CMAKE_CURRENT_SOURCE_DIR}/freetype/*.cpp"
                             "${CMAKE_CURRENT_SOURCE_DIR}/freetype/*.hpp"
                             "${CMAKE_CURRENT_SOURCE_DIR}/freetype/*.c"
                             "${CMAKE_CURRENT_SOURCE_DIR}/freetype/*.cc")

    add_definitions(-DTTF_LOADER_FREETYPE)
    list(APPEND libsrc ${LIBTTF_libsrc})

elseif("${TTF_LOADER}" STREQUAL "STB")
    message( STATUS "STB TTF loader selected." )

    file(GLOB LIBTTF_libsrc "${CMAKE_CURRENT_SOURCE_DIR}/stb/*.h"
                             "${CMAKE_CURRENT_SOURCE_DIR}/stb/*.cpp"
                             "${CMAKE_CURRENT_SOURCE_DIR}/stb/*.hpp"
                             "${CMAKE_CURRENT_SOURCE_DIR}/stb/*.c"
                             "${CMAKE_CURRENT_SOURCE_DIR}/stb/*.cc")

    add_definitions(-DTTF_LOADER_STB)
    list(APPEND libsrc ${LIBTTF_libsrc})
else()
    message( FATAL_ERROR "No TTF backend type was selected." )
endif()

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED REQUIRED)


add_library(${target_name} ${libsrc})

target_link_libraries(${target_name} PUBLIC Threads::Threads)

if("${TTF_LOADER}" STREQUAL "FREETYPE")
    target_link_libraries(${target_name} PRIVATE Freetype::Freetype)
endif()

target_include_directories(${target_name}
    PUBLIC
		${CMAKE_CURRENT_SOURCE_DIR}/..
)
set_target_properties(${target_name} PROPERTIES
    CXX_STANDARD 14
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO
    POSITION_INDEPENDENT_CODE ON
	WINDOWS_EXPORT_ALL_SYMBOLS ON
)

if(MSVC)
	target_compile_options(${target_name} PRIVATE /wd4505)
	target_compile_definitions(${target_name} PRIVATE  _CRT_SECURE_NO_WARNINGS)
else()
	target_compile_options(${target_name} PRIVATE -Wno-unused-function -Wno-old-style-cast)
endif()

include(target_warning_support)
set_warning_level(${target_name} ultra)

include(target_code_style_support)
set_code_style(${target_name} lower_case check_headers "${extra_flags}")

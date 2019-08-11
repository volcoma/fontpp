file(GLOB_RECURSE libsrc *.h *.cpp *.hpp *.c *.cc)

set(target_name fontpp)

add_library(${target_name} ${libsrc})

find_package(Freetype)
if(FREETYPE_FOUND)
	target_link_libraries(${target_name} PRIVATE ${FREETYPE_LIBRARIES})
	target_include_directories(${target_name} PRIVATE ${FREETYPE_INCLUDE_DIRS})

	if(WIN32)
		file(
			COPY $ENV{FREETYPE_DIR}/freetype.dll
			DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
	endif()

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

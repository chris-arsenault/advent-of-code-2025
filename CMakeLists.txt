cmake_minimum_required(VERSION 3.20)
project(advant_code_2025 C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)

set(AOC_DAY "" CACHE STRING "Day executable to run via the 'run' target (e.g. day1)")
file(GLOB AOC_DAY_DIRS RELATIVE "${CMAKE_SOURCE_DIR}" "day*")
list(SORT AOC_DAY_DIRS)

set(AOC_DAY_TARGETS "")
set(AOC_RUN_TARGETS "")

foreach(day_dir IN LISTS AOC_DAY_DIRS)
    if(IS_DIRECTORY "${CMAKE_SOURCE_DIR}/${day_dir}")
        set(day_main "${CMAKE_SOURCE_DIR}/${day_dir}/main.c")
        if(EXISTS "${day_main}")
            add_executable(${day_dir} "${day_main}")
            set_target_properties(${day_dir} PROPERTIES
                    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${day_dir}")
            target_link_libraries(${day_dir} PRIVATE m)

            add_custom_target(run_${day_dir}
                    COMMAND $<TARGET_FILE:${day_dir}>
                    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/${day_dir}"
                    DEPENDS ${day_dir}
                    USES_TERMINAL)

            list(APPEND AOC_DAY_TARGETS ${day_dir})
            list(APPEND AOC_RUN_TARGETS run_${day_dir})
        else()
            message(STATUS "Skipping ${day_dir}: missing main.c")
        endif()
    endif()
endforeach()

if(AOC_DAY)
    if(NOT AOC_DAY IN_LIST AOC_DAY_TARGETS)
        message(FATAL_ERROR "Requested day '${AOC_DAY}' not found. Available: ${AOC_DAY_TARGETS}")
    endif()

    add_custom_target(run
            DEPENDS run_${AOC_DAY})
endif()

if(AOC_DAY_TARGETS)
    add_custom_target(build_all DEPENDS ${AOC_DAY_TARGETS})
endif()

if(AOC_RUN_TARGETS)
    add_custom_target(run_all DEPENDS ${AOC_RUN_TARGETS})
endif()

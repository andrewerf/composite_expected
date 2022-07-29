cmake_minimum_required(VERSION 3.22)
project(composite_expected)

set(CMAKE_CXX_STANDARD 20)
find_package(GTest REQUIRED)
include_directories(include)

add_executable(test tests/main.cpp)
target_link_libraries(test GTest::gtest_main GTest::gtest GTest::gmock)

gtest_discover_tests(test)
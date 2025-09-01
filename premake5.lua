-- Tensor Framework Build Configuration
workspace "TensorFramework"
  architecture "x86_64"
  configurations { "Debug", "Release", "Profile" }
  startproject "TensorFrameworkTests"
  
  -- Global settings
  language "C++"
  cppdialect "C++20"
  staticruntime "on"
  
  -- Output directories
  outputdir = "%{cfg.buildcfg}-%{cfg.system}-%{cfg.architecture}"
  
  -- Global defines
  defines {
    "TENSOR_VERSION_MAJOR=1",
    "TENSOR_VERSION_MINOR=0",
    "TENSOR_VERSION_PATCH=0"
  }

  -- Platform specific settings
  filter "system:windows"
    systemversion "latest"
    defines { "TENSOR_PLATFORM_WINDOWS", "_CRT_SECURE_NO_WARNINGS" }

  filter "system:linux"
    defines { "TENSOR_PLATFORM_LINUX" }

  filter "system:macosx"
    defines { "TENSOR_PLATFORM_MACOS" }

  -- Configuration specific settings
  filter "configurations:Debug"
    defines { "TENSOR_DEBUG", "TENSOR_ENABLE_ASSERTS" }
    runtime "Debug"
    symbols "on"
    optimize "off"

  filter "configurations:Release"
    defines { "TENSOR_RELEASE", "NDEBUG" }
    runtime "Release"
    symbols "off"
    optimize "speed"
    inlining "auto"

  filter "configurations:Profile"
    defines { "TENSOR_PROFILE", "TENSOR_ENABLE_PROFILING" }
    runtime "Release"
    symbols "on"
    optimize "speed"

  filter {}

-- Third party dependencies
group "Dependencies"

-- Google Test
project "GoogleTest"
  location "third_party/googletest"
  kind "StaticLib"
  
  files {
    "third_party/googletest/googletest/src/gtest-all.cc",
    "third_party/googletest/googlemock/src/gmock-all.cc"
  }
  
  includedirs {
    "third_party/googletest/googletest/include",
    "third_party/googletest/googletest",
    "third_party/googletest/googlemock/include",
    "third_party/googletest/googlemock"
  }
  
  filter "system:linux"
    links { "pthread" }

-- Google Benchmark
project "GoogleBenchmark"
  location "third_party/benchmark"
  kind "StaticLib"
  
  files {
    "third_party/benchmark/src/*.cc"
  }
  
  includedirs {
    "third_party/benchmark/include",
    "third_party/benchmark/src"
  }
  
  removefiles {
    "third_party/benchmark/src/benchmark_main.cc"
  }
  
  defines { "BENCHMARK_STATIC_DEFINE" }
  
  filter "system:windows"
    links { "shlwapi" }
  
  filter "system:linux"
    links { "pthread" }

group ""

-- Core tensor library
project "TensorCore"
  location "src"
  kind "StaticLib"
  
  pchheader "tensor_pch.hpp"
  pchsource "src/tensor_pch.cpp"
  
  files {
    "include/tensor/**.hpp",
    "src/**.cpp",
    "src/**.hpp"
  }
  
  includedirs {
    "include",
    "src"
  }
  
  -- Compiler specific flags
  filter "toolset:gcc or toolset:clang"
    buildoptions { 
      "-Wall", "-Wextra", "-Wpedantic", "-Werror",
      "-march=native", "-mtune=native",
      "-ffast-math", "-funroll-loops"
    }
    
  filter "toolset:msc"
    buildoptions { 
      "/W4", "/WX", "/permissive-",
      "/arch:AVX2", "/fp:fast"
    }
    
  filter "configurations:Release"
    filter "toolset:gcc or toolset:clang"
      buildoptions { "-flto", "-fwhole-program" }
    filter "toolset:msc"
      buildoptions { "/GL" }
      linkoptions { "/LTCG" }

-- Main library (header-only wrapper)
project "TensorFramework"
  location "."
  kind "None"
  
  files {
    "include/tensor.hpp",
    "README.md",
    "LICENSE",
    "CHANGELOG.md"
  }

-- Examples
group "Examples"

project "BasicOperations"
  location "examples/basic_operations"
  kind "ConsoleApp"
  
  files {
    "examples/basic_operations/**.cpp",
    "examples/basic_operations/**.hpp"
  }
  
  includedirs {
    "include",
    "examples/basic_operations"
  }
  
  links { "TensorCore" }

project "NeuralNetwork"
  location "examples/neural_network"
  kind "ConsoleApp"
  
  files {
    "examples/neural_network/**.cpp",
    "examples/neural_network/**.hpp"
  }
  
  includedirs {
    "include",
    "examples/neural_network"
  }
  
  links { "TensorCore" }

group ""

-- Tests
group "Tests"

project "TensorFrameworkTests"
  location "tests"
  kind "ConsoleApp"
  
  files {
    "tests/unit/**.cpp",
    "tests/unit/**.hpp",
    "tests/integration/**.cpp",
    "tests/integration/**.hpp",
    "tests/main.cpp"
  }
  
  includedirs {
    "include",
    "tests",
    "third_party/googletest/googletest/include",
    "third_party/googletest/googlemock/include"
  }
  
  links { "TensorCore", "GoogleTest" }
  
  filter "system:linux"
    links { "pthread" }

project "TensorBenchmarks"
  location "tests/benchmarks"
  kind "ConsoleApp"
  
  files {
    "tests/benchmarks/**.cpp",
    "tests/benchmarks/**.hpp"
  }
  
  includedirs {
    "include",
    "tests/benchmarks",
    "third_party/benchmark/include"
  }
  
  links { "TensorCore", "GoogleBenchmark" }
  
  filter "system:windows"
    links { "shlwapi" }
  
  filter "system:linux"
    links { "pthread" }

group ""

-- Tools
group "Tools"

project "CodeGenerator"
  location "tools/code_generator"
  kind "ConsoleApp"
  
  files {
    "tools/code_generator/**.cpp",
    "tools/code_generator/**.hpp"
  }
  
  includedirs {
    "include",
    "tools/code_generator"
  }

project "Profiler"
  location "tools/profiler"
  kind "ConsoleApp"
  
  files {
    "tools/profiler/**.cpp",
    "tools/profiler/**.hpp"
  }
  
  includedirs {
    "include",
    "tools/profiler"
  }
  
  links { "TensorCore" }

group ""

-- Custom actions for development
newaction {
  trigger = "clean",
  description = "Clean build files and directories",
  execute = function()
    print("Cleaning build files...")
    os.rmdir("build")
    os.rmdir("bin")
    os.rmdir("obj")
    os.remove("Makefile")
    os.remove("*.make")
    os.remove("*.vcxproj*")
    os.remove("*.sln")
    print("Clean completed.")
  end
}

newaction {
  trigger = "setup",
  description = "Setup development environment",
  execute = function()
    print("Setting up development environment...")
    
    -- Create necessary directories
    os.mkdir("build")
    os.mkdir("logs")
    os.mkdir("docs/api")
    
    -- Initialize git submodules if they don't exist
    if not os.isdir("third_party/googletest/.git") then
      print("Initializing Google Test submodule...")
      os.execute("git submodule add https://github.com/google/googletest.git third_party/googletest")
    end
    
    if not os.isdir("third_party/benchmark/.git") then
      print("Initializing Google Benchmark submodule...")
      os.execute("git submodule add https://github.com/google/benchmark.git third_party/benchmark")
    end
    
    os.execute("git submodule update --init --recursive")
    print("Setup completed.")
  end
}

newaction {
  trigger = "format",
  description = "Format code using clang-format",
  execute = function()
    print("Formatting code...")
    os.execute("find include src tests examples -name '*.hpp' -o -name '*.cpp' | xargs clang-format -i")
    print("Code formatting completed.")
  end
}

newaction {
  trigger = "lint",
  description = "Run static analysis with clang-tidy",
  execute = function()
    print("Running static analysis...")
    os.execute("clang-tidy -p build/compile_commands.json include/tensor/**/*.hpp src/**/*.cpp")
    print("Static analysis completed.")
  end
}
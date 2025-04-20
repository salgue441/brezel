/**
 * @file codes.hpp
 * @author Carlos Salguero
 * @brief Error code definitions for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines the error codes used throughout the brezel framework,
 * along with utility functions for working with them.
 */

#pragma once

#include <cstdint>
#include <string_view>

#include <brezel/core/config.hpp>
#include <brezel/core/error/categories.hpp>

namespace brezel::error {
/**
 * @brief Error codes enumeration
 */
enum class Code : uint32_t {
    // General error codes (0-99)
    Success = 0,            ///< No error
    Unknown = 1,            ///< Unknown error
    NotImplemented = 2,     ///< Feature not implemented
    InvalidArgument = 3,    ///< Invalid argument
    InvalidOperation = 4,   ///< Invalid operation
    Timeout = 5,            ///< Operation timed out
    Cancelled = 6,          ///< Operation cancelled
    ResourceExhausted = 7,  ///< Resource exhausted
    PermissionDenied = 8,   ///< Permission denied

    // Device errors (100-199)
    DeviceNotFound = 100,        ///< Device not found
    DeviceError = 101,           ///< Generic device error
    DeviceNotSupported = 102,    ///< Device not supported
    DeviceLost = 103,            ///< Device lost during operation
    DeviceOutOfMemory = 104,     ///< Device out of memory
    DeviceNotInitialized = 105,  ///< Device not initialized
    DeviceAlreadyInUse = 106,    ///< Device already in use

    // Memory errors (200-299)
    OutOfMemory = 200,             ///< Out of memory
    MemoryAllocationFailed = 201,  ///< Memory allocation failed
    InvalidMemoryAccess = 202,     ///< Invalid memory access
    MemoryNotAligned = 203,        ///< Memory not aligned
    BufferTooSmall = 204,          ///< Buffer too small

    // Tensor errors (300-399)
    InvalidShape = 300,       ///< Invalid tensor shape
    ShapeMismatch = 301,      ///< Shape mismatch
    DimensionMismatch = 302,  ///< Dimension mismatch
    InvalidDtype = 303,       ///< Invalid data type
    DtypeMismatch = 304,      ///< Data type mismatch
    IndexOutOfBounds = 305,   ///< Index out of bounds

    // I/O errors (400-499)
    FileNotFound = 400,       ///< File not found
    FileIOError = 401,        ///< File I/O error
    InvalidFile = 402,        ///< Invalid file format
    EndOfFile = 403,          ///< End of file
    FileExists = 404,         ///< File already exists
    DirectoryNotFound = 405,  ///< Directory not found

    // Math errors (500-599)
    DivideByZero = 500,        ///< Division by zero
    Overflow = 501,            ///< Arithmetic overflow
    Underflow = 502,           ///< Arithmetic underflow
    InvalidValue = 503,        ///< Invalid numerical value (NaN, Inf)
    Singular = 504,            ///< Singular matrix
    NonConvergent = 505,       ///< Non-convergent algorithm
    InvalidDomainError = 506,  ///< Invalid domain error (e.g., sqrt(-1))

    // Runtime errors (600-699)
    RuntimeError = 600,         ///< Generic runtime error
    AssertionFailed = 601,      ///< Assertion failed
    IllegalState = 602,         ///< Illegal state
    PreconditionFailed = 603,   ///< Precondition failed
    PostconditionFailed = 604,  ///< Postcondition failed
    InvariantViolated = 605,    ///< Invariant violated

    // Internal errors (700-799)
    InternalError = 700,    ///< Internal implementation error
    UnexpectedError = 701,  ///< Unexpected error
    DataCorruption = 702,   ///< Data corruption detected

    // Format errors (800-899)
    ParseError = 800,            ///< Parse error
    FormatError = 801,           ///< Format error
    ValidationError = 802,       ///< Validation error
    SerializationError = 803,    ///< Serialization error
    DeserializationError = 804,  ///< Deserialization error

    // CUDA errors (900-999)
    CudaError = 900,               ///< Generic CUDA error
    CudaDriverError = 901,         ///< CUDA driver error
    CudaRuntimeError = 902,        ///< CUDA runtime error
    CudaIncompatibleDriver = 903,  ///< CUDA incompatible driver
    CudaLaunchError = 904,         ///< CUDA kernel launch error

    // OpenCL errors (1000-1099)
    OpenCLError = 1000,              ///< Generic OpenCL error
    OpenCLBuildProgramError = 1001,  ///< OpenCL program build error
    OpenCLCompilerError = 1002,      ///< OpenCL compiler error

    // Library errors (1100-1199)
    LibraryError = 1100,         ///< Generic library error
    LibraryNotFound = 1101,      ///< Library not found
    SymbolNotFound = 1102,       ///< Symbol not found in library
    LibraryIncompatible = 1103,  ///< Library incompatible

    // System errors (1200-1299)
    SystemError = 1200,          ///< Generic system error
    NetworkError = 1201,         ///< Network error
    ThreadError = 1202,          ///< Thread error
    SynchronizationError = 1203  ///< Synchronization error
};

/**
 * @brief Convert error code to string
 *
 * @param code Error code
 * @return std::string_view String representation
 */
constexpr std::string_view code_to_string(Code code) noexcept {
    switch (code) {
        case Code::Success:
            return "Success";

        case Code::Unknown:
            return "Unknown";

        case Code::NotImplemented:
            return "NotImplemented";

        case Code::InvalidArgument:
            return "InvalidArgument";

        case Code::InvalidOperation:
            return "InvalidOperation";

        case Code::Timeout:
            return "Timeout";

        case Code::Cancelled:
            return "Cancelled";

        case Code::ResourceExhausted:
            return "ResourceExhausted";

        case Code::PermissionDenied:
            return "PermissionDenied";

        case Code::DeviceNotFound:
            return "DeviceNotFound";

        case Code::DeviceError:
            return "DeviceError";

        case Code::DeviceNotSupported:
            return "DeviceNotSupported";

        case Code::DeviceLost:
            return "DeviceLost";

        case Code::DeviceOutOfMemory:
            return "DeviceOutOfMemory";

        case Code::DeviceNotInitialized:
            return "DeviceNotInitialized";

        case Code::DeviceAlreadyInUse:
            return "DeviceAlreadyInUse";

        case Code::OutOfMemory:
            return "OutOfMemory";

        case Code::MemoryAllocationFailed:
            return "MemoryAllocationFailed";

        case Code::InvalidMemoryAccess:
            return "InvalidMemoryAccess";

        case Code::MemoryNotAligned:
            return "MemoryNotAligned";

        case Code::BufferTooSmall:
            return "BufferTooSmall";

        case Code::InvalidShape:
            return "InvalidShape";

        case Code::ShapeMismatch:
            return "ShapeMismatch";

        case Code::DimensionMismatch:
            return "DimensionMismatch";

        case Code::InvalidDtype:
            return "InvalidDtype";

        case Code::DtypeMismatch:
            return "DtypeMismatch";

        case Code::IndexOutOfBounds:
            return "IndexOutOfBounds";

        case Code::FileNotFound:
            return "FileNotFound";

        case Code::FileIOError:
            return "FileIOError";

        case Code::InvalidFile:
            return "InvalidFile";

        case Code::EndOfFile:
            return "EndOfFile";

        case Code::FileExists:
            return "FileExists";

        case Code::DirectoryNotFound:
            return "DirectoryNotFound";

        case Code::DivideByZero:
            return "DivideByZero";

        case Code::Overflow:
            return "Overflow";

        case Code::Underflow:
            return "Underflow";

        case Code::InvalidValue:
            return "InvalidValue";

        case Code::Singular:
            return "Singular";

        case Code::NonConvergent:
            return "NonConvergent";

        case Code::InvalidDomainError:
            return "InvalidDomainError";

        case Code::RuntimeError:
            return "RuntimeError";

        case Code::AssertionFailed:
            return "AssertionFailed";

        case Code::IllegalState:
            return "IllegalState";

        case Code::PreconditionFailed:
            return "PreconditionFailed";

        case Code::PostconditionFailed:
            return "PostconditionFailed";

        case Code::InvariantViolated:
            return "InvariantViolated";

        case Code::InternalError:
            return "InternalError";

        case Code::UnexpectedError:
            return "UnexpectedError";

        case Code::DataCorruption:
            return "DataCorruption";

        case Code::ParseError:
            return "ParseError";

        case Code::FormatError:
            return "FormatError";

        case Code::ValidationError:
            return "ValidationError";

        case Code::SerializationError:
            return "SerializationError";

        case Code::DeserializationError:
            return "DeserializationError";

        case Code::CudaError:
            return "CudaError";

        case Code::CudaDriverError:
            return "CudaDriverError";

        case Code::CudaRuntimeError:
            return "CudaRuntimeError";

        case Code::CudaIncompatibleDriver:
            return "CudaIncompatibleDriver";

        case Code::CudaLaunchError:
            return "CudaLaunchError";

        case Code::OpenCLError:
            return "OpenCLError";

        case Code::OpenCLBuildProgramError:
            return "OpenCLBuildProgramError";

        case Code::OpenCLCompilerError:
            return "OpenCLCompilerError";

        case Code::LibraryError:
            return "LibraryError";

        case Code::LibraryNotFound:
            return "LibraryNotFound";

        case Code::SymbolNotFound:
            return "SymbolNotFound";

        case Code::LibraryIncompatible:
            return "LibraryIncompatible";

        case Code::SystemError:
            return "SystemError";

        case Code::NetworkError:
            return "NetworkError";

        case Code::ThreadError:
            return "ThreadError";

        case Code::SynchronizationError:
            return "SynchronizationError";
        default:
            return "UnknownError";
    }
}

/**
 * @brief Get category for error code
 *
 * @param code Error code
 * @return Category Associated category
 */
constexpr Category get_category_for_code(Code code) noexcept {
    if (code == Code::Success)
        return Category::None;

    if (static_cast<uint32_t>(code) < 100)
        return Category::General;

    if (static_cast<uint32_t>(code) < 200)
        return Category::Device;

    if (static_cast<uint32_t>(code) < 300)
        return Category::Memory;

    if (static_cast<uint32_t>(code) < 400)
        return Category::Tensor;

    if (static_cast<uint32_t>(code) < 500)
        return Category::IO;

    if (static_cast<uint32_t>(code) < 600)
        return Category::Math;

    if (static_cast<uint32_t>(code) < 700)
        return Category::Runtime;

    if (static_cast<uint32_t>(code) < 800)
        return Category::Internal;

    if (static_cast<uint32_t>(code) < 900)
        return Category::Format;

    if (static_cast<uint32_t>(code) < 1000)
        return Category::Cuda;

    if (static_cast<uint32_t>(code) < 1100)
        return Category::OpenCL;

    if (static_cast<uint32_t>(code) < 1200)
        return Category::Library;

    if (static_cast<uint32_t>(code) < 1300)
        return Category::System;

    return Category::General;
}
}  // namespace brezel::error
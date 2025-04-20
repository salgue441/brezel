/**
 * @file error.hpp
 * @author Carlos Salguero
 * @brief Main error system include for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file includes all error system components and provides
 * a unified interface for error handling in the brezel framework
 */

#pragma once

#include <brezel/core/config.hpp>
#include <brezel/core/error/base.hpp>
#include <brezel/core/error/categories.hpp>
#include <brezel/core/error/codes.hpp>
#include <brezel/core/error/context.hpp>
#include <brezel/core/error/device_errors.hpp>
#include <brezel/core/error/format_errors.hpp>
#include <brezel/core/error/guards.hpp>
#include <brezel/core/error/result.hpp>
#include <brezel/core/error/runtime_errors.hpp>
#include <brezel/core/error/system_errors.hpp>
#include <brezel/core/error/tensor_errors.hpp>

namespace brezel {  // Import core error types into main namespace
using Error = error::Error;
using ErrorContext = error::ErrorContext;
using ErrorCode = error::Code;
using ErrorCategory = error::Category;

// Import Result monad
using error::Err;
using error::Ok;
using error::Result;
using error::Try;

// Import error checking utilities
using error::Assert;
using error::CheckBounds;
using error::Ensure;
using error::InvalidArgument;
using error::NotImplemented;
using error::Require;

// Import error guards
using error::CleanupGuard;
using error::ExceptionGuard;

// Import common error types
using DeviceError = error::DeviceError;
using MemoryError = error::MemoryError;
using TensorError = error::TensorError;
using RuntimeError = error::RuntimeError;
using InvalidArgumentError = error::InvalidArgumentError;
using NotImplementedError = error::NotImplementedError;
using OutOfBoundsError = error::IndexOutOfBoundsError;
using ShapeMismatchError = error::ShapeMismatchError;
using InvalidOperationError = error::InvalidOperationError;
using InternalError = error::InternalError;
using MathError = error::MathError;
using IOError = error::IOError;
using SystemError = error::SystemError;
using FormatError = error::FormatError;
using ParseError = error::ParseError;
using ValidationError = error::ValidationError;
using SerializationError = error::SerializationError;
using DeserializationError = error::DeserializationError;
using FileFormatError = error::FileFormatError;
}  // namespace brezel
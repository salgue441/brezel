/**
 * @file logging.hpp
 * @author Carlos Salguero
 * @brief Logging system for Brezel library.
 * @version 0.1
 * @date 2025-09-01
 *
 * @copyright Copyright (c) 2025
 *
 * This header provides a thread-safe logging system with multiple output sinks,
 * configurable log levels, and rich log record information including
 * timestamps, thread IDs, and source locations.
 *
 * @defgroup logging Logging System
 * @brief Flexible logging infrastructure for Tensor operations
 * @ingroup brezel_utils
 *
 * @par Features:
 * - Multiple log levels: TRACE, DEBUG, INFO, WARNING, ERROR, CRITICAL
 * - Thread-safe logging with mutex protection
 * - Multiple sink types: console, file, memory
 * - Source location tracking (file, line, function)
 * - Timestamp precision to milliseconds
 * - Thread ID tracking
 * - Format string support with std::format
 * - Environment variable configuration (TENSOR_LOG_LEVEL)
 * - Global and instance-based logging
 *
 * @par Usage Examples:
 * @code
 * // Basic logging
 * TENSOR_LOG_INFO("Tensor created with shape {}", shape);
 * TENSOR_LOG_ERROR("Invalid operation: {}", error_message);
 *
 * // Programmatic configuration
 * tensor::set_log_level(tensor::LogLevel::Debug);
 * tensor::add_log_sink(std::make_unique<tensor::FileSink>("app.log"));
 *
 * // Format string support
 * TENSOR_LOG_DEBUG("Processing batch {} of {}, progress: {:.1f}%",
 *                 batch_num, total_batches, progress * 100.0);
 * @endcode
 *
 * @par Environment Variables:
 * - `TENSOR_LOG_LEVEL`: Set default log level (TRACE, DEBUG, INFO, WARN, ERROR, CRITICAL, OFF)
 *
 * @par Thread Safety:
 * - All logging operations are thread-safe
 * - Multiple threads can log concurrently without data races
 *
 * @par Exception Safety:
 * - Logging operations provide basic exception safety
 * - File operations may throw std::runtime_error on file open failures
 * - Format operations may throw std::format_error on invalid format strings
 */

#pragma once

#include <chrono>
#include <format>
#include <fstream>
#include <iostream>
#include <memory>
#include <mutex>
#include <source_location>
#include <sstream>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

namespace brezel::logging {
/**
 * @brief Log levels
 */
enum class LogLevel : uint8_t { Trace = 0, Debug = 1, Info = 2, Warning = 3, Error = 4, Critical = 5, Off = 6 };

/**
 * @brief Convert log level to string
 */
constexpr std::string_view log_level_name(LogLevel level) noexcept {
    switch (level) {
        case LogLevel::Trace:
            return "TRACE";
        case LogLevel::Debug:
            return "DEBUG";
        case LogLevel::Info:
            return "INFO";
        case LogLevel::Warning:
            return "WARN";
        case LogLevel::Error:
            return "ERROR";
        case LogLevel::Critical:
            return "CRIT";
        case LogLevel::Off:
            return "OFF";
        default:
            return "UNKNOWN";
    }
}

/**
 * @brief Log record containing all log information.
 *
 * Represents a complete log entry with metadata including timestamp,
 * thread information, source location, and the actual log message. This
 * structure captures all relevant context for a log event.
 *
 * @par Members:
 * - `level`: Severity level of the log message
 * - `timestamp`: Precise time when the log was created (system_clock time_point)
 * - `thread_id`: ID of the thread that generated the log message
 * - `location`: Source code location (file, line, function) where log was called
 * - `message`: The actual log message content
 *
 * @par Example:
 * @code
 * LogRecord record(LogLevel::Info, "Tensor operation completed",
 *                 std::source_location::current());
 * @endcode
 *
 * @par Thread Safety:
 * - Individual LogRecord objects are not thread-safe
 * - Construction should be done in the logging thread context
 *
 * @note The timestamp is captured at construction time, making it suitable
 *       for precise timing of log events.
 */
struct LogRecord {
    LogLevel level;
    std::chrono::system_clock::time_point timestamp;
    std::thread::id thread_id;
    std::source_location location;
    std::string message;

    /**
     * @brief Construct a new LogRecord
     *
     * @param lvl Severity level of the log message
     * @param msg The log message content (will be moved)
     * @param loc Source location information (defaults to call site)
     *
     * @post timestamp is set to current system time
     * @post thread_id is set to current thread's ID
     * @post message contains the moved message content
     *
     * @note The message parameter is moved into the record for efficiency.
     *       Use std::move when possible to avoid unnecessary copies.
     */
    LogRecord(LogLevel lvl, std::string msg, std::source_location loc = std::source_location::current())
        : level(lvl),
          timestamp(std::chrono::system_clock::now()),
          thread_id(std::this_thread::get_id()),
          location(loc),
          message(std::move(msg)) {}
};

/**
 * @brief Abstract base class for log sinks.
 *
 * Defines the interface for all log output destinations. Concrete sink
 * implementations must inherit from this class and implement the pure virtual
 * methods for writing and flushing log records.
 *
 * @par Responsibilities:
 * - Provides level-based filtering (should_log)
 * - Defines interface for writing log records (write)
 * - Defines interface for flushing output (flush)
 * - Manages minimum log level for the sink
 *
 * @par Thread Safety:
 * - Derived classes must implement thread-safe writing and flushing
 * - Level accessors are thread-safe (noexcept)
 * - Level mutators are not thread-safe (synchronization required if used concurrently)
 *
 * @par Implementation Notes:
 * - Derived classes should handle their own synchronization
 * - Derived classes should respect the level filtering in their write implementations
 * - The destructor must be virtual to ensure proper cleanup of derived classes
 *
 * @see ConsoleSink, FileSink, MemorySink
 */
class LogSink {
  public:
    /**
     * @brief Virtual destructor for proper cleanup of derived classes
     */
    virtual ~LogSink() = default;

    /**
     * @brief Write a log record to the sink
     *
     * Pure virtual method that must be implemented by derived classes to handle
     * the actual output of log records to their specific destination.
     *
     * @param record The log record to write
     *
     * @note Implementations should check should_log() before processing the
     *       record
     * @note Implementations must be thread-safe
     */
    virtual void write(const LogRecord& record) = 0;

    /**
     * @brief Flush any buffered output
     *
     * Pure virtual method that must be implemented by derived classes to ensure
     * all buffered log data is written to the final destination.
     *
     * @note Implementations must be thread-safe
     */
    virtual void flush() = 0;

    /**
     * @brief Set the minimum log level for this sink
     *
     * @param level Minimum level to log (inclusive)
     */
    void set_level(LogLevel level) noexcept { m_level = level; }

    /**
     * @brief Get the current minimum log level
     *
     * @return LogLevel Current minimum level for this sink
     */
    LogLevel level() const noexcept { return m_level; }

    /**
     * @brief Check if a log level should be processed by this sink
     *
     * @param level Level to check
     * @return true if the level should be logged, false otherwise
     *
     * @par Example:
     * @code
     * if (sink.should_log(LogLevel::Debug)) {
     *     // Process debug log
     * }
     * @endcode
     */
    bool should_log(LogLevel level) const noexcept { return level >= m_level; }

  protected:
    LogLevel m_level = LogLevel::Info;
};

/**
 * @brief Console sink for logging to stdout/stderr.
 *
 * A concrete LogSink implementation that writes log records to the standard
 * output stream (stdout or stderr). Formats logs messages with timestamps,
 * log levels, source locations, and the actual message content.
 *
 * @par Features:
 * - Configurable output stream (stdout or stderr)
 * - Thread-safe writing and flushing
 * - Rich formatting with millisecond precision timestamps
 * - Automatic level filtering
 * - Standard output stream compatibility
 *
 * @par Format:
 * `[YYYY-MM-DD HH:MM:SS.mmm] [LEVEL] [filename:line] message`
 *
 * @par Example Output:
 * `[2024-01-15 14:30:25.123] [INFO] [tensor_ops.cpp:42] Tensor operation completed`
 *
 * @par Thread Safety:
 * - Fully thread-safe with mutex protection
 * - Multiple threads can log concurrently without output interleaving
 *
 * @par Performance:
 * - Uses mutex locking for thread safety
 * - Formatting operations may have some overhead
 * - Suitable for most applications, but high-frequency logging may impact performance
 */
class ConsoleSink : public LogSink {
  public:
    /**
     * @brief Construct a new ConsoleSink
     *
     * @param use_stderr If true, log to stderr; if false, log to stdout
     *
     * @par Default Behavior:
     * - Defaults to stdout (use_stderr = false)
     * - Use stderr for error logs or when stdout is redirected
     */
    explicit ConsoleSink(bool use_stderr = false) : m_use_stderr(use_stderr) {}

    /**
     * @brief Write a log record to the console
     *
     * Formats and writes the log record to the configured output stream.
     * Includes timestamp, log level, source location, and message.
     *
     * @param record The log record to write
     *
     * @note Automatically filters records based on sink level
     * @note Thread-safe: uses mutex to prevent output interleaving
     * @note Uses std::format for efficient string formatting
     */
    void write(const LogRecord& record) override {
        if (!should_log(record.level)) return;

        std::lock_guard<std::mutex> lock(m_mutex);
        auto& stream = m_use_stderr ? std::cerr : std::cout;
        auto time_t = std::chrono::system_clock::to_time_t(record.timestamp);
        auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(record.timestamp.time_since_epoch()) % 1000;

        stream << std::format("[{:%Y-%m-%d %H:%M:%S}.{:03d}] [{:5s}] [{}:{}] {}\n",
                              std::chrono::system_clock::from_time_t(time_t), ms.count(), log_level_name(record.level),
                              record.location.file_name(), record.location.line(), record.message);
    }

    /**
     * @brief Flush the output stream
     *
     * Ensures all buffered output is written to the console immediately.
     *
     * @note Thread-safe: uses mutex to synchronize flush operations
     * @note Useful for ensuring log output appears before program termination
     */
    void flush() override {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (m_use_stderr) {
            std::cerr.flush();
        } else {
            std::cout.flush();
        }
    }

  private:
    bool m_use_stderr;
    std::mutex m_mutex;
};

/**
 * @brief File sink for logging to files
 *
 * A concrete LogSink implementation that writes log records to a file.
 * Provides persistent storage of log messages with rich formatting including
 * timestamps, thread IDs, source locations, and log levels.
 *
 * @par Features:
 * - File-based log storage with append mode
 * - Thread-safe file operations
 * - Rich formatting with millisecond precision timestamps
 * - Includes thread ID for debugging multi-threaded applications
 * - Automatic level filtering
 * - Exception-safe file handling
 *
 * @par Format:
 * `[YYYY-MM-DD HH:MM:SS.mmm] [LEVEL] [thread_id] [filename:line] message`
 *
 * @par Example Output:
 * `[2024-01-15 14:30:25.123] [INFO] [0x7f8a5bdf9700] [tensor_ops.cpp:42] Tensor operation completed`
 *
 * @par Thread Safety:
 * - Fully thread-safe with mutex protection
 * - Multiple threads can log concurrently without file corruption
 * - File operations are serialized to prevent interleaving
 *
 * @par Error Handling:
 * - Throws std::runtime_error if file cannot be opened
 * - File operations may set failbits on errors (caller should monitor)
 *
 * @par Performance:
 * - Uses mutex locking for thread safety
 * - File I/O may be slower than console output
 * - Consider buffering strategies for high-frequency logging
 */
class FileSink : public LogSink {
  public:
    /**
     * @brief Construct a new FileSink
     *
     * @param filename Path to the log file
     * @throws std::runtime_error if the file cannot be opened
     *
     * @post File is opened in append mode (ios::app)
     * @post File creation if it doesn't exist, or appending if it does
     *
     * @note The file is kept open until the sink is destroyed
     * @note Uses append mode to preserve existing log content
     */
    explicit FileSink(const std::string& filename) : m_file(filename, std::ios::app) {
        if (!m_file.is_open()) {
            throw std::runtime_error("Failed to open log file: " + filename);
        }
    }

    /**
     * @brief Write a log record to the file
     *
     * Formats and writes the log record to the file with comprehensive
     * metadata including thread ID for multi-threaded debugging.
     *
     * @param record The log record to write
     *
     * @note Automatically filters records based on sink level
     * @note Thread-safe: uses mutex to prevent file access conflicts
     * @note Uses std::format for consistent formatting
     * @note Includes thread ID which is useful for debugging concurrent
     *       applications
     */
    void write(const LogRecord& record) override {
        if (!should_log(record.level)) return;

        std::lock_guard<std::mutex> lock(m_mutex);
        auto time_t = std::chrono::system_clock::to_time_t(record.timestamp);
        auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(record.timestamp.time_since_epoch()) % 1000;

        m_file << std::format("[{:%Y-%m-%d %H:%M:%S}.{:03d}] [{:5s}] [{}] [{}:{}] {}\n",
                              std::chrono::system_clock::from_time_t(time_t), ms.count(), log_level_name(record.level),
                              record.thread_id, record.location.file_name(), record.location.line(), record.message);
    }

    /**
     * @brief Flush the file buffer
     *
     * Ensures all buffered data is written to disk immediately.
     * Useful for ensuring log persistence before critical operations
     * or program termination.
     *
     * @note Thread-safe: uses mutex to synchronize flush operations
     * @note Calls std::ofstream::flush() to force write to disk
     */
    void flush() override {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_file.flush();
    }

  private:
    std::ofstream m_file;
    std::mutex m_mutex;
};

/**
 * @brief Memory sink for logging to memory (useful for testing)
 *
 * A concrete LogSink implementation that stores log records in memory rather
 * than writing them to an external destination. Primarily designed for testing
 * and debugging purposes where you need to verify log output programmatically.
 *
 * @par Features:
 * - In-memory log storage for programmatic access
 * - Thread-safe record collection and retrieval
 * - No I/O overhead (fastest logging option)
 * - Full log record preservation including all metadata
 * - Test-friendly interface for inspecting logged messages
 *
 * @par Typical Use Cases:
 * - Unit testing log output verification
 * - Debugging complex logging scenarios
 * - Capturing logs for later analysis without I/O
 * - Performance testing without disk/console overhead
 *
 * @par Thread Safety:
 * - Fully thread-safe with mutex protection
 * - Concurrent writing and reading supported
 * - Clear operation is also thread-safe
 *
 * @par Performance:
 * - Minimal overhead (memory operations only)
 * - No I/O bottlenecks
 * - Suitable for high-frequency logging in tests
 * - Vector growth may cause occasional allocations
 */
class MemorySink : public LogSink {
  public:
    /**
     * @brief Write a log record to memory
     *
     * Stores the complete log record in an internal vector for later retrieval.
     * The record is copied into storage, preserving all metadata.
     *
     * @param record The log record to store
     *
     * @note Automatically filters records based on sink level
     * @note Thread-safe: uses mutex to prevent data races
     * @note Records are stored by value (copied)
     */
    void write(const LogRecord& record) override {
        if (!should_log(record.level)) return;

        std::lock_guard<std::mutex> lock(m_mutex);
        m_records.push_back(record);
    }

    /**
     * @brief Flush operation (no-op for memory sink)
     *
     * Provided for interface compatibility. Memory sink doesn't require
     * flushing as records are immediately available in memory.
     */
    void flush() override {
        // Nothing to flush for memory sink
    }

    /**
     * @brief Retrieve all stored log records
     *
     * Returns a copy of all log records currently stored in the sink.
     * Useful for verifying log output in tests.
     *
     * @return std::vector<LogRecord> Copy of all stored records
     *
     * @note Thread-safe: can be called while other threads are logging
     * @note Returns a copy to avoid thread safety issues with direct access
     *
     * @par Example (testing):
     * @code
     * MemorySink sink;
     * sink.write(some_record);
     * auto records = sink.get_records();
     * ASSERT_EQ(records.size(), 1);
     * ASSERT_EQ(records[0].message, "expected message");
     * @endcode
     */
    std::vector<LogRecord> get_records() const {
        std::lock_guard<std::mutex> lock(m_mutex);
        return m_records;
    }

    /**
     * @brief Clear all stored log records
     *
     * Removes all records from memory storage. Useful for resetting
     * the sink between test cases or logging sessions.
     *
     * @note Thread-safe: can be called while other threads are logging
     * @note Post-condition: records_.empty() == true
     */
    void clear() {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_records.clear();
    }

  private:
    mutable std::mutex m_mutex;
    std::vector<LogRecord> m_records;
};

/**
 * @brief
 *
 */
class Logger {
  public:
    /**
     * @brief Construct a new Logger instance
     *
     * Creates a logger with default log level (Info) and no sinks.
     * Sinks must be added using `add_sink()`.
     */
    Logger() = default;

    /**
     * @brief Add a log sink to the logger
     *
     * @param sink Unique pointer to a LogSink implementation
     * @note Takes ownership of the sink pointer
     * @note Thread-safe: uses mutex to synchronize sink list modification
     *
     * @par Example:
     * @code {cpp}
     * logger.add_sink(std::make_unique<ConsoleSink>());
     * logger.add_sink(std::make_unique<FileSink>("app.log"));
     * @endcode
     */
    void add_sink(std::unique_ptr<LogSink> sink) {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_sinks.push_back(std::move(sink));
    }

    /**
     * @brief Set the global log level
     *
     * @param level Minimum level to log (inclusive)
     *
     * @note Affects all logging through this logger instance
     * @note Individual sinks may have their own level filters
     */
    void set_level(LogLevel level) noexcept { m_level = level; }

    /**
     * @brief Get the current global log level
     *
     * @return LogLevel Current minimum log level
     */
    LogLevel level() const noexcept { return m_level; }

    /**
     * @brief Check if a log level should be processed
     *
     * @param level Level to check
     * @return true if level meets or exceeds global log level
     */
    bool should_log(LogLevel level) const noexcept { return level >= m_level; }

    /**
     * @brief Log a message with specified level
     *
     * @param level Severity level of the message
     * @param message The log message content
     * @param location Source location information (defaults to call site)
     *
     * @note Thread-safe: uses mutex to synchronize sink access
     * @note Respects both global and per-sink level filtering
     */
    void log(LogLevel level, const std::string& message,
             std::source_location location = std::source_location::current()) {
        if (!should_log(level)) return;

        LogRecord record(level, message, location);
        std::lock_guard<std::mutex> lock(m_mutex);

        for (auto& sink : m_sinks) {
            sink->write(record);
        }
    }

    /**
     * @brief Log a formatted message with specified level
     *
     * @tparam Args Types of format arguments
     * @param level Severity level of the message
     * @param fmt Format string following std::format syntax
     * @param args Arguments to be formatted
     * @param location Source location information (defaults to call site)
     *
     * @note Uses std::format for type-safe string formatting
     * @note Thread-safe: uses mutex to synchronize sink access
     * @note May throw std::format_error on invalid format strings
     */
    template <typename... Args>
    void log(LogLevel level, std::format_string<Args...> fmt, Args&&... args,
             std::source_location location = std::source_location::current()) {
        if (!should_log(level)) return;

        std::string message = std::format(fmt, std::forward<Args>(args)...);
        log(level, message, location);
    }

    /**
     * @brief Flush all log sinks
     *
     * Ensures all buffered log data is written to their respective
     * destinations. Useful before program termination or critical operations.
     *
     * @note Thread-safe: uses mutex to synchronize sink access
     * @note Calls flush() on each sink sequentially
     */
    void flush() {
        std::lock_guard<std::mutex> lock(m_mutex);
        for (auto& sink : m_sinks) {
            sink->flush();
        }
    }

    /**
     * @brief Log a TRACE level message
     *
     * @param message The log message content
     * @param location Source location information (defaults to call site)
     */
    void trace(const std::string& message, std::source_location location = std::source_location::current()) {
        log(LogLevel::Trace, message, location);
    }

    /**
     * @brief Log a formatted TRACE level message
     *
     * @tparam Args Types of format arguments
     * @param fmt Format string following std::format syntax
     * @param args Arguments to be formatted
     * @param location Source location information (defaults to call site)
     */
    template <typename... Args>
    void trace(std::format_string<Args...> fmt, Args&&... args,
               std::source_location location = std::source_location::current()) {
        log(LogLevel::Trace, fmt, std::forward<Args>(args)..., location);
    }

    /**
     * @brief Log a DEBUG level message
     *
     * @param message The log message content
     * @param location Source location information (defaults to call site)
     */
    void debug(const std::string& message, std::source_location location = std::source_location::current()) {
        log(LogLevel::Debug, message, location);
    }

    /**
     * @brief Log a formatted DEBUG level message
     *
     * @tparam Args Types of format arguments
     * @param fmt Format string following std::format syntax
     * @param args Arguments to be formatted
     * @param location Source location information (defaults to call site)
     */
    template <typename... Args>
    void debug(std::format_string<Args...> fmt, Args&&... args,
               std::source_location location = std::source_location::current()) {
        log(LogLevel::Debug, fmt, std::forward<Args>(args)..., location);
    }

    /**
     * @brief Log an INFO level message
     *
     * @param message The log message content
     * @param location Source location information (defaults to call site)
     */
    void info(const std::string& message, std::source_location location = std::source_location::current()) {
        log(LogLevel::Info, message, location);
    }

    /**
     * @brief Log a formatted INFO level message
     *
     * @tparam Args Types of format arguments
     * @param fmt Format string following std::format syntax
     * @param args Arguments to be formatted
     * @param location Source location information (defaults to call site)
     */
    template <typename... Args>
    void info(std::format_string<Args...> fmt, Args&&... args,
              std::source_location location = std::source_location::current()) {
        log(LogLevel::Info, fmt, std::forward<Args>(args)..., location);
    }

    /**
     * @brief Log a WARNING level message
     *
     * @param message The log message content
     * @param location Source location information (defaults to call site)
     */
    void warn(const std::string& message, std::source_location location = std::source_location::current()) {
        log(LogLevel::Warning, message, location);
    }

    /**
     * @brief Log a formatted WARNING level message
     *
     * @tparam Args Types of format arguments
     * @param fmt Format string following std::format syntax
     * @param args Arguments to be formatted
     * @param location Source location information (defaults to call site)
     */
    template <typename... Args>
    void warn(std::format_string<Args...> fmt, Args&&... args,
              std::source_location location = std::source_location::current()) {
        log(LogLevel::Warning, fmt, std::forward<Args>(args)..., location);
    }

    /**
     * @brief Log an ERROR level message
     *
     * @param message The log message content
     * @param location Source location information (defaults to call site)
     */
    void error(const std::string& message, std::source_location location = std::source_location::current()) {
        log(LogLevel::Error, message, location);
    }

    /**
     * @brief Log a formatted ERROR level message
     *
     * @tparam Args Types of format arguments
     * @param fmt Format string following std::format syntax
     * @param args Arguments to be formatted
     * @param location Source location information (defaults to call site)
     */
    template <typename... Args>
    void error(std::format_string<Args...> fmt, Args&&... args,
               std::source_location location = std::source_location::current()) {
        log(LogLevel::Error, fmt, std::forward<Args>(args)..., location);
    }

    /**
     * @brief Log a CRITICAL level message
     *
     * @param message The log message content
     * @param location Source location information (defaults to call site)
     */
    void critical(const std::string& message, std::source_location location = std::source_location::current()) {
        log(LogLevel::Critical, message, location);
    }

    /**
     * @brief Log a formatted CRITICAL level message
     *
     * @tparam Args Types of format arguments
     * @param fmt Format string following std::format syntax
     * @param args Arguments to be formatted
     * @param location Source location information (defaults to call site)
     */
    template <typename... Args>
    void critical(std::format_string<Args...> fmt, Args&&... args,
                  std::source_location location = std::source_location::current()) {
        log(LogLevel::Critical, fmt, std::forward<Args>(args)..., location);
    }

  private:
    LogLevel m_level = LogLevel::Info;
    std::vector<std::unique_ptr<LogSink>> m_sinks;
    mutable std::mutex m_mutex;
};

/**
 * @brief Global logger instance
 */
namespace detail {
/**
 * @brief Returns a reference to the global logger instance.
 *
 * This function provides thread-safe access to a singleton Logger instance
 * that is initialized once on first call. The logger is configured with a
 * default console sink and can be customized via environment variables.
 *
 * The logger's log level can be set using the `BREZEL_LOG_LEVEL` environment
 * variable with the following case-insensitive values:
 * - "trace": LogLevel::Trace
 * - "debug": LogLevel::Debug
 * - "info": LogLevel::Info (default if invalid value provided)
 * - "warn" or "warning": LogLevel::Warning
 * - "error": LogLevel::Error
 * - "critical" or "crit": LogLevel::Critical
 * - "off": LogLevel::Off
 *
 * If the environment variable is not set, the logger uses its default log
 * level configuration.
 *
 * @note This function is thread-safe and may be called concurrently from
 *       multiple threads. The initialization occurs exactly once.
 *
 * @return Logger& Reference to the global logger instance.
 * @exception No exceptions are thrown by this function.
 *
 * @see Logger, ConsoleSink, LogLevel
 */
inline Logger& get_global_logger() {
    static Logger logger;
    static std::once_flag init_flag;

    std::call_once(init_flag, []() {
        logger.add_sink(std::make_unique<ConsoleSink>());
        if (const char* env_level = std::getenv("BREZEL_LOG_LEVEL")) {
            std::string level_str(env_level);
            std::transform(level_str.begin(), level_str.end(), level_str.begin(),
                           [](char c) { return std::tolower(c); });

            if (level_str == "trace")
                logger.set_level(LogLevel::Trace);
            else if (level_str == "debug")
                logger.set_level(LogLevel::Debug);
            else if (level_str == "info")
                logger.set_level(LogLevel::Info);
            else if (level_str == "warn" || level_str == "warning")
                logger.set_level(LogLevel::Warning);
            else if (level_str == "error")
                logger.set_level(LogLevel::Error);
            else if (level_str == "critical" || level_str == "crit")
                logger.set_level(LogLevel::Critical);
            else if (level_str == "off")
                logger.set_level(LogLevel::Off);
            else
                logger.set_level(LogLevel::Info);
        }
    });

    return logger;
}
}  // namespace detail

/**
 * @brief Global logging functions
 *
 * These functions provide a convenient interface for logging messages
 * at various severity levels using the global logger instance.
 */

/**
 * @brief Returns a reference to the global logger instance.
 *
 * This is a convenience wrapper around detail::get_global_logger().
 *
 * @return Logger& Reference to the global logger instance.
 * @see detail::get_global_logger()
 */
inline Logger& get_logger() { return detail::get_global_logger(); }

/**
 * @brief Sets the log level for the global logger.
 * @param level The minimum severity level to log. Messages with lower
 *              severity will be discarded.
 *
 * @see LogLevel
 */
inline void set_log_level(LogLevel level) { get_logger().set_level(level); }

/**
 * @brief Adds a sink to the global logger.
 * @param sink Unique pointer to the log sink to add. The logger takes
 *             ownership of the sink.
 *
 * @see LogSink
 */
inline void add_log_sink(std::unique_ptr<LogSink> sink) { get_logger().add_sink(std::move(sink)); }

/**
 * @brief Logs a trace severity message.
 *
 * @param message The message to log.
 * @param location The source location of the log call (automatically captured).
 */
inline void log_trace(const std::string& message, std::source_location location = std::source_location::current()) {
    get_logger().trace(message, location);
}

/**
 * @brief Logs a formatted trace severity message.
 *
 * @tparam Args Types of the format arguments.
 * @param fmt Format string following std::format syntax.
 * @param args Arguments to be formatted.
 * @param location The source location of the log call (automatically captured).
 */
template <typename... Args>
inline void log_trace(std::format_string<Args...> fmt, Args&&... args,
                      std::source_location location = std::source_location::current()) {
    get_logger().trace(fmt, std::forward<Args>(args)..., location);
}

/**
 * @brief Logs a debug severity message.
 *
 * @param message The message to log.
 * @param location The source location of the log call (automatically captured).
 */
inline void log_debug(const std::string& message, std::source_location location = std::source_location::current()) {
    get_logger().debug(message, location);
}

/**
 * @brief Logs a formatted debug severity message.
 *
 * @tparam Args Types of the format arguments.
 * @param fmt Format string following std::format syntax.
 * @param args Arguments to be formatted.
 * @param location The source location of the log call (automatically captured).
 */
template <typename... Args>
inline void log_debug(std::format_string<Args...> fmt, Args&&... args,
                      std::source_location location = std::source_location::current()) {
    get_logger().debug(fmt, std::forward<Args>(args)..., location);
}

/**
 * @brief Logs an info severity message.
 *
 * @param message The message to log.
 * @param location The source location of the log call (automatically captured).
 */
inline void log_info(const std::string& message, std::source_location location = std::source_location::current()) {
    get_logger().info(message, location);
}

/**
 * @brief Logs a formatted info severity message.
 *
 * @tparam Args Types of the format arguments.
 * @param fmt Format string following std::format syntax.
 * @param args Arguments to be formatted.
 * @param location The source location of the log call (automatically captured).
 */
template <typename... Args>
inline void log_info(std::format_string<Args...> fmt, Args&&... args,
                     std::source_location location = std::source_location::current()) {
    get_logger().info(fmt, std::forward<Args>(args)..., location);
}

/**
 * @brief Logs a warning severity message.
 *
 * @param message The message to log.
 * @param location The source location of the log call (automatically captured).
 */
inline void log_warn(const std::string& message, std::source_location location = std::source_location::current()) {
    get_logger().warn(message, location);
}

/**
 * @brief Logs a formatted warning severity message.
 *
 * @tparam Args Types of the format arguments.
 * @param fmt Format string following std::format syntax.
 * @param args Arguments to be formatted.
 * @param location The source location of the log call (automatically captured).
 */
template <typename... Args>
inline void log_warn(std::format_string<Args...> fmt, Args&&... args,
                     std::source_location location = std::source_location::current()) {
    get_logger().warn(fmt, std::forward<Args>(args)..., location);
}

/**
 * @brief Logs an error severity message.
 *
 * @param message The message to log.
 * @param location The source location of the log call (automatically captured).
 */
inline void log_error(const std::string& message, std::source_location location = std::source_location::current()) {
    get_logger().error(message, location);
}

/**
 * @brief Logs a formatted error severity message.
 *
 * @tparam Args Types of the format arguments.
 * @param fmt Format string following std::format syntax.
 * @param args Arguments to be formatted.
 * @param location The source location of the log call (automatically captured).
 */
template <typename... Args>
inline void log_error(std::format_string<Args...> fmt, Args&&... args,
                      std::source_location location = std::source_location::current()) {
    get_logger().error(fmt, std::forward<Args>(args)..., location);
}

/**
 * @brief Logs a critical severity message.
 *
 * @param message The message to log.
 * @param location The source location of the log call (automatically captured).
 */
inline void log_critical(const std::string& message, std::source_location location = std::source_location::current()) {
    get_logger().critical(message, location);
}

/**
 * @brief Logs a formatted critical severity message.
 *
 * @tparam Args Types of the format arguments.
 * @param fmt Format string following std::format syntax.
 * @param args Arguments to be formatted.
 * @param location The source location of the log call (automatically captured).
 */
template <typename... Args>
inline void log_critical(std::format_string<Args...> fmt, Args&&... args,
                         std::source_location location = std::source_location::current()) {
    get_logger().critical(fmt, std::forward<Args>(args)..., location);
}
}  // namespace brezel::logging

/**
 * @brief Logging macros for convenience
 */
#ifdef BREZEL_DEBUG
#define BREZEL_LOG_TRACE(...) ::brezel::logging::log_trace(__VA_ARGS__)
#define BREZEL_LOG_DEBUG(...) ((void)0)
#endif

#define BREZEL_LOG_INFO(...) ::brezel::logging::log_info(__VA_ARGS__)
#define BREZEL_LOG_WARN(...) ::brezel::logging::log_warn(__VA_ARGS__)
#define BREZEL_LOG_ERROR(...) ::brezel::logging::log_error(__VA_ARGS__)
#define BREZEL_LOG_CRITICAL(...) ::brezel::logging::log_critical(__VA_ARGS__)

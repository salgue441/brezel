FROM ubuntu:22.04

# Prevent interactive prompts during package installation
ARG DEBIAN_FRONTEND=noninteractive

# Install basic dependencies
RUN apt-get update && apt-get install -y \
  build-essential \
  cmake \
  git \
  libfmt-dev \
  libblas-dev \
  liblapack-dev \
  libopenblas-dev \
  libomp-dev \
  cppcheck \
  clang-tidy \
  clang \
  curl \
  ca-certificates \
  doxygen \
  graphviz \
  && rm -rf /var/lib/apt/lists/*

# Install spdlog from source
RUN git clone --depth 1 --branch v1.11.0 https://github.com/gabime/spdlog.git /tmp/spdlog \
  && cd /tmp/spdlog \
  && mkdir build && cd build \
  && cmake .. -DCMAKE_BUILD_TYPE=Release -DSPDLOG_BUILD_EXAMPLES=OFF -DSPDLOG_BUILD_TESTS=OFF \
  && cmake --build . --config Release --parallel $(nproc) \
  && cmake --install . \
  && cd / && rm -rf /tmp/spdlog

# Install Catch2 from source
RUN git clone --depth 1 --branch v3.4.0 https://github.com/catchorg/Catch2.git /tmp/catch2 \
  && cd /tmp/catch2 \
  && mkdir build && cd build \
  && cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF \
  && cmake --build . --config Release --parallel $(nproc) \
  && cmake --install . \
  && cd / && rm -rf /tmp/catch2

# Create a non-root user to run the build
RUN useradd -m developer
USER developer
WORKDIR /home/developer

# Make sure we're using the correct bash path
SHELL ["/bin/bash", "-c"]
CMD ["/bin/bash"]
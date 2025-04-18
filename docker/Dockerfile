FROM ubuntu:22.04 

# Set up non-interactive apt installation
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
  build-essential \
  cmake \
  ninja-build \
  git \
  curl \
  zip \
  unzip \
  tar \
  pkg-config \
  python3 \
  python3-pip \
  libomp-dev \
  libopenblas-dev \
  && rm -rf /var/lib/apt/lists/*

# Create a non-root user
ARG USERNAME=dev
ARG USER_UID=1000
ARG USER_GID=${USER_UID}

RUN groupadd --gid ${USER_GID} ${USERNAME} \
  && useradd --uid ${USER_GID} --gid ${USER_GID} -m ${USERNAME}

# Create workspace directory
WORKDIR /workspace
RUN chown ${USER_UID}:${USER_GID} /workspace

# Switch to non-root user
USER ${USERNAME}

# Set up vcpkg
ENV VCPKG_ROOT=/workspace/vcpkg
RUN git clone https://github.com/microsoft/vcpkg.git $VCPKG_ROOT \
  && $VCPKG_ROOT/bootstrap-vcpkg.sh -disableMetrics

# Environment variables & entrypoint
ENV PATH="${VCPKG_ROOT}:${PATH}"
ENTRYPOINT ["/bin/bash"]
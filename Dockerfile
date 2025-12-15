FROM ubuntu:24.04

# 1) System update/upgrade and base tooling
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get upgrade -y && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    ca-certificates curl software-properties-common && \
    rm -rf /var/lib/apt/lists/*

# 2) Language runtimes/compilers
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    build-essential make cmake pkg-config git \
    gcc g++ clang \
    python3 python3-pip python3-venv python-is-python3 \
    golang \
    nodejs npm \
    ruby-full \
    ghc cabal-install libghc-vector-dev \
    sbcl \
    nasm \
    time \
    && rm -rf /var/lib/apt/lists/*

# Julia (binary install; package not available in some Ubuntu repos)
ARG JULIA_VERSION=1.10.5
RUN curl -L https://julialang-s3.julialang.org/bin/linux/x64/1.10/julia-${JULIA_VERSION}-linux-x86_64.tar.gz \
    | tar -xz -C /opt && \
    ln -s /opt/julia-${JULIA_VERSION}/bin/julia /usr/local/bin/julia

# Rust (rustup)
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# 3) Language package installs (cached by manifest copies)
WORKDIR /tmp/build-cache
RUN mkdir -p /tmp/requirements /tmp/day10
COPY day*/requirements.txt /tmp/requirements/
RUN set -eux; \
    for req in /tmp/requirements/*.txt; do \
        if [ -s "$req" ]; then PIP_BREAK_SYSTEM_PACKAGES=1 pip install --no-cache-dir --break-system-packages -r "$req"; fi; \
    done
RUN PIP_BREAK_SYSTEM_PACKAGES=1 pip install --no-cache-dir --break-system-packages networkx numpy

RUN npm install -g ts-node typescript
ENV TS_NODE_TRANSPILE_ONLY=1
ENV TS_NODE_COMPILER_OPTIONS='{"module":"CommonJS"}'

COPY day10/Cargo.toml day10/Cargo.lock /tmp/day10/
RUN cargo fetch --locked --manifest-path /tmp/day10/Cargo.toml

COPY day10/go.mod /tmp/day10/
RUN cd /tmp/day10 && GO111MODULE=on go mod download

RUN cabal update && cabal install --lib vector IntervalMap

# Julia packages needed by solutions
RUN julia -e 'using Pkg; Pkg.add(["IntervalSets","DataStructures","Graphs"])'

# 4) Application sources
WORKDIR /app
COPY . /app

# Allow system pip installs inside container (PEP 668)
ENV PIP_BREAK_SYSTEM_PACKAGES=1

CMD ["python3", "run_all.py", "--install"]

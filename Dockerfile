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
    ghc cabal-install \
    sbcl \
    julia \
    nasm \
    && rm -rf /var/lib/apt/lists/*

# Rust (rustup)
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# 3) Language package installs (cached by manifest copies)
WORKDIR /tmp/build-cache
RUN mkdir -p /tmp/requirements /tmp/day10
COPY day*/requirements.txt /tmp/requirements/
RUN set -eux; \
    for req in /tmp/requirements/*.txt; do \
        if [ -s "$req" ]; then pip install --no-cache-dir -r "$req"; fi; \
    done

RUN npm install -g ts-node typescript

COPY day10/Cargo.toml day10/Cargo.lock /tmp/day10/
RUN cargo fetch --locked --manifest-path /tmp/day10/Cargo.toml

COPY day10/go.mod /tmp/day10/
RUN cd /tmp/day10 && GO111MODULE=on go mod download

RUN cabal update

# 4) Application sources
WORKDIR /app
COPY . /app

CMD ["python3", "run_all.py", "--install"]

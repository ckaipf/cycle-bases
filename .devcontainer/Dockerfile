FROM mcr.microsoft.com/devcontainers/base:bookworm

ARG STACK_VERSION=3.11.1
ARG STACK_SHA256_AMD64=1fda71e657cd8d355625cc66b61b352699279dfee2664c014a392163bd19a952
ARG STACK_SHA256_ARM64=1617ae9976a5cd38ad4daec583b026b589eb45d5482afb045cd4ca8c8d0de6d0

# Native libraries GHC needs to build and link.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential libffi-dev libgmp-dev libncurses-dev pkg-config zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN case "$(dpkg --print-architecture)" in \
        amd64) arch=x86_64;  sha="${STACK_SHA256_AMD64}" ;; \
        arm64) arch=aarch64; sha="${STACK_SHA256_ARM64}" ;; \
        *) echo "unsupported architecture" >&2; exit 1 ;; \
    esac \
    && curl -fsSL -o /tmp/stack.tar.gz \
        "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-${arch}.tar.gz" \
    && echo "${sha}  /tmp/stack.tar.gz" | sha256sum -c - \
    && tar -xzf /tmp/stack.tar.gz -C /usr/local/bin --strip-components=1 --wildcards '*/stack' \
    && rm /tmp/stack.tar.gz

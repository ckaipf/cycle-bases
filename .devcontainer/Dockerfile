FROM mcr.microsoft.com/devcontainers/base:bookworm

ARG STACK_VERSION=3.11.1
ARG STACK_SHA256_AMD64=1fda71e657cd8d355625cc66b61b352699279dfee2664c014a392163bd19a952
ARG STACK_SHA256_ARM64=1617ae9976a5cd38ad4daec583b026b589eb45d5482afb045cd4ca8c8d0de6d0
ARG FOURMOLU_VERSION=0.20.1.0
ARG FOURMOLU_SHA256_AMD64=bdd97063b445145b39191259c428bca489fa30578b82d9066fc7377db390f1d0
ARG FOURMOLU_SHA256_ARM64=56ad7ae69b4bff12d4144ae86fdfec2cab16a19e29f76fd316c491a986c0c335

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

RUN case "$(dpkg --print-architecture)" in \
        amd64) arch=x86_64; sha="${FOURMOLU_SHA256_AMD64}" ;; \
        arm64) arch=arm64;  sha="${FOURMOLU_SHA256_ARM64}" ;; \
        *) echo "unsupported architecture" >&2; exit 1 ;; \
    esac \
    && curl -fsSL -o /tmp/fourmolu.zip \
        "https://github.com/fourmolu/fourmolu/releases/download/v${FOURMOLU_VERSION}/fourmolu-${FOURMOLU_VERSION}-linux-${arch}.zip" \
    && echo "${sha}  /tmp/fourmolu.zip" | sha256sum -c - \
    && unzip -j /tmp/fourmolu.zip '*/fourmolu' -d /usr/local/bin \
    && chmod +x /usr/local/bin/fourmolu \
    && rm /tmp/fourmolu.zip

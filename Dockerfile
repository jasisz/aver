FROM --platform=linux/amd64 rust:1.95.0-bookworm AS builder

ARG LEAN_TOOLCHAIN=leanprover/lean4:v4.32.0
ARG DAFNY_VERSION=4.11.0

ENV ELAN_HOME=/opt/elan
ENV PATH=/opt/elan/bin:/opt/dafny:/usr/local/cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends ca-certificates curl git unzip; \
    rm -rf /var/lib/apt/lists/*

RUN set -eux; \
    curl -sSfL --retry 6 --retry-all-errors --retry-delay 15 --connect-timeout 30 \
        https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
        | sh -s -- -y --default-toolchain "${LEAN_TOOLCHAIN}"; \
    lean --version; \
    lake --version

RUN set -eux; \
    curl -fSL --retry 6 --retry-all-errors --retry-delay 15 --connect-timeout 30 \
        -o /tmp/dafny.zip \
        "https://github.com/dafny-lang/dafny/releases/download/v${DAFNY_VERSION}/dafny-${DAFNY_VERSION}-x64-ubuntu-22.04.zip"; \
    unzip -q /tmp/dafny.zip -d /opt/dafny-install; \
    dafny_dir="$(dirname "$(find /opt/dafny-install -name dafny -type f | head -n 1)")"; \
    ln -s "${dafny_dir}" /opt/dafny; \
    chmod +x /opt/dafny/dafny || true; \
    dafny --version; \
    rm -f /tmp/dafny.zip

WORKDIR /work

COPY Cargo.toml Cargo.lock build.rs ./
COPY aver-memory ./aver-memory
COPY aver-rt ./aver-rt
COPY aver-lsp ./aver-lsp
COPY aver-cert ./aver-cert
COPY src ./src
COPY benches ./benches

RUN cargo build --bin aver --features wasm && \
    cargo build -p aver-cert --bin aver-cert

FROM --platform=linux/amd64 debian:bookworm-slim AS runtime

ENV ELAN_HOME=/opt/elan
ENV PATH=/opt/elan/bin:/opt/dafny:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends ca-certificates; \
    rm -rf /var/lib/apt/lists/*

WORKDIR /work

COPY --from=builder /opt/elan /opt/elan
COPY --from=builder /opt/dafny-install /opt/dafny-install
COPY --from=builder /opt/dafny /opt/dafny
COPY --from=builder /work/target/debug/aver /usr/local/bin/aver
COPY --from=builder /work/target/debug/aver-cert /usr/local/bin/aver-cert
COPY examples/core/hello.av ./examples/core/hello.av
COPY examples/formal/validated_wrapper_law.av ./examples/formal/validated_wrapper_law.av
COPY examples/certification/add_one.av ./examples/certification/add_one.av

RUN set -eux; \
    lean --version; \
    lake --version; \
    dafny --version; \
    aver run examples/core/hello.av; \
    rm -rf /tmp/aver-proof-smoke-build; \
    aver proof examples/formal/validated_wrapper_law.av --backend lean --check -o /tmp/aver-proof-smoke-build; \
    rm -rf /tmp/aver-cert-smoke-build; \
    aver compile examples/certification/add_one.av --target wasm-gc --certify -o /tmp/aver-cert-smoke-build; \
    aver-cert check /tmp/aver-cert-smoke-build/add_one.wasm /tmp/aver-cert-smoke-build/cert

CMD ["sh", "-c", "aver run examples/core/hello.av && rm -rf /tmp/aver-proof-smoke-run /tmp/aver-cert-smoke-run && aver proof examples/formal/validated_wrapper_law.av --backend lean --check -o /tmp/aver-proof-smoke-run && aver compile examples/certification/add_one.av --target wasm-gc --certify -o /tmp/aver-cert-smoke-run && aver-cert check /tmp/aver-cert-smoke-run/add_one.wasm /tmp/aver-cert-smoke-run/cert"]

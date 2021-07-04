#
# Build the frontend.
#
FROM nixos/nix:2.3 as dashboard-builder

WORKDIR /usr/src/app

COPY ui ./

RUN nix-shell --run "yarn run bundle"

FROM rust:1.52-buster as server-builder

WORKDIR /build

COPY Cargo.toml .
COPY Cargo.lock .
COPY sql sql/
COPY src src/

RUN cargo build --release

FROM debian:buster-slim

COPY --from=dashboard-builder /usr/src/app/dist            /app/dist
COPY --from=server-builder    /build/target/release/server /app/blind
COPY --from=server-builder    /build/sql                   /app/sql

RUN apt-get update -qq -y && \
    apt-get install --no-install-recommends -y ca-certificates libssl-dev upx && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    upx /app/blind

CMD ["/app/blind", "-s",  "/app/dist"]
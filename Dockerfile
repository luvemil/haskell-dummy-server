FROM fpco/stack-build-small:lts-17.15 as build
RUN mkdir -p /opt/build/dist
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc --copy-bins --local-bin-path dist

FROM ubuntu:20.04
RUN mkdir -p /opt/myapp
ARG BINARY_PATH
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE
COPY --from=build /opt/build/dist/haskell-dummy-server-exe .
CMD ["/opt/myapp/haskell-dummy-server-exe"]
FROM ubuntu:20.04
RUN mkdir -p /opt/myapp
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE
COPY haskell-dummy-server-exe .
CMD ["/opt/myapp/haskell-dummy-server-exe"]
PROJECT=haskell-dummy-server
SHORT_TAG=$$(git log -1 --pretty=format:%h)

all: build-docker

build-docker:
	if [ -z "$$(git status -s)" ]; \
		then docker build -t ${PROJECT}:${SHORT_TAG} . ; \
		else echo "Refusing to build from unclean tree"; \
	fi
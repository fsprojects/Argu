SOURCE_DIRECTORY := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
TOOLS_FOLDER := $(SOURCE_DIRECTORY)tools
IMAGE_NAME_FILE := $(TOOLS_FOLDER)/image-name
IMAGE_NAME := argu-build
TARGET := Bundle
BUILD_VERSION := 0.0.1-alpha
TEST_COVERAGE := all

init-container:
	mkdir -p $(TOOLS_FOLDER)
	echo "$(IMAGE_NAME)-`date +%s`" > $(IMAGE_NAME_FILE)
	docker build -t `cat $(IMAGE_NAME_FILE)` $(SOURCE_DIRECTORY)
	docker run --detach --name `cat $(IMAGE_NAME_FILE)` \
			`cat $(IMAGE_NAME_FILE)` \
			sh -c 'while true; do sleep 10; done' # Block the main container process forever

build: init-container
	docker exec `cat $(IMAGE_NAME_FILE)` ./build.sh $(TARGET)

copy-artifacts:
	docker cp `cat $(IMAGE_NAME_FILE)`:/app/artifacts $(SOURCE_DIRECTORY)

bash:
	docker exec -it `cat $(IMAGE_NAME_FILE)` bash

clean:
	docker rm -f `cat $(IMAGE_NAME_FILE)`
	rm -f $(IMAGE_NAME_FILE)

.DEFAULT_GOAL := build
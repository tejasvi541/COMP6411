#!/bin/bash

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
  echo "Docker is not running. Please start Docker and try again."
  exit 1
fi

# Start the container with the specified command
docker container start -a -i comp6411

# Capture the exit status of the docker start command
docker_exit_status=$?

# Check if the container started successfully
if [ $docker_exit_status -ne 0 ]; then
  echo "Failed to start the Docker container comp6411."
  exit 1
fi

echo "Docker container comp6411 started successfully and has now stopped."

#!/bin/bash

if [ -n "$DOCKER_HOST" ]; then
  DOCKER_HOST_IP=$(echo $DOCKER_HOST | sed 's/^.*\/\/\(.*\):[0-9][0-9]*$/\1/g')
elif [ -x "$(command -v docker-machine)" ]; then
  DOCKER_HOST_IP=$(docker-machine ip default)
else
  DOCKER_HOST_IP="127.0.0.1"
fi


# Run containers
export COMPOSE_PROJECT_NAME=cotoami-spec
docker-compose up -d


# Make sure to tear down the containers
function tear_down_containers() {
  echo "Tearing down containers..."
  docker-compose down -v
}
trap tear_down_containers 0 1 2 3 15


# Waiting for services to be ready
echo "Waiting for selenium server to be launched..."
while ! nc -z $DOCKER_HOST_IP 4444; do
  sleep 1s
done


# Run specs
stack --docker test

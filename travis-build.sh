#!/bin/bash
set -eo pipefail

git describe --always > REVISION

PROJECT_VERSION=`cat REVISION`

if [ "$TRAVIS_TAG" = "" ]; then
  REV=`git rev-parse --short HEAD`
  VERSION="$PROJECT_VERSION-dev (build $TRAVIS_BUILD_NUMBER)"
  case $TRAVIS_BRANCH in
    master)
      DOCKER_TAG="dev"
      ;;

    release/*)
      DOCKER_TAG="$PROJECT_VERSION-dev"
      ;;

    # This case should be removed once the feature/docker-full branch is merged into master
    feature/docker-full)
      DOCKER_TAG="dev"
      ;;

    *)
      exit 0
      ;;
  esac
else
  TAG_VERSION="${TRAVIS_TAG/-*/}"
  if [ "$PROJECT_VERSION" != "$TAG_VERSION" ]; then
    echo "Project version and tag differs: $PROJECT_VERSION != $TRAVIS_TAG"
    exit 1
  fi

  VERSION="$TRAVIS_TAG (build $TRAVIS_BUILD_NUMBER)"
  DOCKER_TAG="$TRAVIS_TAG"

  if [ "$TAG_VERSION" = "$TRAVIS_TAG" ]; then
    EXTRA_DOCKER_TAG="${TRAVIS_TAG%.*}"
  fi
fi

echo "Version: $VERSION"
echo $VERSION > VERSION

# Build and push Docker image
echo "Docker tag: $DOCKER_TAG"
docker login -u ${DOCKER_USER} -p ${DOCKER_PASS} ${DOCKER_REGISTRY}

docker build -t ${DOCKER_REPOSITORY}:${DOCKER_TAG} .
docker push ${DOCKER_REPOSITORY}:${DOCKER_TAG}

docker build -t ${DOCKER_REPOSITORY}-broker:${DOCKER_TAG} broker
docker push ${DOCKER_REPOSITORY}-broker:${DOCKER_TAG}

# Push extra image on exact tags
if [ "$EXTRA_DOCKER_TAG" != "" ]; then
  echo "Pushing also as $EXTRA_DOCKER_TAG"
  docker tag ${DOCKER_REPOSITORY}:${DOCKER_TAG} ${DOCKER_REPOSITORY}:${EXTRA_DOCKER_TAG}
  docker push ${DOCKER_REPOSITORY}:${EXTRA_DOCKER_TAG}

  docker tag ${DOCKER_REPOSITORY}-broker:${DOCKER_TAG} ${DOCKER_REPOSITORY}-broker:${EXTRA_DOCKER_TAG}
  docker push ${DOCKER_REPOSITORY}-broker:${EXTRA_DOCKER_TAG}
fi

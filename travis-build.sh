#!/bin/bash
set -eo pipefail

source <(curl -s https://raw.githubusercontent.com/manastech/ci-docker-builder/e556ffa1319a966df778d3559a4b29505ca8dceb/travis-build.sh)

dockerSetup
echo $VERSION > VERSION

dockerBuildAndPush
dockerBuildAndPush -s "-broker" -d broker

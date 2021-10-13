#!/bin/bash
set -eo pipefail

source <(curl -s https://raw.githubusercontent.com/manastech/ci-docker-builder/a2d44f8b7f155afe5a9652252cfb262871846a41/build.sh)

dockerSetup
echo $VERSION > VERSION

dockerBuildAndPush
dockerBuildAndPush -s "-broker" -d broker

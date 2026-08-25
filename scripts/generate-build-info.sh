#!/bin/bash

# Usage:
# ./scripts/generate-build-info.sh <config-dir>
# e.g. ./scripts/generate-build-info.sh config/admin

set -e

CONFIG_DIR=$1

source $CONFIG_DIR/component.sh
BUILD_INFO_FILE=$CONFIG_DIR/build-info.yml

# ---------------------------------------------------------------
# 1. Set a name
# ---------------------------------------------------------------
echo "name: $APPLICATION_NAME" > $BUILD_INFO_FILE

# ---------------------------------------------------------------
# 2. Create app version
#       - if there is a git tag, use it
#       - if there is no git tag, use branch and last commit as a identification of build version
#       - releaseVersion is the release tag on HEAD, or the highest one in the repo
#         (it is the API version reported by Swagger)
# ---------------------------------------------------------------
branch=`git rev-parse --abbrev-ref HEAD`
commit=`git rev-parse --short HEAD`
appVersion="$branch~$commit"

gittag=`git tag --points-at HEAD | head -n 1`
if test -n "$gittag"
then
    appVersion="$gittag~$commit"
fi

echo "version: $appVersion" >> $BUILD_INFO_FILE

release=`echo $gittag | sed "s/^v//"`
if test -z "$release"
then
    release=`git tag --sort=-v:refname --list "v*" | head -n 1 | sed "s/^v//"`
fi
if test -z "$release"
then
    release="0.0.0"
fi

echo "releaseVersion: $release" >> $BUILD_INFO_FILE

# ---------------------------------------------------------------
# 3. Create build timestamp
# ---------------------------------------------------------------
builtAtTimestamp=`date -u +"%Y-%m-%dT%H:%M:%SZ"`
echo "builtAt: $builtAtTimestamp" >> $BUILD_INFO_FILE

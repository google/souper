#!/bin/sh -ex

tar cz Dockerfile update_deps.sh CMakeLists.txt docs include lib test tools unittests | docker build -t souperweb -
container=$(/usr/bin/docker run -d souperweb true)
docker export $container | docker import - souperweb_squashed
docker build -t souperweb_final - < Dockerfile.metadata
docker save souperweb_final | gzip -9 > souperweb.tar.gz

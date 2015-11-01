#!/bin/bash

BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
RUN_DIR="$BASEDIR"/run

SYS_CONFIG="$RUN_DIR"/sys.config
KEY_FILE="$RUN_DIR"/reach.key

COOKIE="ClueCon"

ERL_LIBS="$BASEDIR/apps":"$BASEDIR/deps":$ERL_LIBS
export ERL_LIBS

export MONGO_CONFIG_PATH="$RUN_DIR"/mongo-client.ini

mkdir -p "$RUN_DIR"

if [ ! -f "SYS_CONFIG" ]; then
	cp "$BASEDIR"/files/run/sys.config.example "$SYS_CONFIG"
fi

if [ ! -f "$KEY_FILE" ]; then
    echo "Creating key at $KEY_FILE"
    ssh-keygen -t rsa -f "$KEY_FILE" -N "" -q
    if [ "$?" -ne 0 ]; then
        err_exit "ERROR: Unable to create key at $KEY_FILE"
    fi
fi

cd "$RUN_DIR"
erl -name reach \
	-setcookie "$COOKIE" \
	-pa "$BASEDIR/ebin" \
	-config "$SYS_CONFIG" \
	-s reach

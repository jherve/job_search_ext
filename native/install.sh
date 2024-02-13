#!/bin/sh
# Install the Python application and the "backend" manifest, used to allow the extension to communicate
# with a background process.
#
# https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Native_manifests

set -eu

EXTENSION_NAME="job_search_background"
EXTENSION_BACKEND_PATH=$(realpath $0 | xargs dirname)
EXTENSION_BIN=${EXTENSION_BACKEND_PATH}/run.sh

NATIVE_MESSAGING_DIR=~/.mozilla/native-messaging-hosts
NATIVE_MESSAGING_MANIFEST=${NATIVE_MESSAGING_DIR}/${EXTENSION_NAME}.json

mkdir -p ${NATIVE_MESSAGING_DIR}

(cd ${EXTENSION_BACKEND_PATH} && pdm sync)

cat << EOF > ${NATIVE_MESSAGING_MANIFEST}
{
    "name": "${EXTENSION_NAME}",
    "description": "Example host for native messaging",
    "path": "${EXTENSION_BIN}",
    "type": "stdio",
    "allowed_extensions": [
        "job_search@herve.info",
        "new_ext@mozilla.org"
    ]
}
EOF

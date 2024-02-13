#!/bin/sh

EXTENSION_BACKEND_PATH=$(realpath $0 | xargs dirname)
PYTHON_BIN=${EXTENSION_BACKEND_PATH}/.venv/bin/python

${PYTHON_BIN} -m job_search.writer
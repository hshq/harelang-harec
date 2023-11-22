#!/bin/sh

# args=${*//amd64_sysv/amd64_apple}
# qbe ${args//arm64/arm64_apple}

args=${*/amd64_sysv/amd64_apple}
qbe ${args/arm64/arm64_apple}

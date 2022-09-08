#!/usr/bin/env bash

function main() {
  STAGE=${1:?"Stage required"}
  REGION=${2:?"Region required"}
  LAMBDA_VERSION=${3:?"Lambda version required"}

  unset AWS_ACCESS_KEY_ID
  unset AWS_SECRET_ACCESS_KEY
  export AWS_SDK_LOAD_CONFIG=1
  export AWS_PROFILE="deploy-${REGION}-${STAGE}"
  export $(cat env/deploy/.env.${STAGE} | xargs)
  export LAMBDA_VERSION

  stack build
	stack exec eriko-ikeda-portfolio-fp-cli-exe deploy-application-stack
}

main $*

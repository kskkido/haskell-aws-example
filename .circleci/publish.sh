function main() {
  STAGE=${1:?"Stage required"}
  DIR='~/.ssh/'
  TAG=${STAGE}-`git rev-list --count HEAD`-`git rev-parse --short HEAD`

  if [ `git tag -l "$TAG"` ]; then
    echo "Tag exists"
    exit 1
  else
    mkdir -p $DIR && echo -e "Host github.com\n\tStrictHostKeyChecking no\n" > "$DIR/config"
    git tag $TAG
    git push origin $TAG
  fi
}

if [ ${CIRCLECI} ]; then
  main $*
else
  echo "Only to be run inside CircleCI"
  exit 1;
fi


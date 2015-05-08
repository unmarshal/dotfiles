#!/bin/sh

set -e

VIM_BUNDLE_DIR=~/.vim/bundle

cd $VIM_BUNDLE_DIR

for d in $VIM_BUNDLE_DIR/*; do
  cd $d
  export DIR_NAME=`basename $PWD`
  echo "Updating $DIR_NAME"
  git pull
  cd ..
done

#!/bin/sh
files=`git diff-index --name-only --cached --diff-filter=ACMRTX HEAD | grep -v pre-commit.sh`
sed -i "/oauth-token/d" $files
git add $files

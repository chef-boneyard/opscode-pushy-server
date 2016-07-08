#!/bin/sh

git remote add omnibus-pushy https://github.com/chef/omnibus-pushy
git fetch omnibus-pushy

# The following filter-branch command is mostly copypasta'd from
# an example at the end of `man git-filter-branch`.

# It rewrites the history of the target branch (here
# `omnibus-pushy/master`) adding a prefix (here `omnibus/`) to
# the paths of all files in all commits.

# NOTE: This command requires GNU sed. I ran it on OSX
# and provided GNU sed via `brew install gnu-sed` to get the
# `gsed` binary. On Linux, Just Use `sed`.
git filter-branch --index-filter \
    'git ls-files -s | gsed "s-\t\"*-&omnibus/-" |
     GIT_INDEX_FILE=$GIT_INDEX_FILE.new \
     git update-index --index-info &&
     mv "$GIT_INDEX_FILE.new" "$GIT_INDEX_FILE"
    ' omnibus-pushy/master

git merge omnibus-pushy/master

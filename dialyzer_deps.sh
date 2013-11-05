#!/bin/sh

# Compute a list of locations of ebin directories to create a Dialyzer
# PLT for the dependencies of this Erlang project.  In other words, it
# generates values for the 'files_or_dirs' argument for Dialyzer (see
# http://www.erlang.org/doc/man/dialyzer.html)
#
# When invoked with no arguments, all directories in the 'deps'
# directory are used.
#
# Arguments are taken to be names of directories in 'deps' that you
# wish to *remove* from the list, and thus from your Dialyzer analysis
# (perhaps they aren't properly spec'ed, causing spurious dialyzer
# errors).
#
# Thus, if you have the following directory structure:
#
#     deps
#     ├── bar
#     ├── baz
#     ├── foo
#
# calling this script with no arguments will yield
#
#     deps/bar/ebin deps/baz/ebin deps/foo/ebin
#
# If you want to ignore the 'bar' and 'baz' dependencies
# invoke like this:
#
#     ./dialyzer_deps.sh bar baz
#
# This will yield
#
#     deps/foo/ebin

find deps -type d $(echo $* | awk 'BEGIN{RS=" "; ORS=" "}{print("-not \( -path deps/" $1 " -prune \)")}') -name ebin | tr '\n' ' '

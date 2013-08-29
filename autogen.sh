#!/bin/sh

rm -f aclocal.m4
rm -rf autom4te.cache

autoreconf --symlink --install || exit 1

if [ -z "$NOCONFIGURE" ]; then
  ./configure "$@"
fi

#!/usr/bin/env bash

for i in $@; do
  echo -n $i
  if [ -f $i.hs ]; then
    echo ".hs: File exists!"
    continue
  fi

  echo $i | grep '\.hs$' &> /dev/null
  if [ $? == 0 ]; then
    echo ": File is already .hs"
    continue
  fi

  echo
  perl -p -e 's/^([^>].*)$/-- $1/' $i | perl -p -e 's/^> ?(.*)$/$1/' > $i.hs
done

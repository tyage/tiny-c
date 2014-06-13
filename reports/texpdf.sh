#!/bin/bash

DVI_PATH=`echo $1 | sed -e "s/^\(.*\)\.tex$/\1.dvi/"`
PDF_PATH=`echo $1 | sed -e "s/^\(.*\)\.tex$/\1.pdf/"`

platex $1
dvipdfmx $DVI_PATH
if [ -x "`which evince`" ]; then
  evince $PDF_PATH
else
  open $PDF_PATH
fi

#!/usr/bin/env bash

FILENAME=$1

echo "Create pdf file for $FILENAME..."
echo "    1. Create pdf"
echo "    2. Create bibliography and pdf"

read -p ">> What option do you want? " CHOICE
echo " "

ENGINE=$(awk "NR==1 {print}" $FILENAME)
TEST=$(awk 'NR==1 {print $5}' $FILENAME)

if [ $CHOICE -eq 1 ] ; then
       if [ "$TEST" == "pdflatex" ] ; then
           pdflatex $FILENAME
       elif [ "$TEST" == "lualatex" ] ; then
           lualatex $FILENAME
       else
           echo "unknown engine"
       fi
fi

if [ $CHOICE -eq 2 ] ; then
       pdflatex $FILENAME
       biber $FILENAME
       biber $FILENAME
       pdflatex $FILENAME
fi

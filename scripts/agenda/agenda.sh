#!/usr/bin/env bash

FILENAME=$1

LAST_ENTRY=$(awk -F "," '{print $1}' $FILENAME | tail --lines 1)
TODAY=$(date +%d)
MONTH=$(date +%m)
YEAR=$(date +%Y)

while [ $LAST_ENTRY -lt $TODAY ] ; do
    let "LAST_ENTRY++"
    echo "$(date -d $YEAR-$MONTH-$LAST_ENTRY +%d), $(date -d $YEAR-$MONTH-$LAST_ENTRY +%A), 0.0, 0.0, 0.0" >> $FILENAME
done

#awk -F "," '{print $1, $2, $3}' $FILENAME

echo "Add data to the file..."
echo "    1. Research"
echo "    2. University"
echo "    3. Computers"

read -p ">> What option do you want? " CHOICE

if [ $CHOICE -eq 1 ] ; then
    echo "add research data" 
elif [ $CHOICE -eq 2 ] ; then
    echo "add university data" 
elif [ $CHOICE -eq 3 ] ; then
    echo "add computers data" 
else 
    echo "nothing here"
fi

#echo "$(date +%d), $(date +%A), 0.0, 0.0, 0.0" 

#echo "    1. Create pdf"
#echo "    2. Create bibliography and pdf"
#
#read -p ">> What option do you want? " CHOICE
#echo " "
#
#ENGINE=$(awk "NR==1 {print}" $FILENAME)
#TEST=$(awk 'NR==1 {print $5}' $FILENAME)
#
#if [ $CHOICE -eq 1 ] ; then
#       if [ "$TEST" == "pdflatex" ] ; then
#           pdflatex $FILENAME
#       elif [ "$TEST" == "lualatex" ] ; then
#           lualatex $FILENAME
#       else
#           echo "unknown engine"
#       fi
#fi
#
#if [ $CHOICE -eq 2 ] ; then
#       pdflatex $FILENAME
#       biber $FILENAME
#       biber $FILENAME
#       pdflatex $FILENAME
#fi

#!/usr/bin/env bash

read -p "Seminar: " SEMINAR

PRONOUN01=$(awk -F '|' "NR==$SEMINAR {print \$7}" ./speaker.org)
TREATMENT=$(awk -F '|' "NR==$SEMINAR {print \$8}" ./speaker.org)
NAME=$(awk -F '|' "NR==$SEMINAR {print \$2}" ./speaker.org)
SURNAME=$(awk -F '|' "NR==$SEMINAR {print \$3}" ./speaker.org)
DAY=$(awk -F '|' "NR==$SEMINAR {print \$1}" ./speaker.org)
TITLE=$(awk -F '|' "NR==$SEMINAR {print \$5}" ./speaker.org)

BASE_DIR="/home/thiago/Work-repos/thraraujo.github.io/seminars-templates/"
touch "./output/$NAME.tex"
OUTPUT="./output/$NAME.tex"

sed "s/{TREATMENT}/$PRONOUN01\ $TREATMENT/
     s/{SPEAKER}/$NAME\ $SURNAME/
     s/{TITLE}/$TITLE/
     s/{DAY}/$DAY/" ./certificado.tex > $OUTPUT

echo "Creating pdf..." 

cd ./output && pdflatex $NAME.tex

echo "Done!!"

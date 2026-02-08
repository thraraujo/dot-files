#!/usr/bin/env bash

read -p "Seminar: " SEMINAR

DAY=$(awk -F '|' "NR==$SEMINAR {print \$1}" ./speaker.org)
NAME=$(awk -F '|' "NR==$SEMINAR {print \$2}" ./speaker.org)
SURNAME=$(awk -F '|' "NR==$SEMINAR {print \$3}" ./speaker.org)
TITLE=$(awk -F '|' "NR==$SEMINAR {print \$5}" ./speaker.org)
PRONOUN=$(awk -F '|' "NR==$SEMINAR {print \$7}" ./speaker.org)
TREATMENT=$(awk -F '|' "NR==$SEMINAR {print \$8}" ./speaker.org)
AFFILIATION=$(awk -F '|' "NR==$SEMINAR {print \$9}" ./speaker.org)
ABSTRACT=$(awk -F '|' "NR==$SEMINAR {print \$10}" ./speaker.org)

BASE_DIR="/home/thiago/Work-repos/thraraujo.github.io/seminars-templates/"
touch "./output/$NAME-anuncio.org"
OUTPUT="./output/$NAME-anuncio.org"

sed "s/{PRONOUN}/$PRONOUN\ $TREATMENT/
     s/{SPEAKER}/$NAME\ $SURNAME/
     s/{AFFILIATION}/$AFFILIATION/
     s/{TITLE}/$TITLE/
     s/{ABSTRACT}/$ABSTRACT/
     s/{DAY}/$DAY/" ./anuncio.org > $OUTPUT

echo "-- Message --" 
cat $OUTPUT
echo "-- End --" 

echo "Done"

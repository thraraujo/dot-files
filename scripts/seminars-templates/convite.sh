#!/usr/bin/env bash

read -p "Seminar: " SEMINAR

PRONOUN01=$(awk -F '|' "NR==$SEMINAR {print \$6}" ./speaker.org)
PRONOUN02=$(awk -F '|' "NR==$SEMINAR {print \$7}" ./speaker.org)
TREATMENT=$(awk -F '|' "NR==$SEMINAR {print \$8}" ./speaker.org)
NAME=$(awk -F '|' "NR==$SEMINAR {print \$2}" ./speaker.org)
SURNAME=$(awk -F '|' "NR==$SEMINAR {print \$3}" ./speaker.org)
RESEARCH=$(awk -F '|' "NR==$SEMINAR {print \$4}" ./speaker.org)
DAY=$(awk -F '|' "NR==$SEMINAR {print \$1}" ./speaker.org)

BASE_DIR="/home/thiago/Work-repos/thraraujo.github.io/seminars-templates/"
touch "./output/$NAME-convite.org"
OUTPUT="./output/$NAME-convite.org"

sed "s/{GREATINGS}/$PRONOUN01\ $TREATMENT\ $NAME\ $SURNAME/
     s/{PRONOUN}/$PRONOUN02/
     s/{RESEARCH}/$RESEARCH/
     s/{DAY}/$DAY/" ./convite.org > $OUTPUT

echo "-- Message --" 
cat $OUTPUT
echo "-- End --" 

echo "Done"

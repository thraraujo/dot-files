#!/usr/bin/env bash

ARXIV=(hep-th math-ph nlin-ph cond-mat.stat-mech)

BASE_DIR="/home/thiago/Work-repos/thraraujo.github.io/scripts/scraping-arxiv/"
FILENAME=$(date +%F)

for url in "${ARXIV[@]}"

do
    touch "$BASE_DIR/papers/$FILENAME-$url-temp.org"
    TEMP_FILE="$BASE_DIR/papers/$FILENAME-$url-temp.org"
    touch "$BASE_DIR/papers/$FILENAME-$url.org"
    OUTPUT_FILE="$BASE_DIR/papers/$FILENAME-$url.org"

    /home/thiago/Work-repos/thraraujo.github.io/scripts/scraping-arxiv/arxiv.py "$url" > $TEMP_FILE

    LOOP_AUTHORS=$(wc -l /home/thiago/Work-repos/thraraujo.github.io/scripts/scraping-arxiv/authors.org | awk '{print $1}')
    LOOP_FIELDS=$(wc -l /home/thiago/Work-repos/thraraujo.github.io/scripts/scraping-arxiv/fields.org | awk '{print $1}')

    for ((n=1; n <= LOOP_AUTHORS ; n++)) ; do
        author=$(awk "NR==$n {print \$1}" /home/thiago/Work-repos/thraraujo.github.io/scripts/scraping-arxiv/authors.org)
        grep -i "$author" $TEMP_FILE >> $OUTPUT_FILE
    done

    for ((n=1; n <= LOOP_FIELDS ; n++)) ; do
        field=$(awk "NR==$n {print \$1}" /home/thiago/Work-repos/thraraujo.github.io/scripts/scraping-arxiv/fields.org)
        grep -i "$field" $TEMP_FILE >> $OUTPUT_FILE
    done

    rm "$TEMP_FILE"
done

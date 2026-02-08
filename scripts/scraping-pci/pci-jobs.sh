#!/usr/bin/env bash

BASE_DIR="/home/thiago/.config/scripts/02-pci-scraping/" 
FILENAME=$(date +%F.org)

touch "$BASE_DIR/jobs/$FILENAME" 

OUTPUT_FILE="$BASE_DIR/jobs/$FILENAME"

echo "File $FILENAME created at $(date)"

echo "Searching for jobs..."

echo "*** $(date)" >> "$OUTPUT_FILE"
echo " " >> "$OUTPUT_FILE"

python3 $BASE_DIR/pci-ultimas.py >> "$OUTPUT_FILE"

echo " " >> "$OUTPUT_FILE"

echo "added some jobs to $FILENAME."

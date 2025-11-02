#!/bin/bash

# Set this to your processed images root
PROCESSED_ROOT="data/processed/images/AlT"

# Only normalize the first-level folders (Inventarnummer)
find "$PROCESSED_ROOT" -mindepth 1 -maxdepth 1 -type d | while read -r d; do
  base=$(basename "$d")
  norm=$(echo "$base" | tr -s ' ' | sed 's/^ *//;s/ *$//')
  if [ "$base" != "$norm" ]; then
    # Only rename if the normalized folder doesn't already exist
    if [ ! -e "$(dirname "$d")/$norm" ]; then
      echo "Renaming: '$base' -> '$norm'"
      mv "$d" "$(dirname "$d")/$norm"
    else
      echo "WARNING: Target folder already exists: '$norm'. Skipping '$base'."
    fi
  fi
done
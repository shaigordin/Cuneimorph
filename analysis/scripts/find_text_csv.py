import csv
import re
import os

pattern = re.compile(r'PRADAL\s+\d+-')

count = 0
unique_lines = set()

with open('data/raw/metadata/Uga/clean_metadata.csv', newline='', encoding='utf-8') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        field = row['text_near_image']
        if pattern.search(field):
            count += 1
            unique_lines.add(field)

print(f"Number of lines with the pattern: {count}")
print(f"Number of unique full lines: {len(unique_lines)}")

# Save unique lines to a file
output_dir = 'data/processed/metadata/Uga'
os.makedirs(output_dir, exist_ok=True)
output_path = os.path.join(output_dir, 'unique_pradal_lines.txt')

with open(output_path, 'w', encoding='utf-8') as f:
    for line in sorted(unique_lines):
        f.write(line + '\n')

print(f"Unique lines saved to {output_path}")
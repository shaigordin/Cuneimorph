import sys
sys.path.append('/Users/shaigordin/Dropbox/Git-programs/ebl-ai-api')

import os
import json
import csv
from PIL import Image, ImageDraw
from ebl_ai.model import Model
import attr

# --- User input: select site_name ---
site_name = "KAJ"  # <-- Change as needed or make this a script argument

# Paths
BASE_DIR = os.getcwd()
IMAGES_ROOT = os.path.join(BASE_DIR, "data", "processed", "images", site_name)
METADATA_DIR = os.path.join(BASE_DIR, "data", "processed", "metadata")
SITE_METADATA_DIR = os.path.join(METADATA_DIR, site_name)
os.makedirs(SITE_METADATA_DIR, exist_ok=True)

CONFIG_FILE = os.path.join(BASE_DIR, "model", "fcenet_no_dcvn.py")
CHECKPOINT = os.path.join(BASE_DIR, "model", "checkpoint.pth")

# Load model
model = Model(configFile=CONFIG_FILE, checkpoint=CHECKPOINT)

# Aggregate CSV rows for the site
aggregate_csv_rows = []
aggregate_fieldnames = [
    "image_id", "box_index", "line_number", "sign_file", "top_left_x", "top_left_y", "width", "height", "probability",
    "dpi", "crop_width_px", "crop_height_px", "scale_cm_per_pixel"
]

# Walk through all <image_id> folders under the site
for image_id in os.listdir(IMAGES_ROOT):
    image_id_dir = os.path.join(IMAGES_ROOT, image_id)
    if not os.path.isdir(image_id_dir):
        continue

    # Process each image in <image_id> folder
    for fname in os.listdir(image_id_dir):
        if not fname.lower().endswith((".jpg", ".jpeg", ".png")):
            continue
        img_path = os.path.join(image_id_dir, fname)
        base_name = os.path.splitext(fname)[0]
        print(f"Processing {fname} in {image_id}...")

        # Run prediction
        predictions = model.predict(img_path)
        pred_dicts = [attr.asdict(p) for p in predictions]
        json_data = {"boundaryResults": pred_dicts}

        # Save predictions JSON in site-specific metadata dir
        json_path = os.path.join(SITE_METADATA_DIR, f"predictions_{base_name}.json")
        with open(json_path, "w") as f:
            json.dump(json_data, f)
        print(f"Saved predictions to {json_path}")

        # Load image
        image = Image.open(img_path)
        draw = ImageDraw.Draw(image)

        # Prepare signs output folder for this image
        signs_img_dir = os.path.join(image_id_dir, "signs")
        os.makedirs(signs_img_dir, exist_ok=True)
        csv_rows = []

        # Try to get DPI from image info (default to 72 if not found)
        dpi = image.info.get('dpi', (72, 72))[0] if 'dpi' in image.info else 72

        # --- Simple line grouping by y-coordinate clustering ---
        sorted_boxes = sorted(enumerate(pred_dicts), key=lambda x: x[1]["top_left_y"])
        line_number = 1
        line_threshold = 30  # pixels; adjust as needed for your images
        last_y = None
        box_line_numbers = [0] * len(pred_dicts)

        for idx, (orig_idx, box) in enumerate(sorted_boxes):
            y = box["top_left_y"]
            if last_y is not None and abs(y - last_y) > line_threshold:
                line_number += 1
            box_line_numbers[orig_idx] = line_number
            last_y = y

        # Draw boxes for visualization, crop signs, and collect CSV rows
        for idx, box in enumerate(pred_dicts):
            x = box["top_left_x"]
            y = box["top_left_y"]
            w = box["width"]
            h = box["height"]
            prob = box.get("probability", "")
            line_num = box_line_numbers[idx]
            # Draw rectangle and line number on the image
            draw.rectangle([x, y, x + w, y + h], outline="red", width=3)
            draw.text((x, y - 10), f"Line {line_num}", fill="blue")
            # Save sign crop in signs/<line_number_padded>/
            line_folder = os.path.join(signs_img_dir, f"{line_num:02d}")
            os.makedirs(line_folder, exist_ok=True)
            sign_path = os.path.join(line_folder, f"box_{idx+1}.jpeg")
            with Image.open(img_path) as orig_img:
                cropped = orig_img.crop((x, y, x + w, y + h))
                cropped.save(sign_path)
                crop_width, crop_height = cropped.size
            scale_cm_per_pixel = 2.54 / dpi
            row = {
                "image_id": image_id,
                "box_index": idx+1,
                "line_number": line_num,
                "sign_file": sign_path,
                "top_left_x": x,
                "top_left_y": y,
                "width": w,
                "height": h,
                "probability": prob,
                "dpi": dpi,
                "crop_width_px": crop_width,
                "crop_height_px": crop_height,
                "scale_cm_per_pixel": round(scale_cm_per_pixel, 4)
            }
            csv_rows.append(row)
            aggregate_csv_rows.append(row)

        # Save per-tablet CSV in <image_id> folder
        csv_path = os.path.join(image_id_dir, f"{base_name}_bounding_boxes.csv")
        with open(csv_path, "w", newline="") as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=aggregate_fieldnames)
            writer.writeheader()
            for row in csv_rows:
                writer.writerow(row)
        print(f"Saved bounding box data to {csv_path}")

        # Save image with boxes in <image_id> folder
        out_img_path = os.path.join(image_id_dir, f"{base_name}_with_boxes.jpeg")
        image.save(out_img_path)
        print(f"Saved image with bounding boxes to {out_img_path}")

# Save aggregate CSV for the site in metadata dir
aggregate_csv_path = os.path.join(METADATA_DIR, f"{site_name}_aggregate_bounding_boxes.csv")
with open(aggregate_csv_path, "w", newline="") as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=aggregate_fieldnames)
    writer.writeheader()
    for row in aggregate_csv_rows:
        writer.writerow(row)
print(f"Saved aggregate bounding box data to {aggregate_csv_path}")

print("Done.")

import fitz  # PyMuPDF
import os
import pandas as pd

def extract_images_with_nearby_text(pdf_path, output_dir, proximity=50):
    os.makedirs(output_dir, exist_ok=True)
    doc = fitz.open(pdf_path)
    metadata = []

    for page_num in range(len(doc)):
        page = doc[page_num]
        text_blocks = page.get_text("blocks")  # (x0, y0, x1, y1, "text", block_no, block_type)
        images = page.get_images(full=True)
        for img_index, img in enumerate(images):
            xref = img[0]
            # Get image bbox (if available)
            bbox = None
            for b in page.get_image_bbox(xref):
                bbox = b  # (x0, y0, x1, y1)
            if bbox is None:
                continue
            # Extract image
            pix = fitz.Pixmap(doc, xref)
            img_filename = f"page{page_num+1}_img{img_index+1}.png"
            img_path = os.path.join(output_dir, img_filename)
            pix.save(img_path)
            pix = None

            # Find nearby text blocks
            nearby_texts = []
            for block in text_blocks:
                bx0, by0, bx1, by1, text, *_ = block
                # Check if block is within proximity of image bbox
                if (
                    abs(bx0 - bbox.x0) < proximity or abs(bx1 - bbox.x1) < proximity or
                    abs(by0 - bbox.y0) < proximity or abs(by1 - bbox.y1) < proximity
                ):
                    nearby_texts.append(text.strip())
            metadata.append({
                "page": page_num + 1,
                "image_file": img_filename,
                "bbox": bbox,
                "nearby_text": " ".join(nearby_texts)
            })
    return pd.DataFrame(metadata)

# Example usage:
if __name__ == "__main__":
    pdf_path = "your.pdf"
    output_dir = "extracted_images"
    df = extract_images_with_nearby_text(pdf_path, output_dir, proximity=50)
    df.to_csv(os.path.join(output_dir, "image_metadata.csv"), index=False)
    print(df)
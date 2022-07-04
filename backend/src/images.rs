use image;
use image::imageops;

fn scaled_width_and_height(width : u32, height : u32, target_width : u32, target_height : u32) -> (u32, u32, u32, u32)
{
    let width_left_full : (u32, u32) = (target_width, (target_width * height) / width);
    let height_left_full : (u32, u32) = ((target_height * width) / height, target_height);

    if width_left_full.1 >= target_height {
        return (width_left_full.0, width_left_full.1, 0, (width_left_full.1 - target_height) / 2);
    } else {
        return (height_left_full.0, height_left_full.1, (width_left_full.0 - target_width) / 2, 0);
    }
}

// Return the image in png format, with 8 bit rgba component pixels (32 bits per pixel), and size 300x169
pub fn normalize_image(image_bytes : hyper::body::Bytes) -> image::ImageResult<Vec<u8>>
{
    let image_reader = image::io::Reader::new(std::io::Cursor::new(image_bytes))
        .with_guessed_format()
        .expect("Cursor IO never fails.");

    let image = image_reader.decode()?;

    let rgba8_image = image.to_rgba8();

    let (width, height) = rgba8_image.dimensions();
    let (scaled_width, scaled_height, crop_offset_x, crop_offset_y) = scaled_width_and_height(width, height, 300, 169);

    let resized_image = imageops::resize(&rgba8_image, scaled_width, scaled_height, imageops::FilterType::CatmullRom);

    let cropped_image = imageops::crop_imm(&resized_image, crop_offset_x, crop_offset_y, 300, 169).to_image();

    let mut png_encoded_image = Vec::new();

    cropped_image.write_to(&mut std::io::Cursor::new(&mut png_encoded_image), image::ImageOutputFormat::Png)?;

    return Ok(png_encoded_image);
}
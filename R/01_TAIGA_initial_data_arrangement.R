


# library -----------------------------------------------------------------

#data wrangline
library(tidyverse)
library(here)

#audio processing
library(av)
library(birdnetTools)



# extract audio from video ------------------------------------------------


video_folder <- "D:/2026_eastern_grassowl_Taiwan/土庫北A89"
output_folder <- "D:/2026_eastern_grassowl_Taiwan/土庫北A89_audio"

mp4_files <- list.files(path = video_folder, pattern = "\\.mp4$", full.names = TRUE, ignore.case = TRUE)

for (video_path in mp4_files) {

  # Extract the base file name (e.g., "recording1.mp4" -> "recording1")
  file_name <- tools::file_path_sans_ext(basename(video_path))

  # Construct the new output path with a .wav extension
  wav_path <- file.path(output_folder, paste0(file_name, ".wav"))

  cat("Converting:", basename(video_path), "-->", paste0(file_name, ".wav\n"))

  # Run the conversion
  av_audio_convert(video_path,
                   wav_path,
                   channels = 1,
                   sample_rate = 44100)
}

cat("\nAll files successfully converted and saved to:", output_folder, "\n")


# Run the extraction
av_audio_convert(video = video_file,
  output = audio_output,
  channels = 1,          # Converts stereo to mono (highly recommended for BirdNET)
  sample_rate = 44100)

message("Audio extraction complete!")

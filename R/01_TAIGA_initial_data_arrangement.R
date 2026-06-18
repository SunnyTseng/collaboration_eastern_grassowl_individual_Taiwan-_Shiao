


# library -----------------------------------------------------------------

#data wrangline
library(tidyverse)
library(here)
library(janitor)

#audio processing
library(av)
library(tidymedia)
library(birdnetTools)



# extract audio, datetime from video ------------------------------------------------


video_folder <- "D:/2026_eastern_grassowl_Taiwan/順揚有利172"
output_folder <- "D:/2026_eastern_grassowl_Taiwan/順揚有利172_audio"
output_file <- tibble()

mp4_files <- list.files(path = video_folder,
                        pattern = "\\.mp4$",
                        full.names = TRUE,
                        ignore.case = TRUE,
                        recursive = TRUE)


for (video_path in mp4_files) {

  # 1. extract audio
  # get the base file name (e.g., "recording1.mp4" -> "recording1")
  file_name <- tools::file_path_sans_ext(basename(video_path))

  # construct the new output path with a .wav extension
  wav_path <- file.path(output_folder, paste0(file_name, ".wav"))
  cat("Converting:", basename(video_path), "-->", paste0(file_name, ".wav\n"))

  # run the conversion
  av_audio_convert(video_path, wav_path)


  # 2. extract date and time
  video_info <- mediainfo_query(file = video_path,
                                section = "General",
                                parameters = c("Encoded_Date", "Duration", "FileSize"))
  audio_info <- mediainfo_query(file = wav_path,
                                section = "Audio",
                                parameters = c("SamplingRate", "Channels", "BitDepth", "Format"))
  file_info <- c(video_info, audio_info)

  output_file <- bind_rows(output_file, file_info)
}

av_file_metadata <- output_file %>%
  clean_names() %>%
  rename(datetime = encoded_date,
         filepath_video = file_1,
         filepath_audio = file_5)



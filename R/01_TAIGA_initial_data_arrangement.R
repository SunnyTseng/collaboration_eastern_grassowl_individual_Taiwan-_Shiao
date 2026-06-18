


# library -----------------------------------------------------------------

#data wrangline
library(tidyverse)
library(here)
library(janitor)

#audio processing
library(av)
library(tidymedia)
library(birdnetTools)


# functions ---------------------------------------------------------------

extract_audio_metadata <- function(video_folder) {

  # create empty folder for extracted audio
  audio_folder <- paste0(video_folder, "_audio")

  if (!dir.exists(audio_folder)) {
    dir.create(audio_folder, recursive = TRUE)
    message("Folder created: ", audio_folder)
  } else {
    message("Folder already exists: ", audio_folder)
  }

  # create empty tibble for metadata
  output_file <- tibble()

  # list all the video files
  video_files <- list.files(path = video_folder,
                            pattern = "\\.mp4$",
                            full.names = TRUE,
                            ignore.case = TRUE,
                            recursive = TRUE)



  # The main loop to process each video file
  for (video_path in video_files) {

    # 1. extract audio
    # get the base file name (e.g., "recording1.mp4" -> "recording1")
    file_name <- tools::file_path_sans_ext(basename(video_path))

    # construct the new output path with a .wav extension
    audio_path <- file.path(audio_folder, paste0(file_name, ".wav"))
    cat("Converting:", basename(video_path), "-->", paste0(file_name, ".wav\n"))

    # run the conversion
    av_audio_convert(video_path, audio_path)


    # 2. extract date and time and other metadata
    video_info <- mediainfo_query(file = video_path,
                                  section = "General",
                                  parameters = c("Encoded_Date", "Duration", "FileSize"))
    audio_info <- mediainfo_query(file = audio_path,
                                  section = "Audio",
                                  parameters = c("SamplingRate", "Channels", "BitDepth", "Format"))
    file_info <- c(video_info, audio_info)

    output_file <- bind_rows(output_file, file_info)
  }

  av_file_metadata <- output_file %>%
    clean_names() %>%
    rename(datetime = encoded_date,
           filepath_video = file_1,
           filepath_audio = file_5) %>%
    mutate(site = str_split_i(filepath_video, "/", -2) %>% str_extract("\\p{Han}+"),
           owl_id = str_split_i(filepath_video, "/", 3) %>% str_extract("[A-Za-z0-9]+")) %>%
    select(site, owl_id, datetime, sampling_rate, channels, bit_depth, format, filepath_video, filepath_audio)

  return(av_file_metadata)
}

# extract audio, datetime from video ------------------------------------------------


video_folder <- "D:/2026_eastern_grassowl_Taiwan/土庫北A89"

test <- extract_audio_metadata(video_folder)

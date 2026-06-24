# library -----------------------------------------------------------------

#install
#pak::pak("birdnet-team/birdnetR") # developer: Felix Guenther
#26pak::pak("birdnet-team/birdnetTools") # developer: Sunny Tseng

#data wrangline
library(tidyverse)
library(here)
library(janitor)
library(fs)

#audio processing
library(av)
library(tuneR)
library(ohun)
library(warbleR)
library(tidymedia)

#BirdNET related
library(birdnetR)
library(birdnetTools)


# functions ---------------------------------------------------------------

extract_audio_metadata <- function(video_folder,
                                   extraction = TRUE) {

  # create empty folder for extracted audio
  audio_folder <- sub("video", "audio", video_folder)

  if (!dir.exists(audio_folder)) {
    dir.create(audio_folder, recursive = TRUE)
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
  for (video_file in video_files) {

    # 1. extract audio
    audio_file <- video_file %>%
      str_replace("TAIGA_video", "TAIGA_audio") %>%
      str_replace("\\.[^.]+$", ".wav")

    target_dir <- dirname(audio_file)

    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE)
    }

    # run the conversion
    if(extraction == TRUE){
      av_audio_convert(video_file, audio_file)
    } else {
      cat("Audio extraction skipped for:", basename(video_file), "\n")
    }


    # 2. extract date and time and other metadata
    video_info <- mediainfo_query(file = video_file,
                                  section = "General",
                                  parameters = c("Encoded_Date", "Duration", "FileSize"))
    audio_info <- mediainfo_query(file = audio_file,
                                  section = "Audio",
                                  parameters = c("SamplingRate", "Channels", "BitDepth", "Format"))
    file_info <- c(video_info, audio_info)

    output_file <- bind_rows(output_file, file_info)
  }


  # the final metadata table with cleaned column names and selected columns
  av_file_metadata <- output_file %>%
    clean_names() %>%
    rename(datetime = encoded_date,
           filepath_video = file_1,
           filepath_audio = file_5) %>%
    mutate(site = str_split_i(filepath_video, "/", -2) %>% str_extract("\\p{Han}+"),
           owl_id = str_split_i(filepath_video, "/", 4) %>% str_extract("[A-Za-z0-9]+")) %>%
    select(owl_id, site, datetime, duration, sampling_rate, channels, bit_depth, format,
           filepath_video, filepath_audio)

  return(av_file_metadata)
}


extract_audio_events <- function(audio_file,
                                 threshold_detection,
                                 visualize = FALSE) {

  file_name <- basename(audio_file)
  folder_path <- dirname(audio_file)

  detection <- energy_detector(files = file_name,
                               path = folder_path,
                               bp = c(1, 5),
                               threshold = threshold_detection,
                               smooth = 500, # bridges tiny internal gaps
                               hold.time = 1500) # merge selection if less than 1 sec in gap

  if (visualize) {
    sound <- readWave(audio_file)

    label_spectro(wave = sound,
                  detection = detection,
                  envelope = TRUE,
                  threshold = threshold_detection,
                  flim = c(0.5, 5.5))
  }



  # 1. Calculate original durations and midpoints
  orig_duration <- detection$end - detection$start
  midpoints <- (detection$start + detection$end) / 2

  # 2. Determine target duration: round UP to next multiple of 3 (unlimited)
  target_duration <- ceiling(orig_duration / 3) * 3

  # 3. Expand the start and end windows symmetrically around the midpoints
  detection$start <- midpoints - (target_duration / 2)
  detection$end   <- midpoints + (target_duration / 2)

  # 4. Handle FRONT clipping: If start < 0, shift window right to start at 0
  below_zero <- detection$start < 0
  if (any(below_zero)) {
    current_durations <- detection$end - detection$start

    detection$start[below_zero] <- 0
    detection$end[below_zero]   <- current_durations[below_zero]
  }

  # 5. Handle BACK clipping: If end > 15, shift window left to end at 15
  #    (Replace 15 with a dynamic max duration if your files vary in length)
  past_end <- detection$end > 15
  if (any(past_end)) {
    current_durations <- detection$end - detection$start

    detection$end[past_end]   <- 15
    detection$start[past_end] <- 15 - current_durations[past_end]
  }

  # 6. Recalculate final duration column for warbleR/ohun consistency
  detection$duration <- detection$end - detection$start

  # 7. Convert to tibble and append the full file path column
  output_tibble <- as_tibble(detection) %>%
    mutate(audio_file = audio_file)

  return(output_tibble)
}


extract_audio_files <- function(video_folder,
                                audio_folder = sub("video", "audio", video_folder)) {

  # Mirror target structure in the audio repository
  if (!dir.exists(audio_folder)) {
    dir.create(audio_folder, recursive = TRUE)
  }

  # List all the video files
  video_files <- list.files(path = video_folder,
                            pattern = "\\.mp4$",
                            full.names = TRUE,
                            ignore.case = TRUE,
                            recursive = TRUE)

  for (video_file in video_files) {

    # Map video path to target audio path
    audio_file <- video_file %>%
      str_replace("TAIGA_video", "TAIGA_audio") %>%
      str_replace("\\.[^.]+$", ".wav")

    target_dir <- dirname(audio_file)
    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE)
    }

    # Core Safety Check: Extract ONLY if the file does not exist yet
    if (!file.exists(audio_file)) {
      av_audio_convert(video_file, audio_file)
    }
  }
}


build_audio_metadata <- function(video_folder,
                                 audio_folder) {

  video_files <- list.files(path = video_folder,
                            pattern = "\\.mp4$",
                            full.names = TRUE,
                            ignore.case = TRUE,
                            recursive = TRUE)

  audio_files <- list.files(path = audio_folder,
                            pattern = "\\.wav$",
                            full.names = TRUE,
                            ignore.case = TRUE,
                            recursive = TRUE)


  output_file <- tibble()

  for (i in 1:length(video_files)) {

    # 1. Query raw video parameters
    video_info <- mediainfo_query(file = video_files[i],
                                  section = "General",
                                  parameters = c("Encoded_Date", "Duration", "FileSize"))

    # 2. Query audio parameters safely
    if (file.exists(audio_files[i])) {
      audio_info <- mediainfo_query(file = audio_files[i],
                                    section = "Audio",
                                    parameters = c("SamplingRate", "Channels", "BitDepth", "Format"))
    } else {
      # Fallback to prevent breaking bind_rows if audio doesn't exist yet
      audio_info <- list(SamplingRate = NA, Channels = NA, BitDepth = NA, Format = NA)
    }

    file_info <- c(video_info, audio_info)
    output_file <- bind_rows(output_file, file_info)
  }

  # 3. Clean names, extract site details/IDs, and select final outputs
  av_file_metadata <- output_file %>%
    clean_names() %>%
    rename(datetime = encoded_date,
           filepath_video = file_1,
           filepath_audio = file_5) %>%
    mutate(site = str_split_i(filepath_video, "/", -2) %>% str_extract("\\p{Han}+"),
           owl_id = str_split_i(filepath_video, "/", 4) %>% str_extract("[A-Za-z0-9]+")) %>%
    select(owl_id, site, datetime, duration, sampling_rate, channels, bit_depth, format,
           filepath_video, filepath_audio)

  return(av_file_metadata)
}

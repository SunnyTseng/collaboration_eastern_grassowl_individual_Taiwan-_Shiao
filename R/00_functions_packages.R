# library -----------------------------------------------------------------

#install
#pak::pak("birdnet-team/birdnetR") # developer: Felix Guenther
#26pak::pak("birdnet-team/birdnetTools") # developer: Sunny Tseng

#data wrangline
library(tidyverse)
library(here)
library(janitor)
library(fs)

#visualization
library(viridis)
library(umap)

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
           owl_id = str_split_i(filepath_video, "/", 5) %>% str_extract("[A-Za-z0-9]+"),
           audio_id = paste(owl_id, "-", site, "-", datetime)) %>%
    select(owl_id, site, datetime, audio_id, duration, sampling_rate, channels, bit_depth, format,
           filepath_video, filepath_audio)

  return(av_file_metadata)
}


extract_audio_events <- function(audio_folder,
                                 threshold_detection) {

  # List audio files
  audio_files <- list.files(path = audio_folder,
                            pattern = "\\.wav$",
                            full.names = TRUE,
                            ignore.case = TRUE,
                            recursive = TRUE)

  # Get detection for each of the audio files
  for(audio_file in audio_files){

    file_name <- basename(audio_file)
    folder_path <- dirname(audio_file)

    # Get the initial detection
    detection <- energy_detector(files = file_name,
                                 path = folder_path,
                                 bp = c(1, 5),
                                 threshold = threshold_detection,
                                 smooth = 500, # bridges tiny internal gaps
                                 hold.time = 1500) # merge selection if less than 1 sec in gap

    # Drop detections shorter than 3 seconds
    detection_cleaned <- detection %>%
      rowwise() %>%
      filter(duration >= 3)

    if (nrow(detection_cleaned) == 0) {
      message(paste("No detections found for file:", audio_file))
      next
    }

    # Trim the detection to ensure each clip is 3 seconds long
    detection_cleaned_1 <- detection_cleaned %>%
      # Calculate the number of 3-second chunks needed to cover the original duration
      mutate(n_clips = floor(duration / 3),
             midpoint = (start + end) / 2,
             total_clips_duration = n_clips * 3,
             block_start = midpoint - (total_clips_duration / 2)) %>%
      # Generate the individual 3-second start/end pairs
      reframe(sound.files = sound.files,
              call_id = row_number(),
              clip_index = 1:n_clips,
              start = block_start + (clip_index - 1) * 3,
              end = block_start + clip_index * 3) %>%
      # Boundary Safeguards: Ensure windows stay within [0, 15.3] seconds
      mutate(shift_right = if_else(start < 0, 0 - start, 0),
             start = start + shift_right,
             end = end + shift_right,
             shift_left = if_else(end > 15.3, end - 15.3, 0),
             start = start - shift_left,
             end = end - shift_left,
             duration = end - start) %>%
      # Final clean up
      select(call_id, clip_index, start, end, duration)


    for (i in 1:nrow(detection_cleaned_1)) {
      # Clip the audio
      wave <- readWave(audio_file)
      clip <- extractWave(wave,
                          from = detection_cleaned_1$start[i],
                          to = detection_cleaned_1$end[i],
                          xunit = "time")
      # Map audio path to target audio event path
      audio_event_file <- audio_file %>%
        str_replace("TAIGA_audio", "TAIGA_audio_event") %>%
        str_replace(".wav", paste0("_", detection_cleaned_1$clip_index[i], ".wav"))

      target_dir <- dirname(audio_event_file)
      if (!dir.exists(target_dir)) {
        dir.create(target_dir, recursive = TRUE)
      }
      # Write the file
      writeWave(clip, audio_event_file)
    }


  }



  # # 1. Calculate original durations and midpoints
  # orig_duration <- detection$end - detection$start
  # midpoints <- (detection$start + detection$end) / 2
  #
  # # 2. Determine target duration based on original duration, rounded up to the nearest second
  # target_duration <- ceiling(orig_duration)
  #
  # target_duration <- pmax(target_duration, 3)
  # target_duration <- pmin(target_duration, 15)
  #
  # # 3. Expand the start and end windows symmetrically around the midpoints
  # detection$start <- midpoints - (target_duration / 2)
  # detection$end   <- midpoints + (target_duration / 2)
  #
  # # 4. Handle FRONT clipping: If start < 0, shift window right to start at 0
  # below_zero <- detection$start < 0
  # if (any(below_zero)) {
  #   detection$start[below_zero] <- 0
  #   detection$end[below_zero]   <- target_duration[below_zero]
  # }
  #
  # # 5. Handle BACK clipping: If end > 15, shift window left to end at 15
  # #    (Replace 15 with a dynamic max duration if your files vary in length)
  # past_end <- detection$end > 15.3
  # if (any(past_end)) {
  #   detection$end[past_end]   <- 15.3
  #   detection$start[past_end] <- 15.3 - target_duration[past_end]
  # }
  #
  # # 6. Recalculate final duration column for warbleR/ohun consistency
  # detection$duration <- detection$end - detection$start

  return(detection_cleaned)
}







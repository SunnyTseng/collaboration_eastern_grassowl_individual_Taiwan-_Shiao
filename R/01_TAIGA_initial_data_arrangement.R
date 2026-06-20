


# library -----------------------------------------------------------------

#install
#pak::pak("birdnet-team/birdnetR") # developer: Felix Guenther
#26pak::pak("birdnet-team/birdnetTools") # developer: Sunny Tseng

#data wrangline
library(tidyverse)
library(here)
library(janitor)

#audio processing
library(av)
library(tuneR)
library(bioacoustics)
library(ohun)
library(tidymedia)

#BirdNET related
library(birdnetR)
library(birdnetTools)


# functions ---------------------------------------------------------------

extract_audio_metadata <- function(video_folder, extraction = TRUE) {

  # create empty folder for extracted audio
  audio_folder <- paste0(video_folder, "_audio")

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
  for (video_path in video_files) {

    # 1. extract audio
    # get the base file name (e.g., "recording1.mp4" -> "recording1")
    file_name <- tools::file_path_sans_ext(basename(video_path))

    # construct the new output path with a .wav extension
    audio_path <- file.path(audio_folder, paste0(file_name, ".wav"))

    # run the conversion
    if(extraction == TRUE){
      av_audio_convert(video_path, audio_path)
    } else {
      cat("Audio extraction skipped for:", basename(video_path), "\n")
    }


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


  # the final metadata table with cleaned column names and selected columns
  av_file_metadata <- output_file %>%
    clean_names() %>%
    rename(datetime = encoded_date,
           filepath_video = file_1,
           filepath_audio = file_5) %>%
    mutate(site = str_split_i(filepath_video, "/", -2) %>% str_extract("\\p{Han}+"),
           owl_id = str_split_i(filepath_video, "/", 3) %>% str_extract("[A-Za-z0-9]+")) %>%
    select(site, owl_id, datetime, sampling_rate, channels, bit_depth, format,
           filepath_video, filepath_audio)

  return(av_file_metadata)
}

# extract audio, datetime from video ------------------------------------------------

# include all the file except the one with "audio" in the name

folder_list <- list.dirs(path = "D:/2026_eastern_grassowl_Taiwan",
                         full.names = TRUE,
                         recursive = FALSE)

metadata_all <- tibble()
for (folder in folder_list) {
  cat("Processing folder:", folder, "\n")
  metadata <- extract_audio_metadata(folder, extraction = TRUE)
  metadata_all <- bind_rows(metadata_all, metadata)
}

# save the metadata table
write_csv(metadata_all, here("data", "taiga_audio_metadata_5_owls.csv"))





# extract events within the long acoustics - remove silence  --------------








# 1. Read your full 1-minute wave file
wave_obj <- readWave(test)

# 2. Extract the exact start and end sample points from your data list
start_sample <- detections$data$event_start[1]
end_sample   <- detections$data$event_end[1]

# 3. Slice the wave object directly using those indices
vocalization_clip <- wave_obj[start_sample:end_sample]

# 4. Save your extracted call
writeWave(vocalization_clip, "D:/2026_eastern_grassowl_Taiwan/prepared_clips/extracted_call.wav")







# run through BirdNET to get the detections -------------------------------

# initializing a BirdNET model
model <- load_birdnet(type = 'acoustic',
                      version = '2.4',
                      backend = 'tf',
                      precision = 'fp32', # what does this actually mean vs 'fp16' or 'int8'
                      lang = 'en_us')


# specify input audio (top-level folder) with "audio" in the name
audio_folders <- list.dirs("D:/2026_eastern_grassowl_Taiwan",
                           recursive = FALSE,
                           full.names = TRUE) %>%
  grep("audio", ., value = TRUE, ignore.case = TRUE)


# run BirdNET on each audio folder

for (audio_folder in audio_folders) {
  cat("Processing audio folder:", audio_folder, "\n")

  files <- list.files(audio_folder,
                      pattern = "\\.wav$",
                      full.names = TRUE,
                      recursive = TRUE)

  predictions <- predict(model, files)

  write_predictions(predictions,
                    file = here("data", "birdnet_output", paste0(basename(audio_folder), ".csv")),
                    format = "csv")

}




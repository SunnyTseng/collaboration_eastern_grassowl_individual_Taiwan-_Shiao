

# library -----------------------------------------------------------------

source(here::here("R", "00_functions_packages.R"))



# extract audio, datetime from video ------------------------------------------------

extract_audio_files(video_folder = "E:/2026_eastern_grassowl_Taiwan/TAIGA_video")

metadata_all <- build_audio_metadata(video_folder = "E:/2026_eastern_grassowl_Taiwan/TAIGA_video",
                                     audio_folder = "E:/2026_eastern_grassowl_Taiwan/TAIGA_audio")

write_csv(metadata_all, here("data", "taiga_audio_metadata_5_owls.csv"))



# extract events within the long acoustics - remove silence  --------------

audio_data <- read_csv(here("data", "taiga_audio_metadata_5_owls.csv"))

event_detections <- map2_df(audio_data$filepath_audio,
                            audio_data$audio_id,
                            function(path, id) {
                              extract_audio_events(path, threshold_detection = 20, visualize = FALSE) %>%
                                as_tibble() %>%
                                mutate(audio_id = id)})

metadata_event_detections <- event_detections %>%
  left_join(audio_data, by = "audio_id") %>%
  rename(segment_length = duration.x) %>%
  select(owl_id, site, datetime, audio_id, selec, start, end, segment_length, filepath_audio)

write_csv(metadata_event_detections, here("data", "taiga_audio_events_metadata_5_owls.csv"))















# others ------------------------------------------------------------------




# 1. Read your full 1-minute wave file
wave_obj <- readWave(test)

# 2. Extract the exact start and end sample points from your data list
start_sample <- detections$data$event_start[1]
end_sample   <- detections$data$event_end[1]

# 3. Slice the wave object directly using those indices
vocalization_clip <- wave_obj[start_sample:end_sample]

# 4. Save your extracted call
writeWave(vocalization_clip, "D:/2026_eastern_grassowl_Taiwan/prepared_clips/extracted_call.wav")


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
}




# run through BirdNET to get the detections -------------------------------

# initializing a BirdNET model
model <- load_birdnet(type = 'acoustic',
                      version = '2.4',
                      backend = 'tf',
                      precision = 'fp32', # what does this actually mean vs 'fp16' or 'int8'
                      lang = 'en_us')







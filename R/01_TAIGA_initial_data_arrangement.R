

# library -----------------------------------------------------------------

source(here::here("R", "00_functions_packages.R"))


# data exploration --------------------------------------------------------

video_root <- here::here("data", "TAIGA_video")

## check the number of folders - 5
list.dirs(path = video_root,
          full.names = TRUE,
          recursive = FALSE)


# extract audio, datetime from video ------------------------------------------------

extract_audio_files(video_folder = here("data", "TAIGA_video"))

metadata_all <- build_audio_metadata(video_folder = here("data", "TAIGA_video"),
                                     audio_folder = here("data", "TAIGA_audio"))

write_csv(metadata_all, here("data", "taiga_audio_metadata_test.csv"))



# extract events within the long acoustics - remove silence  --------------

audio_metadata <- read_csv(here("data", "taiga_audio_metadata_test.csv"))

## remove the audios that is not "insect call" type of vocalization
audio_metadata_filtered <- audio_metadata %>%
  filter(site != "non_insect")

## check time of the day
audio_metadata_filtered$datetime %>%
  hour() %>%
  unique() %>%
  sort()

## check owl & site summary
audio_metadata_filtered %>%
  group_by(owl_id, site) %>%
  summarize(audios = n())










# break -------------------------------------------------------------------









## extract the audio events from the long audio files
event_metadata <- map_df(.x = audio_metadata_filtered$filepath_audio,
                         .f ~= build_audio_events_metadata(.x,
                                                           threshold_detection = 20,
                                                           visualize = FALSE))



event_detections <- map2_df(audio_metadata_filtered$filepath_audio,
                            audio_metadata_filtered$audio_id,
                            function(path, id) {
                              extract_audio_events(path, threshold_detection = 20, visualize = FALSE) %>%
                                as_tibble() %>%
                                mutate(audio_id = id)})




metadata_event_detections <- event_detections %>%
  left_join(audio_data, by = "audio_id") %>%
  rename(clip_length = duration.x) %>%
  mutate(clip_id = paste0(audio_id, "-", selec)) %>%
  select(owl_id, site, datetime, selec, start, end, clip_length, filepath_audio, audio_id, clip_id)

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







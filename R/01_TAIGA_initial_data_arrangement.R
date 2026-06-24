

# library -----------------------------------------------------------------


source(here::here("R", "00_functions_packages.R"))



# extract audio, datetime from video ------------------------------------------------


extract_audio_files(video_folder = "E:/2026_eastern_grassowl_Taiwan/TAIGA_video")

metadata_all <- build_audio_metadata(video_folder = "E:/2026_eastern_grassowl_Taiwan/TAIGA_video",
                                     audio_folder = "E:/2026_eastern_grassowl_Taiwan/TAIGA_audio")


write_csv(metadata_all, here("data", "taiga_audio_metadata_5_owls.csv"))





# extract events within the long acoustics - remove silence  --------------



# template-based detection - might not work, as the owl call can sometimes be two-notes
# sometimes be four notes. Maybe the energy-based extraction can be a better option
# Be aware for the duration of the final extraction. Try to make the extracted clip
# covered by the sounds, but also be about 3, 6, or 9 seconds for birdnet analysis (?)

audio_data <- read_csv(here("data", "taiga_audio_metadata_5_owls.csv")) %>%
  mutate(filepath_audio = str_replace(filepath_audio, "^D", "E"))


for (audio_file in audio_data$filepath_audio) {
  detection <- extract_audio_events(audio_file,
                                    threshold_detection = 20,
                                    visualize = FALSE)

  if (!exists("all_detections")) {
    all_detections <- detection
  } else {
    all_detections <- bind_rows(all_detections, detection)
  }
}






















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










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
library(ohun)
library(warbleR)
library(tidymedia)

#BirdNET related
library(birdnetR)
library(birdnetTools)


# functions ---------------------------------------------------------------

extract_audio_metadata <- function(video_folder, extraction = TRUE) {

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
    select(owl_id, site, datetime, sampling_rate, channels, bit_depth, format,
           filepath_video, filepath_audio)

  return(av_file_metadata)
}

# extract audio, datetime from video ------------------------------------------------

folder_list <- list.dirs(path = "D:/2026_eastern_grassowl_Taiwan/TAIGA_video",
                         full.names = TRUE,
                         recursive = FALSE)

metadata_all <- tibble()
for (folder in folder_list) {
  metadata <- extract_audio_metadata(folder, extraction = TRUE)
  metadata_all <- bind_rows(metadata_all, metadata)
}

# save the metadata table
write_csv(metadata_all, here("data", "taiga_audio_metadata_5_owls.csv"))





# extract events within the long acoustics - remove silence  --------------



# template-based detection - might not work, as the owl call can sometimes be two-notes
# sometimes be four notes. Maybe the energy-based extraction can be a better option
# Be aware for the duration of the final extraction. Try to make the extracted clip
# covered by the sounds, but also be about 3, 6, or 9 seconds for birdnet analysis (?)

# load example data
data("lbh1", "lbh2", "lbh_reference")

# save sound files
tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# select a subset of the data
lbh1_reference <-
  lbh_reference[lbh_reference$sound.files == "lbh1.wav",]

# print data
lbh1_reference

# install this package first if not installed
# install.packages("Sim.DiffProc")

#Creating vector for duration
durs <- rep(c(0.3, 1), 5)

set.seed(123)
freqs <- sample(c(3, 6), 10, replace = TRUE)


#Creating simulated song
set.seed(12)
simulated_1 <-
  warbleR::simulate_songs(
    n = 10,
    durs = durs,
    freqs = freqs,
    sig2 = 0.1,
    gaps = 0.5,
    harms = 1,
    bgn = 0.1,
    freq.range = 2,
    path = tempdir(),
    file.name = "simulated_1",
    selec.table = TRUE,
    shape = "cos",
    fin = 0.3,
    fout = 0.35,
    samp.rate = 18
  )$wave


# plot spectrogram and envelope
label_spectro(wave = simulated_1,
              env = TRUE,
              fastdisp = TRUE)


# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(2, 8),
    threshold = 50,
    smooth = 150,
    path = tempdir()
  )











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







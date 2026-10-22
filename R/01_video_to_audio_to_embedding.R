

# library -----------------------------------------------------------------

source(here::here("R", "00_functions_packages.R"))


# data exploration --------------------------------------------------------

video_folder <- here::here("data", "TAIGA_video_2")

## check the number of folders (owls)
list.dirs(path = video_folder,
          full.names = TRUE,
          recursive = FALSE)

## check the number of files in each folder (owl)
list.files(video_folder,
           full.names = TRUE,
           recursive = TRUE) %>%
  tibble(file = .) %>%
  mutate(owl = str_extract(file, "(?<=TAIGA_video_2/)[^/]+"),
         site = str_split_i(file, "/", i = -2)) %>%
  summarize(n_files = n(),
            n_site = n_distinct(site),
            .by = owl)



# extract audio files from video ------------------------------------------


extract_audio_files(video_folder = here("data", "TAIGA_video_2"))

metadata_all <- build_audio_metadata(video_folder = here("data", "TAIGA_video_2"),
                                     audio_folder = here("data", "TAIGA_audio_2"))
write_csv(metadata_all, here("data", "taiga_audio_metadata.csv"))




# extract clips from audio ------------------------------------------------

# extract_audio_events(audio_folder = here("data", "TAIGA_audio_2"), threshold = 20)

##test how the detection works
# if (visualize) {
#   sound <- readWave(audio_file)
#
#   label_spectro(wave = sound,


#                 detection = detection,
#                 envelope = TRUE,
#                 threshold = threshold_detection,
#                 flim = c(0.5, 5.5))
# }

# check the number of detections in each folder (owl)
list.files(here("data", "TAIGA_audio_event_2"),
           full.names = TRUE,
           recursive = TRUE) %>%
  tibble(file = .) %>%
  mutate(owl = str_extract(file, "(?<=TAIGA_audio_event/)[^/]+"),
         site = str_split_i(file, "/", i = -2)) %>%
  summarize(n_clips = n(),
            n_site = n_distinct(site),
            .by = owl) # note there is one folder include sounds that is not insect vocal type


# extract embeddings from clips -------------------------------------------

# temporary run on the BirdNET GUI


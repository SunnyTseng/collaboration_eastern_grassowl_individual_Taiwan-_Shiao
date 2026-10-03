

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


# extract_audio_files(video_folder = here("data", "TAIGA_video_2"))
#
# metadata_all <- build_audio_metadata(video_folder = here("data", "TAIGA_video_2"),
#                                      audio_folder = here("data", "TAIGA_audio_2"))
# write_csv(metadata_all, here("data", "taiga_audio_metadata.csv"))




# extract clips from audio ------------------------------------------------

extract_audio_events(audio_folder = here("data", "TAIGA_audio_2"), threshold = 20)

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
list.files(here("data", "TAIGA_audio_event"),
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


# Embedding wrangling -----------------------------------------------------

embedding_folder <- here::here("data", "TAIGA_audio_event_embedding_files")

embedding_files <- list.files(path = embedding_folder,
                              pattern = "\\.txt$",
                              recursive = TRUE,
                              full.names = TRUE)


# Create big dataframe combining all the embeddings
embeddings_df <- map_dfr(embedding_files, function(file_path) {

  df <- read_csv(file_path, col_names = FALSE, show_col_types = FALSE) %>%
    # 1. Pack all numeric feature columns (X1:X1024) into a single list-column
    nest(embeddings = starts_with("X")) %>%
    # 2. Convert each row's 1024 features from a 1-row data frame to a numeric vector
    mutate(embeddings = map(embeddings, as.numeric)) %>%
    # 3. Add metadata columns
    mutate(
      file_name = path_file(file_path),
      owl_species = str_split_i(file_path, "/", i = -3),
      segment_id = row_number()
    )

}, .id = "source_file_index") %>%
  select(file_name, owl_species, segment_id, everything(), -source_file_index)


#save(embeddings_df,
#     file = here("data", "taiga_audio_event_embeddings_df.rds"))








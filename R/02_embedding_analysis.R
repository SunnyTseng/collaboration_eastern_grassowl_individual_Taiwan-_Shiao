


# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(umap)


# import data -------------------------------------------------------------

load(here("data", "taiga_audio_event_embeddings_df.rds"))

data_audio <- read_csv(here("data", "taiga_audio_metadata_test.csv"))



# making figures ----------------------------------------------------------

# Extract matrix directly from the list-column
feature_matrix <- do.call(rbind, embeddings_df$embeddings)
dim(feature_matrix) # Should be N rows x 1024 columns
mode(feature_matrix) # "numeric"



# UMAP
# 1. Run UMAP on the feature matrix
set.seed(42)
umap_out <- umap(feature_matrix)

# 2. Add UMAP dimensions back to the original dataframe containing owl_species
plot_df <- embeddings_df %>%
  mutate(
    UMAP1 = umap_out$layout[, 1],
    UMAP2 = umap_out$layout[, 2]
  )




# 3. Plot using your owl ID / species column
ggplot(plot_df, aes(x = UMAP1, y = UMAP2, color = owl_species)) +
  geom_point(alpha = 0.8, size = 2.5) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 12) +
  labs(
    title = "BirdNET Audio Embedding Separation",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "Owl ID"
  )




# other codes -------------------------------------------------------------



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




list.files(embedding_folder,
           full.names = TRUE,
           recursive = TRUE) %>%
  tibble(file = .) %>%
  mutate(owl = str_extract(file, "(?<=TAIGA_audio_event/)[^/]+"),
         site = str_split_i(file, "/", i = -2)) %>%
  summarize(n_files = n(),
            n_site = n_distinct(site),
            .by = owl)




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



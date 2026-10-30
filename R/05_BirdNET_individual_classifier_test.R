


# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(birdnetTools)


# import data -------------------------------------------------------------

# audio metadata
audio_metadata <- read_csv(here("data", "taiga_audio_metadata.csv")) |>
  mutate(audio_file = str_split_i(filepath_audio, "/", -1))

# cross validation test
test_fold <- 1:5 |>
  set_names() |>
  map(\(fold) birdnet_combine(here("data", paste0("BirdNET_train_test_split_cv_fold_", fold), "test_output"))) |>
  list_rbind(names_to = "fold") |>
  mutate(fold = as.integer(fold))

test_fold_clean <- test_fold |>
  mutate(owl_id = str_split_i(File, "\\\\", -2),
         audio_file = str_split_i(File, "\\\\", -1)) |>
  rename(predicted = `Common name`,
         start = `Start (s)`,
         end = `End (s)`) |>
  left_join(audio_metadata, by = c("owl_id", "audio_file")) |>
  select(site, datetime, audio_id, audio_file, start, end, predicted, owl_id, fold)


# temporal test
test_temporal <- birdnet_combine(here("data", "BirdNET_train_test_split_temporal", "test_output"))

test_temporal_clean <- test_temporal |>
  mutate(owl_id = str_split_i(File, "\\\\", -2),
         audio_file = str_split_i(File, "\\\\", -1)) |>
  rename(predicted = `Common name`,
         start = `Start (s)`,
         end = `End (s)`) |>
  left_join(audio_metadata, by = c("owl_id", "audio_file")) |>
  select(site, datetime, audio_id, audio_file, start, end, predicted, owl_id)




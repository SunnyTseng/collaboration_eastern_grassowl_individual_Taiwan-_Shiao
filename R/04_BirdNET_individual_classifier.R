
# library -----------------------------------------------------------------

library(tidyverse)
library(here)


# import data -------------------------------------------------------------

data_audio <- read_csv(here("data", "taiga_audio_metadata.csv"))



# strategy 1 - train on 2023 to 2024, test on 2026 ------------------------

# Create a folder with train and test split
train <- data_audio %>%
  filter(year(datetime) %in% c(2023, 2024, 2025))

train_dir <- here("data", "BirdNET_train_test_split_temporal", "train")

for (i in 1:nrow(train)) {
  owl_folder <- file.path(train_dir, train$owl_id[i])
  dir.create(owl_folder, showWarnings = FALSE, recursive = TRUE)

  file.copy(from = train$filepath_audio[i], to = owl_folder)
}



test <- data_audio %>%
  filter(year(datetime) == 2026)

test_dir <- here("data", "BirdNET_train_test_split_temporal", "test")

for (i in 1:nrow(test)) {
  owl_folder <- file.path(test_dir, test$owl_id[i])
  dir.create(owl_folder, showWarnings = FALSE, recursive = TRUE)

  file.copy(from = test$filepath_audio[i], to = owl_folder)
}


# train and evaluate the classifier using BirdNET

### Currently run on BirdNET GUI, using the train tab
### run on BirdNET GUI, using batch analysis tab

# get test restuls


# strategy 2 - 5 fold validation ------------------------------------------

set.seed(456)

data_audio_folds <- data_audio %>%
  filter(owl_id != "A82") %>%

  mutate(night = date(datetime)) %>%

  group_by(owl_id) %>%
  mutate(fold = {
    nights <- sample(unique(night))
    rep_len(1:5, length(nights))[match(night, nights)]
  }) %>%
  ungroup()

data_audio_folds %>%
  count(owl_id, fold) %>%
  tidyr::pivot_wider(names_from = fold, values_from = n, values_fill = 0)



# Put the data into the folders

for (k in 1:5) {

  folder <- paste0(here("data", "BirdNET_train_test_split_cv_fold_"), k)

  for (i in 1:nrow(data_audio_folds)) {
    split <- ifelse(data_audio_folds$fold[i] == k, "test", "train")
    dest <- file.path(folder, split, data_audio_folds$owl_id[i])
    if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
    file.copy(data_audio_folds$filepath_audio[i], dest)
  }
}


# train and evaluate the classifier using BirdNET

### Currently run on BirdNET GUI, using the train tab
### run on BirdNET GUI, using batch analysis tab

# get test restuls


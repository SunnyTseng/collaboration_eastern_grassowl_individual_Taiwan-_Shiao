
# library -----------------------------------------------------------------

library(tidyverse)
library(here)


# import data -------------------------------------------------------------

data_audio <- read_csv(here("data", "taiga_audio_metadata.csv"))



# strategy 1 - train on 2023 to 2024, test on 2026 ------------------------

# Create a folder with train and test split

train <- data_audio %>%
  filter(year(datetime) %in% c(2023, 2024, 2025))

for (i in nrow(train)) {
  file.copy(from = audio,
            to = here("data", "BirdNET_train_test_split_temporal", "train"))
}



test <- data_audio %>%
  filter(year(datetime) == 2026)




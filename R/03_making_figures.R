
# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(gt)


# data summary ------------------------------------------------------------

data_audio <- read_csv(here("data", "taiga_audio_metadata.csv"))

table_owl_recordings <- data_audio %>%
  mutate(year = year(datetime)) %>%
  group_by(owl_id) %>%
  mutate(sites = n_distinct(site)) %>%
  ungroup() %>%
  count(owl_id, sites, year) %>%
  complete(nesting(owl_id, sites), year = 2023:2026, fill = list(n = 0)) %>%
  pivot_wider(names_from = year, values_from = n) %>%
  rename(owls = owl_id) %>%
  mutate(total = rowSums(across(`2023`:`2026`))) %>%
  arrange(desc(total)) %>%
  select(owls, sites, total, `2023`, `2024`, `2025`, `2026`) %>%

  gt() %>%
  tab_spanner(label = "Recordings per year",
              columns = `2023`:`2026`) %>%
  cols_label(owls = "Owl ID",
             sites = "Sites",
             total = "Total") %>%
  cols_align(align = "center", columns = -owls) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels())


gtsave(table_owl_recordings,
       here("docs", "tables",
            "table_owl_recordings.rtf"))




# BirdNET training parameters ---------------------------------------------

table_birdnet_parameters <- tibble::tribble(
  ~Parameter,                        ~Definition,                                                                                                 ~Setting,
  "Minimum bandpass frequency (Hz)", "Lowest frequency kept; sound below this value is filtered out",                                            "0",
  "Maximum bandpass frequency (Hz)", "Highest frequency kept; sound above this value is filtered out",                                            "15,000",
  "Audio speed modification",        "Changes the playback speed of the audio; 1.0 means no change",                                              "1.0",
  "Crop mode",                       "How recordings longer than the 3-s model input are cut; smart mode keeps up to the five segments with the highest energy peaks", "Smart",
  "Crop segment overlap (s)",        "Overlap between consecutive training segments; 0 means no overlap",                                         "0",
  "Autotune",                        "Automatically searches for the best hyperparameters, which takes longer",                                   "Yes",
  "Number of trials",                "Number of training runs used to search for hyperparameters",                                                "50",
  "Executions per trial",            "Number of times each set of hyperparameters is repeated during the search, which reduces variance",         "1"
) %>%
  gt() %>%
  cols_align(align = "left") %>%
  cols_width(Parameter ~ px(230),
             Definition ~ px(420),
             Setting ~ px(120)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels())


gtsave(table_birdnet_parameters,
       here("docs", "tables",
            "table_birdnet_parameters.rtf"))






# split the training and testing  -----------------------------------------

test <- data_audio %>%
  mutate(year = year(datetime),
         yday = yday(datetime),
         hour = hour(datetime)) %>%
  ggplot(aes(x = yday, y = hour)) +
    geom_tile(aes(fill = owl_id), alpha = 0.5) +
    facet_wrap(~ year, ncol = 1) +
    theme_bw()






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



# data_summary_figure -----------------------------------------------------

# get the data
data_audio <- read_csv(here("data", "taiga_audio_metadata.csv"))

# order owls by total recordings (largest first)
owl_order <- data_audio %>%
  group_by(owl_id) %>%
  summarise(total = n()) %>%
  arrange(total)

# making figure
fig_owl_recordings <- data_audio %>%
  mutate(year = factor(year(datetime), levels = 2023:2026)) %>%
  count(owl_id, year) %>%
  mutate(owl_id = factor(owl_id, levels = owl_order$owl_id)) %>%

  # ggplot
  ggplot(aes(x = owl_id, y = n, fill = year)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(data = owl_order |> mutate(owl_id = factor(owl_id, levels = owl_id)),
            aes(x = owl_id, y = total, label = total, fill = NULL),
            hjust = -0.5, size = 3.5, inherit.aes = FALSE) +
  # fine tune
  scale_fill_viridis_d(option = "D", drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Owl ID", y = "Number of recordings", fill = "Year") +
  guides(fill = guide_legend(nrow = 1)) +
  coord_flip() +
  # theme settings
  theme_bw(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.65, 0.15),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color = NA))

fig_owl_recordings

ggsave(fig_owl_recordings,
       filename = here("docs",
                       "figures",
                       "fig_owl_recordings.png"),
       width = 16,
       height = 10,
       units = "cm",
       dpi = 300)


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





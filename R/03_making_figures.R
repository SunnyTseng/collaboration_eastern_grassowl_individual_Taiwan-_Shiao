
# library -----------------------------------------------------------------

library(tidyverse)
library(here)



# data summary ------------------------------------------------------------

data_audio <- read_csv(here("data", "taiga_audio_metadata_test.csv"))

test <- data_audio %>%
  mutate(year = year(datetime),
         yday = yday(datetime),
         hour = hour(datetime)) %>%
  ggplot(aes(x = yday, y = hour)) +
    geom_tile(aes(fill = owl_id), alpha = 0.5) +
    facet_wrap(~ year, ncol = 1) +
    theme_bw()





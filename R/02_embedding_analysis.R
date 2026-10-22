


# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(umap)


# import data -------------------------------------------------------------

data_embedding <- read_csv(here("data",
                                "BirdNET_embedding_from_manual_event",
                                "embeddings.csv"))

audio_metadata <- read_csv(here("data", "taiga_audio_metadata.csv"))

# wrangling ---------------------------------------------------------------

# combine the embedding data with the audio metadata
data_embedding_cleaned <- data_embedding %>%
  mutate(owl_id = str_split_i(file_path, "\\\\", -3),
         site = str_split_i(file_path, "\\\\", -2),
         filepath_audio = file_path %>%
           str_replace_all("\\\\", "/") %>%
           str_replace("TAIGA_audio_event_2", "TAIGA_audio_2") %>%
           str_remove("_\\d+(?=\\.wav$)")) %>%
  left_join(audio_metadata, by = c("owl_id", "site", "filepath_audio"))

# make the matrix ready to run UMAP
feature_matrix <- data_embedding_cleaned$embedding |>
  strsplit(",") |>
  lapply(as.numeric) |>
  do.call(what = rbind)

dim(feature_matrix)   # should be 440 x 1024
anyNA(feature_matrix) # should be FALSE



# making figures ----------------------------------------------------------


library(uwot)
set.seed(42)
um <- umap(feature_matrix, n_neighbors = 15, min_dist = 0.1)

plot_df <- data_embedding_cleaned |>
  mutate(UMAP1 = um[, 1], UMAP2 = um[, 2])

data_embedding_cleaned |>
  mutate(UMAP1 = um[, 1], UMAP2 = um[, 2]) |>
  ggplot(aes(UMAP1, UMAP2, color = owl_id)) +
  geom_point(alpha = 0.8, size = 2.5) +
  theme_minimal(base_size = 12)


library(cluster)

owl <- factor(data_embedding_cleaned$owl_id)
d <- dist(feature_matrix)

sil <- silhouette(as.integer(owl), d)
mean(sil[, "sil_width"])                       # overall
tapply(sil[, "sil_width"], owl, mean)          # per owl


rec <- factor(data_embedding_cleaned$audio_id)
sil_rec <- silhouette(as.integer(rec), d)
mean(sil_rec[, "sil_width"])



keep <- data_embedding_cleaned |>
  distinct(owl_id, audio_id) |>
  count(owl_id) |>
  filter(n >= 3) |>
  pull(owl_id)

idx <- which(data_embedding_cleaned$owl_id %in% keep)

D2   <- as.matrix(d)[idx, idx]
owl2 <- droplevels(owl[idx])
rec2 <- data_embedding_cleaned$audio_id[idx]
k <- 5

pred <- sapply(seq_along(idx), function(i) {
  ok <- which(rec2 != rec2[i])
  nn <- ok[order(D2[i, ok])[1:k]]
  names(which.max(table(owl2[nn])))
})

mean(pred == owl2)
max(prop.table(table(owl2)))   # chance
table(truth = owl2, predicted = factor(pred, levels = levels(owl2)))







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

data_embedding_cleaned |>
  mutate(UMAP1 = um[, 1], UMAP2 = um[, 2]) |>
  ggplot(aes(UMAP1, UMAP2, color = owl_id)) +
  geom_point(alpha = 0.8, size = 2.5) +
  theme_minimal(base_size = 12)




# additional analysis -----------------------------------------------------

# 0. Silhouette width

library(cluster)

dat <- data_embedding_cleaned |>
  mutate(night = as.Date(ymd_hms(datetime) - hours(12)))

# silhouette by night and by site
sil_night <- silhouette(as.integer(factor(dat$night)), d)
sil_site  <- silhouette(as.integer(factor(dat$site)), d)
mean(sil_night[, "sil_width"])
mean(sil_site[, "sil_width"])

# keep owls with >= 3 nights
keep <- dat |> distinct(owl_id, night) |> count(owl_id) |> filter(n >= 3) |> pull(owl_id)
idx  <- which(dat$owl_id %in% keep)

D2     <- as.matrix(d)[idx, idx]
owl2   <- droplevels(factor(dat$owl_id[idx]))
night2 <- dat$night[idx]
k <- 5

pred <- sapply(seq_along(idx), function(i) {
  ok <- which(night2 != night2[i])
  nn <- ok[order(D2[i, ok])[1:k]]
  names(which.max(table(owl2[nn])))
})
pred <- factor(pred, levels = levels(owl2))

mean(pred == owl2)                                # accuracy
max(prop.table(table(owl2)))                      # majority-class chance
tab <- table(truth = owl2, predicted = pred)
tab
mean(diag(tab) / rowSums(tab))                    # balanced accuracy


# 1. Variance partitioning (PERMANOVA)
#
# This is the cleanest single result. It asks how much of the distance structure is explained by owl, site, and night.
#
# Report R² for each term. If night and site explain more than owl, that is direct evidence that the unsupervised structure is mostly environment. Since clips from the same night aren’t independent, restrict the permutations by night (the strata argument), or treat the p-values cautiously and lean on the R² values.

library(vegan)
adonis2(d ~ owl_id + site + night, data = dat, by = "margin", permutations = 999)

# 2. Clustering agreement
#
# Cluster without labels (k-means or hierarchical, with k = number of owls), then compare the clusters to each label set. If the ARI against night or site is higher than against owl, the clusters are following the environment.

library(mclust)
cl <- kmeans(prcomp(feature_matrix)$x[, 1:30], centers = 7, nstart = 25)$cluster
adjustedRandIndex(cl, dat$owl_id)
adjustedRandIndex(cl, dat$night)
adjustedRandIndex(cl, dat$site)

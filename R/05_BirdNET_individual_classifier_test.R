


# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(birdnetTools)
library(yardstick)


recording_vote <- function(df) {
  truth <- df |> distinct(audio_id, audio_file, owl_id)

  df |>
    # flag the winning ID in each segment
    group_by(audio_id, audio_file, start) |>
    mutate(won = confidence == max(confidence)) |>
    # votes per predicted ID in each recording (summed confidence breaks ties)
    group_by(audio_id, audio_file, predicted) |>
    summarise(votes = sum(won), conf = sum(confidence), .groups = "drop") |>
    # rank, then take the first and second
    arrange(audio_id, audio_file, desc(votes), desc(conf)) |>
    group_by(audio_id, audio_file) |>
    summarise(predicted_1 = predicted[1],
              predicted_2 = predicted[2],
              .groups = "drop") |>
    left_join(truth, by = c("audio_id", "audio_file"))
}

evaluate_recordings <- function(rec) {
  lv <- sort(union(rec$owl_id, rec$predicted_1))

  d <- rec |>
    mutate(truth = factor(owl_id, lv),
           pred  = factor(predicted_1, lv),
           top1  = truth == pred,
           top2  = top1 | coalesce(owl_id == predicted_2, FALSE))

  cm <- table(d$truth, d$pred)
  per_owl <- tibble(owl = lv,
                    support   = rowSums(cm),
                    n_pred    = colSums(cm),
                    tp        = diag(cm),
                    precision = tp / n_pred,
                    recall    = tp / support,
                    f1        = 2 * tp / (support + n_pred)) |>
    filter(support > 0 | n_pred > 0)

  true_owls <- filter(per_owl, support > 0)

  overall <- tibble(n        = nrow(d),
                    accuracy = mean(d$top1),
                    top2     = mean(d$top2),
                    bal_acc  = mean(true_owls$recall),
                    macro_f1 = mean(true_owls$f1),
                    kappa    = kap(d, truth, pred)$.estimate)

  list(overall = overall, per_owl = per_owl, conf_mat = conf_mat(d, truth, pred))
}


# import data -------------------------------------------------------------


# audio metadata
audio_metadata <- read_csv(here("data", "taiga_audio_metadata.csv")) |>
  mutate(audio_file = str_split_i(filepath_audio, "/", -1))



# import test dataset - cross validation ----------------------------------

# cross validation test
test_fold <- 1:5 |>
  set_names() |>
  map(\(fold) birdnet_combine(here("data", paste0("BirdNET_train_test_split_cv_fold_", fold), "test_output"))) |>
  list_rbind(names_to = "fold") |>
  mutate(fold = as.integer(fold))

test_fold_1 <- test_fold |>
  mutate(owl_id = str_split_i(File, "\\\\", -2),
         audio_file = str_split_i(File, "\\\\", -1)) |>
  rename(predicted = `Common name`,
         start = `Start (s)`,
         end = `End (s)`,
         confidence = Confidence) %>%
  left_join(audio_metadata, by = c("owl_id", "audio_file")) |>
  select(site, datetime, audio_id, audio_file, start, end, confidence, predicted, owl_id, fold)

test_fold_segment <- test_fold_1

test_fold_recording <- test_fold_1 %>%
  recording_vote()



# import test dataset - temporal ------------------------------------------

# temporal test
test_temporal <- birdnet_combine(here("data", "BirdNET_train_test_split_temporal", "test_output"))

test_temporal_1 <- test_temporal |>
  mutate(owl_id = str_split_i(File, "\\\\", -2),
         audio_file = str_split_i(File, "\\\\", -1)) |>
  rename(predicted = `Common name`,
         start = `Start (s)`,
         end = `End (s)`,
         confidence = Confidence) |>
  left_join(audio_metadata, by = c("owl_id", "audio_file")) |>
  select(site, datetime, audio_id, audio_file, start, end, confidence, predicted, owl_id)


test_temporal_segment <- test_temporal_1

test_temporal_recording <- test_temporal_1 %>%
  recording_vote()



# recording level evaluation ----------------------------------------------

performance_fold_recording <- evaluate_recordings(test_fold_recording)
performance_fold_recording$overall
performance_fold_recording$per_owl


autoplot(performance_fold_recording$conf_mat, type = "heatmap")

performance_temporal_recording <- evaluate_recordings(test_temporal_recording)
performance_temporal_recording$overall
performance_temporal_recording$per_owl
autoplot(performance_temporal_recording$conf_mat, type = "heatmap")



library(ggplot2)
library(dplyr)

plot_confmat <- function(cm, title = NULL) {
  df <- as.data.frame(cm$table) |>                  # columns: Prediction, Truth, Freq
    rename(x = Prediction, y = Truth, value = Freq) |>
    mutate(y = factor(y, levels = rev(levels(y))),  # first owl at the top
           fill_val = ifelse(value > 0, log(value), NA))

  ggplot(df, aes(x = x, y = y, fill = fill_val)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1) +
    geom_text(data = filter(df, value > 0), aes(label = value), size = 3.5) +
    scale_fill_gradient(low = "#deebf7", high = "#08519c",
                        na.value = "grey95", name = "log(count)") +
    coord_fixed() +
    labs(x = "Predicted owl", y = "True owl", title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.1, hjust = 0.2),
          panel.grid = element_blank())
}

plot_confmat(res$conf_mat, "5-fold CV (recording level)")
plot_confmat(performance_temporal_recording$conf_mat, "Temporal split (recording level)")

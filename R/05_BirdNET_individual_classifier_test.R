


# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(birdnetTools)
library(yardstick)
library(patchwork)



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

plot_confmat <- function(cm, title = NULL, drop_empty = TRUE) {

  df <- as.data.frame(cm$table) |>
    rename(x = Prediction, y = Truth, value = Freq) |>
    mutate(x = as.character(x),
           y = as.character(y),
           fill_val = ifelse(value > 0, log(value), NA))

  # number of test recordings per owl (true totals) and predictions per owl
  n_true <- df |> group_by(owl = y) |> summarise(n_true = sum(value), .groups = "drop")
  n_pred <- df |> group_by(owl = x) |> summarise(n_pred = sum(value), .groups = "drop")

  owl_totals <- full_join(n_true, n_pred, by = "owl") |>
    arrange(desc(n_true), desc(n_pred))      # most test recordings first

  # drop classes with no test recordings and no predictions
  if (drop_empty) {
    owl_totals <- filter(owl_totals, n_true > 0 | n_pred > 0)
  }

  owl_order <- owl_totals$owl

  df <- df |>
    filter(x %in% owl_order, y %in% owl_order) |>
    mutate(x = factor(x, levels = rev(owl_order)),  # rev() because coord_flip puts the first level at the bottom
           y = factor(y, levels = owl_order))

  ggplot(df, aes(x = x, y = y, fill = fill_val)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1) +
    geom_text(data = filter(df, value > 0), aes(label = value), size = 3.5) +
    scale_fill_gradient(low = "#deebf7", high = "#08519c",
                        na.value = "grey95", name = "log(count)") +
    coord_flip() +
    labs(x = "Predicted Owl ID", y = "Owl ID", title = title) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_blank())
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


performance_temporal_recording <- evaluate_recordings(test_temporal_recording)

performance_temporal_recording$overall
performance_temporal_recording$per_owl
plot_confmat(performance_temporal_recording$conf_mat)


performance_temporal_recording_n_A82 <- test_temporal_recording %>%
  filter(owl_id != "A82") %>%
  evaluate_recordings()

performance_temporal_recording_n_A82$overall
performance_temporal_recording_n_A82$per_owl





fig_overall_performance <- performance_temporal_recording_n_A82$conf_mat %>%
  plot_confmat()


plot_df <- performance_temporal_recording_n_A82$per_owl |>
  mutate(owl_lab = paste0(owl, "\n(n = ", support, ")"),
         owl_lab = fct_reorder(owl_lab, -support)) |>   # most test recordings first
  select(owl_lab, precision, recall) |>
  pivot_longer(c(precision, recall),
               names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric,
                         levels = c("precision", "recall"),
                         labels = c("Precision", "Recall")))

fig_owl_performance <- ggplot(plot_df, aes(x = owl_lab, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", value)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.2) +

  scale_fill_viridis_d(option = "D", begin = 0.25, end = 0.75) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  coord_cartesian(ylim = c(0, 1.1), expand = FALSE) +

  labs(x = "Owl ID", y = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")


fig_temporal_recording <- fig_overall_performance + fig_owl_performance +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

fig_temporal_recording

ggsave(here("docs", "figures", "fig_temporal_recording.png"),
       fig_temporal_recording,
       width = 23,
       height = 14,
       unit = "cm",
       dpi = 300)









# others ------------------------------------------------------------------


performance_fold_recording <- evaluate_recordings(test_fold_recording)

performance_fold_recording$overall
performance_fold_recording$per_owl
plot_confmat(performance_fold_recording$conf_mat)



# PREAMBLE ----------------------------------------------------------------

# libraries
require(here)
library(tidyverse)
library(cowplot)
library(magrittr)

# LABELS ------------------------------------------------------------------

# list of fruit names
fruit_names <- c("banana", "mango", "buroi", "sofeda", "guava", "date.palm",
                 "jackfruit", "lichhi", "custard.apple", "wood.apple", "elephant.apple",
                 "papaya", "watermelon", "melon", "cashew.fruit", "pomegranate", "palmyra",
                 "plum", "rose.apple", "indian.olive", "latkan", "monkey.jack", "uriam",
                 "rattan",  "river.ebony", "star.fruit", "wild.dates", "coconut", "betel",
                 "blackberry.jam", "water.chestnut")

# list of formatted fruit names
fruit_names_text <- c("banana", "mango", "jujube", "sapodilla", "guava", "date",
                      "jackfruit", "lychee", "custard apple", "wood apple", "elephant apple",
                      "papaya", "watermelon", "melon", "cashew fruit", "pomegranate", "palmyra fruit",
                      "plum", "rose apple", "Indian olive", "latkan", "monkey jack", "uriam",
                      "rattan",  "river ebony", "star fruit", "wild date", "coconut", "betel",
                      "blackberry jam", "water chestnut")

# list of fruit names that fruit in winter
fruit_names_winter <- c("banana", "buroi", "sofeda", "guava", "papaya", "star.fruit", "coconut")

# list of animal names
animal_names <- c("cattle", "buffalo", "goats", "pigs",
                  "sheep", "horses", "dogs", "cats")

# FRUIT PRESENCE IN BARI --------------------------------------------------

# create a new object where presence of fruit across village types is ranked
rank_tp_presence_grouped_mean <- tp_presence_grouped %>%
  mutate(winter_fruit = ifelse(fruit %in% fruit_names_winter, "yes", "no")) %>%
  filter(measure == "mean") %>%
  group_by(fruit) %>%
  mutate(average = mean(tp_fruit))
rank_tp_presence_grouped_mean$fruit_text <- rep(fruit_names_text, 3)
rank_tp_presence_grouped_mean %<>%
  arrange(desc(average)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(average)))

# summarize the variation across villages
tp_presence_cols <- paste0("q5_frt", seq(1:31), "_5_2")
tp_presence <- fruit_survey %>%
  dplyr::select(Village_ID, case_control_group, all_of(tp_presence_cols))
colnames(tp_presence) <- c("Village_ID", "case_control_group", fruit_names)
tp_presence_all <- tp_presence %>%
  gather(banana:water.chestnut, key = "fruit", value = "value") %>%
  group_by(Village_ID, case_control_group, fruit) %>%
  summarize(mean = mean(value > 0, na.rm = TRUE),
            count = sum(value > 0, na.rm = TRUE),
            number = sum(!is.na(value))) %>%
  group_by(case_control_group, fruit) %>%
  summarize(grand_count = sum(count),
            grand_total = sum(number)) %>%
  mutate(binom::binom.exact(x = grand_count, n = grand_total))
tp_presence_merged <- full_join(rank_tp_presence_grouped_mean, tp_presence_all)

# format text for plotting
tp_fruit_text <- tp_presence_merged %>%
  distinct(fruit, winter_fruit, fruit_text) %>%
  mutate(face = case_when(winter_fruit == "no" ~ 1,
                          winter_fruit == "yes" ~ 2),
         size = case_when(winter_fruit == "no" ~ 10,
                          winter_fruit == "yes" ~ 11))

# plot fruit presence with x-axis ranked by the average
ggplot() +
  geom_pointrange(data = tp_presence_merged,
                aes(x = reorder(fruit_text, rank), y = mean, ymin = lower, ymax = upper,
                    shape = winter_fruit, color = case_control_group),
                position = position_dodge(width = 0.5), show.legend = TRUE, size = 0.25) +
  labs(x = "Fruit species",
       y = "Proportion of respondents reporting\nfruit presence in bari") +
  scale_color_manual(
    name = "Village type",
    values = c(
      "case" = "#E69F00",
      "control, near" = "#009E73",
      "control, far" = "#56B4E9")
  ) +
  scale_y_continuous(breaks = seq(0, .9, .1), minor_breaks = seq(0, .8, .05)) +
  scale_shape_manual(name = "Available in winter", values = c(16, 18)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = tp_fruit_text$face,
                                   size = tp_fruit_text$size),
        axis.title.y = element_text(vjust = 1),
        legend.position = "bottom",
        legend.justification = 0.5)
ggsave("./Results/survey_fruit_presence.png", height = 5, width = 8, units = "in", dpi = 300, bg = "white")

# HUMAN CONSUMPTION OF FRUIT ----------------------------------------------

# create a new object where average consumption across village types is ranked
rank_h_fruit_grouped_mean <- h_fruit_grouped %>%
  mutate(winter_fruit = ifelse(fruit %in% fruit_names_winter, "yes", "no")) %>%
  filter(measure == "mean") %>%
  group_by(fruit) %>%
  mutate(average = mean(human_fruit))
rank_h_fruit_grouped_mean$fruit_text <- rep(fruit_names_text, 3)
rank_h_fruit_grouped_mean %<>%
  arrange(desc(average)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(average)))

# summarize the variation across villages
human_fruit_cols <- paste0("q5_frt", seq(1:31), "_5_3")
human_fruit <- fruit_survey %>%
  dplyr::select(Village_ID, case_control_group, all_of(human_fruit_cols))
colnames(human_fruit) <- c("Village_ID", "case_control_group", fruit_names)
human_fruit_all <- human_fruit %>%
  gather(banana:water.chestnut, key = "fruit", value = "value") %>%
  group_by(Village_ID, case_control_group, fruit) %>%
  summarize(mean = mean(value > 0, na.rm = TRUE),
            count = sum(value > 0, na.rm = TRUE),
            number = sum(!is.na(value))) %>%
  group_by(case_control_group, fruit) %>%
  summarize(grand_count = sum(count),
            grand_total = sum(number)) %>%
  mutate(binom::binom.exact(x = grand_count, n = grand_total))
human_fruit_merged <- full_join(rank_h_fruit_grouped_mean, human_fruit_all)

# format text for plotting
h_fruit_text <- human_fruit_merged %>%
  distinct(fruit, winter_fruit, fruit_text) %>%
  mutate(face = case_when(winter_fruit == "no" ~ 1,
                          winter_fruit == "yes" ~ 2),
         size = case_when(winter_fruit == "no" ~ 10,
                          winter_fruit == "yes" ~ 11))

# plot human fruit consumption with x-axis ranked by the average
ggplot() +
  geom_pointrange(data = human_fruit_merged,
                  aes(x = reorder(fruit_text, rank), y = mean, ymin = lower, ymax = upper,
                      shape = winter_fruit, color = case_control_group),
                  position = position_dodge(width = 0.5), show.legend = TRUE, size = 0.25) +
  labs(x = "Fruit species",
       y = "Proportion of respondents reporting\nbari members consuming fruit") +
  scale_color_manual(
    name = "Village type",
    values = c(
      "case" = "#E69F00",
      "control, near" = "#009E73",
      "control, far" = "#56B4E9")
  ) +
  scale_y_continuous(breaks = seq(0, 1, .1), minor_breaks = seq(0, 1, .05)) +
  scale_shape_manual(name = "Available in winter", values = c(16, 18)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = h_fruit_text$face,
                                   size = h_fruit_text$size),
        axis.title.y = element_text(vjust = 1),
        legend.position = "bottom",
        legend.justification = 0.5)
ggsave("./Results/survey_human_fruit_consumption.png", height = 5, width = 8, units = "in", dpi = 300, bg = "white")

# HUMAN DROPPED FRUIT CONSUMPTION -----------------------------------------

# create a new object where average consumption across village types is ranked
rank_h_dropped_grouped_mean <- h_dropped_grouped %>%
  mutate(winter_fruit = ifelse(fruit %in% fruit_names_winter, "yes", "no")) %>%
  filter(measure == "mean") %>%
  group_by(fruit) %>%
  mutate(average = mean(human_dropped_fruit))
rank_h_dropped_grouped_mean$fruit_text <- rep(fruit_names_text, 3)
rank_h_dropped_grouped_mean %<>%
  arrange(desc(average)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(average)))

# summarize the variation across villages
dropped_fruit_cols <- paste0("q5_frt", seq(1:31), "_5_4")
h_dropped_fruit <- fruit_survey %>%
  dplyr::select(Village_ID, case_control_group, all_of(dropped_fruit_cols))
colnames(h_dropped_fruit) <- c("Village_ID", "case_control_group", fruit_names)
h_dropped_fruit_all <- h_dropped_fruit %>%
  gather(banana:water.chestnut, key = "fruit", value = "value") %>%
  group_by(Village_ID, case_control_group, fruit) %>%
  summarize(mean = mean(value > 0, na.rm = TRUE),
            count = sum(value > 0, na.rm = TRUE),
            number = sum(!is.na(value))) %>%
  group_by(case_control_group, fruit) %>%
  summarize(grand_count = sum(count),
            grand_total = sum(number)) %>%
  mutate(binom::binom.exact(x = grand_count, n = grand_total))
h_dropped_fruit_merged <- full_join(rank_h_dropped_grouped_mean, h_dropped_fruit_all)

# format text for plotting
h_dropped_fruit_text <- h_dropped_fruit_merged %>%
  distinct(fruit, winter_fruit, fruit_text) %>%
  mutate(face = case_when(winter_fruit == "no" ~ 1,
                          winter_fruit == "yes" ~ 2),
         size = case_when(winter_fruit == "no" ~ 10,
                          winter_fruit == "yes" ~ 11))

# plot animal dropped fruit consumption with x-axis ranked by the average
ggplot() +
  geom_pointrange(data = h_dropped_fruit_merged,
                  aes(x = reorder(fruit_text, rank), y = mean, ymin = lower, ymax = upper,
                      shape = winter_fruit, color = case_control_group),
                  position = position_dodge(width = 0.5), show.legend = TRUE, size = 0.25) +
  labs(x = "Fruit species",
       y = "Proportion of respondents reporting\nbari members consuming dropped fruits") +
  scale_color_manual(
    name = "Village type",
    values = c(
      "case" = "#E69F00",
      "control, near" = "#009E73",
      "control, far" = "#56B4E9")
  ) +
  scale_y_continuous(breaks = seq(0, .9, .1), minor_breaks = seq(0, .9, .05)) +
  scale_shape_manual(name = "Available in winter", values = c(16, 18)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = h_dropped_fruit_text$face,
                                   size = h_dropped_fruit_text$size),
        axis.title.y = element_text(vjust = 1),
        legend.position = "bottom",
        legend.justification = 0.5)
ggsave("./Results/survey_human_dropped_fruit_consumption.png", height = 5, width = 8, units = "in", dpi = 300, bg = "white")

# ANIMAL DROPPED FRUIT CONSUMPTION ----------------------------------------

# create a new object where average consumption across village types is ranked
rank_a_dropped_grouped_mean <- a_dropped_grouped %>%
  mutate(winter_fruit = ifelse(fruit %in% fruit_names_winter, "yes", "no")) %>%
  filter(measure == "mean") %>%
  group_by(fruit) %>%
  mutate(average = mean(animal_dropped_fruit))
rank_a_dropped_grouped_mean$fruit_text <- rep(fruit_names_text, 3)
rank_a_dropped_grouped_mean %<>%
  arrange(desc(average)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(average)))

# summarize the variation across villages
animal_fruit_cols <- paste0("q5_frt", seq(1:31), "_5_5")
a_dropped_fruit <- fruit_survey %>%
  dplyr::select(Village_ID, case_control_group, all_of(animal_fruit_cols))
colnames(a_dropped_fruit) <- c("Village_ID", "case_control_group", fruit_names)
a_dropped_fruit_all <- a_dropped_fruit %>%
  gather(banana:water.chestnut, key = "fruit", value = "value") %>%
  group_by(Village_ID, case_control_group, fruit) %>%
  summarize(mean = mean(value > 0, na.rm = TRUE),
            count = sum(value > 0, na.rm = TRUE),
            number = sum(!is.na(value))) %>%
  group_by(case_control_group, fruit) %>%
  summarize(grand_count = sum(count),
            grand_total = sum(number)) %>%
  mutate(binom::binom.exact(x = grand_count, n = grand_total))
a_dropped_fruit_merged <- full_join(rank_a_dropped_grouped_mean, a_dropped_fruit_all)

# format text for plotting
a_dropped_fruit_text <- a_dropped_fruit_merged %>%
  distinct(fruit, winter_fruit, fruit_text) %>%
  mutate(face = case_when(winter_fruit == "no" ~ 1,
                          winter_fruit == "yes" ~ 2),
         size = case_when(winter_fruit == "no" ~ 10,
                          winter_fruit == "yes" ~ 11))

# plot animal dropped fruit consumption with x-axis ranked by the average
ggplot() +
  geom_pointrange(data = a_dropped_fruit_merged,
                  aes(x = reorder(fruit_text, rank), y = mean, ymin = lower, ymax = upper,
                      shape = winter_fruit, color = case_control_group),
                  position = position_dodge(width = 0.5), show.legend = TRUE, size = 0.25) +
  labs(x = "Fruit species",
       y = "Proportion of respondents reporting\nanimals consuming dropped fruits") +
  scale_color_manual(
    name = "Village type",
    values = c(
      "case" = "#E69F00",
      "control, near" = "#009E73",
      "control, far" = "#56B4E9")
  ) +
  scale_y_continuous(breaks = seq(0, .3, .1), minor_breaks = seq(0, .3, .05)) +
  scale_shape_manual(name = "Available in winter", values = c(16, 18)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = a_dropped_fruit_text$face,
                                   size = a_dropped_fruit_text$size),
        axis.title.y = element_text(vjust = 1),
        legend.position = "bottom",
        legend.justification = 0.5)
ggsave("./Results/survey_animal_dropped_fruit_consumption.png", height = 5, width = 8, units = "in", dpi = 300, bg = "white")

# BAT FRUIT CONSUMPTION ---------------------------------------------------

# create a new object where average consumption across village types is ranked
rank_b_fruit_grouped_mean <- b_fruit_grouped %>%
  mutate(winter_fruit = ifelse(fruit %in% fruit_names_winter, "yes", "no")) %>%
  filter(measure == "mean") %>%
  group_by(fruit) %>%
  mutate(average = mean(bat_fruit))
rank_b_fruit_grouped_mean$fruit_text <- rep(fruit_names_text, 3)
rank_b_fruit_grouped_mean %<>%
  arrange(desc(average)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(average)))

# summarize the variation across villages
bat_fruit_cols <- paste0("q5_frt", seq(1:31), "_5_7")
bat_fruit <- fruit_survey %>%
  dplyr::select(Village_ID, case_control_group, all_of(bat_fruit_cols))
colnames(bat_fruit) <- c("Village_ID", "case_control_group", fruit_names)
b_fruit_all <- bat_fruit %>%
  gather(banana:water.chestnut, key = "fruit", value = "value") %>%
  group_by(Village_ID, case_control_group, fruit) %>%
  summarize(mean = mean(value > 0, na.rm = TRUE),
            count = sum(value > 0, na.rm = TRUE),
            number = sum(!is.na(value))) %>%
  group_by(case_control_group, fruit) %>%
  summarize(grand_count = sum(count),
            grand_total = sum(number)) %>%
  mutate(binom::binom.exact(x = grand_count, n = grand_total))
b_fruit_merged <- full_join(rank_b_fruit_grouped_mean, b_fruit_all)

# format text for plotting
b_fruit_text <- rank_b_fruit_grouped_mean %>%
  distinct(fruit, winter_fruit, fruit_text) %>%
  mutate(face = case_when(winter_fruit == "no" ~ 1,
                          winter_fruit == "yes" ~ 2),
         size = case_when(winter_fruit == "no" ~ 10,
                          winter_fruit == "yes" ~ 11))

# plot bat fruit consumption with x-axis ranked by the average
ggplot() +
  geom_pointrange(data = b_fruit_merged,
                  aes(x = reorder(fruit_text, rank), y = mean, ymin = lower, ymax = upper,
                      shape = winter_fruit, color = case_control_group),
                  position = position_dodge(width = 0.5), show.legend = TRUE, size = 0.25) +
  labs(x = "Fruit species",
       y = "Proportion of baris reporting\nbat fruit consumption") +
  scale_color_manual(
    name = "Village type",
    values = c(
      "case" = "#E69F00",
      "control, near" = "#009E73",
      "control, far" = "#56B4E9")
  ) +
  scale_y_continuous(breaks = seq(0, 1, .1), minor_breaks = seq(0, 1, .05)) +
  scale_shape_manual(name = "Available in winter", values = c(16, 18)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = b_fruit_text$face,
                                   size = b_fruit_text$size),
        axis.title.y = element_text(vjust = 1),
        legend.position = "bottom",
        legend.justification = 0.5)
ggsave("./Results/survey_bat_fruit_consumption.png", height = 5, width = 8, units = "in", dpi = 300, bg = "white")

# TOTAL NUMBER OF FRUITS --------------------------------------------------

# create a new object where average numbers across village types is ranked
fruit_trees_grouped_mean <- fruit_trees_grouped %>%
  mutate(winter_fruit = ifelse(fruit %in% fruit_names_winter, "yes", "no")) %>%
  filter(measure == "mean") %>%
  group_by(fruit) %>%
  mutate(average = mean(trees_in_bari))
fruit_trees_grouped_mean$fruit_text <- rep(fruit_names_text, 3)
fruit_trees_grouped_mean %<>%
  arrange(desc(average)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(average)))

# summarize the variation across villages
tp_count_all <- tp_presence %>%
  gather(banana:water.chestnut, key = "fruit", value = "value") %>%
  group_by(Village_ID, case_control_group, fruit) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            count = sum(value, na.rm = TRUE),
            number = sum(!is.na(value))) %>%
  group_by(case_control_group, fruit) %>%
  summarize(grand_mean = mean(mean, na.rm = TRUE),
            grand_count = sum(count),
            grand_total = sum(number),
            sd = sd(mean, na.rm = TRUE),
            se = sd/sqrt(grand_total),
            crit = qt(0.95/2 + .5, grand_total-1))
tp_count_merged <- full_join(fruit_trees_grouped_mean, tp_count_all)

# format text for plotting
fruit_trees_text <- tp_count_merged %>%
  distinct(fruit, winter_fruit, fruit_text) %>%
  mutate(face = case_when(winter_fruit == "no" ~ 1,
                          winter_fruit == "yes" ~ 2),
         size = case_when(winter_fruit == "no" ~ 10,
                          winter_fruit == "yes" ~ 11))

# plot fruit trees in bari with x-axis ranked by the average
ggplot() +
  geom_pointrange(data = tp_count_merged,
                  aes(x = reorder(fruit_text, rank), y = grand_mean, ymin = grand_mean - crit*se, ymax = grand_mean + crit*se,
                      shape = winter_fruit, color = case_control_group),
                  position = position_dodge(width = 0.5), show.legend = TRUE, size = 0.25) +
  labs(x = "Fruit species",
       y = "Number of fruit trees or plants\ngrowing in or around bari") +
  scale_color_manual(
    name = "Village type",
    values = c(
      "case" = "#E69F00",
      "control, near" = "#009E73",
      "control, far" = "#56B4E9")
  ) +
  scale_y_continuous(breaks = seq(0, 80, 20), minor_breaks = seq(0, 80, 5)) +
  scale_shape_manual(name = "Available in winter", values = c(16, 18)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = fruit_trees_text$face,
                                   size = fruit_trees_text$size),
        axis.title.y = element_text(vjust = 1),
        legend.position = "bottom",
        legend.justification = 0.5)
ggsave("./Results/survey_fruit_number.png", height = 5, width = 8, units = "in", dpi = 300, bg = "white")

# JOIN DATA ---------------------------------------------------------------

# join all the tables together
fruit_consumption <- rbind(tp_presence_merged %>% rename(value = tp_fruit) %>% mutate(subject = "tp_fruit", plot_order = 1),
                           human_fruit_merged %>% rename(value = human_fruit) %>% mutate(subject = "human_fruit", plot_order = 2),
                           h_dropped_fruit_merged %>% rename(value = human_dropped_fruit) %>% mutate(subject = "human_dropped_fruit", plot_order = 3),
                           a_dropped_fruit_merged %>% rename(value = animal_dropped_fruit) %>% mutate(subject = "animal_dropped_fruit", plot_order = 4),
                           b_fruit_merged %>% rename(value = bat_fruit) %>% mutate(subject = "bat_fruit", plot_order = 5))

plot_description <- c("fruit presence\nin bari", "bari members\nconsuming fruit",
                      "bari members\nconsuming\ndropped fruits", "animals\nconsuming\ndropped fruits",
                      "bat fruit\nconsumption")
names(plot_description) <- unique(fruit_consumption$plot_order)

fruit_consumption %>%
  filter(
    fruit %in% c("banana", "guava", "buroi", "sofeda", "star.fruit")
  ) %>%
  ggplot() +
  geom_pointrange(aes(x = fruit_text, y = mean, ymin = lower, ymax = upper, color = case_control_group),
                  shape = 18, size = 0.33, position = position_dodge(width = 0.5), show.legend = TRUE) +
  facet_grid(rows = vars(plot_order),
             labeller = labeller(plot_order = plot_description),
             scale = "free_y") +
  labs(x = "Fruit species",
       y = "Proportion of respondents reporting") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        legend.justification = 0.5) +
  scale_color_manual(
    name = "Village type",
    values = c(
      "case" = "#E69F00",
      "control, far" = "#56B4E9",
      "control, near" = "#009E73"
    )
  )
ggsave("./Results/survey_fruit_comparison.png", height = 6, width = 6, units = "in", dpi = 300, bg = "white")

# filter to just the mean and calculate the average proportion across all columns
fruit_consumption_mean <- full_join(tp_fruit_present, h_fruit_total) %>%
  full_join(h_dropped_total) %>%
  full_join(a_dropped_total) %>%
  full_join(b_fruit_total) %>%
  filter(measure == "mean") %>%
  rowwise() %>% 
  mutate(average = mean(c_across(human_fruit:bat_fruit))) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(average)))

fruit_consumption_long <- full_join(tp_fruit_present, h_fruit_total) %>%
  full_join(h_dropped_total) %>%
  full_join(a_dropped_total) %>%
  full_join(b_fruit_total) %>%
  gather(tp_fruit:bat_fruit, key = "type", value = "value")

fruit_consumption_wide <- fruit_consumption_long %>%
  pivot_wider(names_from = c("type", "measure"), values_from = "value")
fruit_consumption_wide$rank <- fruit_consumption_mean$rank
fruit_consumption_wide <- arrange(fruit_consumption_wide, rank)

write.csv(fruit_consumption_wide, here("Data","fruit_consumption.csv"))

# print a table
kable(fruit_consumption_mean %>%
        arrange(rank) %>%
        dplyr::select(rank),
      digits = 3,
      caption = "Proportion of respondants reporting fruit consumption by bari members and bats and dropped fruit consumption by bari members and domestic animals (ranked by average across columns)")

# ADDITIONAL SURVEY QUESTIONS ---------------------------------------------

# q3_1 How many total date palm trees do people in this bari own?
fruit_survey %>%
  group_by(case_control_group) %>%
  summarize(mean = mean(q3_1, na.rm = TRUE),
            total = sum(q3_1, na.rm = TRUE),
            count = sum(!is.na(q3_1)),
            numNA = sum(is.na(q3_1)))

# RANK CORRELATION --------------------------------------------------------

kendall.test <- function(df, group, variable){
  dat1 <- as_tibble(df[[1]]) %>%
    filter(case_control_group == group[1]) %>%
    pull(variable[1])
  dat2 <- as_tibble(df[[2]]) %>%
    filter(case_control_group == group[2]) %>%
    pull(variable[2])
  cor.test(dat1, dat2, method = "kendall")
}

# human fruit consumption
human_fruit_ranked <- human_fruit_merged %>%
  arrange(fruit)
kendall.test(list(human_fruit_ranked, human_fruit_ranked), c("case", "control, near"), c("human_fruit", "human_fruit"))
kendall.test(list(human_fruit_ranked, human_fruit_ranked), c("case", "control, far"), c("human_fruit", "human_fruit"))
kendall.test(list(human_fruit_ranked, human_fruit_ranked), c("control, near", "control, far"), c("human_fruit", "human_fruit"))

# human dropped fruit consumption
h_dropped_fruit_ranked <- h_dropped_fruit_merged %>%
  arrange(fruit)
kendall.test(list(h_dropped_fruit_ranked, h_dropped_fruit_ranked), c("case", "control, near"), c("human_dropped_fruit", "human_dropped_fruit"))
kendall.test(list(h_dropped_fruit_ranked, h_dropped_fruit_ranked), c("case", "control, far"), c("human_dropped_fruit", "human_dropped_fruit"))
kendall.test(list(h_dropped_fruit_ranked, h_dropped_fruit_ranked), c("control, near", "control, far"), c("human_dropped_fruit", "human_dropped_fruit"))

# animal dropped fruit consumption
a_dropped_fruit_ranked <- a_dropped_fruit_merged %>%
  arrange(fruit)
kendall.test(list(a_dropped_fruit_ranked, a_dropped_fruit_ranked), c("case", "control, near"), c("animal_dropped_fruit", "animal_dropped_fruit"))
kendall.test(list(a_dropped_fruit_ranked, a_dropped_fruit_ranked), c("case", "control, far"), c("animal_dropped_fruit", "animal_dropped_fruit"))
kendall.test(list(a_dropped_fruit_ranked, a_dropped_fruit_ranked), c("control, near", "control, far"), c("animal_dropped_fruit", "animal_dropped_fruit"))

# bat fruit consumption
b_fruit_ranked <- b_fruit_merged %>%
  arrange(fruit)
kendall.test(list(b_fruit_ranked, b_fruit_ranked), c("case", "control, near"), c("bat_fruit", "bat_fruit"))
kendall.test(list(b_fruit_ranked, b_fruit_ranked), c("case", "control, far"), c("bat_fruit", "bat_fruit"))
kendall.test(list(b_fruit_ranked, b_fruit_ranked), c("control, near", "control, far"), c("bat_fruit", "bat_fruit"))

# human fruit vs. bat fruit consumption
kendall.test(list(human_fruit_ranked, b_fruit_ranked), c("case", "case"), c("human_fruit", "bat_fruit"))
kendall.test(list(human_fruit_ranked, b_fruit_ranked), c("control, near", "control, near"), c("human_fruit", "bat_fruit"))
kendall.test(list(human_fruit_ranked, b_fruit_ranked), c("control, far", "control, far"), c("human_fruit", "bat_fruit"))

# human dropped fruit vs. bat fruit consumption
kendall.test(list(h_dropped_fruit_ranked, b_fruit_ranked), c("case", "case"), c("human_dropped_fruit", "bat_fruit"))
kendall.test(list(h_dropped_fruit_ranked, b_fruit_ranked), c("control, near", "control, near"), c("human_dropped_fruit", "bat_fruit"))
kendall.test(list(h_dropped_fruit_ranked, b_fruit_ranked), c("control, far", "control, far"), c("human_dropped_fruit", "bat_fruit"))

# animal dropped fruit vs. bat fruit consumption
kendall.test(list(a_dropped_fruit_ranked, b_fruit_ranked), c("case", "case"), c("animal_dropped_fruit", "bat_fruit"))
kendall.test(list(a_dropped_fruit_ranked, b_fruit_ranked), c("control, near", "control, near"), c("animal_dropped_fruit", "bat_fruit"))
kendall.test(list(a_dropped_fruit_ranked, b_fruit_ranked), c("control, far", "control, far"), c("animal_dropped_fruit", "bat_fruit"))

# human fruit vs. animal dropped fruit consumption
kendall.test(list(human_fruit_ranked, a_dropped_fruit_ranked), c("case", "case"), c("human_fruit", "animal_dropped_fruit"))
kendall.test(list(human_fruit_ranked, a_dropped_fruit_ranked), c("control, near", "control, near"), c("human_fruit", "animal_dropped_fruit"))
kendall.test(list(human_fruit_ranked, a_dropped_fruit_ranked), c("control, far", "control, far"), c("human_fruit", "animal_dropped_fruit"))

# human fruit vs. human dropped fruit consumption
kendall.test(list(human_fruit_ranked, h_dropped_fruit_ranked), c("case", "case"), c("human_fruit", "human_dropped_fruit"))
kendall.test(list(human_fruit_ranked, h_dropped_fruit_ranked), c("control, near", "control, near"), c("human_fruit", "human_dropped_fruit"))
kendall.test(list(human_fruit_ranked, h_dropped_fruit_ranked), c("control, far", "control, far"), c("human_fruit", "human_dropped_fruit"))

# human dropped fruit vs. animal dropped fruit consumption
kendall.test(list(h_dropped_fruit_ranked, a_dropped_fruit_ranked), c("case", "case"), c("human_dropped_fruit", "animal_dropped_fruit"))
kendall.test(list(h_dropped_fruit_ranked, a_dropped_fruit_ranked), c("control, near", "control, near"), c("human_dropped_fruit", "animal_dropped_fruit"))
kendall.test(list(h_dropped_fruit_ranked, a_dropped_fruit_ranked), c("control, far", "control, far"), c("human_dropped_fruit", "animal_dropped_fruit"))

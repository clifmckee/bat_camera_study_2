# libraries
require(here)
library(tidyverse)
library(cowplot)
library(gghalves)
library(ggeffects)
library(MASS)

# function for log mean
logmean <- function(x) {
  log(mean(exp(x))) 
}

# strip labels
species.labs <- c("Non-Pteropus", "Pteropus", "Unidentified")
names(species.labs) <- c("Non Pteropus", "Pteropus", "Unidentified")

ccg.labs <- c("Case", "Control, far", "Control, near")
names(ccg.labs) <- c("case", "control, far", "control, near")

# impute missing zeroes into individual visits
visits <- all_data %>%
  group_by(case_control_group_f, recode_P_NP, dp_or_fruit, Tree_number, Tree_type) %>%
  summarize(num_visits = sum(Number_of_visits),
            num_stays = sum(Number_of_stays),
            num_conts = sum(Number_of_contaminations))

impute_zeroes <- expand.grid(
  Tree_number = unique(visits$Tree_number),
  recode_P_NP = c("Non Pteropus", "Pteropus", "Unidentified"),
  num_visits = NA,
  num_stays = NA,
  num_conts = NA
)

fixed_visits <- visits %>%
  full_join(impute_zeroes) %>%
  arrange(Tree_number) %>%
  ungroup() %>%
  fill(case_control_group_f, dp_or_fruit, Tree_type, .direction = "down") %>%
  distinct(case_control_group_f, dp_or_fruit, recode_P_NP, Tree_number, Tree_type, .keep_all = TRUE) %>%
  filter(recode_P_NP != "no visits") %>%
  replace_na(list(num_visits = 0, num_stays = 0, num_conts = 0))

# individual bat stays at trees
fixed_visits %>% 
  ggplot(aes(x = dp_or_fruit, y = num_stays)) +
  geom_half_violin(aes(fill = dp_or_fruit), side = "r", position = position_nudge(x = 0.15), draw_quantiles = c(0.25, 0.5, 0.75), show.legend = FALSE, scale = "width") +
  geom_jitter(aes(color = dp_or_fruit), pch = ".", height = 0.1, width = 0.1, show.legend = FALSE) +
  stat_summary(fun = "mean", position = position_nudge(x = 0.15), pch = 16, size = 0.25) +
  scale_x_discrete(labels = c("date palm", "fruit")) +
  scale_fill_manual(values = c("Date palm" = "#33CC99",
                               "Fruit" = "#6666CC")) +
  scale_color_manual(values = c("Date palm" = "#33CC99",
                                "Fruit" = "#6666CC")) +
  scale_y_continuous(breaks = seq(0, 200, 50), minor_breaks = seq(0, 200, 10)) +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  labs(x = "Tree type receiving bat visits", y = "Number of visits per tree") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./Results/bat_stays_per_tree.png", height = 5, width = 5, units = "in", dpi = 300, bg = "white")

# total stays
fixed_visits %>%
  group_by(case_control_group_f, dp_or_fruit, recode_P_NP) %>% 
  summarize(sum_stays = sum(num_stays)) %>% 
  ggplot(aes(x= dp_or_fruit, y = sum_stays, fill = dp_or_fruit)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  scale_x_discrete(labels = c("date palm", "fruit")) +
  scale_fill_manual(values = c("Date palm" = "#33CC99",
                               "Fruit" = "#6666CC")) +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, 200), minor_breaks = seq(0, 1100, 100)) +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  geom_text(aes(label = sum_stays), vjust = -.4, size = 3) +
  labs(x = "Tree type receiving bat visits", y = "Total visits") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./Results/total_bat_stays.png", height = 5, width = 5, units = "in", dpi = 300, bg = "white")

# total contaminations
all_data %>%
  filter(recode_P_NP %in% c("Non Pteropus", "Pteropus", "Unidentified"),
         Number_of_contaminations > 0) %>%
  group_by(case_control_group_f, dp_or_fruit, recode_P_NP) %>% 
  summarize(num_conts = sum(Number_of_contaminations)) %>% 
  ggplot(aes(x= dp_or_fruit, y = num_conts, fill = dp_or_fruit)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  scale_x_discrete(labels = c("date palm", "fruit")) +
  scale_fill_manual(values = c("Date palm" = "#33CC99",
                               "Fruit" = "#6666CC")) +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, 200), minor_breaks = seq(0, 1100, 100)) +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  geom_text(aes(label = num_conts), vjust = -.4, size = 3) +
  labs(x = "Tree type receiving bat visits", y = "Total visits with contamination of sap or fruit") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./Results/total_bat_contaminations.png", height = 5, width = 5, units = "in", dpi = 300, bg = "white")

# duration of contamination
all_data %>%
  filter(recode_P_NP %in% c("Non Pteropus", "Pteropus", "Unidentified"),
         Number_of_contaminations > 0,
         DurContT > 0) %>%
  ggplot(aes(x = dp_or_fruit, y = log(DurContT))) +
  geom_half_violin(aes(fill = dp_or_fruit), side = "r", position = position_nudge(x = 0.15), draw_quantiles = c(0.25, 0.5, 0.75), show.legend = FALSE, scale = "width") +
  geom_jitter(aes(color = dp_or_fruit), pch = ".", height = 0.1, width = 0.1, show.legend = FALSE) +
  stat_summary(fun = logmean, position = position_nudge(x = 0.15), pch = 16, size = 0.25) +
  scale_x_discrete(labels = c("date palm", "fruit")) +
  scale_fill_manual(values = c("Date palm" = "#33CC99",
                               "Fruit" = "#6666CC")) +
  scale_color_manual(values = c("Date palm" = "#33CC99",
                                "Fruit" = "#6666CC")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), minor_breaks = seq(0, 10, 1)) +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  labs(x = "Tree type receiving bat visits", y = "Duration of contamination of sap or fruit (log seconds)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./Results/contamination_duration.png", height = 5, width = 5, units = "in", dpi = 300, bg = "white")

# cumulative contamination duration
all_data %>%
  filter(recode_P_NP %in% c("Non Pteropus", "Pteropus", "Unidentified"),
         Number_of_contaminations > 0,
         DurContT > 0) %>%
  group_by(case_control_group_f, dp_or_fruit, recode_P_NP) %>% 
  summarize(cum_cont = sum(DurContT)) %>% 
  ggplot(aes(x= dp_or_fruit, y = cum_cont, fill = dp_or_fruit)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) + 
  scale_x_discrete(labels = c("date palm", "fruit")) +
  scale_fill_manual(values = c("Date palm" = "#33CC99",
                               "Fruit" = "#6666CC")) +
  scale_y_continuous(limits = c(0, 170000), breaks = seq(0, 170000, 50000), minor_breaks = seq(0, 170000, 10000)) +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  geom_text(aes(label = cum_cont), vjust = -.4, size = 3) +
  labs(x = "Tree type receiving bat visits", y = "Cumulative contamination of sap or fruit (seconds)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./Results/cumulative_contamination_duration.png", height = 5, width = 5, units = "in", dpi = 300, bg = "white")

View(fixed_visits %>%
  group_by(case_control_group_f, dp_or_fruit, recode_P_NP) %>%
  summarize(n = n(),
            n_w_visits = sum(num_stays > 0),
            percent_w_visits = round(n_w_visits/n*100, 1),
            mean_stays = round(mean(num_stays), 2),
            median_stays = median(num_stays),
            q1_stays = quantile(num_stays, 0.25),
            q3_stays = quantile(num_stays, 0.75),
            sum_stays = sum(num_stays))
)

View(all_data %>%
  filter(recode_P_NP %in% c("Non Pteropus", "Pteropus", "Unidentified"),
         Number_of_contaminations > 0,
         DurContT > 0) %>%
  group_by(case_control_group_f, dp_or_fruit, recode_P_NP) %>% 
  summarize(n = n(),
            mean_dur = round(mean(DurContT), 0),
            median_dur = median(DurContT),
            q1_dur = quantile(DurContT, 0.25),
            q3_dur = quantile(DurContT, 0.75),
            cum_cont = sum(DurContT))
)

# bat visits by tree type with visit vs. no visit
bat_fruit_tot %>%
  mutate(Tree_type = str_to_lower(Tree_type)) %>%
  group_by(case_control_group_f, Tree_type, yn_visit) %>%
  count() %>%
  ungroup() %>%
  group_by(case_control_group_f, Tree_type) %>%
  mutate(total_count = sum(n)) %>%
  arrange(-total_count) %>%
  ggplot(aes(x = reorder(Tree_type, -total_count), y = n, fill = yn_visit)) +
  geom_col(position = "stack") +
  scale_fill_manual(name = "Bat visits observed",
                     values = c("grey70", "grey30")) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, 10), minor_breaks = seq(0, 35, 5)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.justification = 0.5) +
  facet_grid(cols = vars(case_control_group_f),
             labeller = labeller(case_control_group_f = ccg.labs)) +
  labs(x = "Fruit tree species", y = "Number of trees with camera observations")
ggsave("./Results/fruit_tree_species.png", height = 5, width = 7.5, units = "in", dpi = 300, bg = "white")

# graph of which fruit trees receive bat visits
fixed_visits %>%
  mutate(Tree_type = str_to_lower(Tree_type)) %>%
  filter(dp_or_fruit == "Fruit") %>%
  filter(num_visits > 0) %>%
  group_by(case_control_group_f, Tree_type, recode_P_NP) %>%
  summarize(trees_w_visits = n_distinct(Tree_number)) %>%
  ggplot(aes(x = Tree_type, y = trees_w_visits)) +
  geom_col() +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Fruit tree species", y = "Number of trees with bat visits") +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), minor_breaks = FALSE)
ggsave("./Results/fruit_tree_species_w_visits.png", height = 5, width = 7.5, units = "in", dpi = 300, bg = "white")

# graph of which bat stays per fruit tree
fixed_visits %>%
  mutate(Tree_type = str_to_lower(Tree_type)) %>%
  filter(dp_or_fruit == "Fruit") %>%
  filter(num_stays > 0) %>%
  ggplot(aes(x = Tree_type, y = num_stays)) +
  geom_jitter(aes(color = Tree_type), size = 2, alpha = 0.7, height = 0, width = 0.2, show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  labs(x = "Fruit tree species", y = "Number of visits per tree") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20), minor_breaks = seq(0, 80, 10))
ggsave("./Results/bat_stays_at_fruit_trees.png", height = 5, width = 7.5, units = "in", dpi = 300, bg = "white")

# graph of visit duration to fruit trees
all_data %>%
  mutate(Tree_type = str_to_lower(Tree_type)) %>%
  filter(recode_P_NP %in% c("Non Pteropus", "Pteropus", "Unidentified"),
         Number_of_contaminations > 0,
         DurContT > 0,
         dp_or_fruit == "Fruit",
         Con_place == 2) %>%
  ggplot(aes(x = Tree_type, y = log(DurContT))) +
  geom_half_violin(aes(fill = Tree_type), side = "r", position = position_nudge(x = 0.15), draw_quantiles = c(0.25, 0.5, 0.75), show.legend = FALSE, scale = "width") +
  geom_jitter(aes(color = Tree_type), pch = ".", height = 0.1, width = 0.1, show.legend = FALSE) +
  stat_summary(fun = logmean, position = position_nudge(x = 0.15), pch = 16, size = 0.25) +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 2), minor_breaks = seq(0, 8, 1)) +
  facet_grid(rows = vars(case_control_group_f),
             cols = vars(recode_P_NP),
             labeller = labeller(case_control_group_f = ccg.labs,
                                 recode_P_NP = species.labs)) +
  labs(x = "Fruit tree species", y = "Duration of contamination of fruit (log seconds)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./Results/fruit_contamination_duration.png", height = 5, width = 7.5, units = "in", dpi = 300, bg = "white")

# calculate average duration of visits to banana
all_data %>%
  mutate(Tree_type = str_to_lower(Tree_type)) %>%
  filter(recode_P_NP %in% c("Non Pteropus", "Pteropus", "Unidentified"),
         Number_of_contaminations > 0,
         DurContT > 0,
         dp_or_fruit == "Fruit",
         case_control_group_f == "control, near",
         Tree_type == "banana") %>%
  group_by(recode_P_NP) %>%
  summarize(mean_dur_cont = mean(DurContT))

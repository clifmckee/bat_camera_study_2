# libraries
require(here)
library(tidyverse)
library(cowplot)
library(gghalves)
library(magrittr)

# strip labels
data.type.labs <- c("Both", "Date palm only", "Fruit tree only")
names(data.type.labs) <- c("Both", "Date palm only", "Fruit tree only")

ccg.labs <- c("Case", "Control, far", "Control, near")
names(ccg.labs) <- c("case", "control, far", "control, near")

# plot the number of villages with each tree type receiving bat visits
Fig2a <- vil.total %>%
  filter(Trees_observed %in% c("Both", "Date palm only", "Fruit tree only")) %>%
  ggplot(aes(x = yn_visits, fill = yn_visits)) +
  geom_bar(stat = "count", show.legend = FALSE) +
  facet_grid(case_control_group_f ~ Trees_observed, scales = "free_x",
             labeller = labeller(Trees_observed = data.type.labs,
                                 case_control_group_f = ccg.labs)) +
  scale_fill_manual(values = c("both" = "#CC9966",
                               "date palm" = "#33CC99",
                               "fruit" = "#6666CC", 
                               "no visits" = "black")) +
  scale_y_continuous(breaks = seq(0, 20, 5), minor_breaks = seq(0, 20, 1)) +
  labs(x = "Tree type receiving at least one bat visit", y = "Number of villages") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot the number of trees of a given tree type within each village 
tree_type_visits <- vil.total %>%
  filter(Trees_observed %in% c("Both", "Date palm only", "Fruit tree only")) %>%
  gather(dp.num_trees_w_visits, f.num_trees_w_visits, key = "tree_type", value = "count") %>%
  mutate(tree_type = case_when(tree_type == "dp.num_trees_w_visits" ~ "date palm",
                               tree_type == "f.num_trees_w_visits" ~ "fruit"))
# Replace some zeroes with NA
tree_type_visits[tree_type_visits$Trees_observed == "Date palm only" &
                   tree_type_visits$tree_type == "fruit",]$count <- NA
tree_type_visits[tree_type_visits$Trees_observed == "Fruit tree only" &
                   tree_type_visits$tree_type == "date palm",]$count <- NA
tree_type_visits %<>%
  drop_na(count)

Fig2b <- ggplot(data = tree_type_visits, aes(x = tree_type, y = count)) +
  geom_half_violin(aes(fill = tree_type), side = "r", position = position_nudge(x = 0.15), draw_quantiles = c(0.25, 0.5, 0.75), show.legend = FALSE, scale = "width") +
  geom_jitter(aes(color = tree_type), pch = ".", height = 0.1, width = 0.1, show.legend = FALSE) +
  stat_summary(fun = "mean", position = position_nudge(x = 0.15), pch = 16, size = 0.25) +
  scale_fill_manual(values = c("date palm" = "#33CC99",
                               "fruit" = "#6666CC")) +
  scale_color_manual(values = c("date palm" = "#33CC99",
                               "fruit" = "#6666CC")) +
  # scale_y_continuous(breaks = seq(0, 20, 5), minor_breaks = seq(0, 20, 1)) +
  facet_grid(case_control_group_f ~ Trees_observed, scales = "free_x",
             labeller = labeller(Trees_observed = data.type.labs,
                                 case_control_group_f = ccg.labs)) +
  labs(x = "Tree type receiving bat visits", y = "Number of individual trees with bat visits") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

plot_grid(Fig2a, Fig2b, ncol = 2, labels = c("A", "B"))
ggsave("./Results/village_visits.png", height = 7, width = 10, units = "in", dpi = 300, bg = "white")

# for villages with both tree types, do bats visit one tree type more than the other?
tree_visit_binom <-
  glm(
    cbind(count, total_trees_w_visits) ~ tree_type,
    data = tree_type_visits %>% filter(Trees_observed == "Both"),
    family = "binomial"
  )
summary(tree_visit_binom)
exp(coef(tree_visit_binom))

# bat visits by date palm table 
bat_visits_to_tree_types <- all_data %>%
  group_by(Tree_number) %>%
  summarize(num_visits = sum(Number_of_visits)) %>%
  mutate(yn_visit = case_when(num_visits > 0 ~ "yes", num_visits == 0 ~ "no")) %>%
  left_join(
    all_data %>% dplyr::select(
      Tree_number,
      dp_or_fruit,
      case_control_group_f,
      Trees_observed
    )
  ) %>%
  distinct() %>%
  group_by(Trees_observed, case_control_group_f, dp_or_fruit, yn_visit) %>%
  summarise(n = n()) %>%
  mutate(totalN = cumsum(n),
         percent = round(n / sum(n), 3))

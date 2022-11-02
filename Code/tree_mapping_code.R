# libraries
require(here)
library(tidyverse)
library(cowplot)

# bar plot of villages with each type of data collection
ccg.labs <- c("Case\n(N = 60)", "Control, far\n(N = 74)", "Control, near\n(N = 72)")
names(ccg.labs) <- c("case", "control, far", "control, near")

totals2 <- vil.total %>%
  group_by(case_control_group, Trees_observed) %>%
  count()

Fig1a <- ggplot() +
  geom_bar(data = totals2, aes(x = Trees_observed, y = n, fill = factor(case_control_group)), stat = "identity") +
  geom_text(data = totals2, aes(x = Trees_observed, y = n, label = n), nudge_y = 2, size = 3) +
  scale_x_discrete(name = "Data collection type", 
                   labels=c("Both" = "Both", 
                            "Date palm only" = "Date palm only", 
                            "Fruit tree only" = "Fruit tree only",
                            "No trees" = "No trees\n(community survey only)")) +
  scale_y_continuous(name = "Number of villages",
                     breaks = seq(0, 30, 10)) +
  theme_bw(base_size = 10) +
  facet_wrap(vars(case_control_group), nrow = 3,
             labeller = labeller(case_control_group = ccg.labs),
             strip.position = "top") +
  scale_fill_manual(name = "Village type", 
                    values = c("case" = "#E69F00",
                               "control, far" = "#56B4E9",
                               "control, near" = "#009E73")) +
  theme(legend.position = "none") +
  coord_flip()

# plot villages
Fig1b <- ggplot() +
  geom_polygon(
    data = bd0_fort,
    aes(x = long, y = lat, group = group),
    color = "grey50",
    fill = NA,
    size = 0.25
  ) +
  geom_polygon(
    data = bd1_fort,
    aes(x = long, y = lat, group = group),
    color = "grey50",
    fill = NA,
    size = 0.1
  ) +
  geom_point(data = vil.total, aes(x = Center_long, y = Center_lat, color = case_control_group, shape = Trees_observed)) +
  coord_equal() +
  theme_map(font_size = 10) +
  scale_color_manual(name = "Village type", values = c("case" = "#E69F00",
                                                       "control, far" = "#56B4E9",
                                                       "control, near" = "#009E73")) +
  scale_shape_manual(name = "Data collection type", values = c(15, 1, 2, 4)) +
  labs(x = "Longitude", y = "Latitude")

plot_grid(Fig1a, Fig1b, ncol = 2, rel_widths = c(0.33, 0.67))
ggsave("./Results/tree_map.png", height = 7, width = 10, units = "in", dpi = 300, bg = "white")

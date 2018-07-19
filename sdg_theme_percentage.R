# calculate proportion of total distance for each theme to SDG
library(ggplot2)
library(tidytext)
library(tidyverse)

matched <- read_csv("themes_goals_matched.csv") %>%
  mutate(goal = factor(goal, levels = str_c("Goal", 1:17, sep = " "), ordered = TRUE)) %>%
  mutate(theme = factor(theme, levels = str_c("Theme", 1:160, sep = " "), ordered = TRUE)) %>%
  arrange(goal, theme)

# find total score/distance by SDG and then find each theme's percentage of that total distance
matched_percentages <- matched %>%
  filter(questionable == 0) %>%
  group_by(goal) %>%
  mutate(sdist = sum(score)) %>%
  ungroup %>%
  mutate(percentage = round(score/sdist, 4)) %>%
  arrange(goal, desc(percentage)) %>%
  rename(distance = score, total_dist = sdist) %>%
  select(-order, -questionable)

# make a plot to visualize the SDGs and themes frequency
p <- ggplot(matched_percentages, aes(goal)) + 
  geom_bar(color = "black", fill = "#56B4E9") +
  ggtitle("SDGs and Matched Themes")  +
  theme_classic() +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("SDG") +
  ylab("Theme Count")

ggsave("SDGs_themes.png", p, height = 9, width = 8, units = "in")
saveRDS(matched_percentages, "SDGs_themes_percentages.rds")
write_csv(matched_percentages, "SDGs_themes_percentages.csv")







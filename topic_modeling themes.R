purrr::map(c("dplyr", "readr", "stringr", "ggplot2", "purrr",
             "tidyr", "tidytext", "tm", "topicmodels"), library, character.only = TRUE)

# load themes data, keeping only rows that are about themes
themes <- read_csv("themes_sdgs.csv") %>%
  filter(grepl("(Theme)", number))

# make a count of unique words in each theme, removing stop words
themes_words <- themes %>%
  unite(theme, number, title, sep = "_") %>%
  unnest_tokens(word, description)

word_counts <- themes_words %>%
  anti_join(stop_words) %>%
  count(theme, word, sort = TRUE)

# now make a DTM so we can do LDA later
themes_dtm <- word_counts %>%
  cast_dtm(theme, word, n)

# use LDA to create a 17 topic model
themes_lda <- LDA(themes_dtm, k = 17, control = list(seed = 1035))

# return the lda topic model to a tidy format
themes_lda_tdy <- tidy(themes_lda)

# find the top 5 terms in each topic
top_terms <- themes_lda_tdy %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# visualize top terms by topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free") +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))

# make a document-per-topic-per-row tidy data frame
themes_lda_gamma <- tidy(themes_lda, matrix = "gamma")
themes_lda_gamma

themes_lda_gamma <- themes_lda_gamma %>%
  separate(document, c("theme", "title"), sep = "_", convert = TRUE) %>%
  mutate(theme_no = gsub("(Theme )", "", theme)) %>%
  mutate(theme_no = as.numeric(theme_no))

make_topic_plot <- function(number = NULL) {
  topic_themes_lda_gamma <- themes_lda_gamma %>% filter(theme_no == number)
  
  plot_title <- unique(str_c(topic_themes_lda_gamma$theme, topic_themes_lda_gamma$title, sep = " "))
  
  ggplot(topic_themes_lda_gamma, aes(gamma, fill = factor(topic))) +
    geom_histogram() +
    ggtitle(label = plot_title)
}

for(i in 1:160) {
  #ggsave(str_c("plots/theme_", i, sep = "_"), make_topic_plot(i), device = "png")
}



themes_classifications <- themes_lda_gamma %>%
  group_by(theme) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(topic) %>%
  left_join(themes[,c("number", "description")], by = c("theme"="number")) %>%
  select(theme, title, description, topic)

# we can see which themes go into which topics, now we'll have to decide which topics match with which SDG
themes_classifications %>% 
  filter(topic == 12) %>% 
  View

themes_lda_gamma %>%
  filter(theme == 'Theme 9')

#write_csv(themes_classifications, "themes_topics.csv")

# see how are results compared to doing it manually
sdg_themes_manual <- read_csv("themes_topics_manual.csv")

SDG_titles <- read_csv("SDG_sectors.csv") %>% 
  slice(1:17) %>% 
  set_names("goal", "sdg_title", "description") %>% 
  mutate(sdg = gsub("(Goal )", "", goal))  %>% 
  mutate(sdg = as.integer(sdg)) %>%
  select(sdg, sdg_title, goal)

SDG_themes <- sdg_themes_manual %>% 
  left_join(SDG_titles, by = "sdg") %>% 
  arrange(sdg, theme) %>%
  select(goal, sdg_title, theme, title)

write_csv(SDG_themes, "sdg_themes_matched.csv")


plyr::dlply(sdg_manual, "topic", function(x) length(unique(x[,"sdg"])))















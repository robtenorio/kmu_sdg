library(magrittr)
library(stringr)
library(textstem)
library(tidytext)
library(tidyverse)
library(text2vec)
library(zeallot)

df <- read_csv("themes_sdgs_cat.csv")
sdgs <- df %>% slice(1:17) %>% mutate(number = gsub("Goal ", "", number))
manually_matched_df <- read_csv("sdg_themes_matched.csv") %>% rename(manual_goal_title = sdg_title, 
                                                                     theme_title = title) %>%
  select(theme_title, manual_goal_title)

goals_title <- df %>% slice(1:17) %>% use_series(title)
themes_title <- df %>% slice(18:nrow(.)) %>% use_series(title)
goals_description <- df %>% slice(1:17) %>% use_series(description)
themes_description <- df %>% slice(18:nrow(.)) %>% use_series(description)

stop_words <- unique(get_stopwords()[["word"]])

prep_fun <- function(x) {
  x <- x %>% str_to_lower %>% 
    str_replace_all("[^[:alnum:]]", " ") %>% # replace punctuation
    str_replace_all("[[:digit:]]", " ") %>% # replace numbers
    str_replace_all("\\s+", " ") %>% # replace white space that's not a single space
    unique %>%
    na.omit
  
  stops_lemma <- function(x) {
    split_x <- as.vector(str_split(x, " ", simplify = TRUE))
    stop_x <- split_x[!split_x %in% stop_words]
    lemma_x <- lemmatize_words(stop_x)
    str_c(lemma_x, collapse = " ")
  }
  
map(x, stops_lemma) %>% unlist %>% str_trim
  
}

all_terms <- stack(df)[["values"]] %>% prep_fun

tok_fun <- word_tokenizer

it_all <- itoken(all_terms,
             tokenizer = tok_fun)

vocab <- create_vocabulary(it_all)

vectorizer <- vocab_vectorizer(vocab)

### prepare vectors of documents

goals <- prep_fun(goals_description)
themes <- prep_fun(themes_description)

tf_idf_fun <- function(x) {
  
  it <- itoken(x, tokenizer = tok_fun)
  
  dtm <- create_dtm(it, vectorizer)
  
  tfidf <- TfIdf$new()
  
  dtm_tfidf <- fit_transform(dtm, tfidf)
  
}

goals_tf_idf <- tf_idf_fun(goals)
themes_tf_idf <- tf_idf_fun(themes)

### Now find cosine similarities between indicators and sectors/themes
themes_goals <- sim2(themes_tf_idf, goals_tf_idf, 
                          method = "cosine")

themes_goals_tidy <- themes_goals %>%
  tidy %>%
  mutate(row = as.numeric(row)) %>%
  mutate(column = as.numeric(column))

top_goals_tidy <- function(x, theme_no, places, score = FALSE) {
  x %>% 
    filter(row == theme_no) %>%
    top_n(n = min(places, nrow(.))) %>% # some themes are missing a goal match because of the sparse nature of the matrix
    arrange(desc(value)) %>%
    magrittr::use_series(column) %->% col_result
  
  result <- tibble(theme_title = themes_title[theme_no], 
                   theme_description = themes_description[theme_no],
                   goal_title = goals_title[col_result], 
                   goal_description = goals_description[col_result],
                   theme_number = theme_no,
                   goal_number = col_result,
                   order = 1:min(places, length(col_result)))
  
  if(score == TRUE) {
    # get scores
    x %>% 
      filter(row == theme_no) %>%
      top_n(n = places) %>%
      arrange(desc(value)) %>%
      magrittr::use_series(value) %->% value_result
    
    result$score <- value_result
    
    # find outliers that are 1.5*IQR+75th percentile
    third_q <- quantile(value_result)[4]
    
    inner_fence <- IQR(value_result)*1.5 + third_q
    
    result_outliers <- result %>% filter(score >= inner_fence)
    
    if(nrow(result_outliers) == 0) { # if there are no outliers, pick the top 3 
      final_result <- result[1:3,] # -- another algorithm could be to change the multiplier of the third quantile until you get an outlier
    } else {
      final_result <- result_outliers
    }
  }
  
  return(final_result)
  
}

matched_themes_goals <- tibble()

for(i in 1:length(themes)) {
  result <- top_goals_tidy(x = themes_goals_tidy, theme_no = i, places = 17, score = TRUE)
  matched_themes_goals <- rbind(matched_themes_goals, result)
}

matched_auto_manual <- left_join(matched_themes_goals, manually_matched_df)

# what is the distribution of the number of goals for each theme?
matched_auto_manual %>%
  group_by(theme_number) %>%
  count() %>%
  group_by(n) %>%
  count()

# what percentage of first place goals were correct?
matched_auto_manual %>%
  filter(order == 1) %>%
  filter(goal_title == manual_goal_title) %>%
  nrow(.)/160*100 # 49.4% of the algorithmically matched goals were the same as the manually matched

matched_1 <- matched_auto_manual %>%
  filter(order == 1) %>%
  filter(goal_title == manual_goal_title) %>%
  use_series(theme_number)

matched_df <- matched_auto_manual %>%
  filter(theme_number %in% matched_1)

# reorder rows where the match is second place and third place respectively
matched_2 <- matched_auto_manual %>%
  filter(order == 2) %>%
  filter(goal_title == manual_goal_title) %>%
  use_series(theme_number)

matched_3 <- matched_auto_manual %>%
  filter(order == 3) %>%
  filter(goal_title == manual_goal_title) %>%
  use_series(theme_number)

# make new data frames of the themes where the manually matched goal are 2 and three, respectively
matched_auto_manual_new_order_2 <- matched_auto_manual %>%
  filter(theme_number %in% matched_2) %>%
  mutate(order = case_when(
    order == 2 ~ 1,
    order == 1 ~ 2,
    order == 3 ~ 3
  )) %>%
  arrange(theme_number, order)

matched_auto_manual_new_order_3 <- matched_auto_manual %>%
  filter(theme_number %in% matched_3) %>%
  mutate(order = case_when(
    order == 3 ~ 1,
    order == 1 ~ 2,
    order == 2 ~ 3
  )) %>%
  arrange(theme_number, order)

# make a data frame of the themes where the manually matched goal is not 
# any of the three auto matched goals
not_matched_any <- matched_auto_manual %>%
  filter(!theme_number %in% matched_1) %>%
  filter(!theme_number %in% matched_2) %>%
  filter(!theme_number %in% matched_3)

matched_auto_manual_new_order_none <- not_matched_any %>%
  group_by(theme_number) %>%
  mutate(max_order = max(order)) %>%
  ungroup() %>%
  mutate(order = case_when(
    max_order == 1 & order == 1 ~ max_order,
    max_order == 2 & order == 1 ~ 2,
    max_order == 3 & order == 1 ~ 2,
    max_order == 3 & order == 2 ~ 3,
    order == max_order ~ 1
  )) %>% 
  mutate(goal_title = case_when(
    order != 1 ~ goal_title,
    order == 1 ~ manual_goal_title
  )) %>%
  select(-goal_description, -goal_number, -max_order) %>% # we need the manually corrected goal number and goal description, which isn't included in the df yet
  left_join(sdgs, by = c("goal_title" = "title")) %>%
  rename(goal_number = number, goal_description = description) %>%
  arrange(theme_number, order)

# bind the new order data frames to the matched data frame
matched_auto_manual_new_order <- matched_df %>%
  rbind(matched_auto_manual_new_order_none) %>%
  rbind(matched_auto_manual_new_order_2) %>%
  rbind(matched_auto_manual_new_order_3)

# what is the distribution of the number of goals for each theme?
matched_auto_manual_new_order %>%
  group_by(theme_number) %>%
  count() %>%
  group_by(n) %>%
  count()

matched_auto_manual_new_order %>%
  filter(order == 1) %>%
  filter(goal_title == manual_goal_title) %>%
  nrow(.)/160*100 # now 100% of the goals in order 1 are the manually selected goals

matched <- matched_auto_manual_new_order %>%
  arrange(theme_number, order) %>% 
  mutate(theme = str_c("Theme ", theme_number)) %>%
  mutate(goal = str_c("Goal ", goal_number)) %>%
  select(theme, theme_title, goal, goal_title, theme_description, goal_description, order, score)

# these are questionable goal-theme pairings based on a manual review
questionable <- tibble(theme = c(3, 16, 22, 36, 36, 37, 38, 55, 65, 66, 69, 79, 82, 99, 109, 109),
                  goal = c(5, 15, 15, 16, 6, 15, 10, 13, 5, 5, 11, 5, 2, 9, 12, 13)) %>%
  mutate(theme = str_c("Theme ", theme, sep = "")) %>%
  mutate(goal = str_c("Goal ", goal, sep = "")) %>%
  mutate(questionable = 1)

# keep just the questionable pairs
matched_questionable <- matched %>%
  right_join(questionable) %>%
  select(-questionable)

final_matched <- matched %>%
  left_join(questionable) %>%
  mutate(questionable = case_when(is.na(questionable) ~ 0,
                                  !is.na(questionable) ~ 1))

write_csv(final_matched, "themes_goals_matched.csv")
write_csv(matched_questionable, "themes_goals_questionable.csv")

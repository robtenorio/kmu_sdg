library(pdftools)
library(tidytext)
library(tidyverse)

pdf_data <- pdf_text("themes_master.pdf")

pdf_bag <- str_c(pdf_data, collapse = " ")

theme_pattern <- "(\\([[:digit:]]{2,3}\\))"

toc_pattern <- "([[:digit:]]{2,3})([^[:digit:]]*)([[:digit:]]{1,3})"
definition_pattern <- "(definition:|Definition:)"
activity_pattern <- "(typical[[:blank:]]*examples)"
activity_pattern_greedy <- "(coded\\s*activities:)"

# get definitions
descriptions_df <- pdf_bag %>% tolower %>% data_frame %>% set_names("txt") %>%
  unnest_tokens(txt, txt, token = "regex", pattern = definition_pattern) %>%
  mutate(description = gsub(str_c(activity_pattern, ".*"), "", txt)) %>% 
  mutate(description = gsub(theme_pattern, "", description)) %>%
  slice(2:nrow(.))

#  mutate(description = gsub("(examples of excluded activities:).*", "", txt)) %>%

themes <- str_extract_all(pdf_data[2:3], toc_pattern, simplify = FALSE) %>% unlist %>% data_frame %>%
  set_names("theme") %>% mutate(code_page = gsub(toc_pattern, "\\1:\\3", theme)) %>%
  separate(code_page, c("code", "page"), ":") %>% mutate(theme = gsub(toc_pattern, "\\2", theme)) %>%
  mutate(code = as.numeric(code)) %>%
  mutate(code = case_when(code < 100 ~ code*10,
                          code >= 100 ~ code)) %>% # add 0 to the end of code so it ends up in the correct order
  mutate(page = as.numeric(page)) %>%
  mutate(theme = trimws(theme))

# add some themes that didn't match
themes <- themes %>% 
  mutate(theme = replace(theme, code == 311, "Financial Sector Oversight and Policy/Banking Regulation & Restructuring")) %>%
  mutate(theme = replace(theme, code == 800, "Environment and Natural Resource Management")) %>%
  filter(code != 201) %>%
  dplyr::arrange(page, code) # arrange so the order is correct

# remove some top-level themes that don't have descriptions
top_themes <- tibble(theme = c("ECONOMIC POLICY", "PRIVATE SECTOR DEVELOPMENT", "FINANCE",
                "PUBLIC SECTOR MANAGEMENT", "SOCIAL DEVELOPMENT AND PROTECTION",
                "HUMAN DEVELOPMENT AND GENDER", "URBAN AND RURAL DEVELOPMENT",
                "Environment and Natural Resource Management"))

themes_final <- themes %>% anti_join(top_themes)

themes_descriptions <- tibble(number = str_c("Theme", 1:nrow(themes_final), sep = " "), title = themes_final$theme, description = descriptions_df$description)  

SDG_titles <- read_csv("SDG_sectors.csv") %>% slice(1:17) %>% set_names("number", "title", "description")
#SDG_indicators <- read_csv("SDG_indicators.csv") %>% set_names("number", "description") %>%
#  left_join(SDG_titles)

themes_sdgs <- rbind(SDG_titles, themes_descriptions) %>% select(number, title, description)

write_csv(themes_sdgs, "themes_sdgs_cat.csv")






  

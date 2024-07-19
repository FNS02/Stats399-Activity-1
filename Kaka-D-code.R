library(tidyverse)

data <- read_csv("sentiment-survey-data.csv")
names(data) <- c("four_words", "majority", "instructor")

# separate the 4 words
data <- data %>% separate_rows(four_words, sep = " , ") %>%
  separate_rows(four_words, sep = ", ") %>%
  separate_rows(four_words, sep = ",") %>%
  separate_rows(four_words, sep = "，")

# clean the four_words column (lower cases and take out punctuation and digit)
data[1] <- lapply(data[1], function(word) {
  str_to_lower(word) %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
  })

# take out rows with more than one word in four_words
data <- data %>% filter(!str_count(four_words, "\\S+") > 1) %>%
  filter(!nchar(four_words) == 0)

#clean the other other two rows
clean_row <- function(word) {
  str_to_lower(word) %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
}

for (col_name in names(data)) {
  data[col_name] <- lapply(data[col_name], function(word) clean_row(word))
  data <- data %>% filter(!str_count(col_name, "\\S+") > 1) %>%
    filter(!nchar(col_name) == 0)
}




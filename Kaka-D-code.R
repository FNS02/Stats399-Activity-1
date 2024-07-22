library(tidyverse)
# Use the following install call only if you haven't installed these packages
## install.packages("ggwordcloud")
## install.packages("wordcloud2")
library(wordcloud2)
library(ggwordcloud)

# Read and prepare data
data <- read_csv("sentiment-survey-data.csv")
names(data) <- c("four_words", "majority", "instructor")
data <- data %>% mutate(id = row_number())

# clean_col function tranform the four seperated words into lower case and take out punctuations and numbers
clean_col <- function(word) {
  str_to_lower(word) %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
}

# separate_and_clean function seperates one four_words cell into 4 seperated row and clean, and take out invalid format of response
separate_and_clean <- function(data, column) {
  data %>%
    separate_rows({{column}}, sep = " , |, |,|，") %>%
    mutate({{column}} := clean_col({{column}})) %>%
    filter(!str_count({{column}}, "\\S+") > 1 & nchar({{column}}) > 0)
}

# join_sentiment function join the words column with sentiment_list, and give "column_sentiment" accordingly
join_sentiment <- function(data, column, sentiment_list) {
  data %>%
    inner_join(sentiment_list, by = setNames("word", column)) %>%
    rename_with(~ paste0(column, "_sentiment"), sentiment)
}

# Apply separate_and_clean to four_words
data <- separate_and_clean(data, four_words)

# Count frequency of words in four_words
data <- data %>%
  group_by(four_words) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  distinct(four_words, .keep_all = TRUE)

# Remove repeated words
data <- data %>%
  filter(!duplicated(four_words))

# Read the sentiment list
sentiment_list <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6jVuO3F3DNwX1WApTvCfYqfjehcNKHmuDqupk2_0vJe0lnf81dmUlsXZGkZKmaCeallS5Dqch05ks/pub?gid=422750759&single=true&output=csv")

# Apply join_sentiment for four_words
data <- join_sentiment(data, "four_words", sentiment_list)

# Export the cleaned four words sentiment data as csv
write_csv(data, "cleaned_sentiment_data.csv")

# Read the saved cleaned sentiment data csv, and select the variables to be used
wordcloud_data <- read_csv("cleaned_sentiment_data.csv") %>% 
  select(four_words, frequency, four_words_sentiment)


# ggplot version of the word cloud with different colours for negative/positive
ggplot(wordcloud_data, aes(label = four_words, size = frequency, color = four_words_sentiment,
                           fontface = "bold")) +
  geom_text_wordcloud(shape = "square", show.legend = TRUE) +
  scale_size_area(max_size = 60)+
  scale_color_manual(values = c("positive" = "chartreuse2", "negative" = "red"),
                     labels = c("positive", "Negative"), 
                     name = "Sentiment Among Students") +
  labs(title = "Sentiment Word Cloud",
       subtitle = "A proportional visualization of respondents sentiment about the Stats 399 course based on four descriptive words",
       caption = "Data source: Stats399 Students") +
  theme(
    panel.background = element_rect(fill = 'aliceblue'),
    plot.background = element_rect(fill = "aliceblue"),
    panel.border = element_rect(colour = "white", fill=NA, size = 3),
    legend.background = element_rect( fill= "aliceblue"),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12),
    legend.position = c(0.5, 0.1)
    
  ) +
  guides(size = "none",
         color = guide_legend(nrow = 1))

# Export the plot with wider dimension to contain all words
ggsave("sentiment_wordcloud.png", plot = last_plot(), width = 15, height = 10, dpi = 480)

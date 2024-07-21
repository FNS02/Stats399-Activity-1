library(tidyverse)
install.packages("ggwordcloud")
install.packages("wordcloud2")
library(wordcloud2)
library(ggwordcloud)

data <- read_csv("sentiment-survey-data.csv")
names(data) <- c("four_words", "majority", "instructor")
data <- data %>% mutate(id = row_number())



# separate the 4 words
data <- data %>% separate_rows(four_words, sep = " , ") %>%
  separate_rows(four_words, sep = ", ") %>%
  separate_rows(four_words, sep = ",") %>%
  separate_rows(four_words, sep = "，")

# clean the four_words column (lower cases and take out punctuation and digit)
clean_col <- function(word) {
  str_to_lower(word) %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
}

data["four_words"] <- lapply(data["four_words"], function(word) clean_col(word))

# take out rows with more than one word in four_words
data <- data %>% filter(!str_count(four_words, "\\S+") > 1) %>%
  filter(!nchar(four_words) == 0)

# Create a column that counts the frequency of words in the four_words column
data <- data %>% 
  group_by(four_words) %>%
  mutate(frequency = n()) %>%
  ungroup()

# clean the second row
data["majority"] <- lapply(data["majority"], function(word) clean_col(word))
data <- data %>% filter(!str_count(majority, "\\S+") > 1) %>%
  filter(!nchar(majority) == 0)

# clean the third row
data["instructor"] <- lapply(data["instructor"], function(word) clean_col(word))
data <- data %>% filter(!str_count(instructor, "\\S+") > 1) %>%
  filter(!nchar(majority) == 0)

# Take out repeated words
used_words <- data$four_words[1]
repeated <- FALSE
for (i in 2:length(data$four_words)) {
  if(data$four_words[i] %in% used_words) {
    repeated <- c(repeated, TRUE)} 
  else {
    repeated <- c(repeated, FALSE)
    used_words <- c(used_words, data$four_words[i])
  }
}
data$repeated_word <- repeated
non_repeated_words <- data %>% filter(!repeated_word)

# Read the sentiment list
sentiment_list <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6jVuO3F3DNwX1WApTvCfYqfjehcNKHmuDqupk2_0vJe0lnf81dmUlsXZGkZKmaCeallS5Dqch05ks/pub?gid=422750759&single=true&output=csv")

#Put id col in front
non_repeated_words <- non_repeated_words %>% relocate(id) 
non_repeated_words <- non_repeated_words %>% relocate(frequency, .after = four_words)

# Join four_words with sentiment list
non_repeated_words <- inner_join(non_repeated_words, sentiment_list, by = c("four_words" = "word"))
non_repeated_words <- non_repeated_words %>% relocate(sentiment, .after = four_words)
non_repeated_words <- non_repeated_words %>% rename(four_word_sentiment = 3)
four_words_data <- non_repeated_words %>% select(four_words, frequency, four_word_sentiment) %>% arrange(desc(frequency))

# Join majority with sentiment list
non_repeated_words <- inner_join(non_repeated_words, sentiment_list, by = c("majority" = "word")) #Join majority
non_repeated_words <- non_repeated_words %>% relocate(sentiment, .after = majority)
non_repeated_words <- non_repeated_words %>% rename(majority_sentiment = 6)

# Join instructor with sentiment list
non_repeated_words <- inner_join(non_repeated_words, sentiment_list, by = c("instructor" = "word")) #Join instructor
non_repeated_words <- non_repeated_words %>% relocate(sentiment, .after = instructor)
non_repeated_words <- non_repeated_words %>% rename(finstructor_sentiment = 8)

#Take out the repeated_words col
non_repeated_words <- non_repeated_words[1:8] 
non_repeated_words <- non_repeated_words %>% arrange(desc(frequency))

# Plot the graph with the four_words_data solely
wordcloud2(four_words_data, size = 2, color = "random-light", backgroundColor = "grey")




#################################################################################
# Apply logarithmic transformation to frequencies
wordcloud_data <- four_words_data %>% 
  mutate(log_freq = log1p(frequency))

# Select the necessary columns for the word cloud
wordcloud_data <- wordcloud_data %>% 
  select(four_words, log_freq, four_word_sentiment)

# Generate the word cloud
wordcloud2(wordcloud_data, size = 0.5, color = 'random-light', backgroundColor = "maroon", shape = 'circle')
#################################################################################

# ggplot version of the word cloud with different colours for negative/positive
## Edit the theme and code if you want something different, I don't mind. 
### Also feels like we need another graph to supplement this lol.

ggplot(wordcloud_data, aes(label = four_words, size = frequency, color = four_word_sentiment,
                           fontface = "bold")) +
  geom_text_wordcloud(shape = "square", show.legend = TRUE) +
  scale_size_area(max_size = 30)+
  scale_color_manual(values = c("positive" = "chartreuse2", "negative" = "red"),
    labels = c("positive", "Negative"), 
    name = "Sentiment Among Students") +
  labs(title = "Sentiment Word Cloud",
    subtitle = "A visualisation of the sentiment of respondents to four-word phrases ",
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








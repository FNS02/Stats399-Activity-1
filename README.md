<h1 align="center">
  <br>
  <b>Stats399: Activity 1</b>
  <br>
</h1>

<h4 align="center">This repo holds the R-Code files and graphics that pertain to the Stats399 Activity 1 in 2024 from the University of Auckland</h4>

<h4 align="center">This activity focuses on building a sentiment analysis from a set of data generated by the students of the University of Auckland Statistics Capstone Course (Stats399).</h4>

<p align="center">
  <a href="#Task-Outline">Task Outline</a> •
  <a href="#Instructions">Instructions</a> •
  <a href="#Image">Image</a> •
  <a href="#Report-Summary">Report Summary</a>
</p>

<body style="background-color:#F0F8FF;">



## Task Outline
With your team, submit the following items by your notified deadline.

An informative, attractive **graphic** that opens with standard software and displays on a single screen (e.g. PDF, PNG, JPG). The **CSV of cleaned data** you used; a **code file** (probably an R file); and a **README file** (a text document with reproducible instructions)

Name all files with your team-name: e.g. Kaka-A-Graphic.pdf, Kaka-A-CleanData.csv, Kaka-A-Code.R, Kaka-A-README.txt.

You should spend a total of 6 to 7 hours outside of class on this activity. You can choose how to divide this time between team meetings and individual contributions.

You can use as much or as little of the data as you like: you don't have to include everything in your graphic.


## What we need to submit
Code FIle (R Code)\
Text file (README file) with step bystep instructions for how to run the code\
Cleaned-up CSV file your code runs with\
Clear and reproducible instructions in Markdown\
Other teams should be able to recreate your graphic using your instructions in 5 minutes


### Role Delegation

Coding: Derek

Cleaning the data and interpretation: Lingyu & Mike

ReadME: Dominic, Mike

Graphics: Derek, Dominic


# Instructions

### 1. Load the Required Libraries

Load the three libraries requiredfor data manipulation and generating the graphic. The `tidyverse` library will be used for data manipulation, `ggplot2` is library for creating graphics for our data, and `ggwordcloud` is a third-party library that can be used in conjunction with ggplot to  to create word clouds. If you haven't installed the `ggwordcloud`, remove the hashtags to install the package.

```r
library(tidyverse)
# Use the following install call only if you haven't installed these packages
# install.packages("ggwordcloud")
library(ggwordcloud)
```
Note:`ggwordcloud` may provide a warning message when loaded. This warning can be ignored.

### 2. Read and Organise the Data

Read and create a dataframe for the the sentiment survey CSV file. Assign consise column names to the dataset and create an `id` column to give each row a unique number.

```r
data <- read_csv("sentiment-survey-data.csv")
names(data) <- c("four_words", "majority", "instructor")
data <- data %>% mutate(id = row_number())
```


#### 3. Create Functions to Clean Data

Create a function to convert all words to lowercase and remove words with punctuation and numbers. 

```r
# clean_col function transforms the words into lowercase and removes punctuation and digits
clean_col <- function(word) {
  str_to_lower(word) %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]")
}
```

Create a function which will separate the words in a column so each word will appear in a separate row.
```r
# separate_and_clean function separates words in a column and cleans them
separate_and_clean <- function(data, column) {
  data %>%
    separate_rows({{column}}, sep = " , |, |,|，") %>%
    mutate({{column}} := clean_col({{column}})) %>%
    filter(!str_count({{column}}, "\\S+") > 1 & nchar({{column}}) > 0)
}
```

Lastly, create another function to join the words with a sentiment list to the dataframe
```r
# join_sentiment function joins the words column with a sentiment list
join_sentiment <- function(data, column, sentiment_list) {
  data %>%
    inner_join(sentiment_list, by = setNames("word", column)) %>%
    rename_with(~ paste0(column, "_sentiment"), sentiment)
}
```

#### 4. Clean the `four_words` Column

Apply the `separate_and_clean` function to the `four_words` column.

```r
data <- separate_and_clean(data, four_words)
```

#### 5. Count Word Frequency

Count the frequency of words in the `four_words` column. Afterwards remove any duplicates words leaving only unique words in the `four_words` column.

```r
data <- data %>%
  group_by(four_words) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  distinct(four_words, .keep_all = TRUE)
```


### 6. Read the Sentiment List

Read the sentiment list from the provided Google Sheets. This list contains words and their associated sentiments, which will be used to analyse the sentiment of the words from Stats399 Students

```r
sentiment_list <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6jVuO3F3DNwX1WApTvCfYqfjehcNKHmuDqupk2_0vJe0lnf81dmUlsXZGkZKmaCeallS5Dqch05ks/pub?gid=422750759&single=true&output=csv")
```

#### 7. Join with Sentiment List

Use the previously created `join_sentiment` function on the `four_words` column to add sentiment information for each word.

```r
data <- join_sentiment(data, "four_words", sentiment_list)
```

#### 8. Export the Cleaned Data

Export the cleaned data with sentiment information to a new CSV file.

```r
write_csv(data, "cleaned_sentiment_data.csv")
```

#### 9. Select Data for Word Cloud

Read the saved cleaned sentiment data CSV and select the `four_words`, `frequency` and `four_word_sentiment` to build the word cloud.

```r
wordcloud_data <- read_csv("cleaned_sentiment_data.csv") %>% 
  select(four_words, frequency, four_words_sentiment)
```

#### 10. Create Word Cloud Using `ggplot2`

Create a word cloud using `ggplot2` and `ggwordcloud` with different colors for positive and negative sentiments .

```r
ggplot(wordcloud_data, aes(label = four_words, size = frequency, color = four_words_sentiment,
                           fontface = "bold")) +
  geom_text_wordcloud(shape = "square", show.legend = TRUE) +
  scale_size_area(max_size = 60)+
  scale_color_manual(values = c("positive" = "chartreuse2", "negative" = "red"),
                     labels = c("positive", "Negative"), 
                     name = "Sentiment Among Students") +
  labs(title = "Sentiment Word Cloud",
       subtitle = "A proportional visualization of respondents sentiment about the Stats 399 course based on each respondents four descriptive words",
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
```
**Note**: Depending on the resolution of the screen being used, you may see an multiple warning messages regarding the size of the plot. These can be safely ignored as they have no effect on the final output detailed in Step 11. 


#### 11. Save the Plot

Export the plot. This plot will save in the location of the working directory. The exported image may appear differently to the preview given in Step 10, which is expected.

```r
ggsave("sentiment_wordcloud.png", plot = last_plot(), width = 15, height = 10, dpi = 480)
```


## Image
![](https://raw.githubusercontent.com/FNS02/Stats399-Activity-1/main/Sentiment-Word-Cloud-v1.1.png)

### Image Interpretation



## Report Summary 

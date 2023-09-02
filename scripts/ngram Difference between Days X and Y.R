# Load Libraries
library("ggplot2")
library("tidyverse")
library("dplyr")
# For the %>% operator, but you can
# skip loading tidyverse 
# and just use `|>` as 
# a pipe operator.


# Note: Other code is below to do an alternative method,
#       but the uncommented method is superior.

#load CSVs using code. 
df1 <- read.csv("~/Documents/Stats/4ChanScraper/ngram Aug 31 2023 10:49:52.csv")
df2 <- read.csv("~/Documents/Stats/4ChanScraper/ngram Aug 31 2023 12:08:28.csv")


# Merge data frame, and take difference b/w day 1 and day 2
# subtracting data frames from each other.
# n.x = df1
# n.y = df2
df_merged <- merge(df1, df2, by="word", all=TRUE)
# For ngram below
df_merged$result <- df_merged$n.y - df_merged$n.x


# Feel free to add more "non-words," or "noise" 
# to this list as you see fit.
df_difference_filter <- df_merged %>% 
  filter(!word == "de"
         & !word == "je"
         & !word == "een"
         & !word == "dat"
         & !word == "en"
         & !word == "eu"
         & !word == "te"
         & !word == "tu"
         & !word == "niet"
         & !word == "van"
         & !word == "niet"
         & !word == "ik"
         & !word == "ze"
         & !word == "om"
         & !word == "met"
         & !word == "uk"
         & !word == "qt"
         & !word == "wat"
         & !word == "bb"
         & !word == "op"
         & !word == "ne"
         & !word == "rh")

# assign NA to Zero
df_difference_filter$result[is.na(df_difference_filter$result)] <- 0


# Get bottom 20 (negative) numbers
df_bottom <- df_difference_filter %>% 
  top_n(-20)

# Get top 20 (positive) numbers
df_top <- df_difference_filter %>% 
  top_n(20)

# Bind into new data frame
df_merged2 <- rbind(df_top, df_bottom)

# Gets rid of pair-word noise.
# Add as needed.
df_merged2 <- df_merged2 %>% 
  filter(!word == "niggers niggers"
         & !word == "nigger nigger"
         & !word == "based based"
         & !word == "jew jew"
         & !word == "niggers niggers")

# Colours results based on conditions.
# ngram_fill_bar <- case_when(
#   df_merged2$result < -90   ~ "red4",
#   df_merged2$result <= -70 ~ "red1",
#   df_merged2$result <= -30 ~ "darkgreen",
#   df_merged2$result <= -10 ~ "seagreen",
#   df_merged2$result <= 0 ~ "black",
#   df_merged2$result <= 10 ~ "slategray4",
#   df_merged2$result <= 30 ~ "slateblue",
#   df_merged2$result <= 70 ~ "steelblue3",
#   df_merged2$result <= 90 ~ "cyan4")


# bar graph of difference between Day 2, and Day 1.
df_merged2 %>% 
  top_n(50) %>% 
  mutate(word = reorder(word, result)) %>%
  ggplot(aes(word, result, fill = result)) +
  theme(legend.position = "left") +
  geom_bar(stat = "identity") +
  labs(
    title = "Difference of Word Pairs from Today - Yesterday",
    x = "Words",
    y = "Count",
    caption = "Positive integers = More mentions today. 
    Negative integers = Less mentions today.",
  fill = "Results") +
  coord_flip() +
  theme_dark(base_size = 13)

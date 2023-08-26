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
df1 <- read.csv("~/Documents/Stats/4Chan Scraper/Aug 24 2023 18:11:19.csv")
df2 <- read.csv("~/Documents/Stats/4Chan Scraper/Aug 25 2023 10:51:42.csv")


# Merge data frame, and take difference b/w day 1 and day 2
# subtracting data frames from each other.
# n.x = df1
# n.y = df2
df_merged <- merge(df1, df2, by="word", all=TRUE)
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
         & !word == "op")

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

# Colours results based on conditions.
fill_bar <- case_when(
  df_merged2$result < -200   ~ "red4",
  df_merged2$result <= -150 ~ "red1",
  df_merged2$result <= -100 ~ "darkgreen",
  df_merged2$result <= -50 ~ "seagreen",
  df_merged2$result <= 0 ~ "black",
  # df_merged2$result >= 0 ~ "blue",
  df_merged2$result <= 50 ~ "slategray4",
  df_merged2$result <= 100 ~ "slateblue",
  df_merged2$result <= 150 ~ "steelblue3",
  df_merged2$result <= 200 ~ "cyan4")


# bar graph of difference between Day 2, and Day 1.
df_merged2 %>% 
  top_n(40) %>% 
  mutate(word = reorder(word, result)) %>%
  ggplot(aes(word, result)) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  geom_bar(stat = "identity", fill = fill_bar) +
  labs(
    title = "Difference of Word Count from Day 2 - Day 1",
    x = "Words",
    y = "Count",
    caption = "Positive integers = More mentions on day 2
     Negative integers = Less mentions on day 2.") +
  coord_flip()

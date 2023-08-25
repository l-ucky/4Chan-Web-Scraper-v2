# Load Libraries
# For graphing
library("ggplot2")
# For the %>% operator, but you can
# skip loading this library
# and just use `|>` as 
# a pipe operator.
library("tidyverse")

# Note: Other code is below to do an alternative method,
#       but the uncommented method is superior.

# Load CSVs using code. 
df1 <- read.csv("~/Documents/Stats/4Chan Scraper/Aug 24 2023 18:11:19.csv")
df2 <- read.csv("~/Documents/Stats/4Chan Scraper/Aug 25 2023 10:51:42.csv")


# Merge data frame, and take difference b/w day 1 and day 2
# subtracting data frames from each other.
df_merged <- merge(df1, df2, by="word", all=TRUE)
df_merged$result <- df_merged$n.y - df_merged$n.x



# Feel free to add more "non-words," or "noise," 
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

# Bar graph of difference
df_difference_filter %>% 
  top_n(40) %>% 
  mutate(word = reorder(word, result)) %>% 
  drop_na() %>% 
  ggplot(aes(word, result)) +
  geom_col() + 
  labs(
    title = "Difference of Word Count from Day 1 - Day 2",
    x = "Words",
    y = "Count") +
  coord_flip()


# Assign CSV to data frame if loading in manually with the GUI/IDE.
# df1 <- `Aug.24.2023.18:11:19`
# df2 <- `Aug.25.2023.10:51:42`

# Sort data alphabetically.
# But I don't need to do that.
# sort.df1 <- with(df1,  df1[order(df1$word) , ])
# sort.df2 <- with(df2,  df2[order(df2$word) , ])

# Take 20,000 rows of count data.
# But I don't need to do that.
# df1_ <- sort.df1[1:20000,2]
# df2_ <- sort.df2[1:20000,2]

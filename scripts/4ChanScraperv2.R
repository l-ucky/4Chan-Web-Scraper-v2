##### Code Below #####
#Load Libraries
library("rvest")
library("tidyverse")
library("ggplot2")
library("wordcloud")
library("tidytext")
library("tinytex")
library("syuzhet")
library("lubridate")
library("scales")
library("reshape2")
library("dplyr")

# This scraping is getting all of the internal links

#Page 1 Scrape
pol_threads1 <- read_html("https://boards.4channel.org/pol/") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 2 Scrape
pol_threads2 <- read_html("https://boards.4channel.org/pol/2") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 3 Scrape
pol_threads3 <- read_html("https://boards.4channel.org/pol/3") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 4 Scrape
pol_threads4 <- read_html("https://boards.4channel.org/pol/4") %>%
  html_elements("a") %>%
  html_attr('href')


#Page 5 Scrape
pol_threads5 <- read_html("https://boards.4channel.org/pol/5") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 6 Scrape
pol_threads6 <- read_html("https://boards.4channel.org/pol/6") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 7 Scrape
pol_threads7 <- read_html("https://boards.4channel.org/pol/7") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 8 Scrape
pol_threads8 <- read_html("https://boards.4channel.org/pol/8") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 9 Scrape
pol_threads9 <- read_html("https://boards.4channel.org/pol/9") %>%
  html_elements("a") %>%
  html_attr('href')

#Page 10 Scrape
pol_threads10 <- read_html("https://boards.4channel.org/pol/10") %>%
  html_elements("a") %>%
  html_attr('href')

# Combining all of the threads into 1 data frame
df_pol <-      c(pol_threads1, 
                 pol_threads2, 
                 pol_threads3, 
                 pol_threads4, 
                 pol_threads5, 
                 pol_threads6, 
                 pol_threads7, 
                 pol_threads8, 
                 pol_threads9,
                 pol_threads10)


#tibble makes a table out of data from the scraped links
pol_table <- tibble(txt = df_pol)


# Choosing all of the links that look like: "thread/this-is-a-thread"
df_links <- pol_table %>% 
  filter(str_detect(txt, "(thread/[0-9]{6,}[/][a-z]{1,})"))


# Next step is appending on "https://boards.4chan.org/pol/" before the "thread/this-is-a-thread".
df_links$txt <- paste("https://boards.4chan.org/pol/", df_links$txt, sep = "")


# This code will "apply" the 
# "read_html" 
# "html_elements" and 
# "html_text"
# to each row in the data frame
threads <- lapply(df_links$txt, function(x) {
  read_html(x) %>%
    html_text2()})

# Turn "threads" into a tibble so tidytex can manipulate it
threads_tibble <- tibble(txt = threads)

# Break up all of the sentences into single words
tidy_pol <- threads_tibble %>% 
  unnest_tokens(word, txt, format = "text")


tidy_pol_fixed <- tidy_pol %>%
  filter(!word %in% stop_words$word 
         & !word == "fucking"
         & !word == "https"
         & !word == "shit"
         & !word == "id"
         & !word == "anonymous"
         & !word == "wed"
         & !word == "kb"
         & !word == "var"
         & !word == "png"
         & !word == "mobile"
         & !word == "mb"
         & !word == "catalog"
         & !word == "settings"
         & !word == "display"
         & !word == "advertise"
         & !word == "pass"
         & !word == "bottom"
         & !word == "pol"
         & !word == "shit"
         & !word == "jpg"
         & !word == "view"
         & !word == "vp"
         & !word == "ad"
         & !word == "tv"
         & !word == "fit"
         & !word == "post"
         & !word == "thread"
         & !word == "hr"
         & !word == "gif"
         & !word == "webm"
         & !word == "incorrect"
         & !word == "tg"
         & !word == "comments"
         & !word == "search"
         & !word == "top"
         & !word == "site"
         & !word == "home"
         & !word == "reply"
         & !word == "board"
         & !word == "politically"
         & !word == "return"
         & !word == "time"
         & !word == "owned"
         & !word == "added"
         & !word == "vip"
         & !word == "users"
         & !word == "rules"
         & !word == "legal"
         & !word == "lgbt"
         & !word == "lit"
         & !word == "file"
         & !word == "mu"
         & !word == "hide"
         & !word == "fa"
         & !word == "responsibility"
         & !word == "style"
         & !word == "options"
         & !word == "table"
         & !word == "page"
         & !word == "serve"
         & !word == "contact"
         & !word == "images"
         & !word == "international"
         & !word == "poster"
         & !word == "people"
         & !word == "true"
         & !word == "bant"
         & !word == "vm"
         & !word == "vmg"
         & !word == "vrpg"
         & !word == "vst"
         & !word == "read"
         & !word == "news"
         & !word == "image"
         & !word == "posts"
         & !word == "jp"
         & !word == "sci"
         & !word == "vg"
         & !word == "po"
         & !word == "toy"
         & !word == "vt"
         & !word == "wg"
         & !word == "biz"
         & !word == "ck"
         & !word == "desktop"
         & !word == "enable"
         & !word == "feedback"
         & !word == "int"
         & !word == "verification"
         & !word == "respective"
         & !word == "vr"
         & !word == "wsg"
         & !word == "aco"
         & !word == "adv"
         & !word == "delete"
         & !word == "cm"
         & !word == "disable"
         & !word == "bfutababurichantomorrowphoton"
         & !word == "cgl"
         & !word == "comlen"
         & !word == "cooldowns"
         & !word == "copyrights"
         & !word == "cssversion"
         & !word == "diy"
         & !word == "gd"
         & !word == "hc"
         & !word == "ic"
         & !word == "incorrectreturn"
         & !word == "jsversion"
         & !word == "maxfilesize"
         & !word == "maxlines"
         & !word == "mlp"
         & !word == "payment"
         & !word == "postform"
         & !word == "pw"
         & !word == "qa"
         & !word == "qst"
         & !word == "recaptcha"
         & !word == "refresh"
         & !word == "replyreturn"
         & !word == "soc"
         & !word == "sp"
         & !word == "trademarks"
         & !word == "trv"
         & !word == "uploaded"
         & !word == "hm"
         & !word == "xs"
         & !word == "yotsubayotsuba"
         & !word == "boards"
         & !word == "faq"
         & !word == "announcementcrypto"
         & !word == "bolsheviknatonazihippiepiraterepublicantask"
         & !word == "bypass"
         & !word == "capitalistanarchistblack"
         & !word == "flaggeographic"
         & !word == "huggerunited"
         & !word == "locationanarcho"
         & !word == "login"
         & !word == "nationalistconfederatecommunistcataloniademocrateuropeanfascistgadsdengayjihadikekistanimuslimnational"
         & !word == "nationswhite"
         & !word == "refreshpost"
         & !word == "supremacistfileplease"
         & !word == "ztemplartree"
         & !word == "posters"
         & !word == "wpjizlog"
         & !word == "xxfbsv"
         & !word == "wsr"
         & !word == "mon"
         & !word == "tue"
         & !word == "wed"
         & !word == "thu"
         & !word == "fri"
         & !word == "sat"
         & !word == "sun"
         & !word == "tues"
         & !grepl("[^A-Za-z]", word))

#below will replace a word with another word
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "niggers", "nigger")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "jews", "jew")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "jewish", "jew")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "woman", "women")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "fucked", "fuck")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "fucks", "fuck")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "fuckers", "fuck")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "fuckin", "fuck")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "fucker", "fuck")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "retards", "retard")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "retarded", "retard")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "retardation", "retard")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "campaings", "campaign")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "posted", "posting")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "shitskins", "nigger")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "shitskin", "nigger")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "christians", "christian")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "israeli", "israel")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "guy", "guys")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "whites", "white")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "indians", "indian")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "poos", "poo")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "parties", "party")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "kikes", "jew")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "kike", "jew")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "glow", "glowie")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "glowies", "glowie")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "glows", "glowie")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "glowfags", "glowie")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "glowshills", "glowie")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "guyss", "guy")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "guys", "guy")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "shills", "shill")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "shilling", "shill")
tidy_pol_fixed$word <- str_replace(tidy_pol_fixed$word, "shilled", "shill")

tidy_pol_fixed2 <- tidy_pol_fixed %>% 
  count(word, sort = TRUE) %>% 
  print(n = 10)

# Goodness of fit test ($\chi^2$). p-value < 0.05 = significant result that the 
# observations are indepenent and vary in probabilities of showing up.

top <- subset(tidy_pol_fixed2, tidy_pol_fixed2$n >= 200)
chi <- chisq.test(top$n)
options(scipen = 999) # removes scientific notation
chi


# Time to Visualize

tidy_pol_fixed2 %>% 
  top_n(50) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() + 
  xlab("Words") +
  ylab("Count") +
  coord_flip()

tidy_pol_fixed2 %>% 
  with(wordcloud(word, n, max.words = 100, random.order = FALSE, rot.per = 0.0, 
                 colors = brewer.pal(8, "Dark2")))

# Sentiment analysis, and visuals with a bar grapoh.
# This will take a few minutes to process
# On my Ryzen 7 3700X, and 16Gb of RAM, it took about 5 minutes.
sentiment_tidy_pol_fixed_2 <- get_nrc_sentiment(tidy_pol_fixed2$word)

barplot(colSums(sentiment_tidy_pol_fixed_2),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Pol Sentiment Scores')

# Time to Save the Data
timestamp <- format(Sys.time(), "%b %d %Y %X")
filename <- paste0("~/Documents/Stats/4Chan Scraper/",timestamp,".csv")  
write.csv(tidy_pol_fixed2, file = filename)

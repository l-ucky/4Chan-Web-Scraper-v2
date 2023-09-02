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
library("qdapDictionaries")
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

# get_hrefs <- . %>% read_html %>% html_elements("a") %>%
#   html_attr('href')
# paste("https://boards.4channel.org/pol/", 1:9) %>% map_chr(get_hrefs)

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

# Break up all of the sentences into word pairs
tidy_pol <- threads_tibble %>% 
  unnest_tokens(word, txt, format = "text", token = "ngrams", n = 2, to_lower = TRUE)

tidy_pol_fixed <- tidy_pol %>%
  filter(str_detect(word, "([a-z]{3,} [a-z]{3,})"))


tidy_pol_fixed_separated <- tidy_pol_fixed %>%  
  separate(word, into = c("word1", "word2"), sep = " ") %>% 
  filter(!word1 == "fucking"
         & !word1 == "https"
         & !word1 == "shit"
         & !word1 == "id"
         & !word1 == "anonymous"
         & !word1 == "wed"
         & !word1 == "kb"
         & !word1 == "var"
         & !word1 == "png"
         & !word1 == "mobile"
         & !word1 == "mb"
         & !word1 == "catalog"
         & !word1 == "settings"
         & !word1 == "display"
         & !word1 == "advertise"
         & !word1 == "pass"
         & !word1 == "bottom"
         & !word1 == "pol"
         & !word1 == "shit"
         & !word1 == "jpg"
         & !word1 == "view"
         & !word1 == "vp"
         & !word1 == "ad"
         & !word1 == "tv"
         & !word1 == "fit"
         & !word1 == "post"
         & !word1 == "thread"
         & !word1 == "hr"
         & !word1 == "gif"
         & !word1 == "webm"
         & !word1 == "incorrect"
         & !word1 == "tg"
         & !word1 == "comments"
         & !word1 == "search"
         & !word1 == "top"
         & !word1 == "site"
         & !word1 == "home"
         & !word1 == "reply"
         & !word1 == "board"
         & !word1 == "politically"
         & !word1 == "return"
         & !word1 == "time"
         & !word1 == "owned"
         & !word1 == "added"
         & !word1 == "vip"
         & !word1 == "users"
         & !word1 == "rules"
         & !word1 == "legal"
         & !word1 == "lgbt"
         & !word1 == "lit"
         & !word1 == "file"
         & !word1 == "mu"
         & !word1 == "hide"
         & !word1 == "fa"
         & !word1 == "responsibility"
         & !word1 == "style"
         & !word1 == "options"
         & !word1 == "table"
         & !word1 == "page"
         & !word1 == "serve"
         & !word1 == "contact"
         & !word1 == "images"
         & !word1 == "international"
         & !word1 == "poster"
         & !word1 == "people"
         & !word1 == "true"
         & !word1 == "bant"
         & !word1 == "vm"
         & !word1 == "vmg"
         & !word1 == "vrpg"
         & !word1 == "vst"
         & !word1 == "read"
         & !word1 == "news"
         & !word1 == "image"
         & !word1 == "posts"
         & !word1 == "jp"
         & !word1 == "sci"
         & !word1 == "vg"
         & !word1 == "po"
         & !word1 == "toy"
         & !word1 == "vt"
         & !word1 == "wg"
         & !word1 == "biz"
         & !word1 == "ck"
         & !word1 == "desktop"
         & !word1 == "enable"
         & !word1 == "feedback"
         & !word1 == "int"
         & !word1 == "verification"
         & !word1 == "respective"
         & !word1 == "vr"
         & !word1 == "wsg"
         & !word1 == "aco"
         & !word1 == "adv"
         & !word1 == "delete"
         & !word1 == "cm"
         & !word1 == "disable"
         & !word1 == "bfutababurichantomorrowphoton"
         & !word1 == "cgl"
         & !word1 == "comlen"
         & !word1 == "cooldowns"
         & !word1 == "copyrights"
         & !word1 == "cssversion"
         & !word1 == "diy"
         & !word1 == "gd"
         & !word1 == "hc"
         & !word1 == "ic"
         & !word1 == "incorrectreturn"
         & !word1 == "jsversion"
         & !word1 == "maxfilesize"
         & !word1 == "maxlines"
         & !word1 == "mlp"
         & !word1 == "payment"
         & !word1 == "postform"
         & !word1 == "pw"
         & !word1 == "qa"
         & !word1 == "qst"
         & !word1 == "recaptcha"
         & !word1 == "refresh"
         & !word1 == "replyreturn"
         & !word1 == "soc"
         & !word1 == "sp"
         & !word1 == "trademarks"
         & !word1 == "trv"
         & !word1 == "uploaded"
         & !word1 == "hm"
         & !word1 == "xs"
         & !word1 == "yotsubayotsuba"
         & !word1 == "boards"
         & !word1 == "faq"
         & !word1 == "announcementcrypto"
         & !word1 == "bolsheviknatonazihippiepiraterepublicantask"
         & !word1 == "bypass"
         & !word1 == "capitalistanarchistblack"
         & !word1 == "flaggeographic"
         & !word1 == "huggerunited"
         & !word1 == "locationanarcho"
         & !word1 == "login"
         & !word1 == "nationalistconfederatecommunistcataloniademocrateuropeanfascistgadsdengayjihadikekistanimuslimnational"
         & !word1 == "nationswhite"
         & !word1 == "refreshpost"
         & !word1 == "supremacistfileplease"
         & !word1 == "ztemplartree"
         & !word1 == "posters"
         & !word1 == "wpjizlog"
         & !word1 == "xxfbsv"
         & !word1 == "wsr"
         & !word1 == "mon"
         & !word1 == "tue"
         & !word1 == "wed"
         & !word1 == "thu"
         & !word1 == "fri"
         & !word1 == "sat"
         & !word1 == "sun"
         & !word1 == "tues"
         & !word1 == "emyqupza"
         & !word1 == "nlcbzjyk"
         & !word1 == "oq"
         & !word1 == "4chanvar"
         & !word1 == "style_group"
         & !word1 == "17new"
         & !word1 == "oq"
         & !word1 == "www.youtube.com"
         & !word1 == "fsy"
         & !word1 == "gmzv"
         & !word1 == "pzd"
         & !word1 == "2ufe"
         & !word1 == "bwi"
         & !word1 == "5zpr"
         & !word1 == "cke"
         & !word1 == "clr3"
         & !word1 == "vex"
         & !word1 == "oqvc3qmvz0i"
         & !word1 == "doc_strangelove"
         & !word1 == "en.wikipedia.org"
         & !word1 == "magyar"
         & !word1 == "cygany"
         & !word1 == "hxri"
         & !word1 == "kvan"
         & !word1 == "egr"
         & !word1 == "akq"
         & !word1 == "wiki"
         & !word1 == "archive.org"
         & !word1 == "details"
         & !word1 == "een"
         & !word1 == "ujhs"
         & !word1 == "rhr"
         & !word1 == "fvcc"
         & !word1 == "ygpd"
         & !word1 == "lel"
         & !word1 == "announcement"
         & !grepl('[0-9]', word1)) %>% 
  filter(!word2 == "fucking"
         & !word2 == "https"
         & !word2 == "shit"
         & !word2 == "id"
         & !word2 == "anonymous"
         & !word2 == "wed"
         & !word2 == "kb"
         & !word2 == "var"
         & !word2 == "png"
         & !word2 == "mobile"
         & !word2 == "mb"
         & !word2 == "catalog"
         & !word2 == "settings"
         & !word2 == "display"
         & !word2 == "advertise"
         & !word2 == "pass"
         & !word2 == "bottom"
         & !word2 == "pol"
         & !word2 == "shit"
         & !word2 == "jpg"
         & !word2 == "view"
         & !word2 == "vp"
         & !word2 == "ad"
         & !word2 == "tv"
         & !word2 == "fit"
         & !word2 == "post"
         & !word2 == "thread"
         & !word2 == "hr"
         & !word2 == "gif"
         & !word2 == "webm"
         & !word2 == "incorrect"
         & !word2 == "tg"
         & !word2 == "comments"
         & !word2 == "search"
         & !word2 == "top"
         & !word2 == "site"
         & !word2 == "home"
         & !word2 == "reply"
         & !word2 == "board"
         & !word2 == "politically"
         & !word2 == "return"
         & !word2 == "time"
         & !word2 == "owned"
         & !word2 == "added"
         & !word2 == "vip"
         & !word2 == "users"
         & !word2 == "rules"
         & !word2 == "legal"
         & !word2 == "lgbt"
         & !word2 == "lit"
         & !word2 == "file"
         & !word2 == "mu"
         & !word2 == "hide"
         & !word2 == "fa"
         & !word2 == "responsibility"
         & !word2 == "style"
         & !word2 == "options"
         & !word2 == "table"
         & !word2 == "page"
         & !word2 == "serve"
         & !word2 == "contact"
         & !word2 == "images"
         & !word2 == "international"
         & !word2 == "poster"
         & !word2 == "people"
         & !word2 == "true"
         & !word2 == "bant"
         & !word2 == "vm"
         & !word2 == "vmg"
         & !word2 == "vrpg"
         & !word2 == "vst"
         & !word2 == "read"
         & !word2 == "news"
         & !word2 == "image"
         & !word2 == "posts"
         & !word2 == "jp"
         & !word2 == "sci"
         & !word2 == "vg"
         & !word2 == "po"
         & !word2 == "toy"
         & !word2 == "vt"
         & !word2 == "wg"
         & !word2 == "biz"
         & !word2 == "ck"
         & !word2 == "desktop"
         & !word2 == "enable"
         & !word2 == "feedback"
         & !word2 == "int"
         & !word2 == "verification"
         & !word2 == "respective"
         & !word2 == "vr"
         & !word2 == "wsg"
         & !word2 == "aco"
         & !word2 == "adv"
         & !word2 == "delete"
         & !word2 == "cm"
         & !word2 == "disable"
         & !word2 == "bfutababurichantomorrowphoton"
         & !word2 == "cgl"
         & !word2 == "comlen"
         & !word2 == "cooldowns"
         & !word2 == "copyrights"
         & !word2 == "cssversion"
         & !word2 == "diy"
         & !word2 == "gd"
         & !word2 == "hc"
         & !word2 == "ic"
         & !word2 == "incorrectreturn"
         & !word2 == "jsversion"
         & !word2 == "maxfilesize"
         & !word2 == "maxlines"
         & !word2 == "mlp"
         & !word2 == "payment"
         & !word2 == "postform"
         & !word2 == "pw"
         & !word2 == "qa"
         & !word2 == "qst"
         & !word2 == "recaptcha"
         & !word2 == "refresh"
         & !word2 == "replyreturn"
         & !word2 == "soc"
         & !word2 == "sp"
         & !word2 == "trademarks"
         & !word2 == "trv"
         & !word2 == "uploaded"
         & !word2 == "hm"
         & !word2 == "xs"
         & !word2 == "yotsubayotsuba"
         & !word2 == "boards"
         & !word2 == "faq"
         & !word2 == "announcementcrypto"
         & !word2 == "bolsheviknatonazihippiepiraterepublicantask"
         & !word2 == "bypass"
         & !word2 == "capitalistanarchistblack"
         & !word2 == "flaggeographic"
         & !word2 == "huggerunited"
         & !word2 == "locationanarcho"
         & !word2 == "login"
         & !word2 == "nationalistconfederatecommunistcataloniademocrateuropeanfascistgadsdengayjihadikekistanimuslimnational"
         & !word2 == "nationswhite"
         & !word2 == "refreshpost"
         & !word2 == "supremacistfileplease"
         & !word2 == "ztemplartree"
         & !word2 == "posters"
         & !word2 == "wpjizlog"
         & !word2 == "xxfbsv"
         & !word2 == "wsr"
         & !word2 == "mon"
         & !word2 == "tue"
         & !word2 == "wed"
         & !word2 == "thu"
         & !word2 == "fri"
         & !word2 == "sat"
         & !word2 == "sun"
         & !word2 == "tues"
         & !word2 == "emyqupza"
         & !word2 == "nlcbzjyk"
         & !word2 == "oq"
         & !word2 == "4chanvar"
         & !word2 == "style_group"
         & !word2 == "17new"
         & !word2 == "oq"
         & !word2 == "www.youtube.com"
         & !word2 == "fsy"
         & !word2 == "gmzv"
         & !word2 == "pzd"
         & !word2 == "2ufe"
         & !word2 == "bwi"
         & !word2 == "5zpr"
         & !word2 == "cke"
         & !word2 == "clr3"
         & !word2 == "vex"
         & !word2 == "oqvc3qmvz0i"
         & !word2 == "doc_strangelove"
         & !word2 == "en.wikipedia.org"
         & !word2 == "magyar"
         & !word2 == "cygany"
         & !word2 == "hxri"
         & !word2 == "kvan"
         & !word2 == "egr"
         & !word2 == "akq"
         & !word2 == "wiki"
         & !word2 == "archive.org"
         & !word2 == "details"
         & !word2 == "een"
         & !word2 == "ujhs"
         & !word2 == "rhr"
         & !word2 == "fvcc"
         & !word2 == "fvcc"
         & !word2 == "ygpd"
         & !word2 == "lel"
         & !word2 == "announcement"
         & !grepl('[0-9]', word2))

#### below will replace a word with another word ####
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "niggers", "nigger")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "jews", "jew")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "jewish", "jew")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "woman", "women")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "fucked", "fuck")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "fucks", "fuck")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "fuckers", "fuck")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "fuckin", "fuck")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "fucker", "fuck")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "retards", "retard")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "retarded", "retard")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "retardation", "retard")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "campaings", "campaign")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "posted", "posting")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "shitskins", "nigger")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "shitskin", "nigger")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "christians", "christian")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "israeli", "israel")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "guy", "guys")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "whites", "white")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "indians", "indian")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "poos", "poo")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "parties", "party")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "kikes", "jew")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "kike", "jew")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "glow", "glowie")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "glowies", "glowie")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "glows", "glowie")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "glowfags", "glowie")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "glowshills", "glowie")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "guyss", "guy")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "guys", "guy")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "shills", "shill")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "shilling", "shill")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "shilled", "shill")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "europeans", "european")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "catholics", "catholic")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "masks", "mask")
tidy_pol_fixed_separated$word1 <- str_replace(tidy_pol_fixed_separated$word1, "threadsstop", "threads stop")

tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "niggers", "nigger")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "jews", "jew")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "jewish", "jew")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "woman", "women")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "fucked", "fuck")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "fucks", "fuck")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "fuckers", "fuck")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "fuckin", "fuck")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "fucker", "fuck")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "retards", "retard")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "retarded", "retard")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "retardation", "retard")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "campaings", "campaign")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "posted", "posting")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "shitskins", "nigger")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "shitskin", "nigger")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "christians", "christian")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "israeli", "israel")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "guy", "guys")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "whites", "white")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "indians", "indian")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "poos", "poo")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "parties", "party")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "kikes", "jew")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "kike", "jew")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "glow", "glowie")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "glowies", "glowie")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "glows", "glowie")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "glowfags", "glowie")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "glowshills", "glowie")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "guyss", "guy")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "guys", "guy")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "shills", "shill")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "shilling", "shill")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "shilled", "shill")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "europeans", "european")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "catholics", "catholic")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "masks", "mask")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "threadsstop", "threads stop")
tidy_pol_fixed_separated$word2 <- str_replace(tidy_pol_fixed_separated$word2, "diplomatic", "diplomacy")

#####


tidy_pol_united_ngram<- tidy_pol_fixed_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         word1 %in% GradyAugmented,
         word2 %in% GradyAugmented) %>%
  unite(word, c(word1, word2), sep = " ")


tidy_pol_fixed2_ngram <- tidy_pol_united_ngram %>% 
  count(word, sort = TRUE) %>% 
  filter(!word == "niggers niggers"
         & !word == "nigger nigger"
         & !word == "based based"
         & !word == "jew jew"
         & !word == "niggers niggers") %>% 
  print(n=70)


# =========== Time to Visualize ===========
#tidy_pol_fixed2 <- read.csv("~/Documents/Stats/4ChanScraper/ngram Sep 01 2023 22:26:23.csv"  )


tidy_pol_fixed2_ngram %>% 
  top_n(70) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Used Word Pairs",
       x = "Words",
       y = "Count",
       fill = "Results") +
  coord_flip() +
  theme_dark(base_size = 12.5)



tidy_pol_fixed2_ngram %>% 
  with(wordcloud(word, n, max.words = 75, scale = c(1.5,0.75), random.order = FALSE, rot.per = 0.0, 
                 colors = brewer.pal(8, "Dark2")))

# Time to Save the Data
timestamp <- format(Sys.time(), "%b %d %Y %X")
filename <- paste0("~/Documents/Stats/4ChanScraper/ngram ",timestamp,".csv")  
write.csv(tidy_pol_fixed, file = filename)

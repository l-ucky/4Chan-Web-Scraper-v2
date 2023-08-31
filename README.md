# 4 Chan Webscraper, Version 2

Consider doing your own data analysis. If you save your CSV, and make a pull request, I can add it to this repository for plotting word usage changes over time.

**Highlights:**

- Written in R.
- Objective: Datamining text
- Uses the following libraries: rvest, tidyverse, tidytext, ggplot2, wordcloud, tinytex, syuzhet, scales, reshape2, and dplyr.
- If you don't have these installed in your RStudio software, then install them.
- After installing, and running this script into your RStudio IDE, you can download all posts.
- Downloaded posts are then manipulated to show word frequencies.
- Differs from V1 by scraping all replies to OP, and has a much larger noise filter.
- Sentiment analysis is also performed.
- **X number of "posts by this ID" with graphical representation.**

**Limitations**
- 4chan will recycle poster IDs, so they are not unique identifiers. Therefore, the data mining on n-pbtid isn't fully accurate for the upper-bound, but it should be closer to representative at the lower-bound.
- Time of day scrapes based on time-zone of interest (e.g. New York posting hours) hasn't been implemented, but this can be easily solved by scraping the threads from 0900h - 1700h by intervals of 3 hours to allow old threads to die, for your target local time.
- Some synonyms will not be counted under a single word (e.g. glowie, glow, glows, glower), so manual input will need to be implemented in the v2 script. The same idea goes for plural words, but there is another package that will take root words, and remove word modifiers (e.g. -ing, -ed, -s, etc)
- n-gram analysis has not been completed, but a simple edit to the v2 script will accomodate this. n-gram means the number of paired words (e.g. 2gram = he is, she is, it is; 3gram = i am going, she is going, he is going; etc). This can provide more context of rhetoric within the entire board scrape.

# html_text vs html_text2 from rvest

**I changed my mind. Using html_text2 will correct run-on words that don't naturally belong together. (e.g. webmhello)**

I did an experiment comparing the `tidy_pol_fixed2` output of text.

html_text = 21776 observations

html_text2 = 20004 observations

I will continue using html_text because it contains more observations, which I can later filter out the noise as needed. 
There were no substantial differences that I noticed in the graphs, so retaining a greater number of observations seems better than less.

From the rvest::html_text website:

There are two ways to retrieve text from a element: html_text() and html_text2(). html_text() is a thin wrapper around xml2::xml_text() which returns just the raw underlying text. html_text2() simulates how text looks in a browser, using an approach inspired by JavaScript's innerText(). Roughly speaking, it converts <br /> to "\n", adds blank lines around `<p>` tags, and lightly formats tabular data.

html_text2() is usually what you want, but it is much slower than html_text() so for simple applications where performance is important you may want to use html_text() instead.

# Calculating, and Displaying Post Frequency Differences

I added an extra R script, with an example PDF output. In these files it takes the CSV the scraper auto-saves to your statistics directory, and imports the data set. Then, the top 20 positive, and top 20 negative numbers are taken from subtracting day 2 from day 1. So, if you see a negative number in the bar graph, then there were more mentions of the key word made on day 1, than on day 2. The opposite is also true: if you see a positive numbr in the bar graph, then there were less mentions made on day 1, than on day 2.

I hope you find this additional script helpful.

Please also consider sending me CSV files of your scrapes to this GitHub repository.

# Sentiment Analysis

I added a sentiment analysis which will categorize used words into several columns and count their occurances. I am using the NCR library.


# X Posts by This ID

I wrote a script for finding all unique IDs from all threads, and performed analysis on the number of posts per unique ID (e.g. 2 posts by this ID) and plotted the frequency of the number of posts across all threads. See the example for a more in-depth explanation.

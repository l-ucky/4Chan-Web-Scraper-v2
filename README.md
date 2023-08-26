# 4 Chan Webscraper, Version 2

Consider doing your own data analysis. If you save your CSV, and make a pull request, I can add it to this repository for plotting word usage changes over time.

**Highlights:**

- Written in R.
- Objective: Datamining text, and displaying word frequencies.
- Uses the following libraries: rvest, tidyverse, tidytext, ggplot2, wordcloud, tinytex, syuzhet, scales, reshape2, and dplyr.
- If you don't have these installed in your RStudio software, then install them.
- After installing, and running this script into your RStudio IDE, you can download all posts.
- Downloaded posts are then manipulated to show word frequencies.
- Differs from V1 by scraping all replies to OP, and has a much larger noise filter.
- Sentiment analysis is also performed.

# html_text vs html_text2 from rvest

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

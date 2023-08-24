# html_text vs html_text2 from rvest

I did an experiment comparing the `tidy_pol_fixed2` output of text.

html_text = 21776 observations
html_text2 = 20004 observations

I will continue using html_text because it contains more observations, which I can later filter out the noise as needed. 
There were no substantial differences that I noticed in the graphs, so retaining a greater number of observations seems better than less.

From the rvest::html_text website:

There are two ways to retrieve text from a element: html_text() and html_text2(). html_text() is a thin wrapper around xml2::xml_text() which returns just the raw underlying text. html_text2() simulates how text looks in a browser, using an approach inspired by JavaScript's innerText(). Roughly speaking, it converts <br /> to "\n", adds blank lines around <p> tags, and lightly formats tabular data.

html_text2() is usually what you want, but it is much slower than html_text() so for simple applications where performance is important you may want to use html_text() instead.


# Localharvest Analysis

An Rshiny app using the tidyverse, sf, and data from localharvest and tigris (us census data). It displays a map of the US with each state filled in relative to the number of farms that produce a certain item. There are inputs for product type, palette choice, season, facet wrapping by season, and 
It also _will have_ a histogram of the top producers from a single state and a slider that allows the user to select a minimum and maximum *last updated* range, which corresponds to how recently the farm's localharvest.org page was updated.

The data for this was collectd ethically in accordance with localharvest.org's privacy policy and contains no personal information.

This analysis was done in conjunction with my Master's of Science in Integrative Biology at Kennesaw State University looking at the effect of microbes on tomato growth and crop production. 
As part of that project, I was looking to interview local farmers near me and in my search came upon localharvest.org, a website dedicated to providing small farmers a platform on which to connect with a larger audience, advertise crop share agreements, and more. 
Localharvest has far more farmers than I could possibly talk to, and while the data it contains was not exactly what I was looking for, it was a treasure trove of data and best of all - self-reported and publicly available online. 

I wrote a Python script to scrape farmer data over the course of several months. *Slowly* so as to not overwhelm Localharvest's servers.

### Languages
1. R 
    tigris, dplyr, ggplot2, sf
2. Python
    requests, BeautifulSoup, pandas
3. HTML
4. CSS 
5. Javascript

### Demonstrated skills
1. Scraping publicly available data off the internet
1. Cleaning data
2. Combining original data with public databases
3. 
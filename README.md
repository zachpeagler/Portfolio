# Portfolio
 Zach Peagler's portfolio of design, data analysis, and more

# Analyses

## Localharvest analysis

### LH_cleaning


### LH_info_scraper

A python script that scrapes information off of localharvest.org. This was done *slowly* so as to not overwhelm localharvest's servers and is in compliance with Localharvest's privacy policy.


# Apps

## 01 US Family Farms

Localharvest Analysis project 1
- Leaflet

## 02 US City Crop Production

Localharvest analysis project 2

## 03 US State Crop Production

Localharvest analysis project 3

An Rshiny app using the tidyverse, sf, and data from localharvest and tigris (us census data). It displays a map of the US with each state filled in relative to the number of farms that produce a certain item. There are inputs for product type, palette choice, season, facet wrapping by season, and 
It also _will have_ a histogram of the top producers from a single state and a slider that allows the user to select a minimum and maximum *last updated* range, which corresponds to how recently the farm's localharvest.org page was updated.

The data for this was collectd ethically in accordance with localharvest.org's privacy policy and contains no personal information.

This analysis was done in conjunction with my Master's of Science in Integrative Biology at Kennesaw State University looking at the effect of microbes on tomato growth and crop production. 
As part of that project, I was looking to interview local farmers near me and in my search came upon localharvest.org, a website dedicated to providing small farmers a platform on which to connect with a larger audience, advertise crop share agreements, and more. 
Localharvest has far more farmers than I could possibly talk to, and while the data it contains was not exactly what I was looking for, it was a treasure trove of data and best of all - self-reported and publicly available online. 

I wrote a Python script to scrape farmer data over the course of several months. *Slowly* so as to not overwhelm Localharvest's servers.

This project used multiple languages including R, Python, HTML, CSS, and Javascript.

This particular analysis displays several skills, including
1. Scraping publicly available data off the internet
1. Cleaning data
2. Combining original data with public databases
3. 
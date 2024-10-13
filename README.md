# Portfolio
 Zach Peagler's portfolio of design, data analysis, and more

# 1 Analyses

## 1.1 Localharvest analysis

This analysis was done in conjunction with my Master's of Science in Integrative Biology at Kennesaw State University looking at the effect of microbes on tomato growth and crop production. 
As part of that project, I was looking to interview local farmers near me and in my search came upon localharvest.org, a website dedicated to providing small farmers a platform on which to connect with a larger audience, advertise crop share agreements, and more. 
Localharvest has far more farmers than I could possibly talk to, and while the data it contains was not exactly what I was looking for, it was a treasure trove of data and best of all - self-reported and publicly available online. 

### 1.1.1 LH_cleaning

An R markdown file that takes the data from LH_info_scraper and cleans it, turning it into a dataframe in tidy format with one value per cell.

### 1.1.2 LH_info_scraper

A python script that scrapes information off of localharvest.org. This was done *slowly* so as to not overwhelm localharvest's servers.

The data for this was collectd ethically in accordance with localharvest.org's privacy policy and contains no personal information.

This project used multiple languages including R, Python, HTML, and CSS.

This particular analysis displays several skills, including
1. Scraping publicly available data off the internet
1. Cleaning badly formed data
2. Combining original data with public databases

## 1.2 Tomato Inoculants Analysis

Done as part of my Master's of Science in Integrative Biology at Kennesaw State University, this is a comprehensive set of analyses looking at four years of tomato inoculant data over various trials.

# 2 Apps

> All the apps from my [App-A-Day](https://github.com/zachpeagler/App-A-Day) project can be found in their own repo.

## 2.1 Tomato Inoculants 2024

An R Shiny app created as part of my Master's thesis at Kennesaw State University. It is a comprehensive analysis of a 7 month trial.

This uses R, the packages tidyverse, shiny, showtext, scico, bslib, MASS, and gpubr.

## 2.2 US City Family Farm Products

Localharvest analysis project 2

## 2.3 US State Family Farm Products

An R Shiny app using the tidyverse, sf, and data from localharvest and tigris (us census data). It displays a map of the US with each state filled in relative to the number of farms that produce a certain item. There are inputs for product type, palette choice, season, facet wrapping by season.

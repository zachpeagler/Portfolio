# Zach Peagler Portfolio
[![License: MIT](https://img.shields.io/badge/License-MIT-lightgrey.svg)](https://opensource.org/license/mit)
![experimental](https://img.shields.io/badge/lifecycle-maturing-lightblue)
![year](https://img.shields.io/badge/year-2024-darkblue)

## Introduction

Hello! I'm Zach Peagler, a data scientist, R Shiny developer, and graduate student at Kennesaw State University. I'm proficient with R and have a broad depth of knowledge on statistical tests. I'm currently learning more about machine learning.

## Table of contents


1. [Apps](README.md#1-apps)
   1. [Meteorite Landings map](README.md#11-meteorite-landings-map)
   2. [Distribution fitter](README.md#12-distribution-fitter)
   3. [Scatter example](README.md#13-scatter-example-app)
   4. [Baby names](README.md#14-baby-names)
   5. [Name percent change over time](README.md#15-name-percent-change-over-time)
   6. [Farm products by state](README.md#16-us-state-family-farm-products)
   7. [Map of US family farms](README.md#17-us-family-farms)
2. [Packages](README.md#2-packages)
   1. [RdistUtils](README.md#21-rdistutils)
3. [Analyses](README.md#3-analyses)
   1. [Localharvest](README.md#31-localharvest-analysis)
   2. [Tomato inoculants](README.md#32-tomato-inoculants-analysis)
4. [Posters](README.md#4-posters)
   1. [Ants like it hot](README.md#41-ants-like-it-hot)
   2. [Tomato inoculants](README.md#42-tomato-inoculants-2023)


# 1. Apps

## 1.1. Meteorite landings map

An R Shiny app that uses meteorite data taken from [Data.gov](https://catalog.data.gov/dataset/meteorite-landings). It features a map using Leaflet, a couple of histogram, and a data viewer, and is themed with bootstrap via the R package bslib. It is published on shinyapps.io [here](https://zachpeagler.shinyapps.io/01_meteorites).

![App Screenshot](/Apps/01_meteorites/01_screenshot_map.png)

![App Screenshot 2](/Apps/01_meteorites/01_screenshot_plots.png)

For more information, [README](/Apps/01_meteorites/README.md).


## 1.2. Distribution fitter

A simple distribution fitting app. It takes a variable and fits it across a series of specified distributions. I made functions to fit both the probability density function (PDF) and cumulative distribution function (CDF) and returned both of them as interactive plots. I also made a function to perform a series of Kolmogorov-Smirnov tests against a chosen variable, which returns a table with the distance and pvalue for the desired distributions. These functions are now their own function RdistUtils, which has its own repo [here](github.com/zachpeagler/RdistUtils).

This distribution fitter is for **continuous** distributions only.

I used data from [Data.gov](https://catalog.data.gov/dataset/data-from-plant-strategies-for-maximizing-growth-during-drought-and-drought-recovery-in-so-98fae) from a paper titled "Plant strategies for maximizing growth during drought and drought recovery in Solanum melongena L. (eggplant)" and features data from a study about eggplant drought recovery that has several continuous biological response variables that can be used for fitting continuous distributions.

![Screenshot](Apps/02_distribution_fitter/02screenshot.png)

For more informaiton, see the [README](Apps/02_distribution_fitter/README.md).

## 1.3. Scatter example app
This is a simple interactive scatter plot app using the iris dataset included in R.

![Screenshot](/Apps/03_scatter/03screenshot.png)

For more information, see the [README](/Apps/03_scatter/README.md)

## 1.4. Baby names

For day four I created an app looking at the most popular baby names by year and gender as a bar plot. I also added a scatter plot showing the change in a name's popularity over time.

This required some extensive data wrangling, which can be found as its own script [here](/04_baby_names/data_wrangling.R).

![screenshot04_1](/Apps/04_baby_names/04_screenshot_scatter.png)

![screenshot04_2](/Apps/04_baby_names/04_screenshot_bar.png)

The data for this can be found [here](https://catalog.data.gov/dataset/baby-names-from-social-security-card-applications-national-data). This example data contains *all* the registered names in the United States with more than 5 occurrences since 1880. It was last updated on March 3, 2024.

#### From the National Data on U.S. Birth Name's readme

>For each year of birth YYYY after 1879, we created a comma-delimited file called yobYYYY.txt.
Each record in the individual annual files has the format "name,sex,number," where name is 2 to 15
characters, sex is M (male) or F (female) and "number" is the number of occurrences of the name.
Each file is sorted first on sex and then on number of occurrences in descending order. When there is
a tie on the number of occurrences, names are listed in alphabetical order. This sorting makes it easy to
determine a name's rank. The first record for each sex has rank 1, the second record for each sex has
rank 2, and so forth.
To safeguard privacy, we restrict our list of names to those with at least 5 occurrences. 

For more information, see the [README](/Apps/04_baby_names/README.md)

## 1.5. Name percent change over time
An app looking at the percent change by year of baby names in the United States.

## 1.6. US state family farm products

An R Shiny app using the tidyverse, sf, and data from localharvest and tigris (us census data). It displays a map of the US with each state filled in relative to the number of farms that produce a certain item. There are inputs for product type, palette choice, season, facet wrapping by season.

## 1.7. US family farms

WIP!!!

## 1.8. Tomato inoculants 2024

WIP!!!

An R Shiny app created as part of my Master's thesis at Kennesaw State University. It is a comprehensive analysis of a 7 month trial.

This uses R, the packages tidyverse, shiny, showtext, scico, bslib, MASS, and gpubr.

# 2. Packages

## 2.1 RdistUtils

An R package that helps with fitting multiple proportional density functions (PDFs), cumulative distribution functions (CDFs), and Kolmogorov-Smirnov tests.

Check it out [here.](https://github.com/zachpeagler/RdistUtils)

# 3. Analyses

## 3.1 Localharvest analysis

This analysis was done in conjunction with my Master's of Science in Integrative Biology at Kennesaw State University looking at the effect of microbes on tomato growth and crop production. 
As part of that project, I was looking to interview local farmers near me and in my search came upon localharvest.org, a website dedicated to providing small farmers a platform on which to connect with a larger audience, advertise crop share agreements, and more. 
Localharvest has far more farmers than I could possibly talk to, and while the data it contains was not exactly what I was looking for, it was a treasure trove of data and best of all - self-reported and publicly available online. 

This analysis ultimately spawned two R Shiny apps. [1.6. US state family farm products](README.md#16-us-state-family-farm-products), which uses ggplot2 and tigris, and [1.7. US family farms](README.md#17-us-family-farms) which uses leaflet. These apps represent two different ways to go about an analysis like this, with one being a static map with the data grouped by state, and the other a dynamic interactive map grouped by city.

### 3.1.1 LH_cleaning

An R markdown file that takes the data from LH_info_scraper and cleans it, turning it into a dataframe in tidy format with one value per cell, one sample (farm) per row, and one variable per column.

### 3.1.2 LH_info_scraper

A python script that scrapes information off of localharvest.org. This was done *slowly* so as to not overwhelm localharvest's servers.

The data for this was collectd ethically in accordance with localharvest.org's privacy policy and contains no personal information.

This project used multiple languages including R, Python, HTML, and CSS.

This particular analysis displays several skills, including
1. Scraping publicly available data off the internet
1. Cleaning badly formed data
2. Combining original data with public databases

## 3.2 Tomato inoculants analysis

Done as part of my Master's of Science in Integrative Biology at Kennesaw State University, this is a comprehensive set of analyses looking at four years of tomato inoculant data over various trials.

# 4. Posters

## 4.1 Ants like it hot

My first proper research presentation, made in 2021 during my Bachelor's Degree of Science in Biology at Kennesaw State University.

This poster was the result of my time in the Penick Lab, working under the incredible Clint Penick looking at ant biodiversity in urban areas along a latitudinal gradient on the US east coast.

![4.1poster](/Posters/UG_Poster.png)

## 4.2 Tomato inoculants 2023

The first poster of my Master's of Science in Integrative Biology from Kennesaw State University.

During my graduate degree I did research under the amazing Mario Bretfeld examining the effects of inoculation and salt stress on tomatoes.

![4.1poster](/Posters/TIP23_Poster.png)
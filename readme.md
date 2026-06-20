# Global Carbon Dioxide Analysis Dashboard README file
## Team 18: MTH208 Course Project

## Overview

This repository analyzes global patterns and trends in CO2 using Worldometers website data. It provides:

- An interactive Shiny dashboard for exploration and visualization.
- Reproducible scripts for data acquisition(via scraping), cleaning, and EDA.
- A comprehensive report about the entire project, explaining some of the code snippets too.

## Before we begin:

1. Install the required R packages :

shiny, ggplot2, dplyr, rvest

install.packages(c("shiny", "ggplot2", "dplyr", "rvest"))
 
2. Submitted files:

Project Report Team 18(pdf):A comprehensive report about the entire project
Cleaned_Data.csv(csv file): Dataset obtained after cleaning the scraped data.
scrapingCode(R script): This code scrapes out the data from the worldometers website, cleans it, and stores(by write.csv() at the end) the cleaned dataset mentioned above.
App.R(R Script): This is the code for the shiny app. At the begginning of the app, the code reads the same dataset that was stored at the end of the previous code.

IT IS REQUESTED TO ADJUST/SET THE WORKING DIRECTORY PROPERLY IN CASE THE SUBMITTED DATASET IS DOWNLOADED TO USE.


##DATA SOURCE:
Website: https://www.worldometers.info/co2-emissions/
Contains a table comprising data of the year 2022 on 206 countries about their CO2 emissions, population, per capita emissions, etc.


## Reproducibility

- R version used for development: R 4.3.2 (scripts should work on R >= 4.0).
- If the scraping and cleaning code has to be tested, it has to be run first, and then the app code. 

## Limitations & ethics

- Only aggregated, public datasets are used — no personal or sensitive data.
- Numerical differences and missing values can affect comparability; interpret figures with caution.
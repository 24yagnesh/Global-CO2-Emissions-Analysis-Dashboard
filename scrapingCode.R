#SCRAPING AND CLEANING CODE
library(rvest)
library(dplyr)
library(ggplot2)
library(shiny)

#------------------------------------------------------
# 1. SCRAPE DATA
#------------------------------------------------------
url  <- "https://www.worldometers.info/co2-emissions/"
page  <- read_html(url)
table_node  <- page %>% html_node("table")
co2_raw  <- table_node %>% html_table(fill = TRUE)

#------------------------------------------------------
# 2. CLEANING
#------------------------------------------------------
names(co2_raw)  <- tolower(gsub("\\.+", "_", make.names(names(co2_raw))))
if (!"country" %in% names(co2_raw)) names(co2_raw)[1]  <- "country"

clean_num  <- function(x) {
  x  <- gsub(",", "", x)
  x  <- gsub("%", "", x)
  x  <- gsub("−", "-", x)
  x  <- gsub("[^0-9\\.-]", "", x)
  suppressWarnings(as.numeric(x))
}

col_names  <- names(co2_raw)
em_col  <- col_names[grep("emission|co2", col_names, ignore.case = TRUE)][1]
pop_col  <- col_names[grep("population", col_names, ignore.case = TRUE)][1]
pc_col  <- col_names[grep("capita", col_names, ignore.case = TRUE)][1]
share_col  <- col_names[grep("share", col_names, ignore.case = TRUE)][1]
chg_col  <- col_names[grep("change", col_names, ignore.case = TRUE)][1]

carbon  <- co2_raw %>%
  mutate(
    emissions_tons = clean_num(get(em_col)),
    population_2022 = clean_num(get(pop_col)),
    per_capita_tons = clean_num(get(pc_col)),
    share_percent = clean_num(get(share_col)),
    change_percent = clean_num(get(chg_col)),
    country = gsub("^[0-9]+\\s+", "", country)
  ) %>%
  filter(!is.na(country) & !grepl("World|Total|Unknown|High-income", country, ignore.case = TRUE)) %>%
  mutate(
    emissions_per_million = emissions_tons / (population_2022 / 1e6),
    log_emissions = ifelse(emissions_tons > 0, log10(emissions_tons), NA),
    log_pop = ifelse(population_2022 > 0, log10(population_2022), NA)
  )
data=data.frame(carbon[,2],carbon[,8],carbon[,9],carbon[,10],carbon[,11],carbon[,12],carbon[,13])
data
head(data)
write.csv(data,"Cleaned_Data.csv")

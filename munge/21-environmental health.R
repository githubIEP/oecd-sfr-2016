# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/sfr model data/2016_epi_framework_indicator_scores_friendly.xls"
x <- read_excel(filename, "Indicator Scores")
x <- x[, 1:10] %>% select(Country, `Environmental Health`)
x$year <- 2016
x$variablename <- "Environmental Health"
x <- x %>% rename(iso3c = Country, value = `Environmental Health`)
x <- x[, c("iso3c", "variablename", "year", "value")]
raw.data$envhealth <- x
rmExcept("raw.data") 

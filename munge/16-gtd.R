# Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/sfr model data/GTI Score and Rank.xlsx"
gti <- read_excel(filename)
names(gti)[-1] <- gti[1, -1]
gti <- gti[-1, ]
gti <- gti[, 1:14]
gti <- gti %>% select(Country, `2014`)
names(gti) <- c("iso3c", "value")
gti$year <- 2014
indicators <- raw.data$log %>% filter(source == "IEP/START")
gti$variablename <- indicators$variablename
gti <- gti[, c("iso3c", "variablename", "year", "value")]
gti$iso3c <- country.code.name(gti$iso3c)
gti <- add.zeros.for.missing.values(gti, raw.data)
gti$iso3c <- country.code.name(gti$iso3c)
raw.data$gti <- gti
rmExcept("raw.data")
 

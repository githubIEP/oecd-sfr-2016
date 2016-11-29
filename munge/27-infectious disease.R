# Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% filter(source == "GBD and CSIS")
filename <- c("./data/sfr model data/infectious diseases.csv")
infdis <- read.csv(filename)
infdis <- infdis %>% filter(metric == "deaths")
infdis <- infdis %>% group_by(location_code, year) %>% summarise(value = sum(mean, na.rm = T))
infdis$variablename <- indicators$variablename[1]
infdis <- infdis %>% rename(iso3c = location_code)
infdis <- infdis[, c("iso3c", "variablename", "year", "value")]
infdis <- per.capita.calc(infdis)
infdis$iso3c <- country.code.name(infdis$iso3c)
raw.data$disease <- infdis
rmExcept("raw.data") 

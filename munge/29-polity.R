# Example preprocessing script.
source("./lib/funcs.R")
filename <- c("./data/sfr model data/poltyiv.xls")
polity <- read_excel(filename)
polity <- polity %>% dplyr::select(country, year, democ, autoc, polity2, durable)
polity <- polity %>% dplyr::rename(iso3c = country, democracy = democ, autocracy = autoc, polity = polity2)
polity <- polity %>% gather(variablename, value, -c(iso3c, year))

filename <- c("./data/sfr model data/polity4d.xls")
polity2 <- read_excel(filename)
polity2 <- polity2 %>% dplyr::select(country, eyear, persist)
polity2 <- polity2 %>% dplyr::filter(eyear == 9999)
polity2$eyear <- 2015
polity2 <- polity2 %>% dplyr::rename(iso3c = country, year = eyear, value = persist)
polity2$variablename <- "persistance"
# Guyana is included twice in this dataset
polity2 <- polity2 %>% group_by(iso3c, variablename, year) %>% summarise(value = min(value))

polity <- bind_rows(polity, polity2[, names(polity)])
polity <- polity %>% dplyr::filter(year > 2000, value >= 0)
polity$iso3c <- gsub("Korea North", "North Korea", polity$iso3c)
polity$iso3c <- gsub("Korea South", "South Korea", polity$iso3c)
raw.data$polity <- polity
rmExcept("raw.data") 

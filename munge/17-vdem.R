# Example preprocessing script.
filename <- "./data/sfr model data/vdem.csv"
source("./lib/funcs.R")
vdem <- read.csv(filename)

# drop older years, removes old countries Yemen's People Republic, USSR, Republic of Vietnam, Serbia
# and Montenegro.
vdem <- vdem %>% filter(year > 2000)
vdem$iso3c <- gsub("Congo_Democratic Republic of", "Democratic Republic Congo", vdem$iso3c)
vdem$iso3c <- gsub("Vietnam_Democratic Republic of", "Vietnam", vdem$iso3c)
vdem$iso3c <- gsub("Congo_Republic of the", "Democratic Republic Congo", vdem$iso3c)
vdem$iso3c <- gsub("Korea_North", "Korea North", vdem$iso3c)
vdem$iso3c <- gsub("Korea_South", "Korea South", vdem$iso3c)
vdem$iso3c <- gsub("Burma_Myanmar", "Burma Myanmar", vdem$iso3c)
pos <- grep("Palestine", vdem$iso3c)
vdem$iso3c[pos] <- "Palestine"
pos <- grep("Somali", vdem$iso3c)
vdem$iso3c[pos] <- "Somalia"
# vdem$iso3c2 = country.code.name(vdem$iso3c)
vdem <- most.recent(vdem)
# to account for Paletine being west bank and gaza strip and somalia and somaliland take the minimum
# number for either
vdem <- vdem %>% group_by(iso3c, variablename, year) %>% summarise(value = min(value))
# vdem$iso3c = country.code.name(vdem$iso3c)

raw.data$vdem <- vdem
rmExcept("raw.data")
 

# Example preprocessing script.
filename <- "./data/sfr model data/inform.xlsx"
inform <- read_excel(filename, "INFORM 2016 (a-z)")
names(inform) <- inform[1, ]
inform <- inform[-c(1:2), ]
inform <- inform %>% rename(iso3c = COUNTRY) %>% select(-ISO3) %>% gather(variablename, value, -iso3c)
inform <- inform %>% mutate(year = 2016) %>% select(iso3c, variablename, year, value)
indicators <- raw.data$log %>% filter(source == "INFORM")
inform <- inform %>% filter(variablename %in% indicators$variablename)
inform$iso3c <- gsub("Korea DPR", "North Korea", inform$iso3c)
raw.data$inform <- inform
rmExcept("raw.data") 

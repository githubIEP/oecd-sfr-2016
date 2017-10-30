# Example preprocessing script.
filename <- "./data/sfr model data/inform.xlsx"
inform <- read_excel(filename, "INFORM 2016 (a-z)")
inform <- inform[-c(1), ]
inform <- inform %>% dplyr::rename(iso3c = COUNTRY) %>% dplyr::select(-ISO3) %>% gather(variablename, value, -iso3c)
inform <- inform %>% mutate(year = 2016) %>% dplyr::select(iso3c, variablename, year, value)
indicators <- as.data.frame(raw.data$log) %>% dplyr::filter(source == "INFORM")
inform <- inform %>% dplyr::filter(variablename %in% indicators$variablename)
inform$iso3c <- gsub("Korea DPR", "North Korea", inform$iso3c)
raw.data$inform <- inform
rmExcept("raw.data") 

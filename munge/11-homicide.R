# Example preprocessing script.
indicators <- raw.data$log %>% filter(source == "UNODC")
filename <- "./data/sfr model data/homicide rate.xlsx"
homi <- read_excel(filename, "country-territory")
names(homi) <- tolower(homi[5, ])
names(homi)[7:ncol(homi)] <- homi[6, 7:ncol(homi)]
homi <- homi[-c(1:6), -c(1, 2, 4, 5)]
homi <- homi %>% filter(indicator == "Rate")
homi <- homi %>% rename(iso3c = `country/territory`) %>% select(-indicator)
homi <- homi %>% gather(year, value, -iso3c)
homi$variablename <- "Homicide Rate"
homi <- homi[, c("iso3c", "variablename", "year", "value")]
homi$year <- as.character((homi$year))
raw.data$homi <- homi
rmExcept("raw.data") 

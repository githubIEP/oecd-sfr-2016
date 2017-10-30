# Example preprocessing script.
indicators <- raw.data$log %>% dplyr::filter(source == "UNODC")
filename <- "./data/sfr model data/homicide rate.xlsx"
homi <- read_excel(filename, "country-territory")
names(homi) <- tolower(homi[2, ])
names(homi)[7:ncol(homi)] <- homi[3, 7:ncol(homi)]
homi <- homi[-c(1:3), -c(1, 2, 4, 5)]
homi <- homi %>% dplyr::filter(indicator == "Rate")
homi <- homi %>% dplyr::rename(iso3c = `country/territory`) %>% dplyr::select(-indicator)
homi <- homi %>% gather(year, value, -iso3c) %>% dplyr::filter(!is.na(value))
homi$variablename <- "Homicide Rate"
homi <- homi[, c("iso3c", "variablename", "year", "value")]
homi$year <- as.character((homi$year))
raw.data$homi <- homi
rmExcept("raw.data") 

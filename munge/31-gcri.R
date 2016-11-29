# Example preprocessing script.
filename <- "./data/sfr model data/gcri_data_v5.0.1.xlsx"
gcri <- read_excel(filename)
gcri <- gcri[, -1]

gcri <- gcri %>% rename(iso3c = COUNTRY, year = YEAR) %>% select(-ISO) %>% gather(variablename, value, 
    -c(iso3c, year))
indicators <- raw.data$log %>% filter(source == "GCRI")
gcri <- gcri %>% filter(variablename %in% indicators$variablename)
gcri <- gcri[, c("iso3c", "variablename", "year", "value")]
raw.data$gcri <- gcri
rmExcept("raw.data") 

# Example preprocessing script.
filename <- "./data/sfr model data/institutional profiles database.xlsx"
ipd <- read_excel(filename, "Indicators")
indicators <- raw.data$log %>% filter(source == "IPD")
pos <- which(names(ipd) %in% trim(strsplit(paste(indicators$variablename, collapse = ","), ",")[[1]]))
ipd <- ipd[, c(1:3, pos)]
ipd[2, -c(1:3)] <- names(ipd)[-c(1:3)]
names(ipd) <- ipd[2, ]
ipd <- ipd[-c(1:2), -1]
ipd <- ipd %>% rename(year = Year, iso3c = Country) %>% filter(complete.cases(year))

ipd <- ipd %>% gather(variablename, value, -c(iso3c, year))
ipd <- ipd[, c("iso3c", "variablename", "year", "value")]
raw.data$ipd <- ipd
rmExcept("raw.data") 

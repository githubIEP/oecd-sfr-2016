# Example preprocessing script.
filename <- "./data/sfr model data/corruption perceptions index.xlsx"
ti <- read_excel(filename, "CPI 2015")
ti <- ti[, 2:3]
ti$year <- 2015
names(ti) <- c("value", "iso3c", "year")
ti <- ti[-1, ]
indicators <- raw.data$log %>% filter(source == "TI")
ti$variablename <- unique(indicators$variablename)
ti <- ti[, c("iso3c", "variablename", "year", "value")]
raw.data$ti <- ti
rmExcept("raw.data") 

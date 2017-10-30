# Example preprocessing script.
filename <- "./data/sfr model data/corruption perceptions index.xlsx"
ti <- read_excel(filename, "CPI 2015")
ti <- ti[, 2:3]
ti$year <- 2015
names(ti) <- c("value", "iso3c", "year")
indicators <- raw.data$log %>% dplyr::filter(source == "TI")
ti$variablename <- unique(indicators$variablename)
ti$iso3c <- gsub("Korea \\(North\\)", "North Korea", ti$iso3c)
ti$iso3c <- gsub("Korea \\(South\\)", "South Korea", ti$iso3c)
ti <- ti[, c("iso3c", "variablename", "year", "value")]
raw.data$ti <- ti
rmExcept("raw.data") 

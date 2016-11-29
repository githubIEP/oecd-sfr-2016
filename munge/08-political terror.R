# Example preprocessing script.
filename <- "./data/sfr model data/political terror scale.xls"
pts <- read_excel(filename, "pts country-year, all")
names(pts) <- tolower(names(pts))
pts$amnesty <- as.numeric(pts$amnesty)
pts$`state dept` <- as.numeric(pts$`state dept`)
pts$hrw <- as.numeric(pts$hrw)
pts$value <- rowMeans(select(pts, amnesty, `state dept`, hrw), na.rm = T)
# drop older years, removes old countries North Yemen, South Yemen and USSR
pts <- pts %>% filter(year > 2000)
pts <- pts %>% filter(!(country %in% c("Israel and Occupied Territories*", "Israel in occupied territories only")))
pts$country <- gsub("Israel in pre-1967 borders", "Israel", pts$country)
pts$country <- trim(pts$country)
pts <- pts %>% select(country, year, value)
indicators <- raw.data$log %>% filter(source == "PTS")
pts$variablename <- unique(indicators$variablename)
pts$iso3c <- pts$country
pts <- pts[, c("iso3c", "variablename", "year", "value")]
raw.data$pts <- pts
rmExcept("raw.data") 

# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "WB")
filename <- "./data/sfr model data/gdp per capita.xls"
x <- read_excel(filename, "Data")
pos <- min(grep("Country Name", unlist(x[, 1])))
names(x) <- tolower(x[pos, ])
x <- x[-c(1:pos), ]
x <- x %>% dplyr::rename(iso3c = `country name`, variablename = `indicator name`)
x <- x[, -c(2, 4)]
x <- x %>% dplyr::filter(!is.na(`2014`))
x <- x %>% gather(year, value, -c(iso3c, variablename))
x$value <- as.numeric(x$value)
x <- x %>% dplyr::filter(year %in% c(2010, 2014))
x <- x %>% spread(year, value)
x$value <- (x$`2014`/x$`2010`)^(1/5) - 1
x$year <- 2014
x <- x %>% dplyr::filter(!grepl("Sub-Saharan Africa", iso3c))
x$country <- x$iso3c
x <- most.recent(x)
x$iso3c <- country.code.name(x$iso3c)
x$value <- as.numeric(x$value)
x$iso3c <- country.code.name(x$iso3c)
x$variablename <- paste(x$variablename, "CAGR 2010-2014")
x <- x[, c("iso3c", "variablename", "year", "value")]
x <- x %>% dplyr::filter(complete.cases(.))
raw.data$gdpgrowth <- x
rmExcept("raw.data") 

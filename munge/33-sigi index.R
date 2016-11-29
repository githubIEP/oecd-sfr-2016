# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% filter(source == "OECD")
filename <- "./data/sfr model data/sigi.xlsx"
x <- read_excel(filename)
x <- x %>% select(Country, `Restricted physical integrity Value`)
x$year <- 2014
x$variablename <- "sigi"
x <- x %>% rename(iso3c = Country, value = `Restricted physical integrity Value`)
x <- x[, c("iso3c", "variablename", "year", "value")]
x <- x %>% filter(!is.na(value))
x$iso3c <- country.code.name(x$iso3c)
oecd.countries <- read_excel("./data/additional data/OECD countries.xlsx")
oecd.countries$iso3c <- country.code.name(oecd.countries$Country)
oecd.countries.out <- setdiff(oecd.countries$iso3c, x$iso3c)
oecd.countries.in <- intersect(oecd.countries$iso3c, x$iso3c)
pos <- which(x$iso3c %in% oecd.countries.in)
average <- mean(x$value[pos])
oecd.countries.out <- data.frame(iso3c = oecd.countries.out, variablename = "sigi", year = 2014, value = average)
x <- rbind(x, oecd.countries.out)
x$iso3c <- country.code.name(x$iso3c)
raw.data$sigi <- x
rmExcept("raw.data") 

# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% filter(source == "UNDP/HDI Gender Inequality")
filename <- "./data/sfr model data/2015_statistical_annex_tables_all.xls"
x <- read_excel(filename, "Table 5")
pos <- c(min(grep("Country", unlist(x[8, ]))), min(grep("Gender", unlist(x[5, ]))), min(grep("Maternal", 
    unlist(x[5, ]))), min(grep("seats", unlist(x[5, ]))), min(grep("Labour", unlist(x[5, ]))) - 2, min(grep("Labour", 
    unlist(x[5, ]))))
x <- x[, pos]

names(x) <- c("iso3c", trim(unlist(x[5, ])[-1]))
names(x)[5:6] <- c(paste(names(x)[6], "Male"), paste(names(x)[6], "Female"))
x <- x[-c(1:(grep("Norway", x$iso3c) - 1)), ]
x <- x[1:grep("Tuvalu", x$iso3c), ]
x$iso3c <- country.code.name(x$iso3c)
x$iso3c <- country.code.name(x$iso3c)
x <- x %>% gather(variablename, value, -iso3c)
x$year <- 2014
x$value <- as.numeric(x$value)
x <- x %>% filter(!is.na(value))
x <- x[, c("iso3c", "variablename", "year", "value")]
raw.data$gender <- x
rmExcept("raw.data") 

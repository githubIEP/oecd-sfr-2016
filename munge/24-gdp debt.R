# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% filter(source == "WB")
filename <- "./data/sfr model data/imf debt.aspx"
x <- read.delim(filename)
x$X2014 <- as.numeric(x$X2014)
x <- x %>% filter(!grepl(" net debt", Subject.Descriptor), !is.na(X2014))
x[, 1] <- iconv(x[, 1], "latin1", "UTF-8")
x <- x %>% rename(iso3c = Country, variablename = Subject.Descriptor, year = Estimates.Start.After, value = X2014)
x <- x[, c("iso3c", "variablename", "year", "value")]
x <- x %>% filter(complete.cases(.))
raw.data$gdpdebt <- x
rmExcept("raw.data") 

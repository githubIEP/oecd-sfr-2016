# Example preprocessing script.
filename <- "./data/sfr model data/world governance indicators.xlsx"
wgi <- excel_sheets(filename)
wgi <- wgi[-c(1, length(wgi))]
wgi <- sapply(wgi, FUN = function(x) {
    read_excel(filename, x)
}, simplify = FALSE, USE.NAMES = TRUE)
wgi <- sapply(names(wgi), function(i) {
    x <- wgi[[i]]
    pos <- which(x[, 1] == "Country/Territory")
    x <- x[-(1:(pos - 2)), ]
    x <- x[, -2]
    x <- as.data.frame(t(x))
    x[1, 1] <- "year"
    names(x) <- x[1, ]
    x <- x[-1, ]
    x <- x[, !is.na(names(x))]
    x <- x %>% dplyr::rename(type = `Country/Territory`) %>% dplyr::filter(type == "Estimate")
    x <- x %>% gather(country, value, -c(type, year))
    x$country <- gsub("KOREA, DEM\\. REP\\.", "North Korea", x$country)
    x <- x %>% dplyr::select(-type) %>% mutate(iso3c = country)
    x <- x %>% mutate(variablename = i)
    
    x <- x[, c("iso3c", "variablename", "year", "value")]
    x$year <- as.numeric(x$year)
    return(x)
}, simplify = FALSE, USE.NAMES = TRUE)
wgi <- bind_rows(wgi)
indicators <- raw.data$log %>% dplyr::filter(source == "WGI")
wgi <- wgi %>% dplyr::filter(variablename %in% indicators$variablename)
raw.data$wgi <- wgi
rmExcept("raw.data") 

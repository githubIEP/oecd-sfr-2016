# Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "WB")
filename <- c("./data/sfr model data/neet.csv")
wb <- lapply(filename, function(i) {
    x <- read.csv(i)
    pos <- min(grep("Country", x[, 1]))
    names(x) <- tolower(x[pos, ])
    x <- x[-c(1:pos), ]
    x <- x %>% dplyr::rename(iso3c = `country name`, variablename = `indicator name`)
    x <- x[, -c(2, 4)]
    x <- x %>% gather(year, value, -c(iso3c, variablename))
    x <- x[, c("iso3c", "variablename", "year", "value")]
    x <- x %>% dplyr::filter(!grepl("Sub-Saharan Africa", iso3c))
    return(x)
})
wb <- bind_rows(wb)
wb <- wb %>% dplyr::filter(complete.cases(value))
wb$iso3c <- country.code.name(country.code.name(wb$iso3c))
wb <- wb %>% dplyr::filter(complete.cases(iso3c))

#### Add OECD Data
oecd <- read.csv("./data/sfr model data/oecd_neet.csv")
oecd$Value <- as.numeric(oecd$Value)
oecd <- oecd %>% dplyr::filter(complete.cases(Value))
names(oecd)[1] <- "LOCATION"
oecd <- oecd %>% group_by(LOCATION, TIME) %>% summarise(value = mean(Value, na.rm = T))
oecd$variablename <- wb$variablename[1]
oecd <- oecd %>% dplyr::rename(iso3c = LOCATION, year = TIME)
oecd$iso3c <- (country.code.name(oecd$iso3c))
temp <- setdiff(unique(oecd$iso3c), unique(wb$iso3c))
oecd <- oecd %>% dplyr::filter(iso3c %in% temp)
wb <- bind_rows(wb, oecd)

### Supplement with WDR Job data
neet.wdr <- read_excel("./data/sfr model data/WDR neet.xlsx")
names(neet.wdr) <- paste(unlist(neet.wdr[2, ]), letters[1:7], sep = "")
neet.wdr <- neet.wdr[-c(1:2), ]
names(neet.wdr)[1] <- "iso3c"
neet.wdr <- neet.wdr %>% gather(year, value, -iso3c)
neet.wdr$year <- as.numeric(substr(neet.wdr$year, 1, 4))
neet.wdr$value <- as.numeric(neet.wdr$value)
neet.wdr <- neet.wdr %>% dplyr::filter(!is.na(value))
neet.wdr <- neet.wdr %>% group_by(iso3c, year) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
neet.wdr$iso3c <- country.code.name(country.code.name(neet.wdr$iso3c))
temp <- setdiff(unique(neet.wdr$iso3c), unique(wb$iso3c))
temp <- neet.wdr %>% dplyr::filter(iso3c %in% temp)
temp$variablename <- wb$variablename[1]
wb <- rbind(wb, temp[, names(wb)])

raw.data$neet <- wb
rmExcept("raw.data") 

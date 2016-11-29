# Example preprocessing script.
source("./lib/funcs.R")
load("./data/sfr model data/124934_1ucdp-brd-conflict-2015.rdata")

ucdp <- ucdp.brd
# if Best estiamte is missing use low estimate
ucdp$BdBest[ucdp$BdBest < 0] <- ucdp$BdLow[ucdp$BdBest < 0]
# Take care of countries with multiple locations
ucdp$CountryCount <- stringr::str_count(as.character(ucdp$LocationInc), ",")
ToSplit <- subset(ucdp, CountryCount > 0)
Conflictsub <- subset(ucdp, CountryCount == 0)  # no need to split
if (nrow(ToSplit) + nrow(Conflictsub) != nrow(ucdp)) warning("DANGER DANGER")
if (sum(ucdp$CountryCount) > 0) {
    for (i in 1:nrow(ToSplit)) {
        Splits <- ToSplit[rep(rownames(ToSplit[i, ]), ToSplit$CountryCount[i] + 1), ]
        Splits$LocationInc <- stringr::str_split(ToSplit$LocationInc[i], ", ")[[1]]
        Conflictsub <- rbind(Conflictsub, Splits)
    }
}
ucdp <- Conflictsub[, c("LocationInc", "Year", "BdBest")] %>% filter(Year == max(Year))
# aggregate by country
ucdp <- ucdp %>% group_by(LocationInc) %>% summarise(value = sum(BdBest, na.rm = T))
# column names
ucdp <- as.data.frame(ucdp)
names(ucdp) <- tolower(names(ucdp))
ucdp <- ucdp %>% rename(location = locationinc)
ucdp$location <- gsub("Yemen \\(North Yemen\\)", "Yemen", ucdp$location)
ucdp$iso3c <- country.code.name(ucdp$location)
# sum up deaths in the past 3 years
ucdp <- ucdp %>% group_by(iso3c) %>% summarise(value = mean(value))
# need to get populations
ucdp$year <- max(ucdp.brd$Year)
ucdp <- per.capita.calc(ucdp)
ucdp$value <- log(ucdp$value + 1)
# ucdp$value[ucdp$iso3c == 'SYR'] = rev(sort(ucdp$value))[2] syria = ucdp$iso3c == 'SYR'
# ucdp$value[syria] = 1.2 * sort(ucdp$value, decreasing = T)[2] get variablename
indicators <- raw.data$log %>% filter(source == "UCDP-BD")
ucdp$variablename <- indicators$variablename
ucdp <- ucdp[, c("iso3c", "variablename", "year", "value")]
#### add zeros for missing values
ucdp <- add.zeros.for.missing.values(ucdp, raw.data)
###### 
ucdp$iso3c <- country.code.name(ucdp$iso3c)
raw.data$bd <- ucdp

years.of.peace <- Conflictsub %>% group_by(LocationInc) %>% summarise(last.yr = max(Year))
years.of.peace$LocationInc <- gsub("Yemen \\(North Yemen\\)", "Yemen", years.of.peace$LocationInc)
years.of.peace$iso3c <- country.code.name(country.code.name(as.character(years.of.peace$LocationInc)))
years.of.peace <- years.of.peace %>% select(-LocationInc) %>% group_by(iso3c) %>% summarise(last.yr = max(last.yr))
ucdp <- left_join(ucdp, years.of.peace)
ucdp$value <- ucdp$year - ucdp$last.yr
ucdp$value[is.na(ucdp$value)] <- max(ucdp$year) - 1945
ucdp <- ucdp[, c("iso3c", "variablename", "year", "value")]
ucdp$variablename <- "Years Since In Country Battledeath"
raw.data$years.of.peace <- ucdp
rmExcept("raw.data") 

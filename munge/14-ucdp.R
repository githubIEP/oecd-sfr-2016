# Example preprocessing script.
source("./lib/funcs.R")
load("./data/sfr model data/non state actors.rdata")

ucdp <- ucdpNonState

# Take care of countries with multiple locations
ucdp$CountryCount <- stringr::str_count(as.character(ucdp$location), ",")
ToSplit <- subset(ucdp, CountryCount > 0)
Conflictsub <- subset(ucdp, CountryCount == 0)  # no need to split
if (nrow(ToSplit) + nrow(Conflictsub) != nrow(ucdp)) warning("DANGER DANGER")
if (sum(ucdp$CountryCount) > 0) {
    for (i in 1:nrow(ToSplit)) {
        Splits <- ToSplit[rep(rownames(ToSplit[i, ]), ToSplit$CountryCount[i] + 1), ]
        Splits$location <- stringr::str_split(ToSplit$location[i], ", ")[[1]]
        Conflictsub <- rbind(Conflictsub, Splits)
    }
}

ucdp <- Conflictsub[, c("location", "year", "bestfatalityestimate")] %>% filter(year == max(year))
# aggregate by country
ucdp <- ucdp %>% group_by(location) %>% summarise(value = sum(bestfatalityestimate, na.rm = T))

ucdp$year <- max(Conflictsub$year)
# column names
ucdp <- as.data.frame(ucdp)
names(ucdp) <- tolower(names(ucdp))
ucdp$location <- gsub("Yemen \\(North Yemen\\)", "Yemen", ucdp$location)
ucdp$iso3c <- country.code.name(ucdp$location)

ucdp <- per.capita.calc(ucdp)
# ucdp$value = log(ucdp$value+1) ucdp$value[ucdp$iso3c == 'SYR'] = rev(sort(ucdp$value))[2] get
# variablename
indicators <- raw.data$log %>% filter(source == "UCDP-NS")
ucdp$variablename <- indicators$variablename
ucdp <- ucdp[, c("iso3c", "variablename", "year", "value")]
#### add zeros for missing values
ucdp <- add.zeros.for.missing.values(ucdp, raw.data)
ucdp$iso3c <- country.code.name(ucdp$iso3c)
raw.data$ucdp <- ucdp

rmExcept("raw.data") 

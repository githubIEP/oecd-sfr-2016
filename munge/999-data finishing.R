source("./lib/funcs.R")
sfr.log <- raw.data$log %>% select(reportname, variablename, dimension, type, doesmoreincreasefragility, 
    include)
old.raw <- raw.data
raw.data <- raw.data[-1]
raw.data <- lapply(raw.data, function(x) {
    x$iso3c <- as.character(x$iso3c)
    x$variablename <- as.character(x$variablename)
    x$year <- as.numeric(as.character(x$year))
    x$value <- as.numeric(as.character(x$value))
    return(x)
})
raw.data <- bind_rows(raw.data)
raw.data$iso3c <- trim(raw.data$iso3c)

# fix missing country codes
raw.data <- most.recent(raw.data)
raw.data$iso3c <- str_replace_all(raw.data$iso3c, "[[:punct:]]", "")
raw.data$country <- raw.data$iso3c
raw.data$iso3c <- country.code.name(raw.data$iso3c)

raw.data <- raw.data %>% filter(complete.cases(value))
pos <- is.na(raw.data$iso3c)
unique(raw.data$country[pos])
pos <- grepl("sudan", tolower(raw.data$country)) & is.na(raw.data$iso3c)
raw.data$iso3c[pos] <- "SSD"
pos <- grepl("serbia", tolower(raw.data$country)) & is.na(raw.data$iso3c)
raw.data$iso3c[pos] <- "SRB"
raw.data <- raw.data %>% filter(!is.na(iso3c))
raw.data$country <- country.code.name(raw.data$iso3c)
pos <- is.na(raw.data$country)


# filter data for index
raw.data <- left_join(raw.data, sfr.log)
raw.data <- ungroup(raw.data)
raw.data <- raw.data %>% filter(include == 1)
raw.data <- raw.data %>% select(iso3c, country, dimension, type, reportname, year, value, doesmoreincreasefragility)
raw.data <- raw.data %>% rename(variablename = reportname)


# #####test that data has formatted properly #i.e. one data point per country-variablename
df <- raw.data %>% select(iso3c, variablename, value) %>% distinct()
df <- df %>% group_by(iso3c, variablename) %>% summarise(n = n()) %>% ungroup() %>% arrange(desc(n))
expect_that(max(df$n), equals(1))

#### There are some double entries for countries in Political Terror Scales and Formal Alliances data
#### This is fine to take avearges of because the scores are the same in the duplications raw.data2 =
#### raw.data %>% group_by(iso3c, dimension, type, variablename, year, doesmoreincreasefragility) %>%
#### summarise(value = mean(value, na.rm=T)) %>% ungroup() test that there are no duplicates
pos <- duplicated(raw.data)
expect_that(sum(duplicated(raw.data)), equals(0))
###### remove countries with less than threshold percentage of data
threshold <- 0.7
availability <- as.data.frame(table(raw.data$iso3c, raw.data$variablename))
availability <- availability %>% group_by(Var1) %>% summarise(n = n(), missing = sum(Freq == 0)/n())
availability <- availability %>% filter(1 - missing >= threshold) %>% ungroup()
raw.data <- raw.data %>% filter(iso3c %in% as.character(availability$Var1))

# invert indictors to be in the same direction
pos <- raw.data$doesmoreincreasefragility == 0
raw.data$value[pos] <- -raw.data$value[pos]
# raw.data.bands = raw.data %>% group_by(variablename) %>% summarise(min.band = max(min(value),
# quantile(value)[1] - 1.5*IQR(value)), max.band = min(max(value), quantile(value)[3] +
# 1.5*IQR(value))) data.matrix = split(raw.data, factor(raw.data$dimension)) data.matrix =
# lapply(names(data.matrix), function(i){ x = data.matrix[[i]] x = x %>% select(iso3c, variablename,
# year) %>% distinct() x$year = as.character(x$year) x= x %>% spread(variablename, year) pos =
# is.na(x) x[pos] = '*' write.csv(x, paste('./data_out/indicator-country-years-', i,'.csv', sep =
# ''), row.names = F) return(x) })

data.matrix <- raw.data %>% group_by(variablename) %>% summarise(min.year = min(year), max.year = max(year), 
    num.countries = length(unique(iso3c)))
write.csv(data.matrix, paste("./data_out/indicator-country-years.csv", sep = ""), row.names = F)
# impute and perform pca transformation
raw.data <- impute(raw.data)
# 
raw.data$country <- oecd.country.name(raw.data$iso3c, short = T)
raw.data$country <- iconv(raw.data$country, "latin1", "UTF-8")
rmExcept("raw.data") 

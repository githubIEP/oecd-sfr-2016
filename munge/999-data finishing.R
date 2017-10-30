source("./lib/funcs.R")
save(list = ls(), file = "./cache/errortesting/errortesting.RData")
load("./cache/errortesting/errortesting.RData")

sfr.log <- raw.data$log %>% dplyr::select(reportname, variablename, dimension, type, doesmoreincreasefragility, 
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
#country.code.check = raw.data %>% select(country, iso3c) %>% distinct()
raw.data <- raw.data %>% dplyr::filter(complete.cases(value))
pos <- is.na(raw.data$iso3c)
unique(raw.data$country[pos])
pos <- grepl("sudan", tolower(raw.data$country)) & is.na(raw.data$iso3c)
raw.data$iso3c[pos] <- "SSD"
pos <- grepl("serbia", tolower(raw.data$country)) & is.na(raw.data$iso3c)
raw.data$iso3c[pos] <- "SRB"
raw.data <- raw.data %>% dplyr::filter(!is.na(iso3c))
raw.data$country <- country.code.name(raw.data$iso3c)

# filter data for index
raw.data <- left_join(raw.data, sfr.log)
raw.data <- ungroup(raw.data)
raw.data <- raw.data %>% dplyr::filter(include == 1)
raw.data <- raw.data %>% dplyr::select(iso3c, country, dimension, type, reportname, year, value, doesmoreincreasefragility)
raw.data <- raw.data %>% dplyr::rename(variablename = reportname)



# #####test that data has formatted properly #i.e. one data point per country-variablename
df <- raw.data %>% dplyr::select(iso3c, variablename, value) %>% distinct()
df <- df %>% group_by(iso3c, variablename) %>% summarise(n = n()) %>% ungroup() %>% arrange(desc(n))
expect_that(max(df$n), equals(1))

#### There are some double entries for countries in Political Terror Scales and Formal Alliances data
#### This is fine to take avearges of because the scores are the same in the duplications raw.data2 =
#### raw.data %>% group_by(iso3c, dimension, type, variablename, year, doesmoreincreasefragility) %>%
#### summarise(value = mean(value, na.rm=T)) %>% ungroup() test that there are no duplicates
pos <- duplicated(raw.data)
expect_that(sum(duplicated(raw.data)), equals(0))

#it could be that LUX and PRK are not being picked or OR thresh = 0.5
#need to check this!
load("./cache/2016/raw.data-oecd-2016.RData")
temp = sfr2016 %>% dplyr::filter(!is.na(value))
pos.in.last.year.not.this.year = paste(temp$iso3c, temp$variablename)[!((paste(temp$iso3c, temp$variablename) 
                                                                               %in% paste(raw.data$iso3c, raw.data$variablename)))]

pos.in.this.year.not.last.year = paste(raw.data$iso3c, raw.data$variablename)[!((paste(raw.data$iso3c, raw.data$variablename) 
                                                                                 %in% paste(temp$iso3c, temp$variablename)))]


###### remove countries with less than threshold percentage of data
threshold <- 0.7
availability <- as.data.frame(table(raw.data$iso3c, raw.data$variablename))
availability <- availability %>% group_by(Var1) %>% dplyr::summarise(n = n(), missing = sum(Freq == 0)/n())
availability <- availability %>% dplyr::filter(1 - missing >= threshold) %>% ungroup()
raw.data <- raw.data %>% dplyr::filter(iso3c %in% as.character(availability$Var1))

# invert indictors to be in the same direction
pos <- raw.data$doesmoreincreasefragility == 0
raw.data$value[pos] <- -raw.data$value[pos]

data.matrix <- raw.data %>% group_by(variablename) %>% summarise(min.year = min(year), max.year = max(year), 
    num.countries = length(unique(iso3c)))
write.csv(data.matrix, paste("./data_out/indicator-country-years.csv", sep = ""), row.names = F)

# impute and perform pca transformation
raw.data <- impute(raw.data)
# 
raw.data$country <- oecd.country.name(raw.data$iso3c, short = T)
raw.data$country <- iconv(raw.data$country, "latin1", "UTF-8")

rmExcept("raw.data") 


ipak <- function(pkg) {
    # usage packages <- c('ggplot2', 'plyr', 'reshape2', 'RColorBrewer', 'scales', 'grid') ipak(packages)
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


packages <- c("ProjectTemplate", "plyr", "rworldmap", "rgdal", "maptools", "gridExtra",  "DataCombine",
              "gdata", "readxl", "countrycode", "testthat", "tools", "caret", "RColorBrewer", "FactoMineR", "scales",
              "WDI", "VIM", "tidyverse")
ipak(packages)


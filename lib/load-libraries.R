ipak <- function(pkg) {
    # usage packages <- c('ggplot2', 'plyr', 'reshape2', 'RColorBrewer', 'scales', 'grid') ipak(packages)
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


packages <- c("ProjectTemplate", "plyr", "rworldmap", "rgdal", "maptools", "gridExtra", "ggplot2", "stringr", "DataCombine",
              "gdata", "readxl", "countrycode", "testthat", "tools", "caret", "RColorBrewer", "FactoMineR", "scales",
              "WDI", "dplyr", "tidyr", "VIM")
ipak(packages)

# library(ProjectTemplate)
# library(plyr)
# library(rworldmap)
# library(maptools)
# library(ggplot2)
# library(stringr)
# library(DataCombine)
# library(gdata)
# library(countrycode)
# library(readxl)
# library(testthat)
# library(tools)
# library(caret)
# library(RColorBrewer)
# library(FactoMineR)
# library(scales)
# library(WDI)
# library(VIM)
# library(dplyr)
# library(tidyr)
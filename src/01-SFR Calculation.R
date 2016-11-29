#### States of Fragility Report 2016 - OECD 
# Written by David Hammond Institute for Economics and Peace
# 28 May 2016 This script: 
# (1) processes all original data files into a standard tabular format and
# (2) calculates the SFR 2016 rankings
####

source("./lib/load-libraries.R")
rm(list = ls())
output_folders <- c("./data_out/", "./graphs")
lapply(output_folders, function(x) file.remove(list.files(x, full.names = T)))
options(stringsAsFactors = FALSE)
options(useFancyQuotes = "UTF-8")
reload.project(override.config = list(munging = T))
cache("raw.data")
set.seed(12345)
source("./lib/funcs.R")


##### Step 1 #### Set parameters for calculations 
# 1. The fragile clusters 
# 2. the number of clusters in each dimension 
# 3. whether to drop highly correlated indicators in each dimension 
# 4. The cluster method, ward.d2 selected as the simplest to explain

fragile.clusters <- list(Environmental = c("A", "B", "C", "D"), Political = c("A", "B", "C", "D"), Economic = c("A", 
    "B", "C", "D", "E", "F"), Security = c("A", "B", "C", "D", "E", "F"), Societal = c("A", "B", "C", 
    "E", "D"))
fragile.levels <- read_excel("./data/additional data/dimensional fragility.xlsx")
fragile.levels$join.on <- paste(fragile.levels$dimension, fragile.levels$cluster)

num.clusters <- list(Environmental = 8, Political = 8, Economic = 8, Security = 8, Societal = 8)


drop.indicators.based.on.correlations <- T
cluster.method <- "ward.D2"
pca.axis.labels <- read_excel("./data/additional data/pca axis labels.xlsx")
pca.axis.labels <- split(pca.axis.labels, factor(pca.axis.labels$dimension))
all.drops <- NULL
round.numbers <- T

################### Step 2 #### Calculate PCA for each dimension and plot

all.distances <- NULL
all.dimension.pca.metrics <- list()
counter <- 3
all.results <- sapply(sort(unique(raw.data$dimension)), function(i) {
    # take dimension subset of raw.data
    temp <- raw.data %>% filter(dimension == i)
    temp <- temp %>% select(iso3c, type, variablename, imputed)
    # rename for the bi-plots
    pos <- temp$type == "Coping"
    temp$variablename[pos] <- paste(temp$variablename[pos], " (C)", sep = "")
    temp$variablename[!pos] <- paste(temp$variablename[!pos], " (R)", sep = "")
    # create panel data
    temp <- temp %>% select(-type) %>% distinct() %>% spread(variablename, imputed)
    # drop highly correllated indicators
    drops <- findCorrelation(cor(temp[, -1]))
    if (length(drops) > 0 & drop.indicators.based.on.correlations) {
        all.drops <<- rbind(all.drops, data.frame(dimension = i, indicators = names(temp)[drops + 1]))
        data.frame(dimension = i, indicators = names(temp)[drops + 1])
        temp <- temp[, -(drops + 1)]
    }
    # calculate a pca
    pca <- prcomp(temp[, -1], center = TRUE, scale. = TRUE)
    all.dimension.pca.metrics <<- c(all.dimension.pca.metrics, list(PCA(temp[, -1], graph = F)))
    names(all.dimension.pca.metrics)[length(all.dimension.pca.metrics)] <<- i
    # switch diriction for ease of reading
    somalia <- which(temp$iso3c == "SOM")
    iceland <- which(temp$iso3c == "ISL")
    if (pca$x[somalia, 1] > pca$x[iceland, 1]) {
        pca <- prcomp(-temp[, -1], center = TRUE, scale. = TRUE)
        pca$x <- -pca$x
    }
    if (round.numbers) {
        pca$x <- apply(pca$x, 2, round, digits = 2)
    }
    # create a data frame of the first two principal components
    tmp <- data.frame(iso3c = temp$iso3c, pca$x[, 1:2])
    
    distance <- dist(tmp[, -1], method = "euclidean")
    if (round.numbers) {
        distance <- round(dist(tmp[, -1], method = "euclidean"), digits = 2)
    }
    d2 <- as.data.frame(as.matrix(distance))
    d2$col <- rownames(d2)
    d2 <- d2 %>% gather(row, dist, -col)
    all.distances <<- rbind(all.distances, data.frame(dimension = i, d2))
    
    clusters <- hclust(distance, method = cluster.method)
    clusters <- cutree(clusters, num.clusters[[i]])
    # calculate centroids for the visualisation
    centroids <- as.data.frame(apply(tmp[, -1], 2, function(x) tapply(x, clusters, mean)))
    centroids$dist <- with(centroids, sqrt(PC1^2 + PC2^2))
    centroids$omega <- atan(centroids$PC2/centroids$PC1)
    centroids$cut <- LETTERS[1:nrow(centroids)]
    centroids$rank <- rank(centroids$PC1)
    centroids$labels <- LETTERS[centroids$rank]
    tmp$cut <- LETTERS[clusters]
    tmp <- left_join(tmp, select(centroids, cut, labels))
    # create bi-plot
    p <- ggbiplot.sfr(pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = F, labels = tmp$iso3c, 
        groups = tmp$labels, var.axes = T, coloured.clusters = fragile.clusters[[i]])
    p <- p + theme(legend.position = "none")
    p <- p + ggtitle(i)
    p <- p + geom_text(data = centroids, aes(x = PC1, y = PC2, label = labels))
    p <- oecd.biplot(p, coloured.clusters = fragile.clusters[[i]], n = num.clusters[[i]])
    xlabel <- paste(pca.axis.labels[[i]]$x) 
    ylabel <- paste(pca.axis.labels[[i]]$y) 
    p <- p + xlab(xlabel) + ylab(ylabel)
    ggsave(p, filename = paste("./graphs/Fig A", counter, " cluster ", i, ".pdf", sep = ""), height = 8, 
        width = 10)
    counter <<- counter + 1
    tmp$value <- tmp$labels
    tmp2 <- tmp
    return(tmp)
}, USE.NAMES = T, simplify = F)


contrib <- lapply(all.dimension.pca.metrics, function(x) {
    return(cbind(variablename = rownames(x$var$contrib), as.data.frame(apply(x$var$contrib[, 1:2], 2, 
        round, digits = 2))))
})

contrib <- bind_rows(contrib, .id = "dimension")
contrib <- contrib %>% rename(contrib.to.dim1 = Dim.1, contrib.to.dim2 = Dim.2)
cos2 <- lapply(all.dimension.pca.metrics, function(x) {
    return(cbind(variablename = rownames(x$var$cos2), as.data.frame(apply(x$var$cos2[, 1:2], 2, round, 
        digits = 2))))
})

cos2 <- bind_rows(cos2, .id = "dimension")
cos2 <- cos2 %>% rename(cos2.to.dim1 = Dim.1, cos2.to.dim2 = Dim.2)
cor <- lapply(all.dimension.pca.metrics, function(x) {
    return(cbind(variablename = rownames(x$var$cor), as.data.frame(apply(x$var$cor[, 1:2], 2, round, 
        digits = 2))))
})

cor <- bind_rows(cor, .id = "dimension")
cor <- cor %>% rename(cor.to.dim1 = Dim.1, cor.to.dim2 = Dim.2)

cor <- left_join(contrib, cor)
cor <- left_join(cor, cos2)
cor <- cor[, c(1:3, 5, 7, 4, 6, 8)]
write.csv(cor, "./data_out/dimensional pca contributions.csv", row.names = F)


################### Step 3 #### Create a data frame listing 
# which clusters are fragile

fragile.clusters <- sapply((names(all.results)), function(i) {
    all.results[[i]] %>% filter(labels %in% fragile.clusters[[i]])
}, USE.NAMES = T, simplify = F)
results <- bind_rows(fragile.clusters, .id = ".id")
results <- as.data.frame.matrix(table(results$iso3c, results$.id))
results$total <- rowSums(results)
results$iso3c <- rownames(results)
results <- results %>% arrange(desc(total))
results <- results %>% MoveFront("iso3c")
temp <- data.frame(setdiff(unique(raw.data$iso3c), results$iso3c), 0, 0, 0, 0, 0, 0)
names(temp) <- names(results)
results <- bind_rows(results, temp)
results <- results %>% rename(value = total)
results$value <- as.character(results$value)
all.clusters <- bind_rows(all.results, .id = ".id")
all.clusters$join.on <- paste(all.clusters$.id, all.clusters$labels)
all.clusters <- all.clusters %>% select(-c(value, cut))
# join clusters with fragility levels
cluster.descriptions <- left_join(select(all.clusters, iso3c, join.on), fragile.levels)
# generate output dimensions fragility levels
fragility <- cluster.descriptions %>% select(iso3c, dimension, cluster, fragility) %>% distinct()
fragility$country <- oecd.country.name(fragility$iso3c, short = T)

# create a raw data output file
temp <- raw.data %>% select(country, dimension, type, variablename, imputed)
temp$variablename <- paste("[", temp$dimension, "] [", temp$type, "] ", temp$variablename, sep = "")
temp <- temp %>% distinct() %>% arrange(variablename) %>% select(-c(dimension, type)) %>% spread(variablename, 
    imputed)

temp[, -1] <- apply(temp[, -1], 2, scale)


write.csv(all.clusters, "./data_out/principal components.csv", row.names = F)

rmExcept(keepers = c("raw.data", "all.clusters", "fragility", "cluster.method", "results", "pca.axis.labels", 
    "cluster.descriptions", "fragile.levels", "all.results"))

################### Step 4 ######################################### run further scripts


source("./src/02-Two-Tier PCA Typology.R")





 

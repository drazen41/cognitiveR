# import libraries
install.packages('ggdendro')
install.packages('ggplot2')
library(ggdendro)
library(ggplot2)

putanja = "~/R/cognitiveR/cognitiveR/RML/Unsupervised/"
datoteka = paste(putanja,"WeatherStations.csv",sep="")
WeatherStations <- read.csv(datoteka, sep =',')
head(WeatherStations)
str(WeatherStations)

WeatherStations <- WeatherStations[complete.cases( WeatherStations$Lat, WeatherStations$Long, WeatherStations$P),]
head(WeatherStations)

# Canada Coordinates
llon <- -140
ulon <- -50
llat <- 40
ulat <- 65
filtered_stations <- subset(WeatherStations, Lat > llat & Lat < ulat & Long > llon & Long < ulon)
rownames(filtered_stations) <- 1:nrow(filtered_stations)
head(filtered_stations)

number_stations <- 30 # number of stations
set.seed(123) # ensure reproducibility
random_stations <- filtered_stations[sample.int(nrow(filtered_stations))[1:number_stations],]
rownames(random_stations) <- 1:nrow(random_stations)
head(random_stations)

temp_loc_dataframe <- scale(data.frame("TotalPrecipitation" = random_stations$P,
                                       "Lat" = random_stations$Lat,
                                       "Long" = random_stations$Long), 
                            center = TRUE, scale = TRUE)

rownames(temp_loc_dataframe) <- paste(random_stations$Tn, random_stations$Tm, random_stations$Tx, random_stations$Stn_Name, random_stations$Lat, random_stations$Long, sep=", ")

D <- dist(as.matrix(temp_loc_dataframe))
hc <- hclust(D)
ggdendrogram(hc, rotate = TRUE, theme_dendro = TRUE, color = "tomato")


threshold <- 1.5 # define threshold

groups <- cutree(hc, h=threshold) # cut three at defined threshold
num_clusters <- max(groups) # number of clusters

# show groups
groups


random_stations <- cbind(random_stations, clusters =as.factor(as.vector(groups)))
head(random_stations)



clusters_centers_dataframe <- aggregate(random_stations[, c("Lat", "Long")], list(cluster = random_stations$clusters), mean)
clusters_centers_dataframe



























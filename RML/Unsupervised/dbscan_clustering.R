putanja = "~/R/cognitiveR/cognitiveR/RML/Unsupervised/"
datoteka = paste(putanja,"WeatherStations.csv",sep="")
# Downloading the file in the Data Scientist Workbench
download.file("https://ibm.box.com/shared/static/th5dsj5txtw052dt2k9i5hekkjhydda2.csv",datoteka)

# Reading the csv file
WeatherStations <- read.csv(datoteka, sep =',')

# What does the data look like?
head(WeatherStations)

# Let's check general information about the data!
str(WeatherStations)

# Creating the main subset: It contains the Station Name, latitude, longitude, highest monthly and lowest monthly temperature
WeatherStations.submain <- subset(WeatherStations, select = c(Stn_Name,Lat,Long,Tx,Tn))
head(WeatherStations.submain)

# Changing column names to cleaner names
colnames(WeatherStations.submain) <- c("Stn_Name","Lat", "Long", "Tmax", "Tmin")
head(WeatherStations.submain)

# Removing all the rows that are incomplete (rows that contain at least a NA)
WeatherStations.submain <- WeatherStations.submain[complete.cases(WeatherStations.submain),]
head(WeatherStations.submain)

# Installing leaflet maps
install.packages("leaflet")
library(leaflet)
# Packages used to display the maps in this notebook
library(htmlwidgets)
library(IRdisplay)

# Establishing the limits of our default visualization
lower_lon = -140
upper_lon = -50
lower_lat = 40
upper_lat = 65
# Establishing the center of our default visualization
center_lon = (lower_lon + upper_lon)/2
center_lat = (lower_lat + upper_lat)/2
# Setting the default zoom of our default visualization
zoom = 4

subset <- WeatherStations.submain #'subset' stores the subset we will be using for the leaflet map
weather_map <- leaflet(subset) %>% #creating a leaflet map
  setView(center_lon,center_lat, zoom)%>% #setting the default view for our map
  addProviderTiles("OpenStreetMap.BlackAndWhite")%>% #setting the map that we want to use as background
  addCircleMarkers(lng = subset$Long, #the longitude is the longitude of our subset!
                   lat = subset$Lat, #the latitude is the latitude of our subset!
                   popup = subset$Stn_Name, #pop-ups will show the name of station if you click in a data point
                   fillColor = "Black", #colors of the markers will be black
                   fillOpacity = 1, #the shapes will have maximum opacity
                   radius = 4, #radius determine the size of each shape
                   stroke = F) #no stroke will be drawn in each data point
weather_map
saveWidget(weather_map, file="weather_map.html", selfcontained = F) #saving the leaflet map in html
display_html(paste("<iframe src=' ", 'weather_map.html', " ' width='100%' height='400'","/>")) #displaying the map !


# installing the library 'dbscan'
install.packages("dbscan", dependencies = TRUE)
library('dbscan')

# creating a subset containing only the location information
WeatherStations.sub1 <- subset(WeatherStations.submain, select = c(Lat,Long))

# preparing the dataset for dbscan: center and scale.
scaled_WS.sub1 <- scale(WeatherStations.sub1, center = TRUE, scale = TRUE)
head(scaled_WS.sub1)


# assigning clusters for each
clusters_assignments1 <- dbscan(scaled_WS.sub1, eps = 0.138, minPts = 12)
clusters_assignments1

# clusters must be converted to factor before plotting in different colors
clusters_assignments1$cluster <- as.factor(clusters_assignments1$cluster)

# linking the assigned cluster to each station
WeatherStations.sub1$cluster_no <- clusters_assignments1$cluster
head(WeatherStations.sub1)

# function that calculates the centroid of a given cluster_no and dataframe
cluster_centroid <- function(cluster_no, df){
  cluster_df <- df[ which(df$cluster_no==cluster_no), ]
  lat_mean <- mean(cluster_df[["Lat"]])
  long_mean <-  mean(cluster_df[["Long"]])
  return(c(lat_mean,long_mean))
}

# function that calculates all the centroids latitudes of a given dataframe
all_lats_centroids <- function(df){
  all_lats <- numeric ()
  for (cluster in unique (df$cluster_no)){
    all_lats[cluster] <- cluster_centroid (cluster,df) [1]
  }
  return (all_lats)
}


# function that calculates all the centroids longitudes of a given dataframe
all_longs_centroids <- function(df){
  all_longs <- numeric ()
  for (cluster in unique (df$cluster_no)){
    all_longs[cluster] <- cluster_centroid (cluster,df) [2]
  }
  return (all_longs)
}

# storing the latitude of all cluster centroids
lats_centroids1 <- all_lats_centroids (WeatherStations.sub1)
lats_centroids1

# storing the longitude of all cluster centroids
longs_centroids1 <- all_longs_centroids (WeatherStations.sub1)
longs_centroids1

# plotting the graph
subset <- WeatherStations.sub1
color <- colorFactor("Set1", as.factor(subset$cluster_no))
weather_map1 <- leaflet(subset) %>%
  setView(center_lon,center_lat, zoom)%>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite")%>% 
  addCircleMarkers(lng = subset$Long,
                   lat = subset$Lat, 
                   popup = subset$Stn_Name,
                   fillColor = ~color(cluster_no),
                   fillOpacity = 1,
                   radius = 4,
                   stroke = F) %>% 
  addMarkers(lng = longs_centroids1,
             lat = lats_centroids1,
             popup = unique(subset$cluster_no))%>% 
  addLegend("bottomleft",
            pal = color,
            values = ~cluster_no,
            opacity = 1,
            title = "Cluster")

weather_map1

saveWidget(weather_map1, file="weather_map1.html", selfcontained = F)

# Plotting our raw data points
display_html(paste("<iframe src=' ", 'weather_map.html', " ' width='100%' height='400'","/>")) 

# Plotting our data points clustered by location
display_html(paste("<iframe src=' ", 'weather_map1.html', " ' width='100%' height='400'","/>")) 


# creating a subset containing only the location
WeatherStations.sub2 <- subset(WeatherStations.submain, select = c(Lat,Long,Tmax,Tmin))

# preparing the dataset for dbscan: center and scale.
scaled_WS.sub2 <- scale(WeatherStations.sub2, center = TRUE, scale = TRUE)
head(scaled_WS.sub2)

# assigning clusters for each
clusters_assignments2 <- dbscan(scaled_WS.sub2, eps = 0.27, minPts = 12)
clusters_assignments2


# clusters must be converted to factor before plotting in different colors
clusters_assignments2$cluster <- as.factor(clusters_assignments2$cluster)
# linking the assigned cluster to each station
WeatherStations.sub2$cluster_no <- clusters_assignments2$cluster
head(WeatherStations.sub2)

# storing the latitude of all cluster centroids
lats_centroids2 <- all_lats_centroids (WeatherStations.sub2)
lats_centroids2

# storing the longitude of all cluster centroids
longs_centroids2 <- all_longs_centroids (WeatherStations.sub2)
longs_centroids2

# plotting the graph
subset <- WeatherStations.sub2
color <- colorFactor("Set1", as.factor(subset$cluster_no))
weather_map2 <- leaflet(subset) %>%
  setView(center_lon,center_lat, zoom)%>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite")%>% 
  addCircleMarkers(lng = subset$Long,
                   lat = subset$Lat, 
                   popup = subset$Stn_Name,
                   fillColor = ~color(cluster_no),
                   fillOpacity = 1,
                   radius = 4,
                   stroke = F) %>%
  addMarkers(lng = longs_centroids2,
             lat = lats_centroids2,
             popup = unique(subset$cluster_no)) %>%
  addLegend("bottomleft",
            pal = color,
            values = ~cluster_no,
            opacity = 1,
            title = "Cluster")
weather_map2


saveWidget(weather_map2, file="weather_map2.html", selfcontained = F)

# Plotting our raw data points
display_html(paste("<iframe src=' ", 'weather_map.html', " ' width='100%' height='400'","/>"))

# Plotting our data points clustered by temperature and location
display_html(paste("<iframe src=' ", 'weather_map2.html', " ' width='100%' height='400'","/>"))



#************************************************************************************************
datoteka = paste(putanja,"seeds.txt",sep="")
download.file("https://ibm.box.com/shared/static/c1aw37ex3sx99pb9q2l8fwz643wnbeo6.txt",datoteka)
#seeds <- read.csv("https://ibm.box.com/shared/static/c1aw37ex3sx99pb9q2l8fwz643wnbeo6.txt", sep = "	")
seeds <- read.delim(datoteka,header=TRUE,sep= "\t")
head(seeds)

# Creating the subset: The subset contains the kernel width and the kernel length
seeds.sub <- subset(seeds, select = c(width,length))
head(seeds.sub)

clusters_assignments1 <- dbscan(seeds.sub, eps = .08, minPts = 4)
clusters_assignments1

clusters_assignments1$cluster <- as.factor(clusters_assignments1$cluster)
head(clusters_assignments1$cluster)

seeds.sub$cluster_no <- clusters_assignments1$cluster
head(seeds.sub)

plot(seeds.sub$width, seeds.sub$length, col = clusters_assignments1$cluster, pch = 16, main = "Scatterplot Displaying Clusters", xlab = "Kernel Width", ylab = "Kernel Length")
legend(x = 2.6, y = 6.75, legend = levels(clusters_assignments1$cluster), col = c(1:5), pch = 16, title = "Clusters")


seeds.submain <- seeds[1:7]
# We will run the dbscan algorithm with eps = 0.9 and minPts = 4.
# Note: Using the previous value of eps = 0.08 will create only one cluster containing all datapoints.
clusters_assignments2 <- dbscan(seeds.submain, eps = .9, minPts = 4)
clusters_assignments2
# Clusters must be converted to factor before plotting in different colors
clusters_assignments2$cluster <- as.factor(clusters_assignments2$cluster)
# Linking the assigned cluster to each station
seeds.submain$cluster_no <- clusters_assignments2$cluster
# Visualize results using a pairs plot (noise is shown in black)
pairs(seeds.submain, col = clusters_assignments2$cluster, pch = 16, main = "Scatterplot Displaying Clusters")




















































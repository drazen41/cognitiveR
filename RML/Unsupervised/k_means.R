putanja = "~/R/cognitiveR/cognitiveR/RML/Unsupervised/"
datoteka = paste(putanja,"HKkids.csv",sep="")
datoteka
# Downloading the file to the Data Scientist Workbench
download.file("https://ibm.box.com/shared/static/vw5gm9h25gnd7qdcmxm9exawphh0kgxy.csv", datoteka)

# Reading the csv file
HKkids <- read.csv(datoteka, sep =';')

# What does the data look like?
head(HKkids)

# Changing column names to shorter names
colnames(HKkids) <- c("Index", "Height", "Weight")

# Let's check general information  about the data!
str(HKkids)

# Removing the unnecessary index column
HKkids$Index <- NULL

# It is always good practice to visualize your data!

# install the ggplot2 package
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(ggplot2)

ggplot(HKkids, aes(x = HKkids$Height, y = HKkids$Weight)) +
  geom_point(shape=1, size = 2, color = "black", alpha = 1/3) +
  geom_point(size = 0.1, color = "red4", alpha = 1/3) +
  labs (x = "Height (Inches)", y = "Weight (Pounds)")

# Calculate and store the mean and standard deviation for each dataset, before we normalize
original_height_mean = mean(HKkids$Height)
original_height_sd = sd(HKkids$Height)
original_weight_mean = mean(HKkids$Weight)
original_weight_sd = sd(HKkids$Weight)

# Z-scoring
HKkids$Height <- (HKkids$Height - original_height_mean) / original_height_sd
HKkids$Weight <- (HKkids$Weight - original_weight_mean) / original_weight_sd

head(HKkids)

# "HKkids" is our data
# "centers" is the number of clusters
# "iter.max" is the number of iterations
# Try experimenting with different numbers!
clusters_numbers = kmeans (HKkids, centers = 3, iter.max = 10)

# We are using a color_offset = 4 just for making things easier to see.
# Try experimenting with different numbers!
color_offset = 4

# We have to convert numbers to categorical data in order to color the chart
HKkids$Cluster <- as.factor(clusters_numbers$cluster + color_offset)

# Checking if the cluster assignments are there ! (Notice that the clusters are numbered with the color_offset!)
head(HKkids)

ggplot() +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 2, color = "black", alpha = 1/2) +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 0.1, colour = HKkids$Cluster, alpha = 1/2) +
  labs (x = "Height (Inches)", y = "Weight (Pounds)")


# Extracting the cluster centroids
Cluster_centroids <- as.data.frame(clusters_numbers$centers)


# Plotting the centroids found
ggplot() +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 2, color = "black", alpha = 1/10) +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 0.1, colour = HKkids$Cluster, alpha = 1/4) +
  geom_point(data = Cluster_centroids, aes(x = Cluster_centroids$Height, y = Cluster_centroids$Weight), shape = 1, size = 3, color = "black") +
  geom_point(data = Cluster_centroids, aes(x = Cluster_centroids$Height, y = Cluster_centroids$Weight), shape = 1, size = 5, color = "black") +
  geom_point(data = Cluster_centroids, aes(x = Cluster_centroids$Height, y = Cluster_centroids$Weight), shape = 3, size = 10, color = "black") +
  labs (x = "Height (Inches)", y = "Weight (Pounds)")

# Checking the location of each cluster centroid
Cluster_centroids

# Undoing the normalization
tee_sizes <- Cluster_centroids
tee_sizes$Height <- (tee_sizes$Height * original_height_sd) + original_height_mean
tee_sizes$Weight <- (tee_sizes$Weight * original_weight_sd) + original_weight_mean


# Printing the size of each t-shirt
tee_sizes

small <- which(tee_sizes$Height==min(tee_sizes$Height))
tee_sizes[small,]

medium <- which(tee_sizes$Height==median(tee_sizes$Height))
tee_sizes[medium,]

large <- which(tee_sizes$Height==max(tee_sizes$Height))
tee_sizes[large,]


# Creating subsets for each one of the segments of the population
small_HKkids <- HKkids[HKkids$Cluster == small + color_offset,]
medium_HKkids <- HKkids[HKkids$Cluster == medium + color_offset,]
large_HKkids <- HKkids[HKkids$Cluster == large + color_offset,]


# Storing all the standard deviations of each cluster to our tee_size matrix
tee_sizes$sd_Height[small] = sd(small_HKkids$Height)
tee_sizes$sd_Height[medium] = sd(medium_HKkids$Height)
tee_sizes$sd_Height[large] = sd(large_HKkids$Height)
tee_sizes$sd_Weight[small] = sd(small_HKkids$Weight)
tee_sizes$sd_Weight[medium] = sd(medium_HKkids$Weight)
tee_sizes$sd_Weight[large] = sd(large_HKkids$Weight)

tee_sizes

#*****************************************************************************
datoteka = paste(putanja,"wholesale_customers.csv",sep="")
download.file("https://ibm.box.com/shared/static/36ulo2vaeqyglj1dxz3b75093vdmgp5q.csv", destfile = datoteka, quiet = FALSE)
sale <- read.csv(datoteka, sep =',')
## What does the dataset look like?  ##
head(sale)
## Hint: Exclude the Channel and Region variables first before clustering ## 
threshold <- 1.5 # define threshold
## Answer Code: ##
# Cleaning up the data
sale.group <- sale
sale.group$Channel <- NULL
sale.group$Region <- NULL

milk_mean = mean(sale.group$Milk)
milk_sd = sd(sale.group$Milk)
fresh_mean = mean(sale.group$Fresh)
fresh_sd = sd(sale.group$Fresh)
gro_mean = mean(sale.group$Grocery)
gro_sd = sd(sale.group$Grocery)
frozen_mean = mean(sale.group$Frozen)
frozen_sd = sd(sale.group$Frozen)
paper_mean = mean(sale.group$Detergents_Paper)
paper_sd = sd(sale.group$Detergents_Paper)
del_mean = mean(sale.group$Delicassen)
del_sd = sd(sale.group$Delicassen)

sale.group$Milk <- (sale.group$Milk - milk_mean) / milk_sd
sale.group$Fresh <- (sale.group$Fresh - fresh_mean) / fresh_sd
sale.group$Grocery <- (sale.group$Grocery - gro_mean) / gro_sd
sale.group$Frozen <- (sale.group$Frozen - frozen_mean) / frozen_sd
sale.group$Detergents_Paper <- (sale.group$Detergents_Paper - paper_mean) / paper_sd
sale.group$Delicassen <- (sale.group$Delicassen - del_mean) / del_sd

head(sale.group)

# Elbow Method: Help choose the value of K which minimize the standard deviation within each cluster
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(sale.group)  

# We can also look at how the optimal K changes along with the number of criteria

install.packages("NbClust")
library(NbClust)
set.seed(12345)
nc <- NbClust(sale.group, min.nc=2, max.nc=6, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 6 Criteria")

result <- kmeans(sale.group, 5)  
Cluster_centroids <- as.data.frame(result$centers)
spending <- Cluster_centroids
spending$Fresh <- (spending$Fresh * fresh_sd) + fresh_mean
spending$Frozen <- (spending$Frozen * frozen_sd) + frozen_mean

# the spending on fresh food for the five clusters
spending[,1]

# the spending on frozen food for the five clusters
spending[,4]

## spending of all the categories in the five clusters 
spending

install.packages("flexclust")
library(flexclust)

# A cross-tabulation of Channel and cluster membership is given by

ch.fit <- table(sale$Channel, result$cluster)
ch.fit
randIndex(ch.fit)


#**********************************************************************
install.packages("jpeg")
library("jpeg")
datoteka = paste(putanja,"exercise2.jpg",sep="")
download.file("https://ibm.box.com/shared/static/w9nnfoxpr9rnb1qw82gcrapgadzh3onn.jpg",datoteka,mode='wb')

pic <- readJPEG(datoteka)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(pic,0,0,1,1)

#Obtain the location and colour dimensions of pixels from the image
imgDm <- dim(pic)

picRGB <- data.frame(
  x_axis = rep(1:imgDm[2], each = imgDm[1]),
  y_axis = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(pic[,,1]),
  G = as.vector(pic[,,2]),
  B = as.vector(pic[,,3])
)

library(ggplot2)
ggplot(data = picRGB, aes(x = x_axis, y = y_axis)) +
  geom_point(colour = rgb(picRGB[ c("R", "G", "B")])) +
  labs(title = "Original Image") +
  xlab("x_axis") +
  ylab("y_axis")

# We are now trying to compress the image to 16 colours
kClusters <- 16
kMeans16 <- kmeans(picRGB[, c("R", "G", "B")], centers = kClusters)

kColours <- rgb(kMeans16$centers[kMeans16$cluster,])

ggplot(data = picRGB, aes(x = x_axis, y = y_axis)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")


# Changing the k to 5
kClusters <- 5
kMeans5 <- kmeans(picRGB[, c("R", "G", "B")], centers = kClusters)

kColours <- rgb(kMeans5$centers[kMeans5$cluster,])

ggplot(data = picRGB, aes(x = x_axis, y = y_axis)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")

#As you can see the image got quite blurred using only 5 colours

# Lastly we further compress the picture to 3 colours
kClusters <- 3
kMeans3 <- kmeans(picRGB[, c("R", "G", "B")], centers = kClusters)

kColours <- rgb(kMeans3$centers[kMeans3$cluster,])

ggplot(data = picRGB, aes(x = x_axis, y = y_axis)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y")
# Look at how the K value changes the sharpness of the imag!
# It looks like the dolphine is disappearing in the water!




















































































































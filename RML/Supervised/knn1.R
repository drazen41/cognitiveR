# Install the packages 'class' and 'kknn' and load their libraries, which will be needed for their k-nearest neighbors algorithms
install.packages("class")
install.packages("kknn")
library(class)
library(kknn)

# Load the data and view its structure
balance_scale <- read.csv("https://ibm.box.com/shared/static/684jzm7e6fbbssg87yc2v4dy53dgkdew.txt", sep = ",")
str(balance_scale)

# View the first few rows of the data using the head function
# Note: The raw data does not contain any column names
head(balance_scale)

# Add column names
colnames(balance_scale) <- c("Class_Name","Left_Weight", "Left_Distance", "Right_Weight", "Right_Distance")
head(balance_scale)
# Note: We do not need to standardize the data in this instance since the numerical data values lie on the same scale.
# Calculate the products and differences
Right_Product <- balance_scale[,4]*balance_scale[,5]
Left_Product <- balance_scale[,2]*balance_scale[,3]
Differences <- Right_Product-Left_Product
# Add columns for Right_Product, Left_Product and Differences
balance_scale$Right_Product <- Right_Product
balance_scale$Left_Product <- Left_Product
balance_scale$Differences <- Differences
head(balance_scale)

# Use the sample function to create a vector of indices that will be used
set.seed(1234)
ind <- sample(2, nrow(balance_scale), replace=TRUE, prob=c(0.7, 0.3))


# Create the training and test data from the dataset using ind
bscale.train <- balance_scale[ind==1, 6:8]
bscale.test <- balance_scale[ind==2, 6:8]



# Create the target vectors for the training and test data from the dataset using ind
bscale.trainLabels <- balance_scale[ind==1, 1]
bscale.testLabels <- balance_scale[ind==2, 1]

# Use the knn command to make predictions on the Class_Name of the test data
knn_class <- knn(train = bscale.train, test = bscale.test, cl = bscale.trainLabels, k=3)

# Find the number of incorrectly classified points
correct <- which(knn_class == bscale.testLabels, arr.ind = TRUE)
incorrect <- which(knn_class != bscale.testLabels, arr.ind = TRUE)
cat("Number of incorrectly classified points:",length(incorrect),"\n")

# Find the proportion of correctly classified points
proportion_correct <- length(correct)/length(bscale.testLabels)
cat("Proportion of correctly classified points", proportion_correct,"\n")

# Run the knn regression using the kknn command
knn_reg <- kknn(formula = Differences ~ ., train=bscale.train, test=bscale.test, k=3)

# Find the number of incorrectly classified points
incorrect_reg <- which(knn_reg$fitted.values != bscale.test$Differences, arr.ind = TRUE)
cat("Number of incorrectly classified points:", length(incorrect_reg), "\n");

# Find the proportion of correctly classified points
correct_reg <- which(knn_reg$fitted.values == bscale.test$Differences, arr.ind = TRUE)
cat("Proportion of correctly classified points", length(correct_reg)/length(bscale.test$Differences), "\n")


# Display the first few rows of the regression estimates of the differences and their true values
head(cbind(knn_reg$fitted.values,bscale.test$Differences))

best_reg <- train.kknn(formula = Differences ~ ., data=bscale.train, kmax=8)
best_reg$best.parameters

# Run the knn regression again using the kknn command with k=2
knn_reg2 <- kknn(formula = Differences ~ ., train=bscale.train, test=bscale.test, k=2)

# Find the number of incorrectly classified points
incorrect_reg2 <- which(knn_reg2$fitted.values != bscale.test$Differences, arr.ind = TRUE)
cat("Number of incorrectly classified points:", length(incorrect_reg2),"\n")
# Find the proportion of correctly classified points
correct_reg2 <- which(knn_reg2$fitted.values == bscale.test$Differences, arr.ind = TRUE)
cat("Proportion of correctly classified points",length(correct_reg2)/length(bscale.test$Differences),"\n")

# Display the first few rows of the new regression estimates of the differences and their true values
head(cbind(knn_reg2$fitted.values,bscale.test$Differences))

# The kknn function can also be used for classification
knn_class2 <- kknn(formula = Class_Name ~ ., train=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==1,], test=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==2,], k=3)
# Find the number of incorrectly classified points
incorrect_class2 <- which(knn_class2$fitted.values != bscale.testLabels, arr.ind = TRUE)
cat("Number of incorrectly classified points:",length(incorrect_class2),"\n")

best_class <- train.kknn(formula = Class_Name ~ ., data=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==1,], kmax=8)
best_class$best.parameters

# Using k=1
knn_class3 <- kknn(formula = Class_Name ~ ., train=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==1,], test=subset(balance_scale, select=c(Class_Name,Right_Product,Left_Product,Differences))[ind==2,], k=1)
# Find the number of incorrectly classified points
incorrect_class3 <- which(knn_class3$fitted.values != bscale.testLabels, arr.ind = TRUE)
cat("Number of incorrectly classified points:", length(incorrect_class3),"\n")


# Load the data and view the structure
schools <- read.csv("https://ibm.box.com/shared/static/uummw8ijp41gn3nfkuipi78xnalkss4c.csv", sep = ",")
str(schools)

# Create a subset of the data containing the variables province, latitude, and longitude
schools.sub <- subset(schools, select=c(name, province.name..english, latitude, longitude))
head(schools.sub)
# Change the column names to cleaner names
colnames(schools.sub) <- c("name", "province", "latitude", "longitude")
head(schools.sub)

# Installing leaflet maps
install.packages("leaflet")
library(leaflet)
# Packages used to display the maps in this notebook
library(htmlwidgets)
library(IRdisplay)

# Establish the limits of our default visualization
lower_lon = -140
upper_lon = -50
lower_lat = 40
upper_lat = 65
# Establish the center of our default visualization
center_lon = (lower_lon + upper_lon)/2
center_lat = (lower_lat + upper_lat)/2
# Set the zoom of our default visualization
zoom = 4


# Create a leaflet map
schools_map <- leaflet(schools.sub) %>%
  setView(center_lon,center_lat, zoom)%>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite")%>% # set the map that we want to use as background
  addCircleMarkers(lng = schools.sub$longitude, 
                   lat = schools.sub$latitude, 
                   popup = schools.sub$name, # pop-ups will show the name of school if you click on a data point
                   fillColor = "Black", # colors of the markers will be black
                   fillOpacity = 1, # the shapes will have maximum opacity
                   radius = 4, # radius determine the size of each shape
                   stroke = F) # no stroke will be drawn in each data point

#saveWidget(schools_map, file="schools_map.html", selfcontained = F) #saving the leaflet map in html
#display_html(paste("<iframe src=' ", 'schools_map.html', " ' width='100%' height='400'","/>")) #display the map !
schools_map

# Use the sample function to create a vector of indices that will be used to split our data into training and test data
set.seed(1234)
ind <- sample(2, nrow(schools.sub), replace=TRUE, prob=c(0.7, 0.3)) # This creates a vector of equal length to our data, with approximately 70% of the values being 1 and the remaining values are 2  
# Create training and test sets containing only the geographical locations 
schools.train <- schools.sub[ind==1,3:4]
schools.test <- schools.sub[ind==2,3:4]
# Note: Normalizing or standardizing the geographical attributes of the data will not be helpful since values are already on the same scale 
# Create the target vectors for the training and test data from the original dataset using ind
schools.trainLabels <- schools.sub[ind==1,2]
schools.testLabels <- schools.sub[ind==2,2]
# Run the knn algorithm to classify test data points into the different provinces
prov_pred <- knn(train = schools.train, test = schools.test, cl = schools.trainLabels, k=3)

# Find the number of incorrectly classified points
correct_provinces <- which(prov_pred == schools.testLabels, arr.ind = TRUE)
incorrect_provinces <- which(prov_pred != schools.testLabels, arr.ind = TRUE)
cat("Number of incorrectly classified points:", length(incorrect_provinces),"\n")
# Find the proportion of correctly classified points
proportion_of_correct_provinces <- length(correct_provinces)/length(schools.testLabels)
cat("Proportion of correctly classified points", proportion_of_correct_provinces,"\n")

# Link the knn predictions with the test data
schools.test$prov <- prov_pred
# Plot the test data with predicted provinces
color <- colorFactor(topo.colors(10), schools.test$prov)
schools_map2 <- leaflet(schools.test) %>%
  setView(center_lon,center_lat, zoom)%>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite")%>% # set the map that we want to use as background
  addCircleMarkers(lng = schools.test$longitude, 
                   lat = schools.test$latitude, 
                   popup = schools.test$prov, # pop-ups will show the predicted province if you click on a data point
                   fillColor = ~color(prov),
                   fillOpacity = 1, # the shapes will have maximum opacity
                   radius = 4, # radius determine the size of each shape
                   stroke = F) # no stroke will be drawn in each data point

#saveWidget(schools_map2, file="schools_map2.html", selfcontained = F) #saving the leaflet map in html
#display_html(paste("<iframe src=' ", 'schools_map2.html', " ' width='100%' height='400'","/>")) #display the map !
schools_map2
















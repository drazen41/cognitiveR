putanja = "~/R/cognitiveR/cognitiveR/RML/Dimension reduce - feature extract/"
datoteka <- paste(putanja,"ratings.dat",sep="")
# rating dataset
download.file("https://ibm.box.com/shared/static/q61myoukbyz969b97ddlcq0dny0l07bf.dat", datoteka)

datoteka1 = paste(putanja,"movies.dat")
#Moview dtaset
download.file("https://ibm.box.com/shared/static/dn84btkn9gmxmdau32c5xb0vamie6jy4.dat", datoteka1)

#Loading the movie information into a dataframe
movies_df <- read.csv(datoteka1, header = FALSE, sep=":")
# Head is a function that gets the first 6 rows of a dataframe
head(movies_df)

#Loading the user information into a dataframe
ratings_df <- read.csv(datoteka, header = FALSE, sep=":")
# Alternatively let's look at the first 20 rows of the datatframe
head(ratings_df, 20)

# Here we read the movies data again in the raw format and display the first few rows
lines <- readLines(datoteka1)
head(lines, 20)

# Here we replace the sep character used in the data ("::") with one that does not appear in the data ("~")
lines <- gsub("::", "~", lines)
head(lines, 20)

# Now we recreate the movies dataframe using the updated data
movies_df <- read.csv(text=lines, sep="~", header = FALSE)
head(movies_df, 20)

names(movies_df)[names(movies_df)=="V1"] = "movieId"
names(movies_df)[names(movies_df)=="V2"] = "title"
names(movies_df)[names(movies_df)=="V3"] = "genres"
#Applying the strip function to get rid of any ending whitespace characters that may have appeared
movies_df$title = sub("\\s+$", "", movies_df$title)

head(movies_df, 20)

#Dropping the genres column
movies_df$genres = NULL

# Display the first 20 rows
head(movies_df, 20)

head(ratings_df)

# Removing the Empty Column Ex: V2, V4, V6 using subset function.
# These columns were generated because the data is separated by "::" while the read.csv function only accepts single characters
# for the sep value  such as ":" or "~", thus the read function assumed that our data was separated by single colons (":").
ratings_df <- subset( ratings_df, select = -c(V2, V4, V6 ) )
head(ratings_df)


names(ratings_df)[names(ratings_df)=="V1"] = "userId"
names(ratings_df)[names(ratings_df)=="V3"] = "movieId"
names(ratings_df)[names(ratings_df)=="V5"] = "rating"
names(ratings_df)[names(ratings_df)=="V7"] = "timestamp"
ratings_df$timestamp = NULL
# Here's how the final ratings Dataframe looks like:
head(ratings_df)

inputUser = data.frame("title"=c("Breakfast Club, The (1985)", "Toy Story (1995)", "Jumanji (1995)", "Pulp Fiction (1994)", "Akira (1988)"), 
                       "rating"=c(5, 3.5, 2, 5, 4.5))
head(inputUser)


inputUser$movieId = rep(NA, length(inputUser$title))
for (i in 1:length(inputUser$title)){
  inputUser$movieId[i] = as.character(movies_df$movieId[movies_df$title == inputUser$title[i]])
}
head(inputUser)


#Filtering out users that have watched movies that the input has watched and storing it
userSubset = ratings_df[ratings_df$movieId %in% inputUser$movieId,]
head(userSubset)

top100 <- head(sort(table(factor(userSubset$userId)), decreasing = TRUE), 100)
head(top100)


userList <- as.data.frame.table(top100)
colnames(userList) <-  c("userId","commonMovies")
head(userList)


userSubset = ratings_df[ratings_df$userId %in% userList$userId,]
temp = as.data.frame(table(userSubset$movieId))
names(temp)[names(temp)=="Var1"] = "movieId"
userSubset = merge(temp, userSubset)


head(userSubset)

head(userSubset[userSubset$userId == 533,])

userSubset = userSubset[userSubset$Freq > 10,]
head(userSubset)

pearson_df = data.frame("userId"=integer(), "similarityIndex"=double())
for (user in userList$userId)
{
  userRating = userSubset[userSubset$userId == user,]
  
  moviesInCommonX = userRating[userRating$movieId %in% inputUser$movieId,]
  moviesInCommonX = moviesInCommonX[complete.cases(moviesInCommonX),]
  
  moviesInCommonY = inputUser[inputUser$movieId %in% userRating$movieId,]
  moviesInCommonY = moviesInCommonY[complete.cases(moviesInCommonY),]
  
  #Now let's calculate the pearson correlation between two users, so called, x and y
  Sxx = sum(moviesInCommonX$rating^2) - (sum(moviesInCommonX$rating)^2)/nrow(moviesInCommonX)
  Syy = sum(moviesInCommonY$rating^2) - (sum(moviesInCommonY$rating)^2)/nrow(moviesInCommonY)
  Sxy = sum(moviesInCommonX$rating*moviesInCommonY$rating) - (sum(moviesInCommonX$rating)*sum(moviesInCommonY$rating))/nrow(moviesInCommonX)
  
  
  if(Sxx == 0 | Syy == 0 | Sxy == 0)
  {
    pearsonCorrelation = 0
  }
  else
  {
    pearsonCorrelation = Sxy/sqrt(Sxx*Syy)
  }
  
  pearson_df = rbind(pearson_df, data.frame("userId"=user, "similarityIndex"=pearsonCorrelation))   
}


head(pearson_df)


topUsersRating = merge(userSubset, pearson_df)
head(topUsersRating, 15)


#Multiplies the similarity by the user's ratings
topUsersRating$weightedRating = topUsersRating$similarityIndex*topUsersRating$rating
weightedAverage_df = aggregate(topUsersRating$weightedRating, list(topUsersRating$movieId), mean)
head(weightedAverage_df)


names(weightedAverage_df)[names(weightedAverage_df)=="Group.1"] = "movieId"
names(weightedAverage_df)[names(weightedAverage_df)=="x"] = "weightedAverage"
head(weightedAverage_df)



recommendation_df = merge(weightedAverage_df, movies_df)

head(recommendation_df[order(-recommendation_df$weightedAverage),], 20)


#*********************************************************************************
datoteka2 = paste(putanja,"ratings_cleaned.csv",sep="")
# rating dataset
download.file("https://ibm.box.com/shared/static/226cs3qlqylgkdiiqttjghwij0mfd6sp.csv", datoteka2)
#Moview dtaset
datoteka3 = paste(putanja,"movies_cleaned.csv",sep="")
download.file("https://ibm.box.com/shared/static/jj7hu6jsvdwtyw1q4n9gpfdnt4mtd9rc.csv", datoteka3)

#Loading the movie information into a dataframe
movies_df <- read.csv(datoteka3,  sep=",")

#Loading the user information into a dataframe
ratings_df <- read.csv(datoteka2,  sep=",")
str(movies_df)
str(ratings_df)

head(movies_df)
head(ratings_df)

ratings_df$timestamp <- NULL

install.packages("proxy")
install.packages("recommenderlab")
install.packages("Matrix")
library(recommenderlab)
library(Matrix)

sparse_ratings <- sparseMatrix(i = ratings_df$userId, j = ratings_df$movieId, x = ratings_df$rating,
                               dimnames = list(paste("u", 1:length(unique(ratings_df$userId)), sep = ""), 
                                               paste("m", 1:max(ratings_df$movieId), sep = ""))) 

sparse_ratings[1:10, 1:10]

dim(sparse_ratings)

real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings


rmodel <- Recommender(real_ratings[1:500], method = "UBCF", param=list(normalize = "center", method = "Pearson"))

recom <- predict(rmodel, real_ratings[501:502],n=5)
lrecom <- as(recom, "list")
lrecom

lr <- lapply(lrecom, function(x) as.numeric(sub("m","", x)))
lr

user501=ratings_df$movieId[ratings_df$userId==501]
print("User501")
cat("\n")
for (i in user501){
  movie <- movies_df$title[movies_df$movieId==i]
  print (movie, max.levels=0)
}


print("User-based collaborative filtering")
cat("\n")
cat("\n")
cat("\n")

print("User 501 Recommendations:")
cat("\n")
cat("\n")

u501_recom <- lapply(lr[1], function(x) for (i in x){
  movie <- movies_df$title[movies_df$movieId==i]
  print (movie, max.levels=0)
  genres <- movies_df$genres[movies_df$movieId==i]
  cat("Genres: ", as.character(genres), "\n")
  indices <- which(ratings_df$movieId==i, arr.ind=T)
  cat("Total ratings: ", length(indices), "\n")
  cat("Average rating: ",mean(ratings_df$rating[indices]), "\n")
  cat("\n")
}
)
u501_recom

user502 <- ratings_df$movieId[ratings_df$userId==502]
print("User502")
cat("\n")
for (i in user502){
  movie <- movies_df$title[movies_df$movieId==i]
  print (movie, max.levels=0)
}


print("User-based collaborative filtering")
cat("\n")
cat("\n")
cat("\n")
print("User 502 Recommendations:")
cat("\n")
cat("\n")
u502_recom <- lapply(lr[2], function(x) for (i in x){
  movie <- movies_df$title[movies_df$movieId==i]
  print (movie, max.levels=0)
  genres <- movies_df$genres[movies_df$movieId==i]
  cat("Genres: ", as.character(genres), "\n")
  indices <- which(ratings_df$movieId==i, arr.ind=T)
  indices <- which(ratings_df$movieId==i, arr.ind=T)
  cat("Total ratings: ", length(indices), "\n")
  cat("Average rating: ",mean(ratings_df$rating[indices]), "\n")
  cat("\n")
}
)
u502_recom




















































































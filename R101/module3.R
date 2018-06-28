
# Condition and Loops -----------------------------------------------------

# code to download the dataset
putanja = "~/R/cognitiveR/cognitiveR/R101/movies-db.csv"
download.file("https://ibm.box.com/shared/static/n5ay5qadfe7e1nnsv5s01oe1x62mq51j.csv", destfile=putanja)

movies_Data <- read.csv(putanja, header=TRUE, sep=",")
movies_Data

Movie_Year = 1997

# If Movie_Year is BOTH less than 2000 AND greater than 1990 -- both conditions have to be true! -- ... 
if(Movie_Year < 2000 & Movie_Year > 1990 ) 
{
  # ...then we print this message.
  print('Movie year between 1990 and 2000') 
}

# If Movie_Year is EITHER greater than 2010 OR less than 2000 -- any of the conditions have to be true! -- ... 
if(Movie_Year > 2010 | Movie_Year < 2000 ) 
{
  # ...then we print this message.
  print('Movie year is not between 2000 and 2010') 
}

decade = 'recent'

# If the decade given is recent...
if(decade == 'recent' ){
  # Subset the dataset to include only movies after year 2000.
  subset(movies_Data, year >= 2000)
} else { # If not...
  # Subset the dataset to include only movies before 2000.
  subset(movies_Data, year < 2000)
}

# Get the data for the "year" column in the data frame.
years <- movies_Data['year']

# For each value in the "years" variable...
# Note that "val" here is a variable -- it assumes the value of one of the data points in "years"!
for (val in years) {
  # ...print the year stored in "val".
  print(val)
}

# Creating a start point.
iteration = 1

# We want to repeat until we reach the sixth operation -- but not execute the sixth time.
# While iteration is less or equal to five...
while (iteration <= 5) {
  
  print(c("This is iteration number:",as.character(iteration)))
  
  # ...print the "name" column of the iteration-th row.
  print(movies_Data[iteration,]$name)
  
  # And then, we increase the "iteration" value -- so that we actually reach our stopping condition
  # Be careful of infinite while loops!
  iteration = iteration + 1
}

# First, we create a vector...
my_list <- c(10,12,15,19,25,33)

# ...we can try adding two to all the values in that vector.
my_list + 2

# Or maybe even exponentiating them by two.
my_list ** 2

# We can also sum two vectors element-wise!
my_list + my_list

my_list * my_list

# Functions ---------------------------------------------------------------


ratings <- c(8.7, 6.9, 8.5)
mean(ratings)
sort(ratings)

sort(ratings, decreasing = TRUE)


printHelloWorld <- function(){
  print("Hello World")
}
printHelloWorld()

add <- function(x, y) {
  return(x + y)
}
add(3, 4)

isGoodRating <- function(rating){
  #This function returns "NO" if the input value is less than 7. Otherwise it returns "YES".
  
  if(rating < 7){
    return("NO") # return NO if the movie rating is less than 7
    
  }else{
    return("YES") # otherwise return YES
  }
}

isGoodRating(6)
isGoodRating(9.5)

isGoodRating <- function(rating, threshold = 7){
  if(rating < threshold){
    return("NO") # return NO if the movie rating is less than the threshold
  }else{
    return("YES") # otherwise return YES
  }
}

isGoodRating(6)
isGoodRating(10)
isGoodRating(8, threshold = 8.5)
isGoodRating(8, 8.5) #rating = 8, threshold = 8.5

my_data <- movies_Data
head(my_data)

# Within myData, the row should be where the first column equals "Akira"
# AND the column should be "average_rating"

akira <- my_data[my_data[,1] == "Akira", "average_rating"]
akira

isGoodRating(akira)
watchMovie <- function(moviename){
  rating <- my_data[my_data[,1] == moviename,"average_rating"]
  isGoodRating(rating)
}
watchMovie("Akira")
watchMovie <- function(moviename, my_threshold=7){
  rating <- my_data[my_data[,1] == moviename,"average_rating"]
  isGoodRating(rating, threshold = my_threshold)
}
watchMovie("Akira",9)
watchMovie <- function(moviename, my_threshold = 7){
  rating <- my_data[my_data[,1] == moviename,"average_rating"]
  memo <- paste("The movie rating for", moviename, "is", rating)
  print(memo)
  isGoodRating(rating, threshold = my_threshold)
}

watchMovie("Akira")
memo

myFunction <- function(){
  y <<- 3.14
  return("Hello World")
}
myFunction()

y



# Objects and classes -----------------------------------------------------

movie_rating <- c(8.3, 5.2, 9.3, 8.0) # create a vector from average ratings 
movie_rating # print the variable

class(movie_rating) # show the variable's data type

movies <-c("Toy Story", "Akira", "The Breakfast Club", "The Artist")
movies
class(movies)

combined <- c("Toy Story", 1995, "Akira", 1998)
combined
class(combined)

movie_length <- c(80, 110, 90, 80) # create a vector from movie length
movie_length # print the variable
class(movie_length)


age_restriction <- c(12, 10, 18, 18) # create a vector from age restriction
age_restriction # print the vector

class(age_restriction)
integer_vector <- as.integer(age_restriction)
class(integer_vector)

logical_vector <- c(T,F,F,T,T) # creating the vector
class(logical_vector)

length_Akira <- 125
length_ToyStory <- 81

x <- length_ToyStory > length_Akira      # is ToyStory larger than akira? 
x

x <- length_Akira > length_ToyStory # is akira larger than ToyStory?               
x # print the logical value
class(x)       # print the class name of x

z = 8 + 6i     # create a complex number 
z
class(z)

year <- as.character(1995) # convert integer into character data type
year                    # print the value of year in character data type

Length_ToyStory <- 81
class(81)

length_ToyStory <- as.integer(81) 
class(length_ToyStory)       # print the class name of length_ToyStory

movies <- c("Toy Story", "Akira", "The Breakfast Club", "The Artist") # creating two vectors
genre <- c("Animation/Adventure/Comedy", "Animation/Adventure/Comedy", "Comedy/Drama", "Comedy/Drama")

class(movies)
mode(movies)


movies_genre <- cbind(movies, genre) # kreira matrix
movies_genre 
class(movies_genre)
mode(movies_genre)

sample_array <- array(1:12, dim = c(3, 2, 2)) # create an array with dimensions 3 x 2 x 2 
sample_array
class(sample_array)
mode(sample_array)

Name <- c("Toy Story", "Akira", "The Breakfast Club", "The Artist")
Year <- c(1995, 1998, 1985, 2011)
Length <- c(81, 125, 97, 100)
RowNames = c("Movie 1", "Movie 2", "Movie 3", "Movie 4")

sample_DataFrame <- data.frame(Name, Year, Length, row.names=RowNames) 
sample_DataFrame

class(sample_DataFrame)
mode(sample_DataFrame)

sample_List = list("Star Wars", 8.7, TRUE)
sample_List

class(sample_List)

mode(sample_List)

mode(sample_List[[3]])


z <- c(8.3, 8.1, 7.9, 8, 30, 10.4, 1, 15)
z
attr(z, "dim") <- c(4,2)
z

class(z)
mode(z)

# Debugging ---------------------------------------------------------------

"a" + 10

for(i in 1:3){
  #for every number, i, in the sequence of 1,2,3:
  print(i + "a")
}

tryCatch(10 + 10)
tryCatch("a" + 10) #Error

#If error, print a message without an error
tryCatch(10 + "a", error = function(e) print("Oops, something went wrong!") ) #No error

#If error, return "10a" without an error
x <- tryCatch(10 + "a", error = function(e) return("10a") ) #No error
x

tryCatch(
  for(i in 1:3){
    #for every number, i, in the sequence of 1,2,3:
    print(i + "a")
  }
  , error = function(e) print("Found error.") )

as.integer("A") #Converting "A" into an integer warns the user that is converted to NA


tryCatch(as.integer("A"), warning = function(e) print("Warning.") )























































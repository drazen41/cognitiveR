# install.packages("rjson") 

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,1))
print(result)

movie_vector <- c("Akira","Toy Story","Room","The Wave","Whiplash","Star Wars","The Ring","The Artist","Jumanji")
movie_array <- array(movie_vector,dim=c(4,3))
movie_array
print(movie_array)
movie_array[1,]
movie_array[,2]
movie_matrix <- matrix(movie_vector,nrow=3,ncol=3)
movie_matrix
movie_matrix <- matrix(movie_vector,nrow=3,ncol=3,byrow = TRUE)
movie_matrix
movie_matrix[2:3,1:2]

greska <- c("A",1)
greska + 1

movie <- list("Toy Story", 1995, c("Animation", "Adventure", "Comedy"))
movie
movie[2]
movie[2:3]
#named list
movie <- list(name = "Toy Story",year = 1995,genre = c("Animation", "Adventure", "Comedy"))
movie$genre
movie["genre"]
class(movie$name)
class(movie$foreign)
movie[["age"]] <- 5
movie
movie[["age"]] <- 6
# Now it's 6, not 5
movie
movie[["age"]] <- NULL
movie
# We split our previous list in two sublists
movie_part1 <- list(name = "Toy Story")
movie_part2 <- list(year = 1995, genre = c("Animation", "Adventure", "Comedy"))

# Now we call the function c() to put everything together again
movie_concatenated <- c(movie_part1, movie_part2)

# Check it out
movie_concatenated

movies <- data.frame(name = c("Toy Story", "Akira", "The Breakfast Club", "The Artist",
                              "Modern Times", "Fight Club", "City of God", "The Untouchables"),
                     year = c(1995, 1998, 1985, 2011, 1936, 1999, 2002, 1987),
                     stringsAsFactors=F)

movies
movies$name
# This returns the first (1st) column
movies[1]
str(movies)
class(movies$year)

movies[1,2] #1-Toy Story, 2-1995

head(movies)

tail(movies)

movies['length'] <- NULL
movies['length'] <- c(81, 125, 97, 100, 87, 139, 130,119)
movies
# adding observations
movies <- rbind(movies, c(name="Dr. Strangelove", year=1964, length=94))
movies
#deleting
movies <- movies[-9,]
movies

movies[["length"]] <- NULL
movies

students <- data.frame("student" = c("john", "mary"), "id" = c(1, 2))
students

#array(“student" = c("john", "mary"), "id" = c(1, 2)) greška
# students <- data.frame(student = c(john, mary), id = c(1, 2))
# list(“student" = c("john", "mary"), "id" = c(1, 2))

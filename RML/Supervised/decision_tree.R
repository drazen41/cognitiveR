putanja = "~/R/cognitiveR/cognitiveR/RML/Supervised/"
datoteka = paste(putanja,"mushroom.data",sep="")
datoteka
download.file("https://ibm.box.com/shared/static/dpdh09s70abyiwxguehqvcq3dn0m7wve.data", datoteka)

mushrooms <- read.csv("mushroom.data", header = F)
mushrooms

# Define column names for the mushrooms data frame.
colnames(mushrooms) <- c("Class","cap.shape","cap.surface","cap.color","bruises","odor","gill.attachment","gill.spacing",
                         "gill.size","gill.color","stalk.shape","stalk.root","stalk.surface.above.ring",
                         "stalk.surface.below.ring","stalk.color.above.ring","stalk.color.below.ring","veil.type","veil.color",
                         "ring.number","ring.type","print","population","habitat")
head(mushrooms)

# Define the factor names for "Class"
levels(mushrooms$Class) <- c("Edible","Poisonous")

# Define the factor names for "odor"
levels(mushrooms$odor) <- c("Almonds","Anise","Creosote","Fishy","Foul","Musty","None","Pungent","Spicy")
# Define the factor names for "print"
levels(mushrooms$print) <- c("Black","Brown","Buff","Chocolate","Green","Orange","Purple","White","Yellow")
head(mushrooms)

# Import our required libraries
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

# Create a classification decision tree using "Class" as the variable we want to predict and everything else as its predictors.
myDecisionTree <- rpart(Class ~ ., data = mushrooms, method = "class")
# Print out a summary of our created model.
print(myDecisionTree)

rpart.plot(myDecisionTree, type = 3, extra = 2, under = TRUE, faclen=5, cex = .75)

newCase  <- mushrooms[10,-1]
newCase

predict(myDecisionTree, newCase, type = "class")

n <- nrow(mushrooms)
n
train_ind <- sample(c(1:n), size = 10)

## 75% of the sample size
smp_size <- floor(0.75 * n)

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- base::sample(c(1:n), size = smp_size)

mushrooms_train <- mushrooms[train_ind, ]
mushrooms_test <- mushrooms[-train_ind, ]

newDT <- rpart(Class ~ ., data = mushrooms_train, method = "class")

result <- predict(newDT, mushrooms_test[,-1], type = "class")
head(result)

head(mushrooms_test$Class)

########################## Customers
datoteka = paste(putanja,"custchurn.csv")

download.file("https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv", 
              destfile = datoteka)
#print("File saved under /resources/data/custchurn.csv")

custchurns <- read.csv("custchurn.csv")
head(custchurns)

df <- custchurns[2:ncol(custchurns)]
head(df)
unique(unlist(df["InternetService"]))
# Create a classification decision tree using "churn" as the variable we want to predict and everything else as its predictors.
myDecisionTree <- rpart(Churn ~ ., data = df, method = "class")
print(myDecisionTree)

rpart.plot(myDecisionTree, type = 3, extra = 2, under = TRUE, faclen=5, cex = .75)

# install the packages (note: this may take some time)
install.packages("class")
install.packages("caret")
install.packages("mlbench")
install.packages("e1071")

library(class)
library(caret)
require(mlbench)
library(e1071)
library(base)
require(base)

data(Sonar)
head(Sonar)
tail(Sonar)

cat("number of rows and columns are:", nrow(Sonar), ncol(Sonar))

base::table(Sonar$Class) 
apply(Sonar, 2, function(x) sum(is.na(x))) 

SEED <- 123
set.seed(SEED)
data <- Sonar[base::sample(nrow(Sonar)), ] # shuffle data first
bound <- floor(0.7 * nrow(data))
df_train <- data[1:bound, ] 
df_test <- data[(bound + 1):nrow(data), ]
cat("number of training and test samples are ", nrow(df_train), nrow(df_test))

cat("number of training classes: \n", base::table(df_train$Class)/nrow(df_train))
cat("\n")
cat("number of test classes: \n", base::table(df_test$Class)/nrow(df_test))

X_train <- subset(df_train, select=-Class)
y_train <- df_train$Class
X_test <- subset(df_test, select=-Class) # exclude Class for prediction
y_test <- df_test$Class

model_knn <- knn(train=X_train,
                 test=X_test,
                 cl=y_train,  # class labels
                 k=3)
model_knn

conf_mat <- base::table(y_test, model_knn)
conf_mat

cat("Test accuracy: ", sum(diag(conf_mat))/sum(conf_mat))

knn_loocv <- knn.cv(train=X_train, cl=y_train, k=3)
knn_loocv

conf_mat_cv <- base::table(y_train, knn_loocv)
conf_mat_cv
cat("LOOCV accuracy: ", sum(diag(conf_mat_cv)) / sum(conf_mat_cv))

SEED <- 2016
set.seed(SEED)
# create the training data 70% of the overall Sonar data.
in_train <- createDataPartition(Sonar$Class, p=0.7, list=FALSE) # create training indices
ndf_train <- Sonar[in_train, ]
ndf_test <- Sonar[-in_train, ]


# lets create a function setup to do 5-fold cross-validation with 2 repeat.
ctrl <- trainControl(method="repeatedcv", number=5, repeats=2)

nn_grid <- expand.grid(k=c(1,3,5,7))
nn_grid

set.seed(SEED)

best_knn <- train(Class~., data=ndf_train,
                  method="knn",
                  trControl=ctrl, 
                  preProcess = c("center", "scale"),  # standardize
                  tuneGrid=nn_grid)
best_knn

SEED <- 123 
set.seed(SEED) 
ctrl <- trainControl(method="repeatedcv", number=5, repeats=5) 
nn_grid <- expand.grid(k=c(1, 3, 5, 7)) 
best_knn_reduced <- train( Class~., data=ndf_train, method="knn", 
                           trControl=ctrl, preProcess=c("center", "scale","YeoJohnson"))
X_test <- subset(ndf_test, select=-Class) 
pred_reduced <- predict(best_knn_reduced, newdata=X_test, model="best") 
conf_mat_best_reduced <- confusionMatrix(ndf_test$Class, pred_reduced) 
conf_mat_best_reduced 
























































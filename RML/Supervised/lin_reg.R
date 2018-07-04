my_data <- read.csv("https://ibm.box.com/shared/static/q0gt7rsj6z5p3fld163n70i65id3awz3.csv")
head(my_data)
str(my_data)

plot(my_data$Access_to_Sanitation, my_data$Life_Expectancy, xlab = "Access to Sanitation (% of population)",
     ylab = "Life Expectancy (years)", col = "blue", lwd = 2)

# Order rows increasingly by Sanitation
my_data <- my_data[order(my_data["Access_to_Sanitation"]),]

barplot(my_data[c(1:20),"Access_to_Sanitation"],
        names.arg = as.vector(my_data[c(1:20),"Country"]),
        col = "red", las = 2,
        ylab = "Access to Sanintation (% of Population)")

# Order rows increasingly by Life Expectancy
my_data <- my_data[order(my_data["Life_Expectancy"]),]

barplot(my_data[c(1:20),"Life_Expectancy"],
        names.arg = as.vector(my_data[c(1:20),"Country"]),
        col = "blue", las = 2,
        ylab = "Life Expectancy (years)")

# Create a vector containing the sanitation data
sanitation <- as.vector(my_data$Access_to_Sanitation)

# Build a linear model
model <- lm(Life_Expectancy ~ sanitation, data=my_data)

# Get some information about the model
summary(model)
model


{
  # Plot the previous scatter plot
  plot(my_data$Access_to_Sanitation, my_data$Life_Expectancy, xlab = "Access to Sanitation (% of Population)",
       ylab = "Life Expectancy (years)", col = "blue", lwd = 2)
  
  # Fit a line in the plot
  abline(model, col = "red")
}

# Tips: using point that fall out of the term interval in the training dataset may lead to errors of prediction that are much larger than expected. 
summary(my_data$Access_to_Sanitation)

# Create points to predict, use the point that fall between 6.70 and 100.00
pointsToPredict <- data.frame(sanitation = c(10, 42))

#Use predict() to compute our predictions!
predictionWithInterval <- predict(model, pointsToPredict, interval = 'prediction')
predictionWithInterval

{
  # Plot the previous scatter plot
  plot(my_data$Access_to_Sanitation, my_data$Life_Expectancy, xlab = "Access to Sanitation (% of Population)",
       ylab = "Life Expectancy (years)", col = "blue", ylim=c(45, 85), lwd = 2)
  
  # Add the new predicted points!
  points(pointsToPredict$sanitation, predictionWithInterval[,"fit"], col = "red", lwd = 4)
  points(pointsToPredict$sanitation, predictionWithInterval[,"lwr"], col = "firebrick4", lwd = 4)
  points(pointsToPredict$sanitation, predictionWithInterval[,"upr"], col = "firebrick4", lwd = 4)
  
  legend("topleft",legend = c("Dataset Points", "Prediction Points"), fill = c("blue","red"), bty = "n")
  
  # Fit a line in the plot
  abline(model, col = "red")
}

# ****************************************************************************************
# the cars dataset has two variables (speed and dist)
head(cars)
brzina <- as.vector(cars$speed)
brzina

# Build a linear model
model <- lm(dist ~ brzina, data=cars)

# Get some information about the model
summary(model)
model


{
  # Plot the previous scatter plot
  plot(cars$speed, cars$dist, xlab = "Cars dist",
       ylab = "Cars speed", col = "blue", lwd = 2)
  
  # Fit a line in the plot
  abline(model, col = "red")
}

# Create points to predict, use the point that fall between 6.70 and 100.00
pointsToPredict <- data.frame(brzina = c(13, 60, 120))

predictionWithInterval <- predict(model, pointsToPredict, interval = 'prediction')
predictionWithInterval

















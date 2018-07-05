# Retrieve the "iris" dataset, sans the "Species" column.
iris_numerical <- iris[,-5]

install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

scatterplot3d(iris[,1:3],
              main="3D Scatter Plot",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)",
              zlab = "Petal Length (cm)")

# Perform PCA on the numerical data from "iris"
# prcomp does all the work of centering and scaling our data for us!
pcaSolution <- prcomp(iris_numerical, center=TRUE, scale.=TRUE)

# Print a summary of the analysis
print(pcaSolution)

# Print a summary of the results
summary(pcaSolution)

plot(pcaSolution, type="l", main="Eigenvalues for each Principal Component")

# Calculate and print the eigenvalues for the principal components.
# The eigenvalues are calculated by squaring the standard deviation values for each component.
eigenvalues <- (pcaSolution$sdev)**2
eigenvalues

{
  # Plot the Scree plot.
  screePlot <- plot(pcaSolution, type="l", main="Eigenvalues with Kaiser-Guttman cutoff")
  
  # Add a cutoff line based on the mean of the eigenvalues.
  # This should be equal to one for centered and scaled data.
  abline(h=mean(eigenvalues),lty=2,col="red")
}

# Retrieve the values of the observations for each principal component.
rotated_values <- pcaSolution$x

# Print out the first six rows of data.
head(rotated_values)

dim(rotated_values)
rotated_values = as.data.frame(rotated_values)
colors = iris[,5];
levels(colors)= c(1,2,3)
plot(rotated_values$PC1, rotated_values$PC2, xlab="Principal component 1",
     ylab="principal component 2", pch = 20, cex=2,
     col=adjustcolor(colors,alpha.f=0.5))
legend(2,2, legend=levels(iris[,5]),pch=20,cex=0.8,col=levels(colors))

#*********************************************************************************
putanja = "~/R/cognitiveR/cognitiveR/RML/Dimension reduce - feature extract/"
datoteka <- paste(putanja,"credit_fraud.csv",sep="")
download.file("https://ibm.box.com/shared/static/78g5lc5krkvclwbnxi6zodemlktr9v9r.csv",datoteka)
#data <- read.csv("https://ibm.box.com/shared/static/78g5lc5krkvclwbnxi6zodemlktr9v9r.csv", header=T)
data <- read.csv(datoteka)
head(data)

# get the list of column index that are numeric
numvec=c();
for( i in 1:ncol(data)){
  if ( is.numeric(data[,i])) numvec = append(numvec,i)
}
cat('Numeric columns # ', numvec, "\n")


# let subset the numeric variable and plots the box plots to see their distribution approximately
datn = data[,numvec]
head(datn)

# NOTE: that the distribution of variables are pretty skewed and transformation is needed in order to
#       make them comparable to plot, so instead of transforming each variable we transform the y-axis here
boxplot(datn,col="blue",pch=20, ylab="on original scale")

# Perform a principal component analysis on the data and print the results
pcasolutions = prcomp(datn, center=TRUE, scale=TRUE)

print(pcasolutions)

## check  you answers:
options(warn=-1)
if ( length(pcasolutions$scale)==1 & !(pcasolutions$scale)) { 
  cat(" Scaling of variables is necessary here, ... did you make use of \"scale=TRUE\" option ? ")
}else{
  cat(" passed ....")
}

options(warn=0)

#eigen-values : 
eigenvalues = pcasolutions$sdev**2

# cumulative proportion of variance explained by each principal component:
prop = cumsum(eigenvalues)/sum(eigenvalues)

plot(prop, type="b", ylab="proportion of variance explained", xlab="# of principal components")

# taking a look at the first three PCs 
rotated_data = pcasolutions$x[,1:3]
head(rotated_data)

# Rotation gives you the factor loadings # 
eigenvectors = pcasolutions$rotation[,1:3]

options(warn=-1)
if ( all(dim(eigenvectors) == c(ncol(datn),3) )){
  if ( round(eigenvectors[1,1],4) == 0.6574 & round(eigenvectors[2,1],4) ==0.7180 )
    cat("Passed !!\n")
  
}else{
  cat("NOTE: perhaps you want to go back and check the creation of the PCAs? \n")
}

options(warn=0)

# some colouring based on other categorical variables;
catcol = 'class'
colcolumn  = data[,catcol]
l1 = levels(colcolumn)
colpalatte = c('green','blue', 'cyan')
mycol =colpalatte[as.numeric(colcolumn)]
head(mycol)

install.packages("plot3D")
library(plot3D)

plot(rotated_data[,1],rotated_data[,2],pch=20, col=mycol, xlab="First PC", ylab="Second PC",
     main=paste('Plot of First 2 PC (colour by :',catcol, ")"))
legend(3.6,4, legend =l1, pch=20, col=colpalatte[1:length(l1)])

plot(rotated_data[,2],rotated_data[,3],pch=20, col=mycol, xlab="Second PC", ylab="Third PC",
     main=paste('Plot of 2nd and 3rd PC (colour by :',catcol, ")"))
legend(3,3.5, legend =l1, pch=20, col=colpalatte[1:length(l1)])

plot3D::scatter3D(rotated_data[,1],rotated_data[,2],rotated_data[,3] , pch=20, bty='g',phi=0,cex=2,ticktype='detailed',
                  colkey=FALSE, col = mycol, xlab="First PC", ylab="Second PC", zlab="Third PC")

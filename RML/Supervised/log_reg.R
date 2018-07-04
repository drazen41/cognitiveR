install.packages("dummies")
install.packages('class')
install.packages('VGAM')
print("Done") #Takes about 30 seconds
install.packages('gridExtra')

library(ggplot2)
library(gridExtra)
options(jupyter.plot_mimetypes = 'image/png')

print("Done")

putanja = "~/R/cognitiveR/cognitiveR/RML/Supervised/"
datoteka = paste(putanja,"LoanData.csv",sep="")
#Click here and press Shift+Enter
download.file("https://ibm.box.com/shared/static/sv3oy0gyhuiifmosxsvxt5ogfs71iv37.csv",
              destfile = datoteka, quiet = TRUE)
options(scipen = 999) #disable scientific notation

LoanData <- read.csv(datoteka)
head(LoanData)

nrow(LoanData)
ncol(LoanData)

#library(plyr)
#count(LoanData,'loan_status')
base::table(LoanData['loan_status'])

ggplot(LoanData, aes(x=Principal, fill=loan_status)) +geom_histogram(binwidth=120,alpha=0.35,aes(y=0.5*..density..),position='identity')

ggplot(LoanData, aes(x=terms, fill=loan_status)) +geom_histogram(binwidth=10,alpha=0.45,aes(y=1*..density..),position='identity')+scale_x_continuous(limits = c(0, 40))

ggplot(LoanData, aes(x=age, fill=loan_status)) +geom_histogram(binwidth=1,alpha=0.55,aes(y=1*..density..),position='identity')

hist_top <-ggplot(LoanData, aes(x=Principal, fill=loan_status)) +geom_histogram(binwidth=100,alpha=0.55,aes(y=1*..density..),position='identity')+ theme(legend.position="none")+scale_x_continuous(limits = c(200, 1100))
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

#qplot(Principal, age, data = LoanData, colour = loan_status)
scatter <-ggplot(LoanData, aes(Principal, age),fill= loan_status)  + geom_point(aes(colour = loan_status))+ theme(legend.position="top")
hist_right <-ggplot(LoanData, aes(x=age, fill=loan_status))+scale_x_continuous(limits = c(20, 45)) +geom_histogram(binwidth=1,alpha=0.55,aes(y=0.5*..density..),position='identity')+coord_flip()+ theme(legend.position="none")


grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))


ggplot(LoanData, aes(x=dayofweek, fill=loan_status))  +geom_histogram(binwidth=1,alpha=0.55,aes(y=1*..density..),position='identity')+scale_x_continuous(limits = c(0, 7))


namevector <- c("Weekend")
LoanData[,namevector] <- 0
LoanData$Weekend[LoanData$dayofweek>3]<-1
head(LoanData)

namevector <- c("Gender01")
LoanData[,namevector] <- 0
LoanData$Gender01[LoanData$Gender=='male']=1
head(LoanData[,c('Gender','Gender01')])

base::table(LoanData$Gender01, LoanData$loan_status)
ggplot(LoanData, aes(x=Gender01, fill=loan_status))  +geom_histogram(binwidth=1,alpha=0.55,aes(y=1*..density..),position='identity')+scale_x_continuous(limits = c(0, 2))

library(dummies)

head(LoanData['education'])

LoanData=dummy.data.frame(LoanData, names=c("education"))
head(LoanData[c('educationBechalor', 'educationcollege',  'educationHigh School or Below','educationMaster or Above')])

Colunms <- c('Principal','terms','age','educationBechalor', 'educationcollege',  'educationHigh School or Below','educationMaster or Above','Weekend','Gender01')
Data <- LoanData[Colunms]
head(Data)

NewColumn <- c("Class")
Data[,NewColumn] <- 0
Data$Class[LoanData$loan_status=='PAIDOFF']=1
head(Data[,NewColumn],10)
head(LoanData$loan_status,10)

Data[Colunms] <- scale(Data[Colunms])
head(Data[Colunms])

set.seed(3)
testindex <- sample.int(nrow(Data))[1:floor(0.1*nrow(Data))]
TestData <- Data[testindex,];
head(TestData)
TrainData=Data[-testindex,]
head(TrainData)

model <- glm(Class~.,family=binomial(link='logit'),data=TrainData, control = list(maxit = 50))
summary(model)

fitted.results <- predict(model,newdata=TestData,type='response')
yhat <- ifelse(fitted.results > 0.5,1,0)
yhat[1:5]

y <- TestData[,c('Class')]
y[1:4]

mean(yhat==y)
ConfutationMatrix<- base::table(paste(as.character(yhat)," pred", sep =""), paste(as.character(y)," true", sep =""))
ConfutationMatrix


























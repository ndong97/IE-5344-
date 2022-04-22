####################### Flipped Assignment 6 ######################

library(MASS)

####### Input data #########
setwd('G:/OneDrive - Texas Tech University/IE 5344 Statistical Data Analysis/Flipped Assignment 6')
data<-read.csv('data-table-B8.csv', header = TRUE)
colnames(data) <- c("x1","x2","y")
x0 <- rep(1,36)
X <- cbind(x0,data$x1,data$x2)
colnames(X) <- c("x0","x1","x2")
Y <- as.matrix(data$y)
dim(Y)
#Part a. 
#Y = f(X) + \epsilon = X\beta + \epsilon
#y = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + \epsilon

#Part b. 
Y
dim(Y)
X
dim(X)
# Y is 36*1 and X is 36*3.

#Part c.
beta <- ginv(t(X)%*%X)%*%t(X)%*%Y
beta

fit<-lm(y~x1+x2,data)
fit$coefficients

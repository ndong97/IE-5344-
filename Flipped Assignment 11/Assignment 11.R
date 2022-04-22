###Assignment 11
library(car)
dat <- read.csv("C:/Users/Saunak/OneDrive - Texas Tech University/Work/TTU/Coursework/Spring 2022/Stat Data Analysis/data-table-B21.csv")
head(dat)
dat <- dat[,-1]
colnames(dat) <- c("y","x1","x2","x3","x4")

##Part a
fit <- lm(y~.,data=dat)
summary(fit)
#We notice that the overall model is significant (f-statistic is significant) 
#and the R-squared value is 0.9824 which is high, but the individual predictors are not significant (p-values are larger than 0.05). 

##Part b
vif(fit)

##Part c

#The vif of all predictors was high (above 10). So, we remove the predictor with the highest vif value first.
dat1 <- dat[,-5]
fit2 <- lm(y~.,data=dat1)
summary(fit2)
vif(fit2)

# The VIF of all the predictors is low (below 5). But x3 is not significant, so we remove x3 and check the vif 

dat2 <- dat[,-4]
fit3 <- lm(y~.,data=dat2)
summary(fit3)
vif(fit3)

#The vif of two of the predictors was high (above 10). so we remove x2 and check the vif. 

dat3 <- dat[,-3]
fit4 <- lm(y~.,data=dat3)
summary(fit4)
vif(fit4)

# The VIF of all the predictors is low (below 5). But x3 is not significant, so we remove x1 and check the vif. 

dat4 <- dat[,-2]
fit5 <- lm(y~.,data=dat4)
summary(fit5)
vif(fit5)

#The vif of two of the predictors was high (above 10).
# So, we compare fit2 with fit 4 since all of them have low vif values.
# Regarding the R-squared value and p-value, fit2 should be the best fit, 
#but x3 in fit 2 is not significant but is almost significant in fit4. 
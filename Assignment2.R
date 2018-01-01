rm(list=ls())

########################
####  Question 1  ######
########################

set.seed(5072)
x <- rnorm(100,mean = 0, sd = 1)
eps <- rnorm(100, mean= 0, sqrt(0.25))
y <- -1 + 0.5*x + eps

length(y)

#B_0 = -1.0 and B_1 = 0.50 from the population function

plot(x,y, pch=16, xlab = 'x', ylab='y', main='Scatterplot of y vs x')

#There is a positive relationship between x and y.
#There is a small degree of linearity within this graph. The points
#obviously create a positive trend; however, there is a larger variance
#between any two points (which is made obvious with the plotted population line).

lm.fit <- lm(y~x)
summary(lm.fit)

#Beta_0^ is -1.00298 and Beta_1^ is 0.43523. 
#Beta_0 is slightly more than Beta_0^, while Beta_1 is slightly
#more than Beta_1^; however, they are still relatively close together.

plot(x,y, xlab = 'x', ylab = 'y', main = 'Moderate Error in the Population', legend = TRUE)
abline(lm.fit, pch=16, col='black')
abline(-1, 0.5, pch=16, col='red')
legend('topleft', legend = c("Least Squares", "Population"), col=c('black', 'red'), cex=.75, pch=16)

lm.fit.poly <- lm(y~x+poly(x,2))
summary(lm.fit.poly)

anova(lm.fit.poly)
#The polynomial regression is worse because the
#p-value is higher for the x^2.

#####################################################################

epsless <- rnorm(100, mean= 0, sqrt(0.10))
yless <- -1 + 0.5*x + epsless

length(yless)

#B_0 = -1.0 and B_1 = 0.50 from the population function

plot(x,yless, pch=16, xlab = 'x', ylab='y', main='Scatterplot of y vs x')

#There is a positive relationship between x and y.
#There is a larger degree of linearity within this plot. The points appear
#to create a "thick" line, and the lower variance between any two points is
#also made more obvious.

lm.fitless <- lm(yless~x)
summary(lm.fitless)

#Beta_0^ is -0.99869 and Beta_1^ is 0.52200. 
#Beta_0 is slightly less than Beta_0^, while Beta_1 is slightly
#less than Beta_1^; however, they are still relatively close together.

plot(x,yless, xlab = 'x', ylab = 'y', main = 'Less Error in the Population', legend = TRUE)
abline(lm.fitless, pch=16, col='black')
abline(-1, 0.5, pch=16, col='red')
legend('topleft', legend = c("Least Squares", "Population"), col=c('black', 'red'), cex=.75, pch=16)

lm.fit.polyless <- lm(yless~x+poly(x,2))
summary(lm.fit.polyless)

anova(lm.fit.polyless)
#The p-value for the quadratic term is too large for a polynomial model
#to be any better than just the linear model.

#######################################################################

epsmore <- rnorm(100, mean= 0, sqrt(0.50))
ymore <- -1 + 0.5*x + epsmore

length(ymore)

#B_0 = -1.0 and B_1 = 0.50 from the population function

plot(x,ymore, pch=16, xlab = 'x', ylab='y', main='Scatterplot of y vs x')

#There is a positive relationship between x and y.
#There is a slight degree of linearity in this scatterplot. The points 
#are further apart; however, there is still a obvious few that there is a
#positive relationship between x and y.

lm.fitmore <- lm(ymore~x)
summary(lm.fitmore)

#Beta_0^ is -1.07399 and Beta_1^ is 0.58320. 
#Beta_0 is more than Beta_0^, while Beta_1 is
#larger than Beta_1^. This variance allows for the largest difference 
#between these two values.

plot(x,ymore, xlab = 'x', ylab = 'y', main = 'Higher Error in the Population', legend = TRUE)
abline(lm.fitmore, pch=16, col='black')
abline(-1, 0.5, pch=16, col='red')
legend('topleft', legend = c("Least Squares", "Population"), col=c('black', 'red'), cex=.75, pch=16, lty=1)

lm.fit.polymore <- lm(ymore~x+poly(x,2))
summary(lm.fit.polymore)

anova(lm.fit.polymore)
#The p-value for the quadratic term is too large for the polynomial model
#to be any better than just the linear model.

print('For originial data set: ')
confint(lm.fit, level=.95)
print('For the noiser data set: ')
confint(lm.fitmore, level=.95)
print('For the less noisy data set: ')
confint(lm.fitless, level=.95)
#The less noisy the data set, the smaller the confidence interval; while,
#the more noisy the data set, the larger the condidence interval. The makes
#sense with the amount of variance within each model - more variance, implies
#larger confidence interval.

######################
####  Question 2  ####
######################

set.seed(5072)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)

#beta_0 = 2, beta_1 = 2, beta_2 = 0.3

cor(y,x1, method='pearson')
cor(y,x2, method='pearson')
cor(x1,x2)

pairs(~y + x1 + x2)

#Between any two variables there is a positive correlation; however,
#each pair has a differing degree of linearity. There is less variability
#and a stronger degree of linearity between x1 and x2, while this is not
#necessarily true when comparing x1,x2 to y.

lm.fit.both <- lm(y~x1+x2)
lm.fit.both

#beta^_0 = 2.0409, beta^_1 = 2.3410, beta^_2 = -0.4962

anova(lm.fit.both)

#beta^_1 has a p-value of less than 0.05; therefore, it is statistically significant.
#beta^_2 has a large p-value and is therefore, not statistically significant.

#If beta_1 =0, we would reject the null hypothesis because the value of beta_1
#is statistically significant. If beta_2=0, we would not reject the null hypothesis
#(accept it) because beta_2 is not statistically significant.

lm.fit.justx1 <- lm(y~x1)
lm.fit.justx1

anova(lm.fit.justx1)
#We would reject the null hypothesis because the p-value of beta^_1 forces
#beta^_1 to be statistically significant.

lm.fit.justx2 <- lm(y ~ x2)
lm.fit.justx2

anova(lm.fit.justx2)
#We would reject the null hypothesis because beta^_2 is statistically significant.

#Basing just the model on x1 or x2 forces each value to be statistically
#significant; however, in the overall model, beta^_2 is not statistically significant.

#########################################################################

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.fit.bothnew <- lm(y~x1+x2)
lm.fit.bothnew

#beta^_0 = 2.173, beta^_1 = 1.173, beta^_2 = 1.478

anova(lm.fit.both)

lm.fit.justx1 <- lm(y~x1)
lm.fit.justx1

anova(lm.fit.justx1)
#We would reject the null hypothesis because the p-value of beta^_1 forces
#beta^_1 to be statistically significant.

lm.fit.justx2 <- lm(y ~ x2)
lm.fit.justx2

anova(lm.fit.justx2)

#By adding this 101th value, we still have similar results. In the lm.fit.both model,
#beta^_2 is not statistically significant, but in each "partial" model, the beta value is
#statistically significant.

plot(lm.fit.bothnew)
par(mfrow=c(2,2))
#Based on this plot, we see that point 101 is an outlier, as well as a high-leverage point.

#########################
####  Question 3  #######
#########################

library(MASS)
set.seed(5072)

names <- rep(1:14)
fstat <- rep(1:14)
pvalue <- rep(1:14)
betahat <- rep(1:14)
lm.fit.coef <- rep(1:14)
for(i in 1:14){
  lm.fit <- lm(Boston$crim~Boston[,i])
  #print(coef(summary(lm.fit))[2,4])
  lmsummary <- summary(lm.fit)
  lmanova <- anova(lm.fit)
  lmcoef <- coef(lm.fit)
  names[i] <- names(Boston)[i]
  fstat[i] <- lmsummary$fstatistic[1]
  pvalue[i] <- lmanova$'Pr(>F)'[1]
  betahat[i] <- lmcoef[2]
}
data <- data.frame(names, fstat, pvalue, betahat)[-1,]
data

#All the variables are statistically significant, except the chas-variable
#with a p-value of approximately 0.209.

par(mfrow=c(3,4))
for(i in 2:14){
  if(i != 4){
    plot(Boston[,i], Boston$crim, main=colnames(Boston[i]), xlab='x')
    abline(lm(Boston$crim~Boston[,i]), col='red')
  }
}
par(mfrow=c(1,1))

lm.fit.all <- lm(crim~., data=Boston)
lm.fit.all
summary(lm.fit.all)
for(i in 1:14){
  if(coef(summary(lm.fit.all))[i,4] < 0.05){
    print(colnames(Boston[i]))
  }
}

plot(data$betahat, lm.fit.all$coefficients[-1], xlab = 'Simple', ylab='Multiple')
abline(0,0, col='red', lty=3)
abline(NULL, NULL, v=0, col='red', lty=3)

#For the most part, whether or not one uses the simple or
#multiple regression model does not necessarily matter.
#The results appear the same for either method; however, it
#does look like more points are closer to the vertical zero line
#for the simple regression - allowing for the simple regression
#model to be the more accurate choice.
names <- rep(1:14)
fstat <- rep(1:14)
pvalue <- rep(1:14)
for(i in 1:14){
  if(i != 4){
    lm.fit <- lm(Boston$crim~Boston[,i]+poly(Boston[,i], 3, raw=TRUE))
    print(lm.fit)
    print(colnames(Boston[i]))
    lmsummary <- summary(lm.fit)
    lmanova <- anova(lm.fit)
    names[i] <- names(Boston)[i]
    fstat[i] <- (anova(lm.fit))$'F value'[2]
    pvalue[i] <- lmanova$'Pr(>F)'[2]
  }
}
name = names[-4]
fstat = fstat[-4]
pvalue = pvalue[-4]
data <- data.frame(name, fstat, pvalue)[-1,]
data[order(data$pvalue),]

#We would reject the null hypothesis for the following:
  #medv, dis, nox, indus, age, tax, ptratio, rm, zn, rad, lstat
#From the other model, we would reject the null for the following:
  #zn, dis, rad, black, medv
#So we would reject the null for medv, zn, dis, and rad using both models.

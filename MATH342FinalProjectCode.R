# Reads data into a table to manipulate data=read.table("project_data.txt", header=TRUE) data
attach(data)

# Correlation Matrix to check for collinearity
pairs(data)
cor(data)

# Remove Ex1 from the model since it shows strong collinearity with Ex0
# The full model
modelFull=lm(R ~ Age + S + Ed + Ex0 + LF + M + N + NW + U1 + U2 + W + X, data=data) summary(modelFull)

# Checks the VIF values of the variables in the data set install.packages("car")
library(car)
vif(modelFull)

### Stepwise Regression
model0=lm(R~1, data=data) ### constant mean model
MSEk = anova(modelFull)["Residuals", "Mean Sq"]
step(model0, scope=list(lower=~1, upper=modelFull), direction="both", scale=MSEk) # Returns model R ~ Ex0 + X + Ed + Age + U2 + W

### Forward Selection
model0=lm(R~1, data=data) ### constant mean model
MSEk = anova(modelFull)["Residuals", "Mean Sq"]
step(model0, scope=list(lower=~1, upper=modelFull), direction="forward", scale=MSEk) # Returns model R ~ Ex0 + X + Ed + Age + U2 + W

### Backwards Elimination
model0=lm(R~1, data=data) ### constant mean model
MSEk = anova(modelFull)["Residuals", "Mean Sq"]
step(modelFull, scope=list(lower=~1, upper=modelFull), direction="backward", scale=MSEk) # Returns model R ~ Age + Ed + Ex0 + U2 + W + X

# The model given from Different Variable Selection methods modelA = lm(R ~ Age + Ed + Ex0 + U2 + W + X) summary(modelA)
# Residuals for modelA resid=modelA$residuals resid y.hat=predict(modelA)
install.packages("MASS") library(MASS)
e.star = studres(modelA)

#Plot of Cook's Distancs
influence.measures(modelA)
cook.d.model=influence.measures(modelA)$infmat[,"cook.d"]
par(mfrow=c(1,2))
plot(cook.d.model, main="Plot of Cook's Distance", ylab="Cook's Distance", xlab="Observation Number")

#Plot of Studentized Residuals
plot(e.star, ylim=c(-3,3), ylab="Studentized Residuals",
     xlab="Observation Number", main="Plot of Studentized Residuals") abline(h=3, col="blue", lty=2)
abline(h=-3, col="blue", lty=2)
abline(h=0)

# Remove observation 11 from the data set data=data[-11,]
data
attach(data)

### Stepwise Regression
model0=lm(R~1, data=data) ### constant mean model
MSEk = anova(modelFull)["Residuals", "Mean Sq"]
step(model0, scope=list(lower=~1, upper=modelFull), direction="both", scale=MSEk) # Returns model R ~ Ex0 + X + Ed + Age + U2

# Model given after outlier was removed

modelB=lm(R ~ Ex0 + X + Ed + Age + U2) summary(modelB)

# Model with interaction term
modelI=lm(R ~ Ex0 + X + Ed + Age + U2 + S + S*Ed) summary(modelI)

# ANOVA Table comparing models with and without interaction term anova(modelB, modelI)
# Since p-value is small, we decide that the larger model is the better fit for the data
### Best subset selection
### use install.packages("leaps") to download and install the leaps package. install.packages("leaps")
library(leaps) best.subset.cp=leaps(x=cbind(Age,S,Ed,Ex0,LF,M,N,NW,U1,U2,W,X,S*Ed), y=R, method="Cp")
best.subset.cp
min.cp.value=min(best.subset.cp$Cp) min.cp.value
min.cp.location=which.min(best.subset.cp$Cp) min.cp.location
best.subset.cp$which[min.cp.location,] # Model returned is the same as modelI
# Residuals for modelI resid=modelI$residuals resid y.hat=predict(modelI)
e.star = studres(modelI)
#Plot of Cook's Distancs for modelI influence.measures(modelI) cook.d.model=influence.measures(modelI)$infmat[,"cook.d"] par(mfrow=c(1,2))

plot(cook.d.model, main="Plot of Cook's Distance", ylab="Cook's Distance", xlab="Observation Number")
#Plot of Studentized Residuals for modelI
plot(e.star, ylim=c(-3,3), ylab="Studentized Residuals",
     xlab="Observation Number", main="Plot of Studentized Residuals") abline(h=3, col="blue", lty=2)
abline(h=-3, col="blue", lty=2)
abline(h=0)
# Histogram of Residuals, and Normality probability plot of residuals resid=modelB$residuals
par(mfrow=c(1,2))
hist(resid, main="Histogram of Residuals", xlab="Residuals") qqnorm(resid, main="Normal Probability Plot of Residuals")
qqline

# Prediction Interval
newdata=data.frame(S=0, Age=300, Ed=105, Ex0=450, U2=33, X=431) predict(modelI, newdata, interval="predict")
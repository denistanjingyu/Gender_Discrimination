# Load necessary tools
library(corrplot)
library(caTools)
library(car)
library(data.table)

lawsuit.dt <- fread("lawsuit.csv") #Get the lawsuit file

summary(lawsuit.dt) 
str(lawsuit.dt)
# Convert relevant columns to factors
lawsuit.dt$Dept <- factor(lawsuit.dt$Dept)
lawsuit.dt$Gender <- factor(lawsuit.dt$Gender)
lawsuit.dt$Clin <- factor(lawsuit.dt$Clin)
lawsuit.dt$Cert <- factor(lawsuit.dt$Cert)
lawsuit.dt$Rank <- factor(lawsuit.dt$Rank) 
str(lawsuit.dt)

# since ID has totally no effect on proving discrimination, we decided to remove the column.
lawsuit.dt <- lawsuit.dt[,ID := NULL]

# We would be excluding Sal95 from the regression model since it has an extremely high correlation with Sal94.
cor(lawsuit.dt$Sal94, lawsuit.dt$Sal95)

# Model 1: 
m1 <- lm(Sal94 ~ . -Sal95, data=lawsuit.dt)
summary(m1)

par(mfrow = c(2,2))  # Plot 4 charts in one plot - 2 by 2.
plot(m1)  # Plot model 2 diagnostics
par(mfrow = c(1,1))  # Reset plot options to 1 chart in one plot.

# Running forward and backward selection for m1
m2 <- step(m1, direction = "both")
summary(m2)

# m2 now includes Dept, Clin, Cert, Exper and Rank - Removed Prate and Gender.
# Run VIF to ensure m2 has no multicolinearity.
vif(m2)
# VIF shows that there is no multicolinearity - model 2 is ok.


#Next, conduct a train-test split for both models, to show that model 2 is a better model to predict salary.
set.seed(2004)

# Model 1 train-test:
train <- sample.split(Y = lawsuit.dt$Sal94, SplitRatio = 0.7)
trainset <- subset(lawsuit.dt, train == T)
testset <- subset(lawsuit.dt, train == F)

m1_train <- lm(Sal94 ~ . -Sal95, data = trainset)
summary(m1_train)
residuals(m1_train)

#Finding RMSE for Model 1
RMSE_m1_train <- sqrt(mean(residuals(m1_train)^2))

#Apply model from trainset to predict on testset
predict.m1.test <- predict(m1_train, newdata = testset)
testset.error <- testset$Sal94 - predict.m1.test
RMSE_m1_test <- sqrt(mean(testset.error^2))

# Model 2 train-test:  
m2_train <- lm(Sal94 ~ Dept + Cert + Clin + Exper + Rank, data = trainset)
summary(m2_train)
residuals(m2_train)

#Finding RMSE for Model 2
RMSE_m2_train <- sqrt(mean(residuals(m2_train)^2))

#Apply model from trainset to predict on testset
predict.m2.test <- predict(m2_train, newdata = testset)
testset.error <- testset$Sal94 - predict.m2.test
RMSE_m2_test <- sqrt(mean(testset.error^2))


# Comparing Percentage change in RMSE for both Models.
(RMSE_m1_test - RMSE_m1_train)/RMSE_m1_train

(RMSE_m2_test - RMSE_m2_train)/RMSE_m2_train
# From this, we can infer that m2 produces a more accurate prediction. Therefore, we can say that m2, using variable EXCLUDING gender, gives accurate predictions of Sal94. Gender is not Significant.


# Next, we decided to test for correlation between Gender and Rank, since it was claimed that females were less likely to get promotions at the college than males.
# However, we are unable to run linear regression models for 2 catergorical variables. As such, we decided to make use of the Chisquare test on Rank and Gender to acquire the chisquared value 
chisq.test(lawsuit.dt$Rank, lawsuit.dt$Gender)

#Next, using the crammersV function from the 'lsr' package, it allows to obtain a value that describes correlation between 2 categorical variables via its ChiSquared value. 
library(lsr)

#CramersV requires the data to be in a table, thus we created a table containing the 261 values with only Rank and Gender.
cbind(lawsuit.dt$Rank, lawsuit.dt$Gender)
cramersV(cbind(lawsuit.dt$Rank, lawsuit.dt$Gender))


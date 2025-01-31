library(mgcv)
library(devtools)
detach("package:FBMS", unload = TRUE)
install_github("ylvast/GMJMCMC@FBMSY", force = TRUE)
library(FBMS)
library(mlbench)
library(dplyr)
#set.seed(2024)
train <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Housing/train.csv")
test <- read.csv("/Users/ylvasofietollefsen/Documents/Uio/Master/Experiments/Housing/test.csv")
dim(train)
transforms = c("troot","sigmoid","exp_dbl","p3","p0")
probs <- gen.probs.gmjmcmc(transforms)
params <- gen.params.gmjmcmc(train)
probs$gen <- c(0.22,0.22,0.06,0.06,0.22,0.22)
#probs$gen <- c(0.4,0.4,0.1,0.1,0,0)
params$feat$pop.max <- 20
params$feat$D <- 10
params$feat$L <- 5
params$feat$check.col <- F
model <- fbms(formula = MEDV ~ ., data = train, transforms = transforms, method = "gmjmcmc", P = 200, family="gaussian", params=params)
summary(model)
preds <- predict(model,test[-14])
cor(preds$aggr$mean,test$MEDV)^2
mae <- mean(abs(preds$aggr$mean-test$MEDV))
mae
# RMSE
rmse <- sqrt(mean((preds$aggr$mean-test$MEDV)^2))
rmse

lin <- lm(formula = MEDV~.,data = train)

predslin <- predict(lin,test[,-14])



cor(predslin,test$MEDV)^2



model <- gam(MEDV ~ s(CRIM),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(ZN),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(INDUS),
             data = df)
# Plot the smooth terms
plot(model)

#model <- gam(MEDV ~ s(CHAS),data = df)
# Plot the smooth terms
#plot(model)

model <- gam(MEDV ~ s(NOX),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(RM),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(AGE),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(DIS),
             data = df)
# Plot the smooth terms
plot(model)

#model <- gam(MEDV ~ s(RAD),data = df)
# Plot the smooth terms
#plot(model)

model <- gam(MEDV ~ s(TAX),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(PTRATIO),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(B),
             data = df)
# Plot the smooth terms
plot(model)

model <- gam(MEDV ~ s(LSTAT),
             data = df)
# Plot the smooth terms
plot(model)


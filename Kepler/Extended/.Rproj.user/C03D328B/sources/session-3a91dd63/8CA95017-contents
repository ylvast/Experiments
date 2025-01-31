library(pmlbr)
library(FBMS)
set.seed(2024)
data <- read.table("https://raw.githubusercontent.com/rupakc/UCI-Data-Analysis/refs/heads/master/Boston%20Housing%20Dataset/Boston%20Housing/housing.data")
names(data) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.75,0.25))
train <- data[sample,]
test <- data[!sample,]
train
dim(train)
params <- gen.params.gmjmcmc(train)
summary(data)
plot(data$CRIM)
plot(data$ZN)
plot(data$INDUS)
plot(data$CHAS)
plot(data$NOX)
plot(data$RM)
plot(data$AGE)
plot(data$DIS)
plot(data$TAX)
plot(data$PTRATIO)
plot(data$B)
plot(data$LSTAT)
plot(data$MEDV)
plot(data$RAD)
dim(data)

#here one can turn off correlation checks

params$feat$pop.max = 20

params$feat$check.col <- F

res <- fbms(formula = MEDV~.,data = train,method = "gmjmcmc",family = "gaussian",transforms = c("troot","sigmoid","exp_dbl","sin_deg"),P = 25,params = params)
res$best.margs
sum <- summary(res)
sum$marg.probs

preds <- predict(res,test[,-14])

cor(preds$aggr$mean,test$MEDV)^2

lin <- lm(formula = MEDV~.,data = train)

predslin <- predict(lin,test[,-14])

cor(predslin,test$MEDV)^2

library(randomForest)
library(MASS)
data(Boston)

set.seed(10)

train = sample(1:nrow(Boston), nrow(Boston)/2)

bag.boston = randomForest(medv ~ ., data = Boston, subset = train, mtry = 3,
                          importance = TRUE)
bag.boston


yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]

plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)  


bag.boston = randomForest(medv ~ ., data = Boston, subset = train,
                          mtry = 3, ntree = 25)
bag.boston

yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag-boston.test)^2)  


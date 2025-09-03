set.seed(1)
n = 100
X = rnorm(n)
Epsilon = rnorm(n)

β0 = 1; β1 = 2; β2 = 3; β3 = 4


Y <- β0 + β1*X + β2*X^2 + β3*X^3 + Epsilon
Y

library(leaps)
data = data.frame(Y = Y,X1 = X,X2 = X^2,X3 = X^3,X4 = X^4,X5 = X^5,X6 = X^6,X7 = X^7,X8 = X^8,X9 = X^9,X10 = X^10)
best_subset = regsubsets(Y ~ ., data = data, nvmax = 10)
summary.reg = summary(best_subset)

summary.reg$cp       
summary.reg$bic      
summary.reg$adjr2

par(mfrow = c(1,3)) 

plot(summary.reg$cp, xlab = "Number of Variables", ylab = "Cp", type = "b")
plot(summary.reg$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
plot(summary.reg$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", type = "b")

coef(best_subset, 3)
coef(best_subset, 4)

forward_model = regsubsets(Y ~ ., data = data, nvmax = 10, method = "forward")
summary.forward = summary(forward_model)

par(mfrow = c(1, 3))
plot(summary.forward$cp, xlab = "Number of Variables", ylab = "forward_Cp", type = "b")
plot(summary.forward$bic, xlab = "Number of Variables", ylab = "forward_BIC", type = "b")
plot(summary.forward$adjr2, xlab = "Number of Variables", ylab = "forward_Adjusted R2", type = "b")

backward_model = regsubsets(Y ~ ., data = data, nvmax = 10, method = "backward")
summary.backward = summary(backward_model)

par(mfrow = c(1, 3))
plot(summary.backward$cp, xlab = "Number of Variables", ylab = "backward_Cp", type = "b")
plot(summary.backward$bic, xlab = "Number of Variables", ylab = "backward_BIC", type = "b")
plot(summary.backward$adjr2, xlab = "Number of Variables", ylab = "backward_Adjusted R2", type = "b")

summary.forward$cp       
summary.backward$cp

coef(forward_model, 3)
coef(backward_model,4)




library(glmnet)

x.mat = model.matrix(Y ~ poly(X, 10, raw = TRUE))[,-1]  
y.vec = Y

set.seed(1)
cv.lasso = cv.glmnet(x.mat, y.vec, alpha = 1)  
plot(cv.lasso)

best.lambda = cv.lasso$lambda.min
best.lambda

lasso.fit = glmnet(x.mat, y.vec, alpha = 1, lambda = best.lambda)

coef(lasso.fit)

β7=8
Y_new = β0 + β7*X^7 + Epsilon
Y_new
best_subset_new = regsubsets(Y ~ ., data , nvmax = 10)
summary_new = summary(best_subset_new)

par(mfrow = c(1, 3))
plot(summary_new$cp, xlab = "Number of Variables", ylab = "Cp", type = "b")
plot(summary_new$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
plot(summary_new$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", type = "b")

coef(best_subset_new, 7)
coef(best_subset_new, which.min(summary_new$cp))
coef(best_subset_new, which.min(summary_new$adjr2))

library(glmnet)

x.mat.new = model.matrix(Y_new ~ poly(X, 10, raw = TRUE))[,-1]
y.vec.new = Y_new

set.seed(1)
cv.lasso.new = cv.glmnet(x.mat.new, y.vec.new, alpha = 1)
plot(cv.lasso.new)

best.lambda.new = cv.lasso.new$lambda.min
best.lambda.new

lasso.fit.new = glmnet(x.mat.new, y.vec.new, alpha = 1, lambda = best.lambda.new)
coef(lasso.fit.new)


library(ISLR)
library(MASS)
library(boot)

Carseats = read.csv("Lab2_Carseats.csv", header = T)

Carseats$ShelveLoc = as.factor(Carseats$ShelveLoc)

glm.fit = glm(Sales ~ CompPrice * ShelveLoc + Income, data = Carseats)

summary(glm.fit)

contrasts(Carseats$ShelveLoc)

# Perform 10-fold cross-validation
set.seed(123)  
cv.err = cv.glm(Carseats, glm.fit, K = 10)

cv.err$delta


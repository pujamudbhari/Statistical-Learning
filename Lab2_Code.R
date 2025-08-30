library(ISLR)
library(MASS)

Carseats = read.csv(file = "Lab2_Carseats.csv", header = T)

summary(Carseats)
names(Carseats)

Carseats$ShelveLoc = as.factor(Carseats$ShelveLoc)

lm.fit = lm(Sales ~ CompPrice * ShelveLoc + Income, data = Carseats)

summary(lm.fit)

contrasts(Carseats$ShelveLoc)

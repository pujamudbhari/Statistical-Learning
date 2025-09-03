Auto = read.csv("Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)
fix(Auto)
pairs(Auto[, sapply(Auto, is.numeric)])


cor(Auto[, sapply(Auto, is.numeric)])



lm.fit = lm(mpg ~ . - name, data = Auto)
summary(lm.fit)


plot(lm.fit)

model1 = lm(mpg ~ horsepower * weight + cylinders + displacement + acceleration + year + origin, data = Auto)
summary(model1)


model2 = lm(mpg ~ year * origin + cylinders + displacement + horsepower + weight + acceleration, data = Auto)
summary(model2)


model_all_interactions = lm(mpg ~ (.-name)^2, data = Auto)
summary(model_all_interactions)


model_transformed = lm(mpg ~ log(horsepower) + sqrt(displacement) + weight + I(weight^2) + acceleration + I(acceleration^2) + year + origin, data = Auto)
summary(model_transformed)

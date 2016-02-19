data(iris)

head(iris)
str(iris)

library(h2o)
h2o.init()

iris2 = iris[1:120,]
iris1 = iris[121:150,]
iris1 = as.h2o(iris1)
iris.hex <- as.h2o(iris2)
iris.hex[,5] = as.factor(iris.hex[,5])
iris.dl <- h2o.deeplearning(x = 1:4,y = 5 , training_frame = iris.hex)
# now make a prediction
predictions <- h2o.predict(iris.dl, newdata = iris1, type = "probs")

as.data.frame(predictions)
as.data.frame(iris1[,5])


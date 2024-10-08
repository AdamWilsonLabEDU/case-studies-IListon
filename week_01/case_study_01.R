#load data

data("iris")

#calculate mean of petal length

petal_length_mean <- mean(iris$Petal.Length)

#plot distribution of Petal.Length

hist(iris$Petal.Length)

#install and load ggplot2

library(ggplot2)

#plot histogram with ggplot2

ggplot(data=iris, aes(x=Petal.Length)) + geom_histogram()
ggplot(data=iris, aes(x=Petal.Length)) + geom_histogram(binwidth = 0.05)

#summary table of each column

summary(iris$Sepal.Length)
summary(iris$Sepal.Width)
summary(iris$Petal.Length)
summary(iris$Petal.Width)
summary(iris$Species)

#other visualizations

plot(iris$Species,iris$Sepal.Length, xlab = "Species", ylab = "Sepal.Length")


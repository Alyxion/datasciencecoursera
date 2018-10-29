rm(list=ls())

library(datasets)
data(iris)

virginica_only <- iris[iris$Species=="virginica",]
sepal_mean = mean(virginica_only$Sepal.Length)
sepal_mean
#  what R code returns a vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)

# MPG relation to cylinders, all of the following three are analog
# How can one calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)? Select all that apply.
tapply(mtcars$mpg, mtcars$cyl, mean)
with(mtcars, tapply(mpg, cyl, mean))
sapply(split(mtcars$mpg, mtcars$cyl), mean)

# Continuing with the 'mtcars' dataset from the previous Question, what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
print(hp_mean <- tapply(mtcars$hp, mtcars$cyl, mean))
print(hp_mean["8"]-hp_mean["4"])
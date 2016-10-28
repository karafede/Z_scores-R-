m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
mean_row <- apply(m, 1, mean) ##mean by row
mean_column <- apply(m, 2, mean) ##mean by column


l <- list(a = 1:10, b = 11:20)
lapply(l, mean)

l <- list(a = 1:10, b = 11:20) 
l.mean <- sapply(l, mean)
class(l.mean)


attach(iris)
IRIS <- iris
# mean petal length by species only for Petal.length
MEAN_by_sepcies <- tapply(iris$Petal.Length, Species, mean)
MEAN_by_sepcies <- as.data.frame(MEAN_by_sepcies)

# mean by column
by(iris[, 1:4], Species, colMeans)


library(plyr)
# mean petal length by species
ddply(iris,"Species",summarise, Petal.Length_mean = mean (Petal.Length))

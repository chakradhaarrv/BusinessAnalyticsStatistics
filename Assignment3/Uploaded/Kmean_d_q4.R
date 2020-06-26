library(cluster)

xD <- matrix(c(1, 4, 1, 3, 0, 4, 5, 1, 6, 2, 4, 0), nrow = 6, ncol = 2, byrow = TRUE)

Kmeans <- kmeans(xD, 2, iter.max = 100, nstart = 25)

clusplot(xD, Kmeans$cluster)

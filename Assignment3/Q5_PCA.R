set.seed(100)

x<- matrix(nrow=20, ncol=50)
colnames(x) <- c(
  paste("col", 1:50, sep=""))
for (i in 1:20) {
  col.values <- rnorm(n = 50, mean = 0, sd = 1)
  x[i,] <- c(col.values)
}


x1<- matrix(nrow=20, ncol=50)
colnames(x1) <- c(
  paste("col", 1:50, sep=""))
for (i in 1:20) {
  col.values <- rnorm(n = 50, mean = 5, sd = 1)
  x1[i,] <- c(col.values)
}

x2<- matrix(nrow=20, ncol=50)
colnames(x2) <- c(
  paste("col", 1:50, sep=""))
for (i in 1:20) {
  col.values <- rnorm(n = 50, mean = 10, sd = 1)
  x2[i,] <- c(col.values)
}

x <- rbind(x, x1, x2)


#################

pca <- princomp(x, scale=TRUE) 

summary(pca)

str(pca)

library(reshape2)
library(ggplot2)

plot(pca$scores, pch =8 )

               
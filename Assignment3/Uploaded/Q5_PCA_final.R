set.seed(100)
y <- rep(c(1,2,3),20 )
# Matrix w 60 rows 50 columns.
x <- matrix(rnorm(60*50), ncol=50)
# Adding a mean shift of .575 units
x[y==2,]= x[y==2,] - .575
x[y==3,]= x[y==3,] + .575
dimnames(x) <- list(rownames(x, do.NULL = FALSE, prefix = "row"),
                    colnames(x, do.NULL = FALSE, prefix = "col"))

pca =prcomp(x, scale =FALSE)
#Plot the first two principal axes
plot(pca$x[,1:2], col=y, pch =19, xlab ="PC1", ylab="PC2")


km3 =kmeans(x,3, nstart =20)
table(km3$cluster, y, dnn=c("Cluster","Label"))

km2 =kmeans(x,2, nstart =25)
table(km2$cluster, y, dnn=c("Cluster","Label"))

plot(x[,1:2], col=y, pch=19)
points(x[km2$cluster==1,1:2], pch=5, cex = 1.5)
legend("topleft", c(paste("Cluster",unique(km2$cluster))), pch=c(5,27), cex=.7)

km4 =kmeans(x,4, nstart =25)
table(km4$cluster, y, dnn=c("Cluster","Label"))

km3pc =kmeans(pca$x[,1:2],3, nstart =25)
table(km3pc$cluster, y, dnn=c("Cluster","Label"))

x.scale <- scale(x, center = FALSE, scale = TRUE)
kmscale =kmeans(x.scale, 3, nstart =25)
table(kmscale$cluster, y, dnn=c("Cluster","Class"))

check.col <- cbind(apply(x.scale,2,sd), apply(x,2,sd), apply(x.scale,2,mean), apply(x,2,mean))
colnames(check.col) <- c("Scaled Col Sd","Original Col Sd","Scaled Col Mean","Original Col Mean")
boxplot(check.col, cex.axis=0.7)

check.row <- cbind(apply(x.scale,1,mean), apply(x,1,mean))
colnames(check.row) <- c("Scaled Row Mean","Original Row Mean")
par(mfrow=c(1,3))
boxplot(check.row[y==1,], ylim=c(-1,1), main="Class 1")
boxplot(check.row[y==2,], ylim=c(-1,1), main="Class 2")
boxplot(check.row[y==3,], ylim=c(-1,1), main="Class 3")

par(mfrow=c(1,1))
plot(x[,1:2], col =y, pch=19, xlim=c(-3,3), ylim=c(-3,3))
points(x.scale[,1:2], col =y, pch=1)
legend(-3,3, c("Original","Scaled"), pch=c(19,1), cex=.8)


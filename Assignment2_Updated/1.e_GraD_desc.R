library(Auto)

attach(Auto)

head(Auto)
summary(Auto)
dim(Auto)
Auto$origin
class(Auto$origin)
class(Auto$year)
Auto$year <- as.factor(Auto$year)
pairs(Auto)
aov1 <- aov(Auto$mpg~Auto$year)
avo2 <- aov(Auto$mpg~Auto$origin)
summary(aov1)
summary(avo2)
 reg<- 

#MBGD(Auto, alpha = 0.1, maxIter = 10, nBatch = 2, seed = NULL)
#nrow(Auto)


# Split the data
training_index<-sample(nrow(Auto),0.70*nrow(Auto),replace=FALSE)

# Create the training dataset
trainingDS<-Auto[training_index,]

# Create the test dataset
testDS<-Auto[-training_index,]


x0 <- rep(1, nrow(trainingDS)) # column of 1's
x1 <- trainingDS[, c("cylinders", "displacement", "horsepower", "weight", "acceleration","year","origin")]

#create the x- matrix of explanatory variables
x<-as.matrix(x1)
dim(x)

class(x)
# create the y-matrix of dependent variables

y <- as.matrix(trainingDS$mpg)
dim(y)
class(y)

#Gradient Descent algorithm

gradientDesc <- function(x, y, learn_rate, conv_threshold, max_iter) {
  n <- nrow(x) 
  m <- runif(ncol(x), 0, 1) # m is a vector of dimension ncol(x), 1
  y_pred <- x %*% m # since x already contains a constant, no need to add another one
  
  MSE <- log (sum((y - y_pred) ^ 2)) / n
  
  converged = F
  iterations = 0
  
  while(converged == F) {
    #print("while")
    m <- m - learn_rate * ( 1/n * t(x) %*% (y_pred - y))
    #print(" Coef")
    y_pred <- x %*% m
    MSE_new <- log(sum((y - y_pred) ^ 2)) / n
   # print("diff")
    #print(MSE-MSE_new)
   # print(abs(MSE - MSE_new),conv_threshold)
    
    if( abs(MSE - MSE_new) <= conv_threshold) {
      converged = T
      #print("converged")
    }
    iterations = iterations + 1
    MSE <- MSE_new
    #print("MSE")
    print(MSE_new)
    if(iterations >= max_iter) break
  }
  return(list(converged = converged, 
              num_iterations = iterations, 
              MSE = MSE_new, 
              coefs = m) )
}
  

#dim(m)
#t(x)%*% as.matrix(x)
#class(t(x))

ols <- solve(t(x)%*%x)%*%t(x)%*%y 

output <- gradientDesc(x,y, 0.01, 0.01, 20000)

output$coefs

data.frame(ols, out$coefs)

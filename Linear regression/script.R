# Set working directory 
setwd("D:/dp2/Dropbox/Blog")

#Read data set
data <- read.csv("data.csv")

#Plot
plot(data,xlab="Population of City in 10,000s'",ylab="Profit in $10,000s'")

#Dependent varible
y <- data$profit

#Independent variable
x <- data$population

#Add ones to x 
x <- cbind(1,x)
# initalize theta vector
theta<- c(0,0)
# Number of the observations
m <- nrow(x)
#Calculate cost
cost <- sum(((x%*%theta)- y)^2)/(2*m)

# Set learning parameter
alpha <- 0.001
#Number of iterations
iterations <- 1500
# updating thetas using gradient update
for(i in 1:iterations)
{
  theta[1] <- theta[1] - alpha * (1/m) * sum(((x%*%theta)- y))
  theta[2] <- theta[2] - alpha * (1/m) * sum(((x%*%theta)- y)*x[,2])
}


#Predict for areas of the 35,000 and 70,000 people
predict1 <- c(1,3.5) %*% theta
predict2 <- c(1,7) %*% theta
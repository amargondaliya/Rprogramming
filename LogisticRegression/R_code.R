#Load data
data <- read.csv("data.csv")

#Create plot
plot(data$score.1,data$score.2,col=as.factor(data$label),xlab="Score-1",ylab="Score-2")

#Predictor variables
X <- as.matrix(data[,c(1,2)])

#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(data$label)


#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

#Cost Function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}

#Intial theta
initial_theta <- rep(0,ncol(X))

#Gradient
gradient <- function(theta)
{
  m <- nrow(X)
  grad <- rep(0,length(theta))
  g <- sigmoid(X%*%theta)
  e <- g-Y
  for(i in 1:length(grad))
  {
    grad[i] = sum(e*X[,i])/m
  }
  return(grad)
}

#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)

#gradient at inital theta
gradient(initial_theta)


# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
prob <- sigmoid(t(c(1,45,85))%*%theta)

# Predict function
predict <- function(X,theta)
{
  p <- rep(0,nrow(X))
  p <- sigmoid(X%*%theta)
  return(p)
}

# Preidcted value
predicted <- round(predict(X,theta))

# Calculate training accuracy
original <- as.vector(Y)
conf_matrix <- ftable(original,predicted) #Confusion matrix
accuracy <- sum(diag(conf_matrix))*100/nrow(X)


glm(data$label~.,data=data,family=binomial())

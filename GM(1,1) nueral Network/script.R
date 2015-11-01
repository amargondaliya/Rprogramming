
#Origanal data set of the power demand
x <- c(47919102,53812862,59174751,65227727,69251809,74344947,
       80977405,92084684,98561004,105368193,111139816,118299046,
       128129801)

#AGO information
x_ago <- rep(0,length(x))

for(i in 1:length(x))
{
  sum=0
  for(j in 1:i)
  {
    sum = sum + x[j] 
  }
  x_ago[i] <- sum
}

#a,b estimation
#matrix
B <- matrix(rep(1,(length(x)-1)*2),length(x)-1,2)

for(i in 1:nrow(B))
{
  val <- -(0.5*x_ago[i+1] + (1-0.5)*x_ago[i])
  B[i,1]<- val
}

x_n <- x[2:length(x)]

#Multiplication
mult <- t(B)%*%B

#inverse calculation
inv <- matrix(1,2,2)
inv[1,1] <- mult[2,2]
inv[1,2] <- -mult[1,2]
inv[2,1] <- -mult[2,1]
inv[2,2] <- mult[1,1]
inv <- inv/det(mult)

#a,b matrix generation and a,b value
a_b <- (inv%*%t(B))%*%x_n
a <- a_b[1,1]
b <- a_b[2,1]

#Generating fitted sequence
x_fitted <- rep(0,length(x))

x_fitted[1] <- x[1]

for(i in 2:length(x))
{
  
  val <- (x[1]-(b/a))*(1-exp(a))*exp(-a*(i-1))
  x_fitted[i] <- val
}

#Fitted values
x_fitted

#Error percentage
error_percentage <- (x_fitted-x)*100/x_fitted

# residual of the orginal and fitted sequence
residual <- x-x_fitted

#absolute value of the residual
residual_abs <- abs(residual[2:length(residual)])

#ago information 
residual_ago <- rep(0,length(residual_abs))

for(i in 1:length(residual_abs))
{
  sum=0
  for(j in 1:i)
  {
    sum = sum + residual_abs[j] 
  }
  residual_ago[i] <- sum
}

#Matrix
B <- matrix(rep(1,(length(residual_abs)-1)*2),length(residual_abs)-1,2)

for(i in 1:nrow(B))
{
  val <- -(0.5*residual_ago[i+1] + (1-0.5)*residual_ago[i])
  B[i,1]<- val
}

residual_n <- residual_abs[2:length(residual_abs)]

#Matrix multiplication
mult <- t(B)%*%B

#Inverse of the matrix
inv <- matrix(1,2,2)
inv[1,1] <- mult[2,2]
inv[1,2] <- -mult[1,2]
inv[2,1] <- -mult[2,1]
inv[2,2] <- mult[1,1]
inv <- inv/det(mult)

#a,b estimation 
a_b <- (inv%*%t(B))%*%residual_n

a <- a_b[1,1]
b <- a_b[2,1]

# Residual fitted suqence
residual_fitted <- rep(0,length(residual_abs))

residual_fitted[1] <- residual_abs[1]

for(i in 2:length(residual_abs))
{
  
  val <- (residual_abs[1]-(b/a))*(1-exp(a))*exp(-a*(i-1))
  residual_fitted[i] <- val
}

#fitted values of the residual
residual_fitted




#Nueral network implementation ############

#residual sign creation

resid_sign <- rep(0,length(x))

for(i in 1:length(resid_sign))
{
  if(residual[i]>=0){
    resid_sign[i]=1
  }
    
}


# Creating training data set for training nueral network
resid_sign[1:(length(resid_sign)-2)]
data <- data.frame(x1=resid_sign[1:(length(resid_sign)-2)],x2=resid_sign[2:(length(resid_sign)-1)],y=resid_sign[3:length(resid_sign)])



#Function for nural networks##############################################
costFunction <- function(nn_params)
{
  input_layer_size <- 2
  hidden_layer_size <- 1
  num_labels <- 1
  
  Theta1 <- matrix(nn_params[1:((input_layer_size+1)*hidden_layer_size)],hidden_layer_size,input_layer_size+1)
  Theta2 <- matrix(nn_params[((input_layer_size+1)*hidden_layer_size)+1:length(nn_params)],num_labels,hidden_layer_size+1)
  
  
  # You need to return the following variables correctly 
  J = 0;
  Theta1_grad = matrix(rep(0,nrow(Theta1)*ncol(Theta1)),nrow(Theta1),ncol(Theta1))
  Theta2_grad = matrix(rep(0,nrow(Theta2)*ncol(Theta2)),nrow(Theta2),ncol(Theta2))
  
  m <- nrow(X)
  
  # add ones to feature vector
  X <- cbind(rep(1,nrow(X)),X)
  
  a2 <- sigmoid(Theta1%*%t(X))
  
  a2 <- cbind(rep(1,nrow(X)),t(a2))
  
  h_theta <- sigmoid(Theta2%*%t(a2))
  
  
  
  J <- sum((-y)*log(h_theta) - (1-y)*log(1-h_theta))/m
  
  return(J)
}

grad_params <- function(nn_params)
{
  input_layer_size <- 2
  hidden_layer_size <- 1
  num_labels <- 1
  Theta1 <- matrix(nn_params[1:((input_layer_size+1)*hidden_layer_size)],hidden_layer_size,input_layer_size+1)
  Theta2 <- matrix(nn_params[((input_layer_size+1)*hidden_layer_size)+1:length(nn_params)],num_labels,hidden_layer_size+1)
  
  
  # You need to return the following variables correctly 
  
  Theta1_grad = matrix(rep(0,nrow(Theta1)*ncol(Theta1)),nrow(Theta1),ncol(Theta1))
  Theta2_grad = matrix(rep(0,nrow(Theta2)*ncol(Theta2)),nrow(Theta2),ncol(Theta2))
  
  m <- nrow(X)
  X <- cbind(rep(1,nrow(X)),X)
  for(i in 1:m)
  {
    
    a1 <- X[i,]
    z2 <- Theta1 %*% a1
    
    a2 <- sigmoid(z2)
    a2 <-rbind(1,a2)
    
    z3 <- Theta2 %*% a2
    a3 <- sigmoid(z3)
    
    delta_3 <- a3 - y[i]
    delta_2 <- (t(Theta2)%*%delta_3)*sigmoidGradient(rbind(1,z2))
    
    delta_2 <- delta_2[2:nrow(delta_2)]
    
    Theta1_grad <- Theta1_grad + delta_2 %*% t(a1)
    Theta2_grad <- Theta2_grad + delta_3 %*% t(a2)
  }
  
  Theta1_grad = Theta1_grad/m
  Theta2_grad = Theta2_grad/m
  
  #Unrolling gradient
  grad <- c(as.vector(Theta1_grad),as.vector(Theta2_grad))
  return(grad)
}

sigmoid <- function(z)
{
  g <- 1 / (1 + exp(-z))
  return(g)
}


sigmoidGradient <- function(z)
{
  g <- sigmoid(z)*(1-sigmoid(z))
  return(g)
}

randInitializeWeights <- function(L_in,L_out)
{
  epsilon_init=0.12
  w <- matrix(runif((L_in+1)*L_out),L_out,L_in+1)*2*epsilon_init - epsilon_init
  return(w)
}

Predict <- function(Theta1,Theta2,X)
{
  m <- nrow(X)
  h1 <- sigmoid(cbind(rep(1,m),X)%*%t(Theta1))
  h2 <- sigmoid(cbind(rep(1,m),h1)%*%t(Theta2))
  p <- round(h2)
  return(p)
}
#End of the functions#############################################


intial_theta1 <- randInitializeWeights(2,1)
inital_theta2 <- randInitializeWeights(1,1)
initial_nn_params <- c(as.vector(intial_theta1),as.vector(inital_theta2))

#Intiallize X and y
X <- as.matrix(data[,c(1,2)])
y <- data$y

#Inital cost at randomly initalized theta
j <- costFunction(initial_nn_params) 
grad <- grad_params(initial_nn_params)

#Using the optimization of function
res <-optim(par=initial_nn_params,fn=costFunction,gr=grad_params,control = list(maxit = 400))

#Set value of the parameter in the nn_parameter
nn_params <- res$par

#Cost after new parameters
j <- costFunction(nn_params)

#Unrolling the parameters
Theta1 <- matrix(nn_params[1:((input_layer_size+1)*hidden_layer_size)],hidden_layer_size,input_layer_size+1)
Theta2 <- matrix(nn_params[((input_layer_size+1)*hidden_layer_size)+1:length(nn_params)],num_labels,hidden_layer_size+1)

#Prediction for the new dataset
p <- round(Predict(Theta1,Theta2,X))


# use these initial arbitrary values 
N <- dim(dat)[1]  # number of data points
alpha <- c(0.2,0.3,0.5)  # arbitrary starting mixing parameters
mu <- matrix(  # arbitrary means
  c(5,8,
    7,8,
    9,8),
  nrow = 3, byrow=TRUE
)
sig1 <- matrix(c(1,0,0,1), nrow=2)  # three arbitrary covariance matrices
sig2 <- matrix(c(1,0,0,1), nrow=2)
sig3 <- matrix(c(1,0,0,1), nrow=2)

## write your code here
converged <- FALSE
assign_old <- rep(0,N)
while(!converged){
  likelihood_a <- dmvnorm(dat,mean=mu[1,],sig1)
  likelihood_b <- dmvnorm(dat,mean=mu[2,],sig2)
  likelihood_c <- dmvnorm(dat,mean=mu[3,],sig3)
  marginal <- likelihood_a*alpha[1]+likelihood_b*alpha[2]+likelihood_c*alpha[3]
  w <- cbind(likelihood_a*alpha[1]/marginal,likelihood_b*alpha[2]/marginal,likelihood_c*alpha[3]/marginal)
  assign_new <- apply(w,1,which.max)
  
  N_k <- colSums(w)
  alpha_new <- N_k/N
  mu_k_new <- rbind(t(w[,1])%*%dat/N_k[1],t(w[,2])%*%dat/N_k[2],t(w[,3])%*%dat/N_k[3])
  
  sum_var1 <- 0
  for(i in 1:N){
    sum_var1<-sum_var1+(dat[i,]-mu_k_new[1,])%*%t(dat[i,]-mu_k_new[1,])*w[i,1]
  }
  sigma1_new <- sum_var1/N_k[1]
  
  sum_var2 <- 0
  for(i in 1:N){
    sum_var2<-sum_var2+(dat[i,]-mu_k_new[2,])%*%t(dat[i,]-mu_k_new[2,])*w[i,2]
  }
  sigma2_new <- sum_var2/N_k[2]
  
  sum_var3 <- 0
  for(i in 1:N){
    sum_var3<-sum_var3+(dat[i,]-mu_k_new[3,])%*%t(dat[i,]-mu_k_new[3,])*w[i,3]
  }
  sigma3_new <- sum_var3/N_k[3]
  
  
  if(all(assign_old==assign_new)){
    converged <- TRUE
  }else{
    mu <- mu_k_new
    sig1 <- sigma1_new
    sig2 <- sigma2_new
    sig3 <- sigma3_new
    alpha <- alpha_new
    assign_old <- assign_new
  }
}

N_k
mu
sig1
sig2
sig3
km <- kmeans(dat,3)
print(km)
plot(dat, col = col[em_true_groups], main = "data with true group assignments")
em_assignments <- apply(w,1,which.max)
plot(dat, col = col[em_assignments], main = "data with EM algorithm assignments")
plot(dat, col = col[km$cluster], main = "data with Kmeans algorithm assignments")

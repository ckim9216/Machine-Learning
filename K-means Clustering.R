K-means Clustering
K-means clustering is a clustering method. The algorithm can be described as follows:
Read section 6.2 in the text and https://en.wikipedia.org/wiki/K-means_clustering#Standard_algorithm
Determine how many (k) clusters you will search for.
Randomly assign points in your data to each of the clusters.
Once all values have been assigned to a cluster, calculate the means or the centroid of the values in each cluster.
Reassign values to clusters by associating values in the data set to the nearest (Euclidean distance) centroid.
Repeat steps 2 and 3 until convergence. Convergence occurs when no values are reassigned to a new cluster.

```{r}
# Don't change this code. It will be used to generate the data.
set.seed(2019)
library(mvtnorm)

cv <- matrix(c(1,0,0,1), ncol=2)
j <- rmvnorm(100, mean = c(3,3), sigma = cv)
k <- rmvnorm(100, mean = c(5,8), sigma = cv)
l <- rmvnorm(100, mean = c(8,3), sigma = cv)
dat <- rbind(j,k,l)
true_groups <- as.factor(c(rep("j",100),rep("k",100),rep("l",100) ))
plot(dat, col=true_groups, asp = 1)
```
Task 1
Write code to perform k-means clustering on the values in the matrix dat.
The true group labels are provided in the vector true_groups. 
Of course, you can’t use that until the very end where you will perform some verification.
Requirements:
So everyone will get consistent results, I have performed the initial assignment of points to clusters.
With each iteration, plot the data, colored by their current groupings, and the updated means.
Convergence is reached when group assignments no longer change. Your k-means clustering algorithm should reach convergence fairly quickly.
Print out a ‘confusion’ matrix showing how well the k-means clustering algorithm grouped the data vs the ‘true labels.’


```{r}
set.seed(2019)
assignments <- factor(sample(c(1,2,3), 300, replace = TRUE)) # initial groupings that you will need to update
plot(dat, col = assignments, asp = 1)  # initial plot
```

```{r}
library(dplyr)
dat <- as.data.frame(dat)
dat$assignments <- assignments
distances <- function(point, means){
  # write code here
  # return(c(A = dist_to_centroid_A, B = dist_to_centroid_B, C = dist_to_centroid_C))
  n <- nrow(point)
  dist_to_centroid_A <- rep(NA,n)
  dist_to_centroid_B <- rep(NA,n)
  dist_to_centroid_C <- rep(NA,n)
  for( i in 1:n){
    dist_to_centroid_A[i] <- sqrt((point[i,1] - means[1,2])^2 + (point[i,2] - means[1,3])^2)
    dist_to_centroid_B[i] <- sqrt((point[i,1] - means[2,2])^2 + (point[i,2] - means[2,3])^2)
    dist_to_centroid_C[i] <- sqrt((point[i,1] - means[3,2])^2 + (point[i,2] - means[3,3])^2)
  }
  return(cbind(dist_to_centroid_A,dist_to_centroid_B,dist_to_centroid_C))
}

converged = FALSE
while(!converged){
  # write code here...
  means <- dat %>% group_by(assignments) %>% summarise(x1=mean(V1),x2=mean(V2))
  dist <- distances(dat,means)
  assignments_new <- apply(dist,1,which.min)
  if(all(assignments_new == dat$assignments)){
    converged = TRUE
  }else{
    dat$assignments <- assignments_new
    plot(dat[,1:2], col = assignments_new, asp = 1)
  }
}
```

Part 2: Kernelized K-means Clustering
```{r}
X <- matrix(c(
  0.2,  0.2,
  0.2, -0.2,
  -0.2,  0.2,
  -0.2, -0.2,
  2,  2, 
  2, -2,
  -2, -2,
  -2,  2),
  byrow = TRUE,
  ncol = 2)
plot(X, asp = 1)
```

```{r}
phi <- function(x){
  c(x[1]^2, x[2]^2,sqrt(2)*x[1]*x[2]) # change this
}

transformed <- t(apply(X, 1, FUN = phi))
transformed
```

Task 2a:

```{r}
assignments <- c(2,rep(1,7))
df <- data.frame(transformed, assignments)
means <- df %>% group_by(assignments) %>% summarise(x1=mean(X1),x2=mean(X2),x3=mean(X3))
means <- as.matrix(means[,-1])
means

```

Task 2b:
```{r}
dis_1 <- rep(NA,8)
dis_2 <- rep(NA,8)
for(i in 1:8){
  dis_1[i] <- (df$X1[i]-means[1,1])^2 + (df$X2[i]-means[1,2])^2 + (df$X3[i]-means[1,3])^2
  dis_2[i] <- (df$X1[i]-means[2,1])^2 + (df$X2[i]-means[2,2])^2 + (df$X3[i]-means[2,3])^2
} 
dist <- cbind(dis_1,dis_2)
dist



```

Task 2c:
  
  
```{r}
assignments_new <- apply(dist,1,which.min)
z_a <- as.integer(assignments_new==1)
z_a

z_b <- as.integer(assignments_new==2)
z_b

```

Task 3a:
  
```{r}
N <- nrow(X)
Ke <- matrix(NA,ncol = N, nrow = N)
for (m in 1:N){
  for (r in 1:N){
    Ke[m,r] <- (t(X[m,])%*%X[r,])^2
  }
}
Ke

```

Task 3c:
  
```{r}
z_a <- as.integer(assignments==1)
z_b <- as.integer(assignments==2)
double_sum_a <- 0
for(m in 1:N){
  for(r in 1:N){
    double_sum_a <- double_sum_a + z_a[m]*z_a[r]*Ke[m,r]
  }
}
dis_a <- rep(NA,N)
N_a <- sum(z_a)
for(n in 1:N){
  dis_a[n] <- Ke[n,n] - 2/N_a * sum(z_a*Ke[n,]) + 1/N_a^2 * double_sum_a
}

dis_b <- rep(NA,N)
N_b <- sum(z_b)
double_sum_b <- sum( (z_b %*% t(z_b)) * Ke )
for(n in 1:N){
  dis_b[n] <- Ke[n,n] - 2/N_b * sum(z_b*Ke[n,]) + 1/N_b^2 * double_sum_b
}

dist_K <- cbind(dis_a,dis_b)
dist_K


```


Task 3d:
```{r}
assignments_new <- apply(dist_K,1,which.min)
z_a <- as.integer(assignments_new==1)
z_a

z_b <- as.integer(assignments_new==2)
z_b


```
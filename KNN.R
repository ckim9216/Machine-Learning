K-nearest neighbors Classifier for the Iris data
Task: Write a classifier using the K-nearest neighbors algorithm for the iris data set.
First write a function that will calculate the euclidean distance from a vector A (in 4-dimensional space) to another vector B (also in 4-dimensional space).
Use that function to find the k nearest neighbors to then make a classification.
The function will accept four inputs: a row matrix for the x values of the test case, a matrix of x values for the training data, a vector of class labels for the training data, and the k parameter.
The function will return a single label.

```{r}
distance <- function(a, b){
  # Write your code here
  sqrt( (a[1]-b[1])^2 + (a[2]-b[2])^2 + (a[3]-b[3])^2 + (a[4]-b[4])^2)
}

iris_knn <- function(testx, trainx, trainy, k){
  # Write your code here
  
  
  distance <- rep(NA,nrow(trainx))
  for(i in 1:nrow(trainx)){
    distance[i]<- distance(testx,trainx[i,])
  }
  label<-order(distance)[1:k]
  ta <- trainy[label] %>% table()
  names(ta[which.max(ta)])
}
iris_knn(test_case_a, training_x, training_y, 5)
iris_knn(test_case_b, training_x, training_y, 5) # will incorrectly label as virginica with this training data
iris_knn(test_case_c, training_x, training_y, 5)
iris_knn(test_case_a, training_x2, training_y2, 5)
iris_knn(test_case_b, training_x2, training_y2, 5)
iris_knn(test_case_c, training_x2, training_y2, 5) # will incorrectly label as versicolor with this training data
```

KNN with R
Again, if you plan on using KNN in real-life, use a function from a package.
I’ve included some code for using the knn() function that is part of the class package. 
No need to modify anything. The results prediced by knn() should match the results from the function you wrote,
including the misclassification of some of the test cases based on the training data.

```{r}
library(class)
knn(train = training_x, cl = training_y, test = test_case_a, k = 5)
knn(train = training_x, cl = training_y, test = test_case_b, k = 5) # will incorrectly label as virginica with this training data
knn(train = training_x, cl = training_y, test = test_case_c, k = 5)
knn(train = training_x2, cl = training_y2, test = test_case_a, k = 5)
knn(train = training_x2, cl = training_y2, test = test_case_b, k = 5)
knn(train = training_x2, cl = training_y2, test = test_case_c, k = 5) # will incorrectly label as versicolor with this training data
```

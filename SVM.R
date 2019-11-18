SVM
Manual implementation of SVM is a bit of a pain (quadratic programming is hard), 
and I will not include it in the hw. FYI, I refer them to the code 
from book’s companion github repository: 
https://github.com/sdrogers/fcmlcode/blob/master/R/chapter5/svmhard.R 
and this post on stackexchange: 
https://stats.stackexchange.com/questions/179900/optimizing-a-support-vector-machine-with-quadratic-programming
Instead, I will use an example of a mixture model that can be separated via SVM.
The mixture model comes from the excellent (but advanced) textbook, 
The Elements of Statistical Learning, which is made to be freely available by the authors at: 
https://web.stanford.edu/~hastie/ElemStatLearn/
  
```{r}
library(ElemStatLearn)
data(mixture.example)
df <- data.frame(mixture.example$x, y = as.factor(mixture.example$y)) # turn the data into a dataframe
plot(df$X1,df$X2, col = df$y, pch = 19) # create a plot of the mixture

```
We will use the svm() function available in package e1071.
Read the documentation on the function svm().
For the following models, we will use a radial-basis function, 
which is equivalent to using a Gaussian Kernel function. 
(The Gaussian Kernel function projects the 2-dimensional data into infinite dimensional space and 
takes the inner product of these infinite dimensional vectors. It doesn’t actually do this, 
but the resulting inner product can be found and used to draw a decision boundary.)
The svm function allows for multiple arguments, but we will focus on the effect of the arguments for gamma and cost.
I have created 9 classification models using SVM and different values of gamma and cost.
Pay attention to the values of gamma and cost. At the very end comment on the effect of each parameter on the resulting model.

```{r}
library(e1071)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 1, cost = 1)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 1, cost = 0.1)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 1, cost = 10)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 0.5, cost = 1)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 0.5, cost = 0.10)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 0.5, cost = 10)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 5, cost = 1)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 5, cost = 0.1)
model <- svm(y ~ . , data = df, scale = FALSE, kernel = "radial", gamma = 5, cost = 10)
```

Write about the effect of the cost paramter:
Cost parameter generally penalizes in C-classification. 
This modifies to what extent we are willing to allow points to sit within the margin band or on the wrong side of the decision boundary. 
When it is small, the decision boundary is smoother but the classification of trainging data is not accurate. 
When it is big, the decision boundary is wigglier, but the classification of training data is more accurate.

Write about the effect of the gamma parameter:
Gamma is the Gausian radial basis function-specific kernel parameter. 
It defines how far the influence of a single training point reaches. 
When gamma is small, the points influence a lot even far away from the decision boundary, 
thus the boundary is smoother. 
For a bigger gamma, only the influence of points near the decision boundary is large, 
so the boundary is less smooth and more complex.

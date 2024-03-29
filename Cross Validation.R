 
R version 3.6.1 (2019-07-05) -- "Action of the Toes"
Copyright (C) 2019 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.70 (7684) x86_64-apple-darwin15.6.0]

[Workspace restored from /Users/chan/.RData]
[History restored from /Users/chan/.Rapp.history]

> ## Part 3: Cross-Validation

We can use cross-validation to evaluate the predictive performance of several competing models.

I will have you manually implement leave-one-out cross-validation from scratch first, and then use the built-in function in R.

We will use the dataset `ironslag` from the package `DAAG` (a companion library for the textbook Data Analysis and Graphics in R).

The description of the data is as follows: The iron content of crushed blast-furnace slag can be determined by a chemical test at a laboratory or estimated by a cheaper, quicker magnetic test. These data were collected to investigate the extent to which the results of a chemical test of iron content can be predicted from a magnetic test of iron content, and the nature of the relationship between these quantities. [Hand, D.J., Daly, F., et al. (1993) __A Handbook of Small Data Sets__]
A

The `ironslag` data has 53 observations, each with two values - the measurement using the chemical test and the measurement from the magnetic test.

We can start by fitting a linear regression model $y_n = w_0 + w_1 x_n + \epsilon_n$. A quick look at the scatterplot seems to indicate that the data may not be linear.

```{r linear_model}
#install.packages("DAAG") # if necessary
library(DAAG)
x <- seq(10,40, .1) # a sequence used to plot lines

L1 <- lm(magnetic ~ chemical, data = ironslag)
plot(ironslag$chemical, ironslag$magnetic, main = "Linear fit", pch = 16)
yhat1 <- L1$coef[1] + L1$coef[2] * x
lines(x, yhat1, lwd = 2, col = "blue")
```

In addition to the linear model, fit the following models that predict the magnetic measurement (Y) from the chemical measurement (X).

Quadratic: $y_n = w_0 + w_1 x_n + w_2 x_n^2 + \epsilon_n$

Exponential: $\log(y_n) = w_0 + w_1 x_n + \epsilon_n$, equivalent to $y_n = \exp(w_0 + w_1 x_n + \epsilon_n)$

log-log: $\log(y_n) = w_0 + w_1 \log(x_n)  + \epsilon_n$

## Task 3A

```{r other_models}
# I've started each of these for you. 
# Your job is to create the plots with fitted lines.

L2 <- lm(magnetic ~ chemical + I(chemical^2), data = ironslag)
plot(ironslag$chemical, ironslag$magnetic, main = "linear fit for quadratic ", pch = 16)
yhat2 <- L2$coef[1] + L2$coef[2] * x + L2$coef[3]*x^2
lines(x, yhat2, lwd = 2, col = "blue")

L3 <- lm(log(magnetic) ~ chemical, data = ironslag)
plot(ironslag$chemical, ironslag$magnetic, main = "linear fit for exponential", pch = 16)
yhat3 <- L3$coef[1] + L3$coef[2] * x # log(y-hat)
yhat3 <- exp(yhat3) #exponentiate log(y-hat)
lines(x, yhat3, lwd = 2, col = "blue")
# when plotting the fitted line for this one, create estimates of log(y-hat) linearly
# then exponentiate log(y-hat)

L4 <- lm(log(magnetic) ~ log(chemical), data = ironslag)
plot(log(ironslag$chemical), log(ironslag$magnetic), main = "linear fit for log-log", pch = 16)
yhat4 <- L4$coef[1] + L4$coef[2] * log(x)  #log(y-hat)
lines(log(x), yhat4, lwd = 2, col = "blue")
# for this one, use plot(log(chemical), log(magnetic))
# the y-axis is now on the log-scale, so you can create and plot log(y-hat) directly
# just remember that you'll use log(x) rather than x directly
```


## Task 3B: Leave-one-out Cross validation

You will now code leave-one-out cross validation. In LOOCV, we remove one data point from our data set. We fit the model to the remaining 52 data points. With this model, we make a prediction for the left-out point. We then compare that prediction to the actual value to calculate the squared error. Once we find the squared error for all 53 points, we can take the mean to get a cross-validation error estimate of that model.

To test out our four models, we will build a loop that will remove one point of data at a time. Thus, we will make a `for(i in 1:53)` loop. For each iteration of the loop, we will fit the four models on the remaining 52 data points, and make a prediction for the remaining point.

```{r loocv}

# create vectors to store the validation errors for each model
# error_model1 <- rep(NA, 53)
# ...

error_model1 <- rep(NA, 53)
error_model2 <- rep(NA, 53)
error_model3 <- rep(NA, 53)
error_model4 <- rep(NA, 53)

for(i in 1:53){
  valid<- ironslag[i,]
  training<-ironslag[-i,]
  
  # write a line to select the ith line in the data
  # store this line as the 'validation' case
  # store the remaining as the 'training' data
  
  model1 <- lm(magnetic ~ chemical, data = training)
  fitted_value <- predict(model1, valid)
  error_model1[i] <- (valid[1,2] - fitted_value)^2 #valid[1,2] is y
   
  model2 <- lm(magnetic ~ chemical + I(chemical^2), data = training)
  fitted_value <- predict(model2, valid)
  error_model2[i] <- (valid[1,2] - fitted_value)^2

  model3 <- lm(log(magnetic) ~ chemical, data = training)
  fitted_value <- predict(model3,valid)
  error_model3[i] <- (valid[1,2] - exp(fitted_value))^2
  
  model4 <- lm(log(magnetic) ~ log(chemical), data = training)
  fitted_value <- predict(model4, valid)
  error_model4[i] <- (valid[1,2] - exp(fitted_value))^2
    
  # fit the four models and calculate the prediction error for each one
  # hint: it will be in the form
  # model1 <- lm(magnetic ~ chemical, data = training)
  # fitted_value <- predict(model1, test_case)
  # error_model1[i] <- (validation_actual_value - fitted_value)^2
  # ...
  # model2 <- 
  # ...
  # ...
  # for models where you are predicting log(magnetic), you'll want to 
  # exponentiate the fitted value before you compare it to the validation case 
  # error[i] <- (validation_actual_value - exp(fitted_value))^2
  

}


# once all of the errors have been calculated, find the mean squared error
# ...
# mean(error_model1)

mean(error_model1) #cv score
mean(error_model2)
mean(error_model3)
mean(error_model4)

```

Compare the sizes of the cross validation error to help you decide which model does the best job of predicting the test cases.


## Task 3C: Cross-validation with R

Now that you have written your cross-validation script from scratch, we can use the built-in functions in R. Library(boot) has the function `cv.glm()` which can be used to estimate cross-validation error.

To make use of `cv.glm()` on the linear models, we must first use `glm()` to fit a generalized linear model to our data. If you do not change the attribute "family" in the function `glm()`, it will fit a linear model

```{r cvwithR, error = TRUE}
library(boot)
gL1 <- glm(magnetic ~ chemical, data = ironslag) # equivalent to lm(magnetic ~ chemical)

# find the LOOCV CV values for all of the models
# for the models with log(magnetic), use the argument cost to specify your own 
# cost function: cost = function(y, yhat) (exp(y) - exp(yhat))^2

cv1 <- cv.glm(ironslag,gL1)$delta[1]
cv1

gL2 <- glm(magnetic ~ chemical + I(chemical^2), data = ironslag)
cv2 <- cv.glm(ironslag,gL2)$delta[1]
cv2

gL3 <- glm(log(magnetic) ~ chemical, data = ironslag)
cost <- function(y, yhat) (exp(y) - exp(yhat))^2
cv3 <- cv.glm(ironslag, gL3, cost = cost)$delta[1]
cv3

gL4 <- glm(log(magnetic) ~ log(chemical), data = ironslag)
cv4 <- cv.glm(ironslag, gL4, cost = cost)$delta[1]
cv4

```

Your LOOCV estimates from `cv.glm()` should match your estimates when you coded your algorithm from scratch.


Based on your Cross Validation scores, which model seems to be the best?
- Quadratic model seems to be the best since it has the lowest cross validation score.

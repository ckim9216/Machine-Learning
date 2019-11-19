This is STATS101C coding part which should done around 30 minutes.

wineA <- read.csv('wineA.csv')
library(glmnet)

# Ridge
# make sure there's no NA
# model.matrix produce a matrix 19 predictors, also transform qualitative to dummy
set.seed(1118)
fullsampleA.nona <-na.omit(wineA)
x=model.matrix(total.sulfur.dioxide~.,data=fullsampleA.nona)
y=fullsampleA.nona$total.sulfur.dioxide
dim(fullsampleA.nona)
x.train <- x[1:4000,]
y.train <- y[1:4000]
x.test <- x[-(1:4000),]
y.test <- y[-(1:4000)]

# grid is for labmda 0.01 to 10,000,000,000
i=seq(10,-2,length=100)
lambda.v=10^i
#fit a ridge
ridge.mod=glmnet(x.train,y.train,alpha=0,lambda=lambda.v)
names(ridge.mod)
coeffs = coef(ridge.mod)
dim(coeffs)
# get the coeffs for the model that corresponds to each value of lambda
ridge.mod$lambda[50] # coeffs with lambda=11498
coeffs[,50] # look up coeffs values
sqrt(sum(coeffs[-1,50]^2)) # l2norm(length), measures size
ridge.mod$lambda[60] # lambda=705
coeffs[,60] # look up coeffs values
sqrt(sum(coeffs[-1,60]^2)) # l2norm(length), measures size
# When lambda is big, the value of coeffs gets smaller (small in size)

# use CV that testing MSE smallest
cv.output=cv.glmnet(x,y,alpha=0)
plot(cv.output)
# minimum is somewhere between log(lambda) 0 to 7
names(cv.output)
bestlamb.cv=cv.output$lambda.min
bestlamb.cv # that resulsts in the smallest cv error

bestmod <- glmnet(x.train,y.train,alpha=0,lambda=bestlamb.cv)
coef(bestmod)
pred <- predict(bestmod, newx=x.test)
mean((pred-y.test)^≥≥≥≥≥÷2)



# Lasso
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# fit lasso
bestmod <- glmnet(x.train,y.train,alpha=1,lambda=bestlam)
coef(bestmod)
pred <- predict(bestmod, newx=x.test)
mean((pred-y.test)^2)

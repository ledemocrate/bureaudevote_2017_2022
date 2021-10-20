seque <- seq(0,1,0.01)
seque_par_1 <- seque[seque <0.1]




seque <- seq(0,1,0.01)
seque_par_1 <- seque**(log(0.5)/log(0.1))
seque_par_2 <- seque[seque >0.5]**(log(0.5)/log(1-0.1))/2 + 0.5
seque_final <-c(seque_par_1,seque_par_2)
plot(seque_par_2)
plot(seque_par_1)
plot(seque_final)

plot(1/(1+5*exp(seque)))

0.7**1.943358
0.9**6.5
log(0.5)/log(0.7)
0.25/2

# function needed for visualization purposes
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x)))
}


x = 1:53
y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.1,0.18,0.18,0.18,0.33,0.33,0.33,0.33,0.41,
      0.41,0.41,0.41,0.41,0.41,0.5,0.5,0.5,0.5,0.68,0.58,0.58,0.68,0.83,0.83,0.83,
      0.74,0.74,0.74,0.83,0.83,0.9,0.9,0.9,1,1,1,1,1,1,1)

# fitting code
fitmodel <- nls(y~a/(1 + exp(-b * (x-c))), start=list(a=1,b=.5,c=25))

# visualization code
# get the coefficients using the coef function
params=c(1,1,0.1)

y2 <- sigmoid(params,seque)
plot(y2,type="l")

points(y)
dglogis(5, 5, 1, 2)
library("rmutil")
dglogis(5, 5, 1, 2)
install.packages("grofit")

seque <- seq(0,1,0.01)

plot(1/(1+exp(-30*(seque-0.7))))
sum(1/(1+exp(-0*(seque-0.5)))-seque)**2


curve(logistic(x,a=1.702),-3,3,ylab="Probability of x",
      main="Logistic transform of x",xlab="z score units") 
#logistic with a=1.702 is almost the same as pnorm 
x <- seq(0,1,0.01)
x <- x**(log(0.5)/log(0.1))
zscore(x)
test <- logistic(x,d=0, a=1,c=0, z=1)
curve(logistic(x,d=0, a=1,c=0, z=1))
curve(pnorm(x),add=TRUE,lty="dashed")  
curve(logistic(x),add=TRUE)
text(2,.8, expression(alpha ==1))
text(2,1.0,expression(alpha==1.7))
curve(logistic(x),-4,4,ylab="Probability of x",
      main = "Logistic transform of x in logit units",xlab="logits")
curve(logistic(x,d=-1),add=TRUE)
curve(logistic(x,d=1),add=TRUE)
curve(logistic(x,c=.2),add=TRUE,lty="dashed")
text(1.3,.5,"d=1")
text(.3,.5,"d=0")
text(-1.5,.5,"d=-1")
text(-3,.3,"c=.2")
#demo of graded response model
curve(logistic.grm(x,r=1),-4,4,ylim=c(0,1),main="Five level response scale",
      ylab="Probability of endorsement",xlab="Latent attribute on logit scale")
curve(logistic.grm(x,r=2),add=TRUE)
curve(logistic.grm(x,r=3),add=TRUE)
curve(logistic.grm(x,r=4),add=TRUE)
curve(logistic.grm(x,r=5),add=TRUE)

text(-2.,.5,1)
text(-1.,.4,2)
text(0,.4,3)
text(1.,.4,4)
text(2.,.4,5)
library("psych")



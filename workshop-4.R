# Recap Exercises
# 1) F-Test
# 2) Similar comparing average of two samples, unpaired t is mean and paramteric, MW U is mediam and non-paramentric
# 3) No non-parametric, uses ranks of vars and doesn't assume normality 
# 4) x Testing whether a population is likely to have a given proportion of successes, given a sample
# 5) x chi-square and fishers exact are to test for association between vars, f exact is for small samples
# 6) type I error is false reject of H0 (false positive), type II error is false reject of H1 (false negative)


x<- c(0, 1, 2, 3, 4, 5)
y<- c(0, 2, 4, 6, 8, 10)
plot(x,y)


# Exercise 1
# 1) 
x <- c(0, 1, 2, 3, 4, 5)
y <- c(0, -2, -4, -6, -8, -10)
plot(x,y)
# y = -2x
# 2)
x <- c(-5, -4, -3, -2, -1,0, 1, 2, 3, 4, 5)
y <- c (-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10)        
plot(x,y)
# y = 2x

# Exercise 2
A <- c(1,2,3,4)
B <- c(2,4,6,8)
D <- c(-2,-4,-6,-8)
cor(A,B)
cor(A, D)

# Exercise 3
# 1)
x <- c(0, 1, 2, 3, 4, 5) 
y <- c(0, -2, -4, -6, -8, -10)
cor.test(x,y)
# since p<0.05 we can reject correlation = 0
# 2)
x <- c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5) 
y <- c(-10, -7, -6, -3, -2, 0, 1, 4, 6, 8, 10)
cor.test(x,y)                                                    
# since p<0.05 we can reject correlation = 0

# Exercise 4
data <- read.csv("CHD.csv")
str(data)
# 1)
qqnorm(data$sbp)
qqline(data$sbp)
qqnorm(data$age)
qqline(data$age)
cor.test(data$sbp, data$age)
?cor.test
# since p<0.05 we can reject correlation is zero
# weak positive correlation
# 2)
data <- read.csv("A and E Data.csv")
str(data)
cor.test(data$Arrival.Hour, data$Mins.on.AE, method = 'spearman')
# use spearman's because variables are not normal 
qqnorm(data$Arrival.Hour)
qqline(data$Arrival.Hour)
qqnorm(data$Mins.on.AE)
qqline(data$Mins.on.AE)

# Exercise 5
# 1)
data <- read.csv("CHD.csv")
lm(data$obesity ~ data$sbp)
plot(data$sbp, data$obesity)
abline(lm(data$obesity ~ data$sbp), col = 2)
plot(lm(data$obesity ~ data$sbp))       
summary(lm(data$obesity ~ data$sbp))
# assumptions met
# can reject parameters are zero
# can reject mean is a better model
# 2)
lm(data$alcohol~data$obesity)
plot(data$alcohol, data$obesity)
?abline
abline(lm(data$alcohol~data$obesity), col=2)
plot(lm(data$alcohol~data$obesity))
summary(lm(data$alcohol~data$obesity))
# assumptions not met
# can't reject parameters shouldn't be zero
# can't reject mean is a better model
# 3)
plot(data$obesity, data$sbp)
model <- lm(data$sbp ~ data$obesity)
abline(model$coefficients)
plot(model) # assumptions met
summary(model) # model only explains 5% variance in dependent
# 4)
plot(data$sbp, data$age)
model <- lm(data$age ~ data$sbp)
abline(model$coefficients, col=2)
plot(model) # assumptions met enough
summary(model)

# Exercise 6
data <- read.csv("CHD.csv")
# 1)
model <- lm(data$sbp ~ data$obesity + data$tobacco +data$age)
summary(model)
plot(model) # residuals and fitted not related
# obesity and age may predict sbp better than mean
# 2)
model <- lm(data$sbp ~ data$famhist)
summary(model)
plot(model)
# here binary variable, it means on average people with famhist have higher sbp
# and on average it is 3.56 times higher


# Exercise 7
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
head(group)
group
weight
tree = data.frame(weight, group)
tree
summary(lm(weight ~ group, data=tree))
# on average trees that are treated are lighter by a factor of 0.37
# but this wasn't significant and is consistent with chance
# can't say treatment systematically affects weight
# this is mathematically equivalent to doing a t-test!
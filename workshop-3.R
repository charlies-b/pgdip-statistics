# recap 
x <- c(100,3,1,5,7,4,5)
mean(x)

## Exercise 2
# (a)
x <- rnorm(1000, 3, 4)
plot(density(x))
?qnorm
qnorm(0.75, 3, 4)

# (b)
y_1 <- c(5,4,7,3,4,5,6,3,7,7)
mean(y_1)/length(y_1)

# (c)
set.seed(1)
pois.5 <- rpois(5,10)
lambda.5 <- mean(pois.5)
lambda.5

pois.100 <- rpois(100,10)
mean(pois.100)

## confidence intervals
set.seed(1037) # sets the random number generator so you can recreate the same random sample
lambda.est<-0
for(i in 1:500){
  pois.sim<-rpois(i,10)
  lambda.est[i]<-mean(pois.sim)
}
plot(lambda.est,xlab="Sample size", ylab=expression(paste("Estimate of ",lambda)), type="l")
lines(c(0,500),c(10,10),col=2)

## hypothesis testing
# p < 0.05 so reject null

# Exercise 3
data <- read.csv('gardens.csv')
head(data)

var.test(data$gardenB, data$gardenC, alternative = "greater")
var.test(data$gardenB, data$gardenC, alternative = "two.sided")

# Exercise 4
head(data)
data
var(data$gardenB)
var(data$gardenC)
"less"

## hypothesis test for a single mean
# p-value is greater than 0.05
# t value lies outside critical region ?

## Exercise 5

# (a)
type_C <- c(70,77,80,65,71)
mean_C <- mean(type_C)
mean_C

sd_C <- sd(type_C)
n_C <- length(type_C)
se_C <- sd_C/sqrt(n_C)

mu_C <- 68
t.stat <- (mean_C - mu_C)/se_C
t.stat

df_C <- n_C-1
df_C
?pt

2*pt(t.stat, df_C)
t.test(type_C, mu= 68)
# no evidence

# (b)
type_D <- c(60,65,65,70,100)
# null: mu = 50
t.test(type_D, mu=50, alternative = "greater" )
# evidence to reject 

# (c)
# null: mu = 90
t.test(type_D, , mu=90, alternative = "less")
# evidence to reject

## hypothesis testing for comparing means of two samples

# Exercise 6 
#(a) 
type_C <- c(70, 75,80,75,76,71)
type_D <- c(70,77,69,68,71)
# null: variances equal
var.test(type_C, type_D)
# no evidence to reject

# null muC = muD
t.test(type_C, type_D)
# p>0.05 no evidence to reject

#(b)
type_D <- c(60, 77, 61, 68, 100)
?t.test
# null: muC = muD
t.test(type_C, type_D, var.equal = FALSE)
# p >0.05 no evidence to reject

# (c)
CHD <- read.csv("CHD.csv")
head(CHD)

present <- CHD$obesity[CHD$chd == 1]
absent <- CHD$obesity[CHD$chd == 0]
t.test(present, absent, "greater", var.equal = T)
# p<0.05 bmi higher in present group

# Exercise 7
# a) 
choc_A <- c(1,4,3,5,2,1)
choc_B <- c(5,4,8,9,10,3)
wilcox.test(choc_A, choc_B)
# p<0.05 medians different

# b)
male <- c(10,15,20,40,2)
female <- c(5,10,20,25,20)
wilcox.test(male, female, alternative="greater")
# p>0.05 no evidence to reject

# c)
living <- c(100,50,70,40)
kitchen <- c(30,50,65,125)
wilcox.test(living,kitchen)
# p>0.05 no evidence to reject

# Exercise 8
# a)
group1 <- c(70,75,80,75,76,71)
group2 <- c(70,77,69,68,71,71)
t.test(group1,group2, paired =T)
# p>0.05 no evidence to reject (means equal)

# b)
group2 <- c(70,77,69,68,100,100)
t.test(group1,group2, paired =T, "less", exact = F)
# p>0.05 no evidence to reject (means equal)

# Exercise 9
# a)
group1 <- c(70,75,80,75,76,71)
group2 <- c(70,77,69,68,71,71)
wilcox.test(group1,group2, paired =T, exact=F)
# p >0.05 no evidence to reject (medians equal)

# b)

group2 <- c(70,77,69,68,100,100)
wilcox.test(group1,group2, paired =T, exact=F, alternative = "less")
# p >0.05 no evidence to reject (medians equal)

# Exercise 10
# a) 
prop.test(35, 95, p=1/3, correct = T)
# p >0.05 no evidence to reject

# b) 
prop.test(45, 110, p=0.5)
# p> 0.05 no evidence to reject

# c)
prop.test(50, 300, p=0.2, correct = T)
# p> 0.05 no evidence to reject

# Exercise 11
# a)
prop.test(x = c(35,40), n=c(95,60), correct = T)
# p<0.05 evidence to reject

# b)
prop.test(x=c(45, 5), n=c(110, 80), alternative = c("greater"))
# p<0.05 evidence to reject

# c)
prop.test(x=c(50,100), n=c(300, 150), alternative = c("less"))
# p<0.05 evidence to reject

#Exercise 12
#BSc x MSc 
# a) mathematics, statistics, computing, biology
count <- matrix(c(35,25,98,90 ), nrow = 2)
count
chisq.test(count, correct = F)
# p>0.05 no evidence to reject (independence)

# b)
count <- matrix(c(25,5, 59,6), nrow =2)
count
chisq.test(count, correct =F)
# p>0.05 no evidence to reject (independence)

#c) 
count <- matrix(c(20,10, 50, 40), nrow=2)
count
chisq.test(count, correct = F)


# Exercise 13
# a) 
count <- matrix(c(5,8,3,2), ncol = 2)
count
fisher.test(count)
# p>0.05 no evidence to reject independence

# b) 
count <- matrix(c(2,1,3,2), ncol = 2)
count
fisher.test(count)
# p>0.05 no evidence to reject independence

# c)
count <- matrix(c(2,10,5,4), ncol = 2)
fisher.test(count)
# p>0.05 no evidence to reject independence

# Exercise 14
# a) 0.1, .9
# b) 0.05
# c) 0.95
# d)  0.8
# e) 0.21
# f) 0.98

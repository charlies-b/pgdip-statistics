CHD <- read.csv("CHD.csv")

str(CHD)

## Recap Exercises
mean(CHD$obesity)
var(CHD$sbp)
# is.numeric()
summary(CHD) # box plot whiskers

quantile(CHD$sbp, 0.025)

# Exercise 5
quantile(CHD$sbp, c(0.05, 0.95))

AandE <- read.csv("A and E Data.csv")

barplot(table(AandE[,12]), main = "Triage", xlab="triage code")

hist(AandE[,7], main='histo mins on A&E', xlab = 'mins')
hist(AandE[,7], main='histo mins on A&E', xlab = 'mins', breaks=100)

boxplot(AandE[,7], main="box mins on A&E")

group1 <- CHD[CHD$chd==0,]
group2 <- CHD[CHD$chd==1,]
boxplot(group1$obesity, group2$obesity, names=c("CHD absent", "CHD present"), ylab="bmi", pch = 20)

## Exercise 6
# (a)
colnames(AandE)
boxplot(AandE[,19], main = "box arrival to seen", ylab="mins")
# -ve skew - mean and median won't coincide 

# (b)
hist(AandE[,19], main = "histo arrivale to seen")  
hist(AandE[,19], main = "histo arrivale to seen", breaks = 50)  
# skewed and not normal - maybe bimodal

# (c)
ftable(AandE[,15])
table(AandE$Arrival_Mode) # what's the difference
barplot(table(AandE[,15]), ylab="count", main= 'arrival mode')

# (d)
colnames(CHD)

head(CHD$famhist)
group1 <- CHD[CHD$famhist == "Present",] 
group2 <- CHD[CHD$famhist == "Absent",]
boxplot(group1$age, group2$age, names=c("present","absent"), main="box age vs. famhist")

plot(CHD$age, CHD$tobacco, xlab="age", ylab="tobacco", pch=20, type="p")

## Exercise 7
plot(CHD$sbp, CHD$obesity, xlab="sbp", ylab="obesity", main="sbp vs. obeisty", pch=20)


# probability distributions

# normal dist
sim.std.norm1 <- rnorm(10000, mean =5, sd=1)
sim.std.norm2 <- rnorm(10000, mean =5, sd=2)
plot(density(sim.std.norm1), col="Blue")
lines(density(sim.std.norm2) , col="red")
legend(2,0.4,c(expression(paste(sigma, " = 1")),expression(paste(sigma, " = 1")) ))

qqnorm(sim.std.norm1)
qqline(sim.std.norm1)
 
plot(density(CHD$sbp), main="dist of sbp")
qqnorm(CHD$sbp)
qqline(CHD$sbp)

# Exercise 8
colnames(CHD)

plot(density(CHD$ldl))
qqnorm(CHD$ldl)
qqline(CHD$ldl) # not normal

plot(density(CHD$adiposity))
qqnorm(CHD$adiposity)
qqline(CHD$adiposity) # not normal

plot(density(CHD$famhist)) # x not numeric

plot(density(CHD$typeA)) # x not numeric

plot(density(CHD$obesity))
qqnorm(CHD$obesity)
qqline(CHD$obesity) # not normal

plot(density(CHD$alcohol))
qqnorm(CHD$alcohol)
qqline(CHD$alcohol) # not normal

plot(density(CHD$age))
qqnorm(CHD$age)
qqline(CHD$age) # not normal

plot(density(CHD$chd)) # x discrete 


# binomial dist
par(mfrow=c(1,2)) #plot two plots side by side

x <- seq(from=0, to=12, by =1)
p1 <- dbinom(x, size=10, p=0.1)
plot(x, p1, col=3, type="o")
p2 <- dbinom(x, size=10, p=0.2)
plot(x, p2, col=5, type="o")
p3 <- dbinom(x, size=10, p=0.5)
lines(x, p3, col=7, type="o")
legend(10,  0.35, c(0.1, 0.2, 0.5), col=c(3,5,7), pch=21, lwd=1, bty= 'n')


# Exercise 9

# (a)
?which
CHD$days_smoking<-0
CHD$days_smoking[which(CHD$tobacco>0 & CHD$tobacco <=2.5)]<-1
CHD$days_smoking[which(CHD$tobacco>2.5 & CHD$tobacco <=5.0)]<-2
CHD$days_smoking[which(CHD$tobacco>5 & CHD$tobacco <=7.5)]<-3
CHD$days_smoking[which(CHD$tobacco>7.5 & CHD$tobacco <=10)]<-4
CHD$days_smoking[which(CHD$tobacco>10 & CHD$tobacco <=12.5)]<-5
CHD$days_smoking[which(CHD$tobacco>12.5 & CHD$tobacco <=15)]<-6
CHD$days_smoking[which(CHD$tobacco>15)]<-7

# (b)
table(CHD$days_smoking)
barplot(table(CHD$days_smoking), main='days smoking', xlab='days')

# (c)
p <- mean(CHD$days_smoking)/ 7
p

# (d)
N <- length(CHD$days_smoking)
plot(table(CHD$days_smoking)/N, xlim=c(0,7), ylab="probability")
dp <- dbinom(0:7, size=7, p)
lines(0:7, dp, type="o", col="red")

## Poisson 

x <- seq(from=0, to=15, by=1)
pois3 <- dpois(x, lambda=3)
lines(x, pois3, type="o", col=3)
pois5 <- dpois(x, lambda=5)
lines(x, pois5, type="o", col=5)
pois7 <- dpois(x, lambda=7)
lines(x, pois7, type="o", col=7)
pois9 <- dpois(x, lambda=9)
lines(x, pois9, type="o", col=9)
legend(10,0.2, c(expression(paste(lambda,"= 3")),expression(paste(lambda,"= 5")),expression(paste(lambda,"= 7")),expression(paste(lambda,"= 9"))), col=c(3,5,7,9),pch=21, lwd=1, bty='n')

## Exercise 10
N <- length(CHD$days_smoking)
plot(table(CHD$days_smoking)/N, xlim=c(0,7), ylab="probability")
dp <- dbinom(0:7, size=7, p)
lines(0:7, dp, type="o", col="red")
dp.pois <- dpois(0:7, mean(CHD$days_smoking))
lines(0:7, dp.pois, type="o", col="blue")
# binomial? - because poisson isn't limited and could predict greater than 7
# question was just asking what dist empirically fits data better

## Exercise 11
# (A) cdf
# (B) pdf

# quantiles from distributions

x <- seq(from=-3, to=3, by=0.01)
pd <- dnorm(x)
plot(x, pd, type="o", col=4, xlab="x", main="Standard normal distribution", ylab="Density")

hn <- seq(from=-3, to=3, by=0.01)
pd <- dnorm(hn)
plot(hn, pd, type="l", col=4, xlab="x", main="Standard normal distribution", ylab="Density")
yv <- pd[hn<=1]
xv <- hn[hn<=1]
xv <- c(xv[1], xv,1)
yv <- c(0, yv,0)
polygon(xv, yv, col="CadetBlue") # look up polygon function

pnorm(1) # P(X<=1)
qnorm(pnorm(1)) # value x such that P(X<=x) = p

## Exercise 12 
qnorm(0.995)
?qnorm
qnorm(0.995, mean=2, sd=9)

## Exercise 13
?hist
p <- hist(freq=FALSE,CHD$obesity, breaks = 30, xlab="bmi")

## Exercise 14
x <- rnorm(1000, mean=mean(CHD$obesity), sd=sd(CHD$obesity) )
lines(density(x))

## Exercise 15, 16
# 1:6
# discrete

## Exercise 17, 18
qnorm(0.975)

se = sd(CHD$obesity)/sqrt(length(CHD$obesity))
c(mean(CHD$obesity)-se*qnorm(0.975),mean(CHD$obesity)-se*qnorm(0.025) )
# based on the data we can be confident the mean lies in this range

# larger se => greater interval

## Exercise 19
# (a) 
se = 5/sqrt(100)

# (b)
c(1+se*qnorm(0.025), 1+se*qnorm(0.975))

# (c)
c(1+se*qnorm(0.05), 1+se*qnorm(0.95))
c(1-se*qnorm(0.985), 1+se*qnorm(0.985))
c(1-se*qnorm(0.995), 1+se*qnorm(0.995))

             
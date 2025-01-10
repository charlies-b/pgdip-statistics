# Reading Data

# getwd()
# read.csv()  # read.csv2() #read.table()
# read.xlsx()  # install.packages("xlsx"); library(xlsx) 

data_CHD <- read.csv('data/CHD.csv')

# Exploring Data

# dim()
# str()
# colnames()
# head()  # tail()
# class()

dim(data_CHD)
colnames(data_CHD)
str(data_CHD) # summary of data variables
class(data_CHD$famhist)

# Converting Variables

# is.numeric()  # factor, character
# as.numeric()  # as.numeric(as.character())

# Vectors / Matrices / Indexing

# seq()  # from, to, by
# c()  # index from 1
# matrix()  # [ROWS, COLUMNS]
# cbind()
# rbind()

v <-1:10  # integer vector
v <- seq(from = 1, to = 10, by = 2)  # interval vector
v <- c(2,3)  # vector
m <- matrix(c(1:6), nrow=2)  # vector as matrix

data_CHD[2:3,]  # rows 2 to 3
data_CHD[,2:3]  # col 2 to 3
data_CHD$sbp[1:10]
data_CHD[1, c(1,3,5)]
data_CHD[1, c('sbp', 'ldl', 'famhist')]
colnames(data_CHD)[5] <- "familyhistory"
colnames(data_CHD)

# R Environment

ls()
# rm()  # rm(list=ls())

# Summarising Data

# 4 principle methods: 
# - simple frequencies, 
# - measures of location (averages),
# - measures of spread 
# - graphical methods (box plots, histograms)

# IQR()
# quantile()  # 25th quantile is value that occurs 25% of the way along ordered values
# summary() # use median over mean for highly skewed data (mean is inflated by few large values)
# plot()  # ylim, ylab, xlab, pch
# abline()  # h, col: add straight line to plot
# lines()  # add line to plot between two (x,y)
# var()  # mean() sd()
# min()  # max() #sum() #range()

str(data_CHD)
var_age <- data_CHD$age
quantile(var_age)
quantile(var_age, 0.975) # 97.5% quantile
IQR(var_age)
summary(var_age)
summary(data_CHD[, c("age")])

# Functions & Loops

myFunc <- function(x,y){
  return(x+y)
}

for(i in 1:10){
  print(i)
}

i <- 0
while(i<10){
  print(i)
  i <- i + 1
} 

# Good Practice

#  1 comment whole line with one space and inline with two
#  2 use <- for assignment and not =
#  3 leave a space after each comma
#  4 leave a space between operators
#  5 avoid more than 5 lines of code at once

# R Help

help()
# print()

# Visualising Data

# barplot()  # categorical frequencies
# hist()  # numerical frequencies
# boxplot()  # spread & skew of data - median or mean?
# plot()  # type = 'p','l'.... (points, lines)

data_AandE <- read.csv('data/A and E Data.csv')
str(data_AandE)

barplot(table(data_AandE$Triage_Main_Code), main = 'Triage Code Frequencies', xlab = 'Triage Code')  # table counts frequencies
unique(data_AandE$Triage_Main_Code)  # values 2-5

hist(data_AandE$Mins.on.AE, breaks=100, main = 'Histogram of Mins on A&E', xlab = 'Mins on A&E')  # breaks - number of bins

str(data_CHD)
group1 <- data_CHD[data_CHD$chd == 0,]  # CHD absent
group2 <- data_CHD[data_CHD$chd == 1,]  # CHD present
boxplot(group1$tobacco, group2$tobacco, pch = 20, cex = 0.5, names = c('CHD absent', 'CHD present'), ylab = 'tobacco (g / week)')

plot(data_CHD$alcohol, data_CHD$obesity, type = 'p', pch = 20, cex = 0.5, xlab = 'alcohol (units / week)', ylab = 'obesity (BMI)')  # scatter

# Probability Distributions

#  Probability distribution of variable - list of probabilities associated with each of possible values
#  Used in confidence intervals and hypothesis tests that make assumptions about underlying distribution of X
#  pdf functions - dnorm, pnorm, qnorm, rnorm (density, cdf, quantile, random sample)
#  (binom, pois, geom, nbinom, unif, norm, gamma, chisq, beta, t)

sim.std.norm1 <- rnorm(10^6)  # 10^6 samples from N(0,1)
sim.std.norm2 <- rnorm(10^6, sd = 2)  # sd, mean 
sim.std.norm3 <- rnorm(10^6, sd = 3)  

plot(# plot normal density functions
  density(sim.std.norm1),   
  main = 'Normal Distribution',
  ylab = 'P(X=x)',
  xlab = 'x',  
  col = 'coral2',  # colors()
  lwd = 2
  ); 

lines(density(sim.std.norm2), col = 'aquamarine3', lwd = 2)  # add to plot
lines(density(sim.std.norm3), col = 'burlywood4', lwd = 2)

legend(
  3, 0.4,  # x y plot axes coords
  c(  # vector of expressions 
    expression(paste(sigma, ' = 1')),  # render math expression, see plotmath for R syntax
    expression(paste(sigma, ' = 2')),
    expression(paste(sigma, ' = 3'))
    ),
  col = c('red','aquamarine3','burlywood4'),
  lwd = 2,  # default 0! 
  bty = 'n'  # box y/n
  )

# qqnorm()  # normal qq plots theoretical quantiles against sample quantiles to check normality
# qqline()  # a normally distributed X lies on a line

plot(density(data_CHD$sbp), main = 'Distribution of sbp', xlab='x', ylab='P(Xsbp = x)');  # density just smooths the empirical mass function with some clever defaults
qqnorm(data_CHD$sbp)
qqline(data_CHD$sbp)  # not normal

sim.beta.pskew = rbeta(10^6, shape1 = 2, shape2 = 5)  # +ve skew data -  tail toward +ve  direction
sim.beta.nskew = rbeta(10^6, shape2 = 2, shape1 = 5)  # +ve skew data -  tail toward +ve  direction
plot(density(sim.beta.pskew), col =' blue', xlab = 'x', ylab = 'P(X=x)', main = 'Skew Distributions', lwd = 2)  # density can be > 1 
lines(density(sim.beta.nskew), col = 'cadetblue', lwd = 2)  
legend(0.8,2, c('+ve', '-ve'), col = c('blue', 'cadetblue'), lwd = 2)

#  Two main pdfs associated with discrete data: binomial and poisson

#  Binomial - pdf of n binary events with probability p occurring (e.g # heads in n coin tosses, p=0.5)
x <- seq(from = 0, to = 12, by = 1)
p1 <- dbinom(x, size= 10, p=0.1) # this is pdf - list of probabilities associated with possible X

# par(mfrow=c(1,1)) # par(mfrow=c(1,2)) #  plot two plots side by side
plot(x, p1, type = 'o', ylab = 'P(X=x)', col = 'seagreen', main = 'Binomial Distributions' )

pvals <- seq(from = 0.2, to = 0.9, by = 0.1)
cols <- c('seagreen')
cind <- sample(1:100, length(pvals), replace = FALSE)
labs <- c('p=0.1')
for(i in 1:length(pvals)){
  pd <- dbinom(x, size = 10, p = pvals[i])
  col <- colors()[cind[i]]
  cols <-  c(cols, col)
  lab <- paste('p=', pvals[i])
  labs <- c(labs, lab)
  lines(x, pd, type = 'o', col = col)
}
legend(10.25, 0.4, labs, col = cols, lwd = 2 )

#  Poisson - number of random events that occur in time or space, with a rate of occurrence of lambda
#  In poisson X is restricted to positive real integers, but is unbounded

x <- seq(from = 0, to = 15, by = 1)
lamvals <- seq(from = 1, to = 10, by = 1)
cind <- sample(1:100, length(lamvals), replace = FALSE)
pd <- dpois(x, lamvals[1])
col <- colors()[cind[1]]
lab <- as.expression(bquote(lambda == .(lamvals[1])))
plot(x, pd, type = 'o', col = col, main='Poisson Distributions ')
legend(10, 0.35, lab, col = col, lwd = 2 )

cols <- c(col)
labs <- c(lab)
for(i in 2:length(lamvals)){
  pd <- dpois(x, lamvals[i])
  col <- colors()[cind[i]]
  cols <-  c(cols, col)
  lab <- as.expression(bquote(lambda == .(lamvals[i])))
  labs <- c(labs, lab)
  lines(x, pd, type = 'o', col = col)
}
legend(10, 0.35, labs, col = cols, lwd = 2 )

#  pdf is referred to as a probability mass function for discrete var, probability density function for continuous

# pnorm()  # cdf; pnorm(1) = P(X≤1) for X ~ N(0,1)
# qnorm()  # inverse cdf; qnorm() e.g x st. P(X≤x)=0.95
# polygon()

x <- seq(from=-3, to= 3, by=0.1)
p <-  dnorm(x)
plot(x, p, type='l')
px <- x[x<1]
py <- p[x<1]
polygon(c(-3,px,1), c(0,py,0), col ='turquoise3') # grap
pnorm(1)  # area of turquoise region
qnorm(pnorm(1))  # inverse
#  Requirements for a pdf - defined for all real x, the cdf is non-decreasing with min 0 and max 1 (area under density curve sums to 1)

# Single Variable Analysis

#  Sample - one possible realisation of an underlying process (that generates the data) 
#  Standard Error - standard deviation of means we would see across different samples
#  Confidence Interval - interval estimate of a variable in which we expect the true value of a population parameter to lie at some alpha level
#  Central Limit Theorem -  regardless of underlying process, any sample statistic of X will be normally distributed if sample size is large enough (around 30)
sample_size <- length(data_CHD$obesity)
sample_mean <- mean(data_CHD$obesity)
sample_sd <-  sd(data_CHD$obesity)
standard_error <- sample_sd/sqrt(sample_size) 

# 95% confidence interval - 95% sure the true population value of statistic lies within interval
# The sample mean is an estimate of the true mean
# 95% confidence interval of true mean is: sample_mean ± 1.96*standard_error

# 1.96 is the 0.975-quantile of the standard normal - the value X at which P(x≤X) = 0.975 and symmetry means probability x lies between ± is  alpha %
# 1.96 is denoted Z_(1-0.025)
n <- sample_size
CI <- c(mean(data_CHD$obesity)-1.96*(sd(data_CHD$obesity)/sqrt(n)), mean(data_CHD$obesity)+1.96*(sd(data_CHD$obesity)/sqrt(n)))
CI




# Reading Data

# getwd()
# read.csv()  # read.csv2() #read.table()
# read.xlsx()  # install.packages("xlsx"); library(xlsx) 

df_CHD <- read.csv('data/CHD.csv')

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

# I Probability Distributions

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

#  Binomial distribution

#  There are two main pdfs associated with discrete data: binomial and poisson
#  pdf of n binary events with probability p occurring (e.g # heads in n coin tosses, p=0.5)
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

#  Poisson distribution

#  Number of random events that occur in time or space, with a rate of occurrence of lambda
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

# II Classical Statistical Tests

# Hypothesis tests

# A statistical hypothesis specifies if there is a difference in a specific quantity between two groups
# Null Hypothesis: No difference; Alternative Hypothesis otherwise
# The Hypothesis of a One-tailed tests specifies the direction of the inequality, otherwise it is Two-tailed
# Procedure for statistical test (1) specify a Null, collect data, perform test, interpret significance level
# Test produces p-value that is taken significant if < 0.05 - the chance of seeing what has occurred is less than 5% assuming the Null hypothesis
# We only ever reject the null hypothesis, if we cannot reject the null hypothesis there is not enough evidence to suggest otherwise

# Fisher F - Compare two variances

# Let type A and B be the counts of apples from two apple trees, does the variance of these counts differ?
type_A<- c(50, 75, 100, 35, 90, 20) 
type_B<- c( 70, 77, 80, 65, 71)
var.test(type_A, type_B)  # two sided Null: p-value<0.05, reject - the variance differs 
var.test(type_A, type_B, alternative = 'greater')  # one sided Null: p-value<0.05, reject - evidence supports var_A > var_B
var.test(type_A, type_B, alternative = 'less')  # one sided Null: p-value<0.05, cannot reject - evidence does not support var_B > var_A

# T Test - Test single mean

t.test(type_A, mu=70)  # could type A apples come from a population with mean 70 ?
# cannot reject that type_A apples do not come from a population with mean 70, there is no evidence to support otherwise

# Unpaired T - Compare two means

t.test(type_A, type_B)  # do type A and type B apples come from populations with differing means ?
# Not enough evidence to reject null that mean does not differ
# Assumptions: statistical independence (one group's values do not affect the other), observations are normally distributed and variances of observations are similar (not necessarily equal) 

# Wilcoxon Rank Sum -  Compare two medians (non-normally distributed)

wilcox.test(type_A, type_B)  
# No evidence to suggest medians are not equal (cannot reject null)
# Wilcoxon Rank Sum does not assume normality and is used as substitution T Test if normality is not satisfied 
# It is a parametric test - it uses observation ranks rather than values, if there are equal values the rank midpoint will be used and an exact p-value cannot be computed see exact=F

# Paired T - Compare means of two paired samples

# Suppose we have 5 patients and we take blood pressures before and after treatment
group1<- c(140, 150, 145, 150, 160) 
group2<- c(140, 145, 140, 150, 160)
# These samples are not independent: the values before affect the values after, the measurements are paired
# We want to test if the blood pressure has been reduced by treatment
t.test(group1, group2, paired = T, alternative = 'greater') # there is not enough evidence to suggest the treatment has been effective. 

# Wilcoxon Signed Rank - Compare medians of two paired samples (non-normal distribution)

group_1<- c(140, 150, 145, 151, 160) 
group_2<- c(139, 138, 141, 142, 159)
wilcox.test(group_1, group_2, paired=T, alternative='greater', exact=F)  # reject the null medians are equal evidence to suggest median_1 > median_2

# Proportion test - value of a proportion

# Suppose of 200 registered patients with respiratory issues, 90 are women - is there enough evidence to suggest the percentage of women on the register is different from 50%
# We can use an Exact Binomial Test or a Z-test
binom.test(90, 200, alternative ='two.sided', conf.level = 0.95)  # Exact binomial - No
prop.test(90, 200, p = 0.5)  # Z-test - No
# Z-test Assumptions: the sample is randomly selected, a normal approximation can be used
# A Poisson and Normal approximation can be used to approximate binomial (see poisson and binomial distribution plots earlier)
# A condition to discern if a normal approximation can be used - np and n(1-p) are both greater than 5
# There is a 'continuity correction' when approximating a binomial with a normal distribution

# Chi-squared - association of two variables in contingency table

# Suppose we have counts of people with brown and blue eyes, and dark and fair hair in the following table
col_1<- c(38, 14, 52)
col_2<- c(11, 51, 62)
col_3<- c(49, 65, 114)
Table_1<- cbind(col_1, col_2, col_3) 
row.names(Table_1) <- c('Fair_Hair', 'Dark_Hair', 'Column_Totals')
colnames(Table_1) <- c('Blue_Eyes', 'Brown_Eyes', 'Row_Totals')

# Is eye colour associated with hair colour in any way? 
# Null assumes these variables are independent - the probability of blue eyes, is not affected whether the hair is dark or fair for example
counts <- matrix(c(38, 14, 11, 51), nrow =2) 
chisq.test(counts)  # reject null -the probabilities of someone's eye colour is affected by whether they have fair or dark hair and visa versa
# Yates continuity corrections aims to correct error in the p-value introduced by assuming discrete probabilities in the frequency table can be approximated by a continuous distribution, as in chi-squared.

# Fisher Exact - contingency table association for small samples

# Used instead when at least one of the frequencies is < 5
counts <- matrix(c(6, 2, 4, 8), nrow =2) 
fisher.test(counts) # cannot reject there is no association

# Power, Type I, II errors and sample size

# type I error: reject the null when true - alpha is the probability of making a type I 
# type II error: fail to reject null when false - the probability of making a type II is 1 - the power of the test (beta)
# Usual values: alpha=0.05, power=0.8
# To decrease the probability of making a type II, we increase the test power 

# Power is affected by many factors

# alpha - tests with lower alpha have a lower power
# sample size - larger sample means more power
# standard deviation - more spread data means lower power
# effect size - larger true difference between groups, a test has more power

# Power analysis is used to choose sample size - advanced statistics (important part of practice)

# III Regression

# Correlation

# Uses Covariance of random variables (measure of joint variability - expectation of product of variable mean deviations over product of standard deviations)
Var_1<- c(1, 2, 3, 4) 
Var_2<- c(2, 4, 10, 15)
cor.test(Var_1,Var_2) # Pearson correlation coefficient - assumes normality of samples

A<- c(1, 2, 3, 4) 
B<- c(2, -5, 10, 15)
cor.test(A, B, method='spearman') ## Spearman's rank correlation - parametric version
cor.test(Var_1, Var_2,method='spearman') # 1 because rank - data ordered

# Simple Linear Model

# Correlation does not imply causality - may be a confounding variable (another causal variables they are both related to)
# Regression analysis can be used to try to establish if there is a causal effect between two variables
# Simple Linear Model - can a straight line be used to model a dependent value (predict form independent)
# Residuals are model errors from actual values
# Assumptions: Residuals look random (no pattern with fitted values and be normally distributed), observations are independent

df_tannin <-read.csv('Datasets from Crawley/tannin.csv')
str(data_tannin)
lm(data_tannin$growth ~ data_tannin$tannin) # dependent ~ independent
plot(data_tannin$tannin, data_tannin$growth)
abline(lm(data_tannin$growth ~ data_tannin$tannin), col='red')

model_predict <- predict(lm(data_tannin$growth ~ data_tannin$tannin)) # predicted values
for (i in 1:length(model)){
  lines(c(data_tannin$tannin[i], data_tannin$tannin[i]), 
        c(data_tannin$growth[i], model_predict[i]), col ='red') # ( (x1,x2), (y1, y2) )
}

plot(lm(data_tannin$growth ~ data_tannin$tannin)) # first two plots check assumptions

summary(lm(data_tannin$growth ~ data_tannin$tannin)) # coefficients (estimate and standard error), R value, Null F-statistic
# Coefficient standard errors can be used to calculate coefficient confidence intervals
# t-values and p-values for each coefficient test against null hypothesis coefficient is zero (alternatively non zero)
# R-squared (coefficient of determination; measure of how well the model fits the data) - the proportion of the variance (dependent variable) explained by model (predictable from the independent variable)
# The F-statistic p-value tests the hypothesis that the fit of the model is equal to an intercept only model (model with no predictors)

# Multivariate Linear Regression

df_ozone <- read.csv('Datasets from Crawley/ozone.data.csv')
str(df_ozone)
model <- lm(df_ozone$ozone ~ df_ozone$rad + df_ozone$temp + df_ozone$wind)
summary(model) # R-squared to assess the fit - coefficients hard to interpret fo multivariate

# Non-linear Relationships

plot(df_ozone$rad, df_ozone$ozone) # plot to visualise possible relationships
plot(df_ozone$temp, df_ozone$ozone)
plot(df_ozone$wind, df_ozone$ozone)
model <- lm(df_ozone$ozone ~
              df_ozone$rad + df_ozone$temp + df_ozone$wind +
              I(df_ozone$rad^2) + I(df_ozone$temp^2) + I(df_ozone$wind^2)
            )  # I - use operators arithmetically in formula (not as part of formula, the model is still linear - beta values linear)
summary(model) # improved R-squared

# Linear Model Categorical Data

pain <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5) 
drug <- as.factor(as.character(c(rep("A", 9), rep("B", 9), rep("C", 9))))
pain
drug  # is there a relationship between pain score and drug type?
summary(lm(pain ~ drug))  # coefficients relative to first factor level - drugA
# On average drug B and C have higher pain scores by ~2 pts  
# Coefficient p-values indicate the increased scores are significant and not consistent with random chance

# Control and treated tree weight data 
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69) 
group <- gl(2, 10, 20, labels = c("Ctl","Trt")) # generate factor levels
weight <- c(ctl, trt)
df_tree <- data.frame(weight, group)
str(df_tree)
summary(lm(weight ~ group), data=df_tree)
# On average treated trees are -0.37 units lighter, but this is not statistically significant and consistent with random chance
t.test(ctl,trt) # This is equivalent to doing a t-test

# IV ANOVA & GLM

# GLMs

# A GLM uses a link function and can be used when the residuals aren't normally distributed or the response variable isn't continuous (two assumptions of SLMs)

# Logistic Regression

# Used to model a binary outcome with a logit link function
head(df_CHD$chd)  # boolean 
head(df_CHD$tobacco)  # continuous
fit <- glm(chd ~ tobacco, family = binomial, data = df_CHD)  # chd ~ Bin(1, p) and logit(p) ~ obesity, is the model
summary(fit)  # tobacco has a significant association with chd
newdat <- data.frame(tobacco=seq(0, 30, len=100))
newdat$chd <- predict(fit, newdata=newdat, type="response")
plot(chd ~ tobacco, data=df_CHD, col="red4") 
lines(chd ~ tobacco, newdat, col="green4", lwd=2)

# Poisson Regression

# Used to model a response variable with a Poisson distribution (count) with a log link function
df_CHD$days_smoking <- 0  # count data
df_CHD$days_smoking[which(df_CHD$tobacco >= 0.1 & df_CHD$tobacco <= 2.5)] <- 1
df_CHD$days_smoking[which(df_CHD$tobacco >= 2.6 & df_CHD$tobacco <= 5)] <- 2
df_CHD$days_smoking[which(df_CHD$tobacco >= 5.1 & df_CHD$tobacco <= 7.5)] <- 3
df_CHD$days_smoking[which(df_CHD$tobacco >= 7.6 & df_CHD$tobacco <= 10)] <- 4
df_CHD$days_smoking[which(df_CHD$tobacco >= 10.1 & df_CHD$tobacco <= 12.5)] <- 5
df_CHD$days_smoking[which(df_CHD$tobacco >= 12.6 & df_CHD$tobacco <= 15)] <- 6
df_CHD$days_smoking[which(df_CHD$tobacco >= 15.1)] <- 7
head(df_CHD)

fit <- glm(days_smoking~age+chd+sbp, family = poisson, data=df_CHD)
summary(fit)  # sbp doesn't have significant association with days smoking
fit <- glm(days_smoking~age+chd, family = poisson, data=df_CHD)

newdat1 <- data.frame(age=seq(10,75, len=100), chd=rep(0, len=100))  # chd absent
newdat1$days <- predict(fit, newdata = newdat1, type="response")
newdat2 <- data.frame(age=seq(10,75, len=100), chd=rep(1, len=100))  # chd present
newdat2$days <- predict(fit, newdata = newdat2, type="response")

plot( days_smoking ~ age, data=df_CHD, col='red4')
lines (days ~ age, data=newdat1, col='green4')
lines (days ~ age, data=newdat2, col='blue4')
legend(50,7,c('chd absent','chd present'), c( 'green4', 'blue4' ))

# ANOVA

# Used when all explanatory variables are categorical
# For one factor with two levels, ANOVA is a t-test

# One-way ANOVA

# One explanatory factor with multiple levels (>2)
# We construct hypotheses so that the null is that means are equal between all level groups
pain = c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5) 
drug =as.factor(as.character(c(rep("A",9), rep("B",9), rep("C",9))))
pain
drug  # is there a difference between pain score for each treatment group ?
df_pain = data.frame(pain,drug)
summary(aov(pain ~ drug), data=df_pain)  # significant p-value - mean pain score is different between at least two treatment groups
# Assumption that each level group has more than 30 elements
# Kruskal-Wallis is a non-parametric equivalent to one-way ANOVA

# Box and whisker plots are used to interpret difference in level groups for one-way ANOVA
boxplot(pain ~ drug, data =df_pain)  # we can see drug group A is the difference

# Two-way ANOVA

# We have two explanatory factors each with a number of levels
# We test three null hypotheses (1) There is no difference in means for the first factor (2) There is no difference means for the second factor (3) There is no interaction between the two factors (they are not independent)

time<- c(rep('Morning',2),rep('Night',2),rep('Morning',2),rep('Night',2), rep('Morning',2),rep('Night',2),rep('Morning',2),rep('Night',2))
shoes<- c(rep('Others',4),rep('Favourite',4),rep('Others',4),rep('Favourite',4)) 
made<- c(26,26,27,27,32,22,30,34,35,34,33,30,33,37,36,38)
df_shoes <- data.frame(time,shoes,made )
str(df_shoes)

tapply(made, time, mean)  # more shoes made at night
tapply(made, shoes, mean)  # more Favorite shoes made
summary(aov(made ~ time * shoes))  # p-value for interaction is large, so can ignore
summary(aov(made~ time + shoes))  # p-values are large so no association and differences are consistent with random variation
# Null - there is no differences between factor groups on number of shoes made

bloodpressure <- c(158,163,173,178,168,188,183,198,178,193,186,191,196,181,176,185,190,195,200,180)
biofeedback <- factor(c(rep("present",10),rep("absent",10)))
drug1 <- factor(rep(c(rep("present",5),rep("absent",5)),2))
bpdata <- data.frame(bloodpressure, biofeedback, drug1)
print(bpdata)

summary(aov(bloodpressure ~ drug1, data = bpdata))  # there is a difference in mean blood pressure with drug1 treatment 
summary(aov(bloodpressure ~ biofeedback, data = bpdata))  # cannot reject null 
summary(aov(bloodpressure ~ drug1 * biofeedback, data = bpdata))  # there is difference between blood pressure and factor groups and factors are not independent

boxplot(bloodpressure ~ drug1+biofeedback, data=bpdata) # when both are present there is a difference in blood pressure

# Survival Analysis (NB)

# Other more sophisticated models exist, for example, survival analysis is time-to-event analysis e.g survival time

CHD <- read.csv2("CHD.csv", header=T, sep=",", stringsAsFactors = T)
CHD$obesity <- as.numeric(as.character(CHD$obesity))
CHD$tobacco <- as.numeric(as.character(CHD$tobacco))
CHD$ldl <- as.numeric(as.character(CHD$ldl))
CHD$adiposity <- as.numeric(as.character(CHD$adiposity))
CHD$alcohol <- as.numeric(as.character(CHD$alcohol))


# Exercise 1
model <- glm(chd ~ age, family=binomial(link="logit"), data=CHD)
model
summary(model)

# Exercise 2
model <-  glm(chd ~ age + obesity, family=binomial(link="logit"), data=CHD)
summary(model)
# age is significant, but obeisty is not

# Exercise 3
model <-glm(tobacco ~ age + sbp, family = poisson, data=CHD)
summary(model)
# age significant, but sbp not

# Exercise 4
data <- read.csv("oneway.csv")
str(data)
summary(aov(ozone~garden, data=data))   
# reject null that no sig diff between means

# Exercise 5
Shoes_ <- c(rep('Others',4),rep('Favourite',4),rep('Others',4),rep('Favourite',4))
Made_<- c(26,26,27,27,32,22,30,34,35,34,33,30,33,37,36,38)
summary(aov(Made_ ~ Shoes_))
# no sig diff between means 

# Exercise 6
bloodpressure <- c(158,163,173,178,168,188,183,198,178,193,186,191,196,181,176,185,190,195,200,180)
biofeedback <- factor(c(rep("present",10),rep("absent",10)))
drug1 <- factor(rep(c(rep("present",5),rep("absent",5)),2))
bpdata <- data.frame(bloodpressure, biofeedback, drug1)
bpdata
summary(aov(bloodpressure ~ biofeedback))
# cannot reject there is no difference between groups 
summary(aov(bloodpressure~drug1))
# can reject that theirs is no difference between groups
summary(aov(bloodpressure~biofeedback*drug1))
# all p-values are significant
# can reject no difference between outcome and biofeedback
# can reject no difference btween bloodpressure and drug1
# can reject that there is no interaction between biofeedback and drug1 on bloodpressure

### REVISION Qs ###

# Workshop 1
# 1) R is a programming language and environment for statistical computing
# 2) standard mathematical functions and constants are built in
# 3) 0
# 4) R is case sensitive
# 5) 5, R indexes from 1
# 6) You do need to install packages in R
# 7) 9
# 8) 3
# 9) 1,2,5
# 10) True
# 11) mean is average value, median is middle value, variance is average distance squared of each data point from the mean

# Workshop 2
# 1) mean(), var() # sample variance?
# 2) remove one d.o.f from total number of observations in denominator
# 3) sd is +sqrt of var
# 4) the standard error is the standard deviation in sample means we would expect to see in different realisations 
# 5) se = sd/sqrt(n)
# 6) 71492+_7.28 ( mean +_ qnorm(0.95)*se )
# 7) pmf is the probability of a outcome, cdf is the probability of an outcome less than or equal to a value
# 8) 
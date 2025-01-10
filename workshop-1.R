rm(list=ls())

###############################################################
## Introducing to R
## 05 October 2021
##
rm(list=ls()) # Clean our R environment
x <- c(5, 1, 7, 9, 3, 6, 10, 14, 12, 8) # insert the row of data manually
ls() # list all variables saved in our R environment
max(x)
min(x)
range(x)

## Loading data

getwd()

CHD <- read.csv2("CHD.csv", sep=",", stringsAsFactors = T)
AandE <- read.csv("A and E Data.csv", header=T, stringsAsFactors = T)

install.packages("xlsx")
library(xlsx)
CHD_xlsx <- read.xlsx("CHD.xlsx", sheetIndex = 1)

dim(CHD)
str(CHD)

head(CHD$obesity)
class(CHD$obesity)

## Converting a factor to numeric

CHD$obesity <- as.numeric(as.character(CHD$obesity))
CHD$tobacco <- as.numeric(as.character(CHD$tobacco))
CHD$ldl <- as.numeric(as.character(CHD$ldl))
CHD$adiposity <- as.numeric(as.character(CHD$adiposity))
CHD$alcohol <- as.numeric(as.character(CHD$alcohol))

class(CHD$tobacco)

## Exercise 1
# (a) cbind() - column bind, row bind
a <- c(1,2)
b <- c(1,2,3)
x <- cbind(a,b)
y <- rbind(a, b)
x
y

# (b) element-wise multiplication
x <- a*b
# when calculating derived values on a data frame rows simultaneously 

newValue = CHD$obesity - CHD$adiposity
data = cbind(CHD, newValue)
head(data)
data$newValue
head(CHD$obesity)
head(CHD$adiposity)
head(data$newValue)
# Yes, as expected?

# (b)
data <- (cbind(data, data$alcohol/7))
head(data)
colnames(data)[12] <- "daily intake"
colnames(data)

# (c)
View(AandE[22,])
colnames(AandE)
AandE$Departure_Date_Time
# no departure date

# (d)
AandE$newValue <- AandE$Arrival_To_Seen - AandE$Time_To_Initial_Asessment


## Exercise 3
# (a)
mean(AandE$Mins.on.AE)
median(AandE$Mins.on.AE)

# (b)
mean(AandE$Arrival_To_Seen)
median(AandE$Arrival_To_Seen)

# (c) 
colnames(CHD)[7] <- "BMI"
var(CHD$BMI)

#(d) becuase contiuous data

# Exercise 4
# (a)
help.search("deviation")
sd(CHD$sbp)

# (b)
sd(CHD$adiposity)
IQR(CHD$adiposity)

# Exercise 5 
difference <- function(x, y) {
  return(x-y)
}

difference(3,4)

# Exercise 6

my_var <- function(x){
  return( sum((x - mean(x))^2)/(length(x)-1) )
}

my_var(c(1,2,3))
x <- c(5, 1, 7, 9, 1, 6, 10, 40, 12, 2) 
my_var(x)

my_var_steps <- function(x){
  mu <- mean(x)
  summand <- (x-mean(x))^2
  var <- sum(summand)/(length(x) - 1)
  return(var)
}

my_var_steps(x)

# Exercise 7
check <- function(x){
  if(x<10){
    print("x less than 10")
  }else if(x == 10){
    print("x is 10")
  }else{
    print("x is greater than 10")
  }
}

check(1)
check(10)
check(11)

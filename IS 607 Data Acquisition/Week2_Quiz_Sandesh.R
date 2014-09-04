# Sandesh Sadalge Week 2 QUIZ

# Question 1
sample.vector <- c(2,2,5,7,18,200,200,54,34,87,29,29,21,21,64,28,71,81,81,95)

# Question 2
sample.charvec <- as.character(sample.vector)

# Question 3
sample.factorvec <- as.factor(sample.vector) # 15 levels

# Question 4
str(sample.factorvec) # 15 Levels

# Question 5
q5.vector <- (3*sample.vector^2 - 4*sample.vector + 1)

# Question 6

X <- matrix(c(1,1,1,1,1,1,1,1,5,4,6,2,3,2,7,8,8,9,4,7,4,9,6,4),nrow=8)
(X)
y <- matrix(c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1), ncol = 1)
(y)
B <- solve(t(X)%*%X)%*%t(X)%*%y
(B)

# Question 7

named.list <- list(first.element = 1:10, second.element = matrix(1:10,nrow = 5))
named.list$first.element
named.list$second.element

# Question 8

char.col <- c("Apple","Ball","Cat","Dog","Egg","Free","Green","Hello","India","Juice")
factor.col <- as.factor(c(101,101,101,102,103,102,103,102,103,103))
numeric.col <- c(10:19)
date.col <- c(as.Date("2014-01-01"),as.Date("2014-02-01"),as.Date("2014-03-01"),as.Date("2014-04-01"),as.Date("2014-05-01"),as.Date("2014-06-01"),as.Date("2014-07-01"),as.Date("2014-08-01"),as.Date("2014-09-01"),as.Date("2014-10-01"))
sample.dataframe <- data.frame(char.col, factor.col,numeric.col, date.col)
sample.dataframe


# Question 9

# First I figured out how to use rbind to adding a row with existing factor values:
new.row <- data.frame(char.col="Kodak",factor.col=103,numeric.col=20,date.col=as.Date("2014-11-01"))
sample.dataframe <- rbind(sample.dataframe, new.row)

# Adding a new factor value '104':
new.row <- data.frame(char.col="Little",factor.col2=104,numeric.col=20,date.col=as.Date("2014-12-01"))
factor.col2 <- factor(sample.dataframe$factor.col,levels=c(levels(sample.dataframe$factor.col),104))
sample.dataframe <- data.frame(char.col=sample.dataframe$char.col, factor.col2,numeric.col=sample.dataframe$numeric.col, date.col=sample.dataframe$date.col)
sample.dataframe <- rbind(sample.dataframe, new.row)
# I feel like I used a really convoluted way to do this so I would love to check out the solution when it's available.


# Question 10

temp.df <- read.csv("temperatures.csv",header=TRUE) #assuming there is a header

# Question 11

temp.df <- read.csv("c:/anotherlocation/temperatures.csv",header=TRUE) #assuming there is a header

# Question 12

temp.df <- read.table("http://somewebsite.com/somwebsitesubdirectory/temperatures.csv",header=TRUE,sep = "|")

# Question 13

result <- 1
for (i in 1:12) {
  result <- result * i
}
result

# Question 14

P <- 1500 # Principal
r <- 0.0324 # Annual interest rate
n <- 12 # Number of compounding periods per year
t <- 6 # number of years

A <- P
for (i in 1:(n*t)) {
  A <- round(A*(1+(r/n)),2)
}
A
# The answer, using above is $1821.36 BUT it does matters exactly where you round.  If you use the above rounding then you get $0.04 cents less but I bet this is how banks calculate it :-)
# If you round at the end instad of every month you get $1821.40.


# Question 15

sample.vector <- c(2,2,5,7,18,200,200,54,34,87,29,29,21,21,64,28,71,81,81,95)
# The below gives a "Warning" because the sample vector isn't a multiple of 3 BUT it does correctly zero out all but the 3rd value
(sum(sample.vector*c(0,0,1)))


# Question 16

result <- 0
for (i in 1:10) {
  result <- result + 2^i
}
result
# answer is 2046

# Question 17

i <- 10
result <- 0
while (i > 0) {
  result <- result + 2^i
  i <- i - 1
}
result
# answer is 2046

# Question 18

sum(2^(c(1:10)))
# answer is 2046

# Question 19

x <- seq(from=20, to=50, by=5)
x

# Question 20

x <- rep(c("example"),times=10)
x

# Question 21
#  Ok, so I stole this from the assignment due in a few days since I was trying to stay ahead of the curve:

quadratic <- function(a,b,c)
{
  print(sprintf("Quadratic Coefficients are: A = %s, B = %s and C = %s", a,b,c))
  
  if ((b^2 - 4*a*c) < 0)
  {
    print("No real solutions as discriminant < 0 means there are only imaginary solutions")
  } else if ((b^2 - 4*a*c) == 0)
  {
    root1 = -b/(2*a)
    print(sprintf("One real solution: %s",root1))
  } else
  {
    root1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
    root2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)  
    print(sprintf("Two real solutions: %s and %s", root1, root2))
  }
}

quadratic(1,2,3) # No Real Solutions
quadratic(1,2,1) # One Real Solution
quadratic(1,3,2) # Two Real Solutions
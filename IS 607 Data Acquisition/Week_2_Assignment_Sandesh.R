# Week 2 Assignment - Sandesh Sadalge

# Question 1-A
queue <- c("James", "Mary", "Steve", "Alex","Patricia")

# Question 1-B
queue <- c(queue, "Harold")

# Question 1-C
queue <- queue[!queue=="James"]
queue

# Question 1-D
# Ok, so for this one, I'm going to create a new vector consisting of three things:
# 1. Everything to the left of Steve
# 2. "Pam"
# 3." Everything to the right of Steve

queue <- c(queue[1:which(queue=="Steve")-1],"Pam",queue[which(queue=="Steve"):length(queue)])
queue

# Question 1-E

queue <- queue[!queue=="Harold"]
queue

# Question 1-F

queue <- queue[!queue=="Alex"]
queue

# Question 1-G

which(queue=="Patricia")
# 4th Place

# Question 1-H

length(queue)
# 4 people left

# Question 2

quadratic <- function(a,b,c)
{
  print(sprintf("Quadratic Coefficients are: A = %s, B = %s and C = %s", a,b,c))

  if ((b^2 - 4*a*c) < 0)
  {
    print("No real solutions as discriminant < 0 means there are only imaginary solutions")
  } else if ((b^2 - 4*a*c) == 0)
  {
    root1 = -b/(2*a)
    sprintf("One real solution: %s",root1)
  } else
  {
    root1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
    root2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)  
    sprintf("Two real solutions: %s and %s", root1, root2)
  }
}

quadratic(1,2,3) # No Real Solutions
quadratic(1,2,1) # One Real Solution
quadratic(1,3,2) # Two Real Solutions

# Question 3

numbrs <- c(1:1000)
numbrs <- numbrs[!numbrs%%3==0]
numbrs <- numbrs[!numbrs%%7==0]
numbrs <- numbrs[!numbrs%%11==0]
numbrs

# Question 4

pythag <- function(f,g,h)
{
  if ((f >= g) & (f >= h))
  {
    triple <- (f^2 == (g^2 + h^2))
  } else if ((g >= f) & (g >= h))
  {
    triple <- (g^2 == (f^2 + h^2))
  } else 
  {
    triple <- (h^2 == (f^2 + g^2))
  }
  
  if (triple)
    sprintf("The numbers %s, %s and %s ARE pythagorean triples.",f,g,h)
  else
    sprintf("The numbers %s, %s and %s are NOT pythagorean triples.",f,g,h)
  
}
pythag(1,2,3)
pythag(3,4,5)
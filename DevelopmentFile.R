## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("~/Documents/Git/Class/midterm")

# create package skelton so it can find the package root
# create package.skeleton only on first use
package.skeleton(code_files = c("integrateItPack-package.R", "integrateIt.R", "Trapezoid.R", "Simpson.R",
                 "tolTest.R"), name="integrateItPack")

# Run to create package
current.code <- as.package("integrateItPack")
# load all functions
load_all(current.code)
# make help files
document(current.code)

# Look through the R directory to find these files

# "integrateItPack-package.R"
# "integrateIt.R"
# "Trapezoid.R"
# "Simpson.R"
# "tolTest.R"

# Check the NAMESPACE as well

# now that everything looks okay:
# run R checks
check(current.code)
# install package
install(pkg=current.code, local=TRUE)
build(current.code, path=getwd()) 

# load package into environment
library(integrateItPack)

### test functionality ###
# create test data
xTest1 <- as.numeric(c(1:10))
yTest1 <- cos(xTest2)
# create object of class Simpson and use integrateIt function
integrateIt(xVec = xTest1, yVec = yTest1, a = 1, b = 10, rule ="Simpson")

### test plot and print Trapezoid ###
# assign object of class Trapezoid and use integrateIt function
testTrap1 <- integrateIt(xVec = xTest1, yVec = yTest1, a = 1, b = 10, rule ="Trapezoid")
# test functions 
# show all attributes
show(testTrap1)
# print (just show result)
print(testTrap1)
# plot
plot(testTrap1)

### test plot and print Simpson ###
# assign object of class Simpson and use integrateIt function
xTest2 <- as.numeric(c(1:15))
yTest2 <- cos(xTest2)
testSimp1 <- integrateIt(xVec = xTest2, yVec = yTest2, a = 1, b = 15, rule ="Simpson")
# test functions 
# show
show(testSimp1)
# print
print(testSimp1)
# plot
plot(testSimp1)


### test tolTest ###
# normal distribution
phi <- function(x) exp(-x^2/2)/sqrt(2*pi)
tolTest(inputFunc = phi, a = 0, b = 1, tol=1e-8, rule="Trapezoid")
tolTest(inputFunc = phi, a = 0, b = 1, tol=1e-8, rule="Simpson")

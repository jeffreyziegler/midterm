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
xTest <- as.numeric(c(1:10))
yTest <- rep(1, 10)
integrateIt(xVec = xTest, yVec = yTest, a = 1, b = 10, rule ="Simpson")
testTrap <- integrateIt(xVec = xTest, yVec = yTest, a = 1, b = 10, rule ="Trapezoid")
print(testTrap)
plot(testTrap)

#normal distribution
phi <- function(x) exp(-x^2/2)/sqrt(2*pi)
tolTest(inputFunc = phi, a = 0, b = 1, tol=1e-8, rule="Trapezoid")
tolTest(inputFunc = phi, a = 0, b = 1, tol=1e-8, rule="Simpson")

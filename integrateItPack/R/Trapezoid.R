#' A Trapezoid object 
#' 
#' Objects of class \code{Trapezoid} are created through the \code{integrateIt} function. Objects of
#' this class include \code{plot} and \code{print} methods.
#' 
#' An object of the class 'Trapezoid' has the following slots:
#' \itemize{
#' \item \code{xVec} Vector containing x values (numeric vector).
#' \item \code{yVec} Vector containing x values (numeric vector).
#' \item \code{a} Lower limit of integration (numeric).
#' \item \code{b} Upper limit of integration (numeric).
#' \item \code{rule} Clarifying that the Trapezoid rule will be used (character).
#' }
#'
#' @author Jeff Ziegler
#'
#' @aliases Trapezoid-class initialize,Trapezoid-method print,Trapezoid-method plot, Trapezoid-method
#' @rdname Trapezoid
#' @export
# construct class Trapezoid object
setClass(Class="Trapezoid",
         # specify input classes
         slots = c(
           xVec = "numeric",
           yVec = "numeric",
           a = "numeric",
           b = "numeric",
           n = "numeric",
           s = "numeric",
           rule = "character"
         ),
         # set default values for slots
         prototype = prototype(
           xVec = NULL,
           yVec = NULL,
           a = NULL,
           b = NULL,
           n = numeric(),
           s = numeric(),
           rule = character()
         ),
         # create validity check
         validity = function(object){
           # make sure input values are specified
           if(is.null(object@xVec) & is.null(object@yVec)){
             stop("Please specify the x and y input values!")
           }
           # make sure area to be integrated is specified
           if(is.null(object@a) | is.null(object@b)){
             stop("Please specify a defined area to integrate!")
           }
           # make sure area to be integrated is specified
           if(object@n %% 1 != 0 | object@n < 0){
             stop("N must be a positive integer")
           }
         }
)
#' @export
# create method to create object of class Trapezoid
# returns object, input values, and integrated value for objects of class Trapezoid
setMethod("initialize", "Trapezoid", 
          # requires input for name, delegatesWon, and party
          function(.Object, xVec, yVec, a, b){
            # assign .Object attributes to variables
            .Object@xVec <- xVec
            .Object@yVec <- yVec
            .Object@a <- a
            .Object@b <- b
            .Object@rule <- "Trapezoid"
            
            # put x and y values in order
            ordering <- order(.Object@xVec)
            x <- sort(.Object@xVec)
            y <- .Object@yVec[ordering]
            
            n <- length(x[x>=a & x<=b])
            # calculate numerical integral using the Trapezoid rule
            # determine h, assume a < b
            h <- (b-a)/n
            # provide values of x to plug into function
            x <- seq(a, b, by=h)
            s <-  y[1] + 2*sum(y[2:n-1]) + y[n]
            # multiple s by outside product
            .Object@s <- s*(h/2)
            # store the number of subdivisions
            .Object@n <- n
            # return values
            value=callNextMethod()
            return(value)
          }
) 
#' @export
# create method to print object of class Trapezoid
setMethod(f="print",
          # method for use with object of class Trapezoid
          signature="Trapezoid",
          # create function
          definition=function(x){
            # check validity
            validObject(x)
            # print object of class Trapezoid
            show(x@s)
          }  
)
#' @export
# create method to plot object of class Trapezoid
setMethod(f="plot",
          # method for use with object of class Trapezoid
          signature="Trapezoid",
          # create function
          definition=function(x=NULL, y=x, ...){
            # sort x and y vectors
            ordering <- order(x@xVec)
            xVec <- sort(x@xVec)
            yVec <- x@yVec[ordering]
            # retain the number of subdivisions
            n <- x@n
            # check validity
            validObject(x)
            # open plot
            plot(xVec, yVec,
                 # set limits of plot
                 xlim = c(x@a, x@b), ylim = c(min(yVec)-1, max(yVec) + 5),
                 # set labels of plot
                 xlab = "X", ylab = "f(x)", main = "Plot of function using the Trapezoid rule", pch=19)
            # create trapezoid line overtop "function"
            segments(xVec[1:n-1], yVec[1:n-1], xVec[2:n], yVec[2:n], col="blue")
            # create n segments to show subdivisions
            segments(xVec, rep(0,n), xVec, yVec, col="black", lty=2)
          }  
)
#' A Simpson object 
#' 
#' Objects of class \code{Simpson} are created through the \code{integrateIt} function. Objects of
#' this class include \code{plot} and \code{print} methods.
#'
#' 
#' An object of the class 'Simpson' has the following slots:
#' \itemize{
#' \item \code{xVec} Vector containing x values (numeric vector).
#' \item \code{yVec} Vector containing x values (numeric vector).
#' \item \code{a} Lower limit of integration (numeric).
#' \item \code{b} Upper limit of integration (numeric).
#' \item \code{rule} Clarifying that Simpson's rule will be used (character).
#' }
#'
#' @author Jeff Ziegler
#'
#' @aliases Simpson-class initialize,Simpson-method print,Simpson-method plot, Simpson-method
#' @rdname Simpson
#' @export
# construct class Simpson object
setClass(Class="Simpson",
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
         }
)
#' @export
# create method to create object of class Simpson
# returns object, input values, and integrated value for objects of class Simpson
setMethod("initialize", "Simpson", 
          # requires input for vectors, start, end
          function(.Object, xVec, yVec, a, b){
            # assign .Object attributes to variables
            .Object@xVec <- xVec
            .Object@yVec <- yVec
            .Object@a <- a
            .Object@b <- b
            .Object@rule <- "Simpson"
            # calculate numerical integral using Simpson's rule
            # sort x and y vectors
            ordering <- order(.Object@xVec)
            x <- sort(.Object@xVec)
            y <- .Object@yVec[ordering]
            # specify n
            n <- length(x[x>=a & x<=b])
            
            # determine h, assume a < b
            h <- (b-a)/n
            # specify integration based on n
            if (n == 2) {
              s <- y[1] + y[2]
            }
            # define s if n==3
            if (n == 3) {
              s <- y[1] + 4*y[2] + y[3]
            }
            # define s for all other positive values of n
            else {
              s <- y[1] + y[n] + 2*sum(y[seq(3,n-2,by=2)]) + 4*sum(y[seq(2,n-1,by=2)])
            }
            
            # multiply by outside fraction
            .Object@s <- s*(h/3)
            # store the number of subdivisions
            .Object@n <- n
            
            value=callNextMethod()
            return(value)
          }
) 
#' @export
# create method to print object of class Simpson
setMethod(f="print",
          # object of class Simpson
          signature="Simpson",
          # create function
          definition=function(x){
            # check validity
            validObject(x)
            # print object of class Simpson
            show(x@s)
          }  
)
#' @export
# create method to plot object of class Simpson
setMethod(f="plot",
          # object of class Simpson
          signature="Simpson",
          # open print function
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
                 xlab = "X", ylab = "f(x)", main = "Plot of function using Simpson's rule", pch=19)

            # create n segments to show subdivisions
            segments(xVec, rep(0,n), xVec, yVec, col="black", lty=2)
            # add parabolas
            # function to create (m, f (m))
            for (i in seq(2, n, 2)){
              # note the number of desired "points" for the lines to connect through
              mid <- seq(from = xVec[i-1], to = xVec[i+1], length.out = n*3)
              # define first component of p(x)
              firstComponent <- yVec[i-1]*((mid-xVec[i])*(mid-xVec[i+1]))/
                ((xVec[i-1]-xVec[i])*(xVec[i-1]-xVec[i+1]))
              # define second component of p(x)
              secondComponent <- yVec[i]*((mid-xVec[i-1])*(mid-xVec[i+1]))/
                ((xVec[i]-xVec[i-1])*(xVec[i]-xVec[i+1]))
              # define third component of p(x)
              thirdComponent <- yVec[i+1]*((mid-xVec[i-1])*(mid-xVec[i]))/
                ((xVec[i+1]-xVec[i-1])*(xVec[i+1]-xVec[i]))
              # combine all components to yield p(x)
              pX <- firstComponent + secondComponent + thirdComponent
              # draw parabola lines, connecting 
              lines(mid, pX, col="blue")
            }
          }   
)

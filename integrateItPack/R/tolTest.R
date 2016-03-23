#' Calculate the numeric integral with a specified degree of tolerance
#' 
#' Function that increases the number of intervals until the estimated integral is within the specified tolerance of the correct answer. 
#'
#' @param inputFunc Function to input into \code{tolTest} (function).
#' @param a Number of delegates not yet allocated in a given candidate's party primary (numeric).
#' @param b Lower limit of integration (numeric).
#' @param tol The amount of tolerance between estimate and real integral (numeric).
#' @param rule Clarify which rule with be used, Simpson's Rule or the Trapezoidal Rule (character).
#' 
#' @return Returns the initial inputs, the partition size (final n), and absolute error of estimate (list).
#' 
#' @author Jeff Ziegler
#' @examples
#' 
#' @seealso \code{\link{Trapezoid}}, \code{\link{Simpson}}, \code{\link{integrateIt}}
#' @rdname tolTest
#' @aliases tolTest,ANY-method
#' @export
# create generic for tolTest
setGeneric(name = "tolTest",
           def=function(inputFunc, a, b, tol=1e-8, rule="Trapezoid")
           {standardGeneric("tolTest")}
)

# create method
setMethod(f="tolTest",
          definition=function(inputFunc, a, b, tol=1e-8, rule="Trapezoid"){
            # create initial n
            n <- 4
            # calculate h
            h <- (b-a)/4
            # retrieve x values from inputFunc
            x <- seq(a, b, by=h)
            y <- inputFunc(x)
            # determine which rule to use
            initialIntegrate <- function(y, h, n, rule) {
              # Simpson's rule
              if (rule == "Simpson") {
                s <- y[1] + y[n+1] + 4*sum(y[seq(2,n,by=2)]) + 2 *sum(y[seq(3,n-1, by=2)])
                s <- s*h/3
              }
              # Trapezoidal rule
              else if (rule == "Trapezoid") {
                s <- h * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
              }	
              # return the calulated integral
              return(s)
            }
            # perform the initial integration
            s <- initialIntegrate(y, h, n, rule)
            # save value as initial integration
            sInitial <- s
            # update the difference specified by the tolerance
            s.diff <- tol + 1
            # begin while loop that continues until tolerance is met
            while (s.diff > tol ) {
              # replace new s with old one
              sPrior <- s
              # update n
              n <- 2*n
              # update h
              h <- h/2
              # reuse old function values
              y[seq(1, n + 1, by = 2)] <- y
              y[seq(2, n, by = 2)] <- sapply(seq(a + h, b - h, by = 2*h), inputFunc)
              # redo integration
              s <- initialIntegrate(y, h, n, rule)
              # update the difference between new intergration and old
              s.diff <- abs(s-sPrior)
            }
            # return items in a list
            return(list(
              "inputFunc" = inputFunc, "a" = a, "b" = b, "tol" = tol, "n" = n,
              "absoluteError" = abs(sInitial - sPrior), rule = rule))
          }
)
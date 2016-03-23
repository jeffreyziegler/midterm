#' Perform numeric integration
#'
#' Function creates an object of class 'Simpson' or 'Trapezoid' and determines the numeric
#' integral performing using the specified rule.
#'
#' @param xVec Vector containing x values (numeric vector).
#' @param yVec Vector containing x values (numeric vector).
#' @param rule Clarify which rule, Simpson's or Trapezoid, will be used (character).
#'
#' @return An object of class 'Candidate' containing
#' \itemize{
#' \item \code{xVec} Vector containing x values (numeric vector).
#' \item \code{yVec} Vector containing x values (numeric vector).
#' \item \code{a} Lower limit of integration (numeric).
#' \item \code{b} Upper limit of integration (numeric).
#' \item \code{rule} Clarify which rule, Simpson's or Trapezoid, will be used (character).
#' }
#' @author Jeff Ziegler
#' @examples
#' 
#' xTest <- as.numeric(c(1:10))
#' yTest <- rep(1, 10)
#' newTrap <- integrateIt(xVec = xTest, yVec = yTest, a = 1, b = 10, rule ="Trapezoid")
#' print(newTrap)
#' 
#' @seealso \code{\link{Simpson}} \code{\link{Trapezoid}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
# create generic for integrateIt
setGeneric(name = "integrateIt",
           def=function(xVec, yVec, a, b, rule)
           {standardGeneric("integrateIt")}
)

# create method
setMethod(f="integrateIt",
          definition=function(xVec, yVec, a, b, rule){
            # return object of class Simpson
            if(rule == "Simpson"){
              return(new("Simpson", xVec = xVec, yVec = yVec, a = a, b = b))
            }
            # return object of class Trapezoid
            if(rule == "Trapezoid"){
              return(new("Trapezoid", xVec = xVec, yVec = yVec, a = a, b = b))
            }
          }
)
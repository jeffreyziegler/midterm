\name{Trapezoid-class}
\Rdversion{1.1}
\docType{class}
\alias{Trapezoid-class}
\alias{initialize,Trapezoid-method}
\alias{plot,Trapezoid-method}
\alias{print,Trapezoid-method}

\title{Class \code{"Trapezoid"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Trapezoid", xVec, yVec, a, b)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{xVec}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{yVec}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{a}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{b}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{n}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{s}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{rule}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "Trapezoid")}: ... }
    \item{plot}{\code{signature(x = "Trapezoid")}: ... }
    \item{print}{\code{signature(x = "Trapezoid")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Trapezoid")
}
\keyword{classes}

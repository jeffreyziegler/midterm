\name{Simpson-class}
\Rdversion{1.1}
\docType{class}
\alias{Simpson-class}
\alias{initialize,Simpson-method}
\alias{print,Simpson-method}

\title{Class \code{"Simpson"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Simpson", xVec, yVec, a, b)}.
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
    \item{initialize}{\code{signature(.Object = "Simpson")}: ... }
    \item{print}{\code{signature(x = "Simpson")}: ... }
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
showClass("Simpson")
}
\keyword{classes}

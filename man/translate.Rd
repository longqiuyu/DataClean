\name{translate}
\alias{translate}
\title{
Translate code into interpretable output
}
\description{
Translate codes starting with some certain letters to output.
}
\usage{
translate(data, code, output)
}
\arguments{
  \item{data}{
vector to translate
}
  \item{code}{
original codes (can be a vector of codes)
}
  \item{output}{
interpretable output
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Translate "SOP", "SOX" into "Sunday Only"
freq=c("SOP","SOX","SOP","SOP","SOX")
codes="SO"
output="Sunday Only"
translate(freq,codes,output)
}

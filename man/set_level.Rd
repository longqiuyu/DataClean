\name{set_level}
\alias{set_level}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set base level for factors
}
\description{
Set base level for factors. (most frequency level as base by default)
}
\usage{
set_level(data, base = default)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data to set level
}
  \item{base}{
defult: automatically choose the most frequency level as base.
customized value: set base level yourself
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
## The function is currently defined as
function (data, base = default)
{
    default = names(which.max(table(data)))
    relevel(as.factor(data), ref = base)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

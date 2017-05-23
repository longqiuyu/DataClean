\name{recode}
\alias{recode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recode to output
}
\description{
Recode a vector of codes into a vector of output. If codes not mentioned, go into "Other"
}
\usage{
recode(data, code_list, output_list, other = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector to recode
}
  \item{code_list}{
Code list
}
  \item{output_list}{
Output list (1 on 1 according to code list)
}
  \item{other}{
True: codes not mentioned go into "Other"
False: codes not mentioned kept as original codes
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
function (data, code_list, output_list, other = TRUE)
{
    data = as.character(data)
    for (i in 1:length(code_list)) {
        data = translate(data, code_list[i], output_list[i])
    }
    if (other)
        data[!data \%in\% output_list] = "Other"
    data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

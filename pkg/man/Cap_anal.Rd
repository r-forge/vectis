\name{vectis.cap}
\alias{vectis.cap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to Compute and Display Process Capability
}
\description{
Computes the process capability of a continuous variable
}
\usage{
vectis.cap (x, distribution = "normal",
                       USL = NA,
                       LSL = NA,
                       target = NA,
                       main = "Capabilities Analysis",
                       sub = "",
                       groupsize = 1,
                       mrlength = 2,
                       alpha = 0.05,
                       tol = 5.15,
                       unbias_sub = TRUE,
                       unbias_overall = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data values}
  \item{distribution}{Distribution of sampled data.  Currently only "normal" is supported.}
  \item{USL}{Upper Spec Limit}
  \item{LSL}{Lower Spec Limit}
  \item{target}{Target}
  \item{main}{Chart Title}
  \item{sub}{Chart Subtitle}
  \item{groupsize}{Group size for grouped data.  Data must be sorted by groups.  Currently only a groupsize of 1 is supported.}
  \item{mrlength}{Number of observations used in the moving range to compute the within standard deviation for groupsize = 1.}
  \item{alpha}{Probability of a Type I error}
  \item{tol}{Sigma multiplier for capabilities statistics.  Usually defined as 5.15 or 6}
  \item{unbias_sub}{Logical value as to whether the subgroup standard deviation should include the unbiasing constant. Default is TRUE}
  \item{unbias_overall}{Logical value as to whether the overall standard deviation should include the unbiasing constant. Default is FALSE}
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
Christopher Battles
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

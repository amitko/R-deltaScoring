\name{dS.personDscore}
\alias{dS.personDscore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the so called d-Score for a person
 with a given response vector over a set
 of items with precalculated deltas;
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dS.personDscore(itemData, deltas, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{itemData}{
 0/1 item response
}
  \item{deltas}{
Item delta values
}
  \item{type}{
"relative_to_d" - default value

"relative_to_n"

"total"
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
Dimitar Atanasov, 2022

\email{datanasov@nbu.bg}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

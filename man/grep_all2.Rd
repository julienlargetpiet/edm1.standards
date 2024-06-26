% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fun.R
\name{grep_all2}
\alias{grep_all2}
\title{grep_all2}
\usage{
grep_all2(inpt_v, pattern_v)
}
\arguments{
\item{inpt_v}{is the input vectors to grep elements from}

\item{pattern_v}{is a vector contaning the patterns to grep}
}
\description{
Performs the grep_all function with another algorythm, potentially faster
}
\examples{

print(grep_all2(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
               pattern_v = c("z", "4")))

[1] 15 23 25  4 14 19

print(grep_all2(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
               pattern_v = c("z", "^4$")))

[1] 15 23 25  4 19

print(grep_all2(inpt_v = c(1:14, "z", 1:7, "z", "a", "z"), 
               pattern_v = c("z")))

[1] 15 23 25

}

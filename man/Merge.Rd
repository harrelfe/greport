\name{Merge}
\alias{Merge}
\title{Merge Multiple Data Frames}
\usage{
Merge(..., id, all = TRUE, verbose = TRUE)
}
\arguments{
  \item{\dots}{two or more dataframes}

  \item{id}{a formula containing all the identification
  variables such that the combination of these variables
  uniquely identifies subjects or records of interest}

  \item{all}{set to \code{FALSE} to drop observations not
  found in second and later data frames}

  \item{verbose}{set to \code{FALSE} to not print
  information about observations}
}
\description{
Merges an arbitrarily large series of data frames
containing common \code{id} variables.  Information about
number of observations and number of unique \code{id}s in
individual and final merged datasets is printed.  The first
data frame has special meaning in that all of its
observations are kept whether they match \code{id}s in
other data frames or not.  For all other data frames, by
default non-matching observations are dropped.  The first
data frame is also the one against which counts of unique
\code{id}s are compared.  Sometimes \code{merge} drops
variable attributes such as \code{labels} and \code{units}.
These are restored by \code{Merge}.
}
\examples{
a <- data.frame(sid=1:3, age=c(20,30,40))
b <- data.frame(sid=c(1,2,2), bp=c(120,130,140))
d <- data.frame(sid=c(1,3,4), wt=c(170,180,190))
all <- Merge(a, b, d, id = ~ sid)
}


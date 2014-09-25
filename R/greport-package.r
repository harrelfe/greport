#' Graphical Reporting for Clinical Trials
#'
#' @author Frank E Harrell Jr \email{f.harrell@@vanderbilt.edu}
#'
#' @@maintainer Frank E Harrell Jr \email{f.harrell@@vanderbilt.edu}
#'
#' @export Merge accrualReport dNeedle dReport eReport endPlot exReport getgreportOption nriskReport putFig sampleFrac setgreportOption startPlot survReport upFirst
#' @import Hmisc lattice latticeExtra data.table
#' @importFrom rms npsurv survplot
#' @importFrom survival Surv survfit
#' @importFrom Formula Formula model.part
#' @docType package
#' @aliases greport package-greport
#' @name greport

# The caching and check for conflicts require looking for a pattern of objects; the search may be avoided by defining an object ‘.noGenerics’
# see ?library
.noGenerics <- TRUE

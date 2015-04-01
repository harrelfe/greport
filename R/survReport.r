#' Survival Report
#'
#' Generate a Survival Report with Kaplan-Meier Estimates
#'
#' @param formula a formula with survival (\code{Srv}) objects on the left hand side and an optional stratification factor on the right (or \code{1} if none).  The survival object component variables should be labeled; these labels are used for graph annotation.
#' @param data data.frame
#' @param subset optional subsetting criteria
#' @param na.action function for handling \code{NA}s while creating a data frame
#' @param ylab character. Passed to \code{\link[rms]{survplot.npsurv}} as the \code{ylab} argument.  Constructed by default.
#' @param what \code{"S"} (the default) to plot survival functions or \code{"1-S"} to plot cumulative incidence functions.
#' @param conf character. See \code{\link[rms]{survplot.npsurv}}.
#' @param panel character string.  Name of panel, which goes into file base names and figure labels for cross-referencing.
#' @param subpanel character string.  If calling \code{dReport} more than once for the same type of chart (categorical or continuous), specify \code{subpanel} to distinguish the multiple calls.  In that case, \code{-subpanel} will be appended to \code{panel} when creating figure labels and cross-references.
#' @param head character string.  Specifies initial text in the figure caption, otherwise a default is used.
#' @param tail optional character string.  Specifies final text in the figure caption, e.g., what might have been put in a footnote in an ordinary text page.  This appears just before any needles.
#' @param h numeric. Height of plots.
#' @param w numeric. Width of plots in inches.
#' @param multi logical.  If \code{TRUE}, multiple figures are produced, othewise a single figure with a matrix of survival plots is made.
#' @param mfrow numeric 2-vector, used if \code{multi=FALSE}.  If not specified, default plot matrix layout will be figured.
#' @param y.n.risk used if \code{what="1-S"}, to specify \code{y} coordinate for putting numbers at risk, typically below the \code{x}-axis label
#' @param bot number of spaces to reserve at bottom of plot for numbers at risk, if \code{what="1-S"}
#' @aehaz logical.  Set to \code{FALSE} to not print number of events and hazard rate on plots.
#' @param append logical. If \code{TRUE} output will be appended instead of overwritten.
#' @param \dots ignored
#' @export
#' @examples
#' ## See tests directory test.Rnw for a live example
#' \dontrun{
#'   set.seed(1)
#'   n <- 400
#'   dat <- data.frame(t1=runif(n, 2, 5), t2=runif(n, 2, 5),
#'                     e1=rbinom(n, 1, .5), e2=rbinom(n, 1, .5),
#'                     treat=sample(c('a','b','c'), n, TRUE))
#'   dat <- upData(dat,
#'                 labels=c(t1='Time to operation',
#'                          t2='Time to rehospitalization',
#'                          e1='Operation', e2='Hospitalization',
#'                          treat='Treatment')
#'                 units=c(t1='year', t2='year'))
#'   survReport(Srv(t1, e1) + Srv(t2, e2) ~ treat, data=dat)
#' }

survReport <- function(formula, data=NULL, subset=NULL, na.action=na.retain,
                       ylab=NULL, what=c('S', '1-S'),
                       conf=c('diffbands', 'bands', 'bars', 'none'),
                       panel='surv', subpanel=NULL, head=NULL, tail=NULL,
                       h=3, w=4.5, multi=FALSE, mfrow=NULL, y.n.risk=-.5,
                       bot=2, aehaz=TRUE, append=FALSE, ...)
{
  if(grepl('[^a-zA-Z-]', panel))
    stop('panel must contain only A-Z a-z -')
  if(length(subpanel) && grepl('[^a-zA-Z-]', subpanel))
    stop('subpanel must contain only A-Z a-z -')
    
  what <- match.arg(what)
  conf <- match.arg(conf)
  form <- Formula(formula)
  Y <- if(length(subset)) model.frame(form, data=data, subset=subset,
                                      na.action=na.action)
  else model.frame(form, data=data, na.action=na.action)
  X <- model.part(form, data=Y, rhs=1)
  Y <- model.part(form, data=Y, lhs=1)
  
  conf <- match.arg(conf)

  texdir <- getgreportOption('texdir')
  file   <- if(getgreportOption('texwhere') == 'gentex')
    sprintf('%s/%s.tex', texdir, panel) else ''
  if(file != '' && ! append) cat('', file=file)
  lb <- if(length(subpanel)) sprintf('%s-%s', panel, subpanel)
   else panel

  namx <- labx <- NULL
  if(length(X)) {
    x <- X[[1]]
    namx <- names(X)[1]
    labx  <- upFirst(ifelse(label(x) == '', namx, label(x)), lower=TRUE)
  }

  Nobs <- nobsY(formula, group=getgreportOption('tx.var'),
                data=data, subset=subset, na.action=na.action)

  ny <- ncol(Y)
  if(ny == 1) multi <- FALSE
  
  if(! multi) {
    mf <- if(length(mfrow)) mfrow else mfrowSuggest(ny)
    startPlot(lb, h=h, w=w, mfrow=mf, bot=if(what=='1-S') bot else 0,
              lattice=FALSE, ...)
  }

  gro <- getgreportOption()
  x.is.tx <- FALSE; ng <- 0
  if(length(X)) {
    x <- X[[1]]
    ng <- if(is.factor(x)) length(levels(x)) else
     length(unique(x[!is.na(x)]))
    if(namx == gro$tx.var) {
      x.is.tx <- TRUE
      col <- gro$tx.linecol
      lwd <- gro$tx.lwd
    }
    else {
      ## col <- rep(c(gray(c(0, .7)), 'blue', 'red', 'green'), length=ng)
      col <- rep(gro$nontx.col, length=ng)
      lwd <- rep(c(1, 3), length=ng)
    }
  } else {
    x <- rep('', nrow(Y))
    col <- 1
    lwd <- 2
  }

  nobs <- rep(0, 1 + x.is.tx * ng)
  evlab <- character(ny)
  for(i in 1 : ny) {
    y <- Y[[i]]
    evlab[i] <- upFirst(label(y), lower=TRUE)
    no <- nrow(y[! is.na(y)])
    if(multi) {
      lbi <- paste(lb, i, sep='-')
      startPlot(lbi, h=h, w=w, bot=if(what=='1-S') bot else 0, lattice=FALSE, ...)
    }
    yl <- if(length(ylab)) ylab else {
      yl <- upFirst(evlab[i])
      if(what == 'S') paste(yl, '-Free Probability', sep='')
       else paste('Incidence of', yl)
    }

    s <- npsurv(y ~ x)
    if(conf == 'diffbands' && length(s$strata) < 2) conf <- 'bands'
    if(x.is.tx) {
      no        <- c(no, s$n)
      names(no) <- c('randomized', levels(x))
    }
    if(what == 'S')
      survplot(s, 
               n.risk=TRUE, conf=conf, lwd=lwd,
               lty=1, col=col, ylab=yl,
               label.curves=list(keys='lines', key.opts=list(bty='n')),
               levels.only=TRUE, aehaz=aehaz, ...)
    else
      survplot(s, fun=function(y) 1 - y,
               n.risk=TRUE, y.n.risk=y.n.risk, conf=conf, lwd=lwd,
               lty=1, col=col, ylab=yl,
               label.curves=list(keys='lines', key.opts=list(bty='n')),
               levels.only=TRUE, aehaz=aehaz, ...)

    capconf <- if(conf == 'diffbands') ', along with half-height of 0.95 confidence limits centered at survival estimate midpoints. $N$=' else
    ', along with 0.95 confidence bands.  $N$='
    
    if(multi) {
      endPlot()
      shortcap <- if(length(head)) head else 'Kaplan-Meier estimates'
      shortcap <- paste(shortcap, 'for', evlab[i])
      if(length(labx))
        shortcap <- paste(shortcap, 'stratified by', labx)
      cap <- paste(shortcap, capconf, no[1], '. ', tail, sep='')
      dNeedle(sampleFrac(no, Nobs), name='lttsurv', file=file)
      cap <- sprintf('%s~\\hfill\\lttsurv', cap)
      putFig(panel=panel, name=lbi, caption=shortcap, longcaption=cap)
    }
    if(! multi) for(j in 1:length(nobs)) nobs[j] <- max(nobs[j], no[j])
    names(nobs) <- names(no)
    if(! length(names(no)) && length(nobs) == 1) names(nobs) <- 'randomized'
  }
  
  if(! multi) {
    past <- function(x) {
      l <- length(x)
      if(l < 2) x
      else if(l == 2) paste(x, collapse=' and ')
      else paste(paste(x[1 : (l - 1)], collapse=', '), x[l], sep=', and ')
    }

    endPlot()
    shortcap <- if(length(head)) head else 'Kaplan-Meier estimates'
    shortcap <- paste(shortcap, 'for', past(evlab))
    if(length(labx))
      shortcap <- paste(shortcap, 'stratified by', labx)
    cap <- paste(shortcap, capconf, nobs[1], '. ', tail, sep='')
    dNeedle(sampleFrac(nobs, Nobs), name='lttsurv', file=file)
    cap <- sprintf('%s~\\hfill\\lttsurv', cap)
    putFig(panel=panel, name=lb, caption=shortcap, longcaption=cap)
  }
  invisible()
}

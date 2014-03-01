\name{putFig}
\alias{putFig}
\title{Put Figure}
\usage{
putFig(panel, name, caption = NULL, longcaption = NULL,
  tcaption = caption, tlongcaption = NULL, poptable = NULL,
  popfull = FALSE, sidecap = FALSE, append = TRUE)
}
\arguments{
  \item{panel}{character. Panel name.}

  \item{name}{character. Name for figure.}

  \item{caption}{character. Short caption for figure.}

  \item{longcaption}{character. Long caption for figure.}

  \item{tcaption}{character.  Short caption for supporting
  table.}

  \item{tlongcaption}{character.  Long caption for
  supporting table.}

  \item{poptable}{an optional character string containing
  LaTeX code that will be used as a pop-up tool tip for the
  figure (typically a tabular)}

  \item{popfull}{set to \code{TRUE} to make the pop-up be
  full-page}

  \item{sidecap}{set to \code{TRUE} (only applies if
  \code{greportOption(figenv="SCfigure")}) to assume the
  figure is narrow and to use side captions}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}
}
\description{
Included a generated figure within LaTex document.
\code{tcaption} and \code{tlongcaption} only apply if
\code{setgreportOption(tablelink="hyperref")}.
}


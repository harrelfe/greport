R greport package: Graphical Reporting of Clinical Trials
=======
Statisticians and statistical programmers spend a great deal of time analyzing data and producing reports for clinical trials, both for final trial reports and for interim reports for data monitoring committees.  Point and Click interfaces and copy-and-paste are now believed to be bad models for reproducible research.  Instead, there are advantages to developing a high-level language for producing common elements of reports related to accrual, exclusions, descriptive statistics, adverse events, time to event, and longitudinal data.

It is well appreciated in the statistical and graphics design communities that graphics are much better than tables for conveying numeric information.  There are thus advantages for having statistical reports for clinical trials that are almost completely graphical.   For those reviewers of clinical trial reports who insist on seeing tables, and for those who occasionally like to have tables to see "exact" figures for certain data elements, supporting tables can be placed in an appendix.  These tables are hyperlinked to the main graphics.  Small tables can also pop-up when one hovers the mouse over a graphic.  These two approaches are facilitated by features of Adobe Acrobat Reader.  Reviewers who prefer printed reports can print the appendix in order to have a complete document.

greport marries R, the R Hmisc and lattice packages, knitr, and LaTeX
to produce reproducible clinical trial reports with a minimum of
coding.  greport composes all figure captions and makes heavy use of
analysis file annotations such as variable labels and units of
measurement.  Some new graphical elements are introduced such as
special dot charts that replace tables, extended box plots, split
violin plots for longitudinal continuous variables, half confidence
intervals for differences, new charts for representing patient flow,
and pop-up tooltips.  Supporting tables are hyperlinked to graphics,
and the graphics are hyperlinked back from the tables.  Figure
captions contain supporting table numbers, and tables contain figure
numbers.

Current Goals
=============
* In accrual report cumulative randomized plots add text for deficit at last recorded randomized subject
* Add Svetlana Eden's function in rreport package for graphically summarizing adverse events by major and minor categories (e.g., body system and preferred term)
* Add function similar to that in rreport for group sequential monitoring boundary presentation
* Need executable tests in tests/
* See if current tests should become vignettes


Web Sites
=============
* Overall: http://biostat.mc.vanderbilt.edu/Greport
* CRAN: http://cran.r-project.org/web/packages/greport
* Changelog: https://github.com/harrelfe/greport/commits/master

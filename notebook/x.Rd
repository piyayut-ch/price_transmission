\name{x}
\alias{x}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("x")}
\format{
  The format is:
Formal class 'ca.jo' [package "urca"] with 26 slots
  ..@ x        : Time-Series [1:84, 1:4] from 1980 to 2001: 930 930 930 931 933 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:4] "e" "prod" "rw" "U"
  ..@ Z0       : num [1:81, 1:4] 1.1093 1.2343 0.8889 -0.0194 -0.4546 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:4] "e.d" "prod.d" "rw.d" "U.d"
  ..@ Z1       : num [1:81, 1:9] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:9] "constant" "e.dl1" "prod.dl1" "rw.dl1" ...
  ..@ ZK       : num [1:81, 1:4] 930 931 933 934 934 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:4] "e.l1" "prod.l1" "rw.l1" "U.l1"
  ..@ type     : chr "maximal eigenvalue statistic (lambda max)"
  ..@ model    : chr "with linear trend"
  ..@ ecdet    : chr "none"
  ..@ lag      : int 3
  ..@ P        : int 4
  ..@ season   : NULL
  ..@ dumvar   : NULL
  ..@ cval     : num [1:4, 1:3] 6.5 12.91 18.9 24.78 8.18 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:4] "r <= 3 |" "r <= 2 |" "r <= 1 |" "r = 0  |"
  .. .. ..$ : chr [1:3] "10pct" "5pct" "1pct"
  ..@ teststat : num [1:4] 0.061 10.721 16.358 43.818
  ..@ lambda   : num [1:4] 0.41781 0.182865 0.123973 0.000752
  ..@ Vorg     : num [1:4, 1:4] -0.246 -0.158 0.172 -0.135 1.345 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:4] "e.l1" "prod.l1" "rw.l1" "U.l1"
  .. .. ..$ : chr [1:4] "e.l1" "prod.l1" "rw.l1" "U.l1"
  ..@ V        : num [1:4, 1:4] 1 0.641 -0.7 0.551 1 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:4] "e.l1" "prod.l1" "rw.l1" "U.l1"
  .. .. ..$ : chr [1:4] "e.l1" "prod.l1" "rw.l1" "U.l1"
  ..@ W        : num [1:4, 1:4] 0.02558 0.00132 0.09528 0.00771 0.15429 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:4] "e.d" "prod.d" "rw.d" "U.d"
  .. .. ..$ : chr [1:4] "e.l1" "prod.l1" "rw.l1" "U.l1"
  ..@ PI       : num [1:4, 1:4] 0.1561 0.2445 0.0662 -0.1297 0.0744 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:4] "e.d" "prod.d" "rw.d" "U.d"
  .. .. ..$ : chr [1:4] "e.l1" "prod.l1" "rw.l1" "U.l1"
  ..@ DELTA    : num [1:4, 1:4] 0.097 -0.0265 -0.0309 -0.059 -0.0265 ...
  ..@ GAMMA    : num [1:4, 1:9] -150.687 -195.87 -11.669 114.367 0.597 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:4] "e.d" "prod.d" "rw.d" "U.d"
  .. .. ..$ : chr [1:9] "constant" "e.dl1" "prod.dl1" "rw.dl1" ...
  ..@ R0       : num [1:81, 1:4] 0.84797 0.34828 0.00776 -0.24893 0.10595 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:4] "R0.e.d" "R0.prod.d" "R0.rw.d" "R0.U.d"
  ..@ RK       : num [1:81, 1:4] -7.092 -0.949 1.89 0.075 -6.817 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:4] "RK.e.l1" "RK.prod.l1" "RK.rw.l1" "RK.U.l1"
  ..@ bp       : logi NA
  ..@ spec     : chr "transitory"
  ..@ call     : language ca.jo(x = Canada, type = "eigen", K = 3, spec = "transitory")
  ..@ test.name: chr "Johansen-Procedure"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(x)
## maybe str(x) ; plot(x) ...
}
\keyword{datasets}

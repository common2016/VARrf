% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IRFrf_gen.R
\name{IRFrf_gen}
\alias{IRFrf_gen}
\title{Generalized IRF}
\usage{
IRFrf_gen(data, pmax = 5, s = 12, d = 1, shockvar = 1, ncores = 6)
}
\arguments{
\item{pmax}{max lag order where select a lag order which has minimum OOB MSE.}

\item{s}{the horizon of IRF}

\item{d}{the size of shock
Impulse Response Function by Random Forest}

\item{shockvar}{a numeric scalor which denotes the shock variable.}
}
\description{
GIRF using LP method by
}

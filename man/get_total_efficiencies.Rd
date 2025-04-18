% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_total_efficiencies.R
\name{get_total_efficiencies}
\alias{get_total_efficiencies}
\title{Get CES Total Efficiencies}
\usage{
get_total_efficiencies(gdxName)
}
\arguments{
\item{gdxName}{Path to \code{.gdx} file}
}
\value{
A \code{data.frame} with columns \code{t}, \code{regi}, \code{pf}
    (production factor) and the \code{alpha} value.
}
\description{
Computes total efficiency
\eqn{\alpha = \xi \left(\theta \delta \right)^\rho} from \code{pm_cesdata}.
}
\author{
Michaja Pehl
}

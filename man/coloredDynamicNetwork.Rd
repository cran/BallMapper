% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BallMapper.R
\name{coloredDynamicNetwork}
\alias{coloredDynamicNetwork}
\title{This procedure produces a dynamic graph with colors. It allows zoom-in operation and displays information about vertices when they are clicked upon.}
\usage{
coloredDynamicNetwork(outputOfBallMapper, showLegend = FALSE)
}
\arguments{
\item{outputOfBallMapper, }{an output from the BallMapper function}

\item{showLegend, }{if set to TRUE a legend will be displayed indicating the coloring of the values of vertices.}
}
\value{
None
}
\description{
This procedure produces a dynamic graph with colors. It allows zoom-in operation and displays information about vertices when they are clicked upon.
}
\examples{
var <- seq(from=0,to=6.3,by=0.1)
points <- as.data.frame( cbind( sin(var),cos(var) ) )
values <- as.data.frame( sin(var) )
epsilon <- 0.25
l <- BallMapper(points,values,epsilon)
coloredDynamicNetwork(l)
}

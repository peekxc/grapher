% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{updateNetwork}
\alias{updateNetwork}
\title{Update the network.}
\usage{
updateNetwork(id, net)
}
\arguments{
\item{id}{Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in.}

\item{net}{A JSON network exported with \code{exportJSON()} to fully replace the current network.}
}
\description{
Update the network.
}

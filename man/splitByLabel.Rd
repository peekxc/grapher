% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{splitByLabel}
\alias{splitByLabel}
\title{Splits nodes by a given labeling}
\usage{
splitByLabel(id, node_labels, label_xy, isolate_links = TRUE)
}
\arguments{
\item{id}{Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in.}

\item{node_labels}{An integer vector of class labels to group nodes with. The 0 class is never positioned. See details.}

\item{label_xy}{A data.frame with containing for each group a label (column 1), a relative 'x' coordinate, and a relative 'y' coordinate.}
}
\description{
Splits nodes by a given labeling
}
\details{
TODO
}

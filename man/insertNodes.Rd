% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{insertNodes}
\alias{insertNodes}
\title{#' Generates the JSON configuration of a given grapher object. 
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @export
get_json_config <- function(id){
  # if (!is(g, "grapher")){ stop("'get_json_config' expects a grapher object.") }
  export <- list(id = id, method = "getGraph")
  callJS()
}
Insert nodes.}
\usage{
insertNodes(id, node_config)
}
\arguments{
\item{id}{Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in.}

\item{node_config}{A json node configuration to add to the network.}
}
\description{
Inserts nodes into the grapher instance. If the nodes supplied already exist in the network, no insertion is performed. 
and the properties of pre-existing nodes are not modified. If the node does not exist, it is created with 
the supplied properties. To modify an existing nodes properties by id, see \code{setNodes}.
}
\seealso{
getDefaultJsonConfig setNodes
}

## api.R 
## Contains the API for the grapher R package, an interface for the 'grapher.js' library by Ayasdi. 
## Author: Matt Piekenbrock

# Unexported helper functions
normalize <- function(x) { (x - min(x))/(max(x) - min(x)) }

## Generic meta-wrapper for passing commands from R to JavaScript
## 1. The reactive domain is null, in which case shiny is not being used. Call the widget method.
## 2. The reactive domain is not null, shiny is being used, use the appropriate message handler
## Props to https://deanattali.com/blog/htmlwidgets-tips/#api-abstract for coming up with a nice meta-wrapper
## for handling both shiny-contexts and magrittr-piped contexts
callJS <- function() {
  message <- as.list(parent.frame(1)[["export"]])
  if (methods::is(message$id, "grapher")) {
    widget <- message$id
    widget$x$api <- c(widget$x$api, list(message))
    return(widget)
  } else if (is.character(message$id)){ ## Shiny mode
    session <- shiny::getDefaultReactiveDomain()
    method <- paste0("grapher:", message$method)
    cat(sprintf("Sending message: %s \n", method))
    session$sendCustomMessage(method, message)
    return(message$id)
  } else {
    stop("Invalid JS method called.")
  }
}

# createEmptyJsonConfig <- function(network){
#   json_config <- list()
#   n_nodes <- nrow(network)
#   graph <- igraph::graph_from_adjacency_matrix(network)
#   el <- igraph::
#   json_config$nodes <- data.frame(x=rep(0L, n_nodes), y=rep(0L, n_nodes), r=rep(1L, n_nodes), color = rgb(0, 0, 0))
#   json_config$links <- data.frame(from=el[, 1]-1L, to=el[, 2]-1L, color=rgb(0, 0, 0))
#   graph <- igraph::graph_from_adjacency_matrix(network)
# }

#' Creates a default JSON configuration
#' @param network An adjacency matrix. See details. 
#' @description Exports a network into a JSON object suitable for visualization with the grapher library.
#' @details The 'grapher.js' library uses a D3-like network representation with the following format:
#' (network) : {
#'   nodes: [{ x: 0, y: 0, r: 20, color: (swatch or hex/rgb), ... }, ... ],
#'   links: [{ from: 0, to: 1, color: (swatch or hex/rgb), ... }, ... ]
#' }
#' Thus, a valid JSON configuration includes minimal set information needed to create a grapher network.
#' This function returns a list with the following members:
#'  \code{nodes} := a data.frame w/ members 'x', 'y', 'r', and optionally 'color'
#'  \code{edges} := a data.frame w/ members 'from', 'to', and optionally 'color'
#' If customization is needed the JSON network object sent to the grapher htmlwidgets, these values can be changed
#' directly before passing to grapher. This may be useful in the situation where additional data is associated with 
#' the network is needed on the JavaScript end, i.e. extra node or edge attributes. Alternatively, 
#' 
#' 
#' Note that if one is using shiny, this method only needs to be called once. Subsequent API calls use the shiny reactive context 
#' to update the values of the network on the JS side directly, as opposed to regenerating the JSON configuration on the R side. 
#' @export
getDefaultJsonConfig <- function(network = NULL){

  ## If no network was given, return a blank configuration
  if (missing(network)){ return(list(nodes = data.frame(x=0, y=0, r=1, color = rgb(0, 0, 0, 1)), links = data.frame(from=0, to=0, color=rgb(0, 0, 0)))) }
  
  ## Create the configuration based on the adjacency matrix
  json_config <- list()
  
  ## Create the node configuration; a layout algorithm is called to smooth initial performance in grapher
  if (length(igraph::V(network)) > 0){
    node_xy <- apply(igraph::layout.auto(network), 2, normalize) ## normalize coordinates to [0, 1]
    json_config$nodes <- data.frame(x=node_xy[, 1], y=node_xy[, 2], r=5L, color = rgb(0, 0, 0))
  }
  
  ## Create link configuration
  if (length(igraph::E(network)) > 0){
    el <- igraph::as_edgelist(network, names = FALSE)
    json_config$links <- data.frame(from=el[, 1]-1L, to=el[, 2]-1L, color=rgb(0, 0, 0))
  }
  
  ## Return the configuration
  return(json_config)
}

## Creates a mapper JSON configuration with nice defaults
getDefaultMapperConfig = function(mapper_obj){
  ## Create mapper defaults
  json_config <- list()
  rbw_pal <- rev(rainbow(100, start = 0, end = 4/6))
  if ("MapperRef" %in% class(mapper_obj)){ 
    ## Color nodes and edges by a default rainbow palette
    agg_node_val <- sapply(sapply(mapper_obj$G$nodes, function(n_idx){ apply(as.matrix(mapper_obj$cover$filter_values[n_idx,]), 1, mean)}), mean)
    binned_idx <- cut(agg_node_val, breaks = 100, labels = F)
    
    ## Make defaults 
    graph <- igraph::graph_from_adjacency_matrix(mapper_obj$G$adjacency, mode = "undirected", add.colnames = NA) 
    node_sizes <- sapply(mapper_obj$G$nodes, length) 
    node_default_colors <- rbw_pal[binned_idx]
  } else if ("Mapper" %in% class(mapper_obj)){
    graph <- igraph::graph_from_adjacency_matrix(mapper_obj$adjacency, mode = "undirected", add.colnames = NA)
    node_sizes <- sapply(mapper_obj$nodes, length)
    node_default_colors <- rep(rgb(0, 0, 0, 1), length(mapper_obj$nodes))
  } else { stop("'getDefaultMapperConfig' expects a Mapper object.") }
  
  ## Create the nodes. By default, color them on a rainbow palette according to their mean filter value.
  if (length(igraph::V(graph)) > 0){
    node_radii <- (15L - 10L)*normalize(log(node_sizes)) + 10L
    node_xy <- apply(igraph::layout.auto(graph), 2, normalize)
    json_config$nodes <- data.frame(x=node_xy[, 1], y=node_xy[, 2], r=node_radii,
                                     color=node_default_colors,
                                     index = 0:(length(node_sizes)-1))
  }
  
  ## Create the links w/ a similar coloring scheme.
  if (length(igraph::E(graph)) > 0){
    el <- igraph::as_edgelist(graph, names = FALSE)
    if ("MapperRef" %in% class(mapper_obj)){
      edge_binned_idx <- apply(el, 1, function(node_ids) { (binned_idx[node_ids[1]] + binned_idx[node_ids[2]])/2 })
      edge_links <- cbind(as.data.frame(apply(el, 2, as.integer) - 1L), substr(rbw_pal[edge_binned_idx], start = 0, stop = 7))
    } else {
      edge_links <- cbind(as.data.frame(apply(el, 2, as.integer) - 1L), rgb(0, 0, 0))
    }
    json_config$links <- structure(edge_links, names = c("from", "to", "color"))
  }
  
  ## Return the configuration
  return(json_config) 
}


#' Remove selected nodes. 
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param node_ids A vector of node ids to remove. 
#' @export
removeNodes = function(id, node_ids){
  export <- list(id = id, method = "removeNodes", node_ids = node_ids)
  callJS()
}

#' Add selected edges. 
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param v1 A vector of node ids to connect 'from'. 
#' @param v2 A vector of node ids to connect 'to'.
#' @param color A vector of edge colors.
#' @description Add all edges connecting nodes from 'v1' to 'v2' (or vice versa, since graphs are assumed undirected). 
#' @export
addEdges = function(id, v1, v2, color = rgb(0, 0, 0)){
  new_edges <- jsonlite::toJSON(data.frame(from=pmin.int(v1, v2) - 1L, to = pmax.int(v1, v2) - 1L, color = color))
  export <- list(id = id, method = "addLinks", links_to_add=new_edges)
  callJS()
}

#' Remove selected edges. 
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param v1 A vector of node ids to connect 'from'. 
#' @param v2 A vector of node ids to connect 'to'.
#' @description Removes all edges connecting nodes from 'v1' to 'v2' (or vice versa, since graphs are assumed undirected). 
#' @export
removeEdges = function(id, v1, v2){
  export <- list(id = id, method = "removeLinks", links_to_remove=cbind(from=pmin.int(v1, v2), to = pmax.int(v1, v2)))
  callJS()
}



#' Update the node colors. 
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param color A vector of hexadecimal color codes. Transparency values supported. 
#' @export
updateNodeColor = function(id, color){
  rgb_colors <- col2rgb(color, alpha = TRUE)
  color <- apply(rgb_colors, 2, function(rgba) { sprintf("rgba(%d,%d,%d,%.2f)", rgba[1], rgba[2], rgba[3], rgba[4]) })
  export <- list(id = id, method = "updateNodeColor", color = color)
  callJS()
}

#' Update the node sizes. 
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param r A vector of integers representing the node's relative size.
#' @export
updateNodeSize = function(id, r){
  export <- list(id = id, method = "updateNodeSize", size = r)
  callJS()
}

#' Update the edge colors. 
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param color A vector of hexadecimal color codes. Transparency values are *not* supported. 
#' @export
updateEdgeColor = function(id, color){
  if (!all(nchar(color) == 7L)){ stop("'updateEdgeColor' expects a vector of 7-length hexadecimal color codes. Transparency is not supported.") }
  export <- list(id = id, method = "updateEdgeColor", color = color)
  callJS()
}

#' Update the network.
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param net A JSON network exported with \code{exportJSON()} to fully replace the current network.
#' @export
updateNetwork = function(id, net){
  export <- list(id = id, method = "updateNetwork", net = net)
  callJS()
}

#' Logs the network to the rendering console. Useful for debugging. 
logNetwork = function(id){
  export <- list(id = id, method = "logNetwork")
  callJS()
}

#' Group singletons.
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param centroid an optional 2-vector of XY coordinates to group the singletons around.
#' @export
groupSingletons = function(id, centroid = c(0.9, 0.1), strength = 0.18){
  export <- list(id = id, method = "groupSingletons", centroid = unname(centroid), strength = strength)
  callJS()
}

#' Splits nodes by a given labeling
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @param node_labels An integer vector of class labels to group nodes with. The 0 class is never positioned. See details.
#' @param label_xy A data.frame with containing for each group a label (column 1), a relative 'x' coordinate, and a relative 'y' coordinate.
#' @details TODO
#' @export
splitByLabel <- function(id, node_labels, label_xy, isolate_links = TRUE){
  if (is.null(dim(label_xy)) || ncol(label_xy) != 3){ stop("'splitByLabel' expects to have a label defined in the 'label' attribute of the nodes.") }
  if (!is.data.frame(label_xy) || !all(names(label_xy) %in% c("label", "x", "y"))){ stop("'splitByLabel' expects label_xy to be a data.frame with valid 'label', 'x', and 'y' columns.")}
  label_xy <- jsonlite::toJSON(structure(apply(label_xy[, c("x", "y")], 1, as.list), names = as.character(label_xy$label)), auto_unbox = TRUE)
  if (isolate_links){
    
  }
  export <- list(id = id, method = "splitByLabel", node_labels=node_labels, label_xy=label_xy)
  callJS() 
}

#' Center the network
#' @param id Either the grapher's htmlwidget instance or, if in a shiny context, the container id the widget is housed in. 
#' @description Centers the grapher network at the center of the canvas' viewport. 
#' @export
center <- function(id){
  export <- list(id = id, method = "center")
  callJS()
}

#' @export
updateForce <- function(id, ...){
  # possible_force_opts <- c("charge", "gravity", "linkStrength", "linkDistance", "friction")
  # invalid_opts <- !names(force_opts) %in% possible_force_opts
  # if (any(invalid_opts)){ warning(sprintf("Detected unknown foce options %s, please use one of: %s", names(force_opts)[which(invalid_opts)], possible_force_opts)) }
  export <- list(id = id, method = "updateForce", force = list(...))
  callJS()
}

getForces <- function(id, forces = "all"){
  
}


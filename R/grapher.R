#' Grapher
#' @description Given a adjacency matrix or edgelist, constructs an interactive grapher htmlwidget.
#' @author Matt Piekenbrock
#' @import htmlwidgets
#' @importFrom magrittr %>%
#' @export
grapher <- function(x){
  if (is(x, "igraph")){
    json_config <- getDefaultJsonConfig(x)
  } else if (is.matrix(x) && isSymmetric(x)){ 
    json_config <- getDefaultJsonConfig(igraph::graph_from_adjacency_matrix(x, mode = "undirected", add.colnames = NA))
  } else if (is.matrix(x) && dim(x)[[2]] == 2){
    json_config <- getDefaultJsonConfig(igraph::graph_from_edgelist(x, directed = FALSE))
  } else if (is.list(x) && all(c("nodes", "links") %in% names(x))) {
    json_config <- x   
  } else { stop("'grapher' can only handle adjacency matrices or edgelists.") }
  createWidget(jsonlite::toJSON(json_config))
}

# Grapher instances need to be stored in canvas elements
# "Note that this function is looked up within the package implementing the widget by the convention widgetname_html
# so it need not be formally exported from your package or otherwise registered with htmlwidgets."
#' @import htmltools
grapher_html <- function(id, style, class, ...){
  htmltools::tags$canvas(id = id, class = class, style = style)
}

#' @export
grapherOutput <- function(outputId, width = "100%", height = "100%") {
  shinyWidgetOutput(outputId = outputId, name = "grapher", width = width, height = height, package = "grapher")
}

#' @export
renderGrapher <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr = expr, outputFunction = grapherOutput, env, quoted = TRUE)
}

createWidget <- function(json_net, width = NULL, height = NULL, elementId = NULL){
  x <- list(net = json_net, api = list())
  htmlwidgets::createWidget(name = "grapher", x = x, width = width, height = height,
                            elementId = elementId, package = "grapher",
                            sizingPolicy =  htmlwidgets::sizingPolicy(padding = 0, viewer.padding = 0, browser.fill = TRUE, viewer.fill = TRUE))
}

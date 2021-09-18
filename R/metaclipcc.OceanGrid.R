#' @keywords internal

metaclipcc.OceanGrid <- function(dc.description = "Ocean model grid",
                                 ref.URL = NULL) {
    graph <- make_empty_graph()
    grid.nodename <- paste("RectangularGrid", randomName(), sep = ".")
    attr.list <- list()
    attr.list[["dc:description"]] <- dc.description
    if (!is.null(ref.URL)) attr.list[["ds:referenceURL"]] <- ref.URL
    graph <- add_vertices(graph,
                          nv = 1,
                          name = grid.nodename,
                          label = "Ocean Grid",
                          className = "ds:RectangularGrid",
                          attr = attr.list)
    return(list("graph" = graph, "parentnodename" = grid.nodename))
}
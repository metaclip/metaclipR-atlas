#' @title Map Projection
#' @description Creates a single node graph that defines a map projection
#' @param proj A Valid individual instance of class go:MapProjection
#' @keywords internal
#' @export
#' @author J Bedia

metaclipcc.MapProjection <- function(proj) {
    # # stopifnot(proj %in% knownClassIndividuals(proj, classname = "MapProjection",
    #                                           vocabulary = "graphical_output",
    #                                           source.vocab = "graphical_output"))
    graph <- make_empty_graph()
    graph <- my_add_vertices(graph,
                             name = proj,
                             label = paste(proj, "Projection"),
                             className = "go:MapProjection")
    return(list(graph = graph, parentnodename = proj))
}
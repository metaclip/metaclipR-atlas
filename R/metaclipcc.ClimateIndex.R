#' @title Encode metadata for IPCC product climate index definition
#' @description Encode metadata for IPCC product climate index definition
#' @param graph.list A list of input graphs.
#' @param index.code Index code
#' @return A metaclipR object
#' @details The function asssumes that the index.code passed as argument is an individual instance of the ds:ClimateIndex class,
#' defined in the ipcc_terms vocabulary
#' @export
#' @author J Bedia
#' @importFrom metaclipR my_add_vertices my_union_graph getNodeIndexbyName randomName
#' @importFrom igraph add_edges

metaclipcc.ClimateIndex <- function(graph.list, index.code) {

    ref <- showIPCCvars(names.only = FALSE)[grep(index.code, showIPCCvars()),]
    descr <- paste0("Calculation of \'", index.code,
                    "\' Climate Index (", ref$description, ")")
    nodename <- paste0("ClimateIndexCalculation.", randomName())
    graph <- graph.list[[1]]$graph
    graph <- my_add_vertices(graph,
                             name = nodename,
                             label = "Climate Index Calculation",
                             className = "ds:ClimateIndexCalculation",
                             attr = list("dc:description" = descr))

    for (i in 1:length(graph.list)) {
        if (class(graph.list[[i]]$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
        graph <- my_union_graph(graph, graph.list[[i]]$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, graph.list[[i]]$parentnodename),
                             getNodeIndexbyName(graph, nodename)),
                           label = "ds:hadClimateIndexCalculation")
    }
    # Climate index node -------------------------------------------------------
    descr <- paste("The climate index", ref$description, "is calculated")

    if (!is.na(ref$scale)) {
        paste0(descr, paste(", considering a scale of", ref$scale, "months"))
    }
    ci.nodename <- ref$vocabulary
    cn <- "ds:ClimateIndex"
    graph <- my_add_vertices(graph,
                             name = ci.nodename,
                             label = index.code,
                             className = cn,
                             attr = list("dc:description" = descr))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, nodename),
                         getNodeIndexbyName(graph, ci.nodename)),
                       label = "ds:withClimateIndex")
    # Update temporalResolution ------------------------------------------------
    timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
    if (ref$time_step != "P1D") {
        graph <- my_add_vertices(graph,
                                 name = timeres.nodename,
                                 label = paste("Update time resolution to", ref$time_step),
                                 className = "ds:TemporalResolution",
                                 attr = list("ds:hasTimeStep" = ref$time_step,
                                             "dc:description" = paste("The data time resolution is updated to the monthly time resolution of",
                                                                      ref$description)))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, nodename),
                             getNodeIndexbyName(graph, timeres.nodename)),
                           label = "ds:hasTemporalResolution")
    }
    return(list("graph" = graph, "parentnodename" = nodename))
}


#' @title Encode metadata for IPCC product climate index definition
#' @description Encode metadata for IPCC product climate index definition
#' @param graph Input graph
#' @param index.code Index code
#' @param new.time.res Temporal resolution of the index. Default to monthly
#' @return A metaclipR object
#' @details The function asssumes that the index.code passed as argument is an individual instance of the ds:ClimateIndex class,
#' defined in the ipcc_terms vocabulary
#' @export
#' @author J Bedia
#' @importFrom metaclipR my_add_vertices getNodeIndexbyName randomName knownClassIndividuals
#' @importFrom igraph add_edges


metaclipcc.ClimateIndex <- function(graph, index.code, new.time.res = "P1M") {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (is.null(index.code)) {
        stop("The 'index.code' argument is missing in the argument list, with no default")
    }
    ## Hay que arreglar esto!
    index.choices <- knownClassIndividuals(classname = "ClimateIndex", vocabulary = "ipcc_terms") ## Arreglar esto
    index.code <- match.arg(index.code,
                            choices = c("TASMAXNA35", "PRNAP99"))
    orig.node <- graph$parentnodename
    graph <- graph$graph
    cicalc.node <- paste("ClimateIndexCalculation", randomName(), sep = ".")
    # ClimateIndex calculation node
    graph <- my_add_vertices(graph,
                             name = cicalc.node,
                             label = "ClimateIndexCalculation",
                             className = "ds:ClimateIndexCalculation")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, cicalc.node)),
                       label = "ds:hadClimateIndexCalculation")
    # Climate index node
    nodename <- paste0("ipcc:", index.code)
    cn <- "ds:ClimateIndex"
    graph <- my_add_vertices(graph,
                             name = nodename,
                             label = paste("ClimateIndex", index.code, sep = "."),
                             className = cn)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, cicalc.node),
                         getNodeIndexbyName(graph, nodename)),
                       label = "ds:withClimateIndex")
    # TemporalResolution ---------------------
    timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
    graph <- my_add_vertices(graph,
                             name = timeres.nodename,
                             label = "TemporalResolution",
                             className = "ds:TemporalResolution",
                             attr = list("ds:hasTimeStep" = new.time.res))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, cicalc.node),
                         getNodeIndexbyName(graph, timeres.nodename)),
                       label = "ds:hasTemporalResolution")

    return(list("graph" = graph, "parentnodename" = cicalc.node))
}


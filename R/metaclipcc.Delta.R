##     metaclipcc.Delta Construct a directed graph for encoding climate change signal calculation
##
##     Copyright (C) 2020 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
##
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
##
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Directed metadata graph construction for Climate Change Signal calculation
#' @description Build a directed metadata graph describing a Climate Change Signal calculation
#' @param graph An output from a previous \pkg{metaclipR} function containing a list with the i-graph class object containing
#'  the input grid whose anomaly is to be computed, plus the terminal node from which the Anomaly Step will hang
#' @param referenceGraph An output from a previous \pkg{metaclipR} function containing a list with the i-graph class object containing the reference Transformation-class object
#' used as base to compute the climatology, plus the name of its terminal node
#' @param delta.type Character string. Either \code{"absolute"} (default), or \code{"relative"}, if the delta change is computed as a
#'  ratio instead of a difference.
#' @details This function takes as reference the semantics defined in the Data Source and Transformation ontology
#' defined in the Metaclip Framework (\url{http://www.metaclip.org}).
#' @family transformation
#' @export
#' @importFrom igraph add_edges
#' @importFrom metaclipR my_add_vertices my_union_graph
#' @author J. Bedia

metaclipcc.Delta <- function(graph,
                             referenceGraph = NULL,
                             delta.type = c("absolute", "relative")) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (class(referenceGraph$graph) != "igraph") stop("Invalid input reference graph (not an 'igraph-class' object)")
    delta.type <- match.arg(delta.type, choices = c("absolute", "relative"), several.ok = FALSE)
    withInput <- graph$parentnodename
    graph <- graph$graph
    orig.nodes.command <- c()
    anom.nodename <- paste("delta", randomName(), sep = ".")
    orig.nodes.command <- c(orig.nodes.command, anom.nodename)
    if (delta.type == "absolute") {
        anom.class <- "ds:DifferenceSignal"
        anom.label <- "Absolute Delta Change"
        descr <- "The climate change signal is computed as the arithmetic difference between the climatologies of the future time slice and the historical scenario"
    } else {
        anom.class <- "ds:RelativeSignal"
        anom.label <- "Relative Delta Change"
        descr <- "The climate change signal is computed as the ratio (in %) between the climatologies of the future and the historical scenarios"
    }
    graph <- my_add_vertices(graph,
                             name = anom.nodename,
                             label = anom.label,
                             className = anom.class,
                             attr = list("dc:description" = descr))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, withInput),
                         getNodeIndexbyName(graph, anom.nodename)),
                       label = "ds:hadClimateChangeSignalCalculation")
    # Graphs 1 and 2 are joined
    uniongraph <- my_union_graph(graph, referenceGraph$graph)
    graph <- add_edges(uniongraph,
                       c(getNodeIndexbyName(uniongraph, anom.nodename),
                         getNodeIndexbyName(uniongraph, referenceGraph$parentnodename)),
                       label = "ds:withReference")
    return(list("graph" = graph, "parentnodename" = anom.nodename))
}



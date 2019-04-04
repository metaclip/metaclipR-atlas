
#' @title Encode metadata of graphical output for anomaly maps
#' @param graph input METACLIP graph structure
#' @param proj Projection string
#' @param spatial.ref Input spatial reference indicating map spatial extent
#' @importFrom igraph add_vertices
#' @export
#' @author J Bedia

metaclipcc.AnomalyMap <- function(graph, proj, spatial.ref) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    withInput <- graph$parentnodename
    graph <- graph$graph
    map.nodename <- paste("Map", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = map.nodename,
                          label = "Anomaly Map",
                          className = "go:Map",
                          attr = list("go:hasProjection" = proj))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, spatial.ref$parentnodename)),
                       label = "go:hasMapExtent")
    maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "Heatmap of anomalies",
                          className = "go:MapRaster")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, withInput),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hadGraphicalRepresentation")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")
    # Coastline
    maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "Coastline",
                          className = "go:MapLines",
                          attr = list("go:hasLayerDescription" = "Physical vector map of coastline boundaries"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")
    # IPCC regions
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "IPCC World Regions",
                          className = "go:MapLines",
                          attr = list("go:hasLayerDescription" = "IPCC-AR5 World Regions"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")
    return(list("graph" = graph, "parentnodename" =  map.nodename))
}

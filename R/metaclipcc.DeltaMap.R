
#' @title Encode metadata of graphical output for anomaly maps
#' @param graph input METACLIP graph structure
#' @param proj Projection string
#' @param spatial.ref Input spatial reference indicating map spatial extent
#' @importFrom igraph add_vertices
#' @export
#' @author J Bedia

metaclipcc.DeltaMap <- function(graph, proj, spatial.ref) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    withInput <- graph$parentnodename
    graph <- graph$graph
    map.nodename <- paste("Map", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = map.nodename,
                          label = "Anomaly Map",
                          className = "go:Map",
                          attr = list("go:hasProjection" = proj,
                                      "dc:description" = "Final map product, consisting of different superposed layers and other graphical elements (legend, title etc.)"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, spatial.ref$parentnodename)),
                       label = "go:hasMapExtent")
    maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "Heatmap of anomalies",
                          className = "go:MapRaster",
                          attr = list("dc:description" = "The anomalies are graphically displayed on the map as a raster heatmap layer"))
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
                          label = "Coastline boundaries",
                          className = "go:MapLines",
                          attr = list("dc:description" = "Vector layer. Physical map of coastline boundaries"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")
    # IPCC regions
    # maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
    # graph <- add_vertices(graph,
    #                       nv = 1,
    #                       name = maplayer.nodename,
    #                       label = "IPCC World Regions",
    #                       className = "go:MapLines",
    #                       attr = list("dc:description" = "IPCC-AR5 World Regions"))
    # graph <- add_edges(graph,
    #                    c(getNodeIndexbyName(graph, map.nodename),
    #                      getNodeIndexbyName(graph, maplayer.nodename)),
    #                    label = "go:hasMapLayer")
    # STIPPLING
    # IPCC regions
    maplayer.nodename <- paste("mapLayer", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "Consensus Stippling",
                          className = "go:Mask",
                          attr = list("dc:description" = "The underlying anomaly grid cells with less than 66% of model agreement are partially masked by the stippling point layer"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")


    return(list("graph" = graph, "parentnodename" =  map.nodename))
}

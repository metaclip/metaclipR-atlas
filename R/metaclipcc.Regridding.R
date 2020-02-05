##     metaclipcc.Regridding Construct a directed graph for regridding operations
##
##     Copyright (C) 2020 Santander Meteorlogy Group (http://www.meteo.unican.es)
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

#' @title Directed metadata graph construction for Regridding Transformations
#' @description Build a directed metadata graph describing a regridding step
#' @param graph Input metaclip graph structure
#' @param RefSpatialExtent A reference spatial extent used for interpolation. The reference spatial extent can be initiated with \code{\link{metaclipR.SpatialExtent}}
#' @param RefRectangularGrid A reference rectangular grid used for interpolation. The reference rectangular extent can be initiated with \code{\link{metaclipR.RectangularGrid}}
#' @param InterpolationMethod Interpolation method. Current possible choices include \code{"nearest"}, \code{"bilinear"}, \code{"bicubic"},
#'  \code{"IDW"}, \code{"spline"} and \code{"ConservativeRemapping"}, although some of them won't be probably used in the IPCC AR6 Atlas
#' @param dc.description Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
#'  "dc:description" annotation to the ds:Interpolation-class node.
#' @importFrom igraph add_vertices add_edges
#' @importFrom metaclipR getNodeIndexbyName my_union_graph
#' @author J. Bedia
#' @keywords internal

metaclipcc.Regridding <- function(graph,
                                  RefSpatialExtent = NULL,
                                  RefRectangularGrid = NULL,
                                  InterpolationMethod,
                                  dc.description = NULL) {
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    interp.method <- match.arg(InterpolationMethod,
                               choices = c("nearest", "bilinear", "bicubic", "IDW", "spline", "conservative"))
    interp.method.class <- switch(interp.method,
                                  "nearest" = "ds:NearestNeighbor",
                                  "bilinear" = "ds:BilinearInterpolation",
                                  "bicubic" = "ds:BicubicInterpolation",
                                  "IDW" = "ds:InverseDistanceWeighting",
                                  "spline" = "ds:Splines",
                                  "conservative" = "ds:ConservativeRemapping")
    orig.node <- graph$parentnodename
    graph <- graph$graph
    regnodename <- paste("Interpolation", randomName(), sep = ".")
    if (is.null(dc.description)) {
        graph <- add_vertices(graph,
                              nv = 1,
                              label = paste(interp.method, "interpolation"),
                              name = regnodename,
                              className = interp.method.class)
    } else {
        graph <- add_vertices(graph,
                              nv = 1,
                              label = paste(interp.method, "interpolation"),
                              name = regnodename,
                              className = interp.method.class,
                              attr = list("dc:description" = dc.description))
    }
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, orig.node),
                         getNodeIndexbyName(graph, regnodename)),
                       label = "ds:hadInterpolation")
    regmethod.nodename <- if (interp.method == "conservative") {
        "ds:EUROCordexConservativeRemapping"
    } else {
        paste("InterpolationMethod", randomName(), sep = ".")
    }
    graph <- add_vertices(graph,
                          nv = 1,
                          name = regmethod.nodename,
                          label = interp.method,
                          className = interp.method.class)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, regnodename),
                         getNodeIndexbyName(graph, regmethod.nodename)),
                       label = "ds:hadInterpolationMethod")
    # Link SpatialExtent

    if (!is.null(RefSpatialExtent)) {
        if (class(RefSpatialExtent$graph) != "igraph") stop("Invalid \'RefSpatialExtent\' structure")
        spatextent.nodename <- RefSpatialExtent$parentnodename
        graph <- my_union_graph(graph, RefSpatialExtent$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, regnodename),
                             getNodeIndexbyName(graph, spatextent.nodename)),
                           label = "ds:hasHorizontalExtent")
    }
    # Link Grid
    if (!is.null(RefRectangularGrid)) {
        if (class(RefRectangularGrid$graph) != "igraph") stop("Invalid \'RefRectangularGrid\' structure")
        grid.nodename <- RefRectangularGrid$parentnodename
        graph <- my_union_graph(graph, RefRectangularGrid$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, regnodename),
                             getNodeIndexbyName(graph, grid.nodename)),
                           label = "ds:usedReferenceCoordinates")
    }
    return(list("graph" = graph, "parentnodename" = regnodename))
}

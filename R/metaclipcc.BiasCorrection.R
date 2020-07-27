##     metaclipcc.BiasCorrection Construct a directed graph for bias correction
##
##     Copyright (C) 2020 Predictia (http://www.predictia.es)
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

#' @title Directed metadata graph construction for bias correction steps
#' @description Build a directed metadata graph from bias correction routines. A IPCC Atlas targeted version of the
#' more general \code{\link[metaclipR]{metaclipR.BiasCorrection}}.
#' @param graph metaclipR output containing the data top be bias-corrected.
#' @param TrainingGraph metaclipR output containing the training data (e.g. 20C3M/historical scenario in
#'  climate change applications etc.)
#' @param ReferenceGraph metaclipR output containing the reference predictand (typically observations)
#' @param ReferenceGraphSpatialExtent Default to \code{NULL} and unused. Otherwise, this points to a SpatialExtent class node
#' containing the horizontal spatial extent information of the observations. This will update the Spatial extent of the calibrated
#' dataset to that of the reference observations used for calibration.
#' @param ReferenceGraphRectangularGrid Default to \code{NULL} and unused. Otherwise, this points to a ds:RectangularGrid class node
#' containing the grid definition of the predictand. This will update the Spatial extent of the calibrated
#' dataset to that of the reference observations used for calibration.
#' @param BC.method Character string indicating the name of the bias correction method. Currently accepted
#' values are \code{"EQM"} and \code{"ISIMIP3"} (the only ones implemented so far in the Atlas Chapter).
#' @param dc.description Default to \code{NULL} and unused. Otherwise, this is a character string that will be appendend as a
#'  "dc:description" annotation to the ds:Calibration node.
#' @details This function takes as reference the semantics defined in the Calibration ontology defined in the
#' Metaclip Framework (\url{http://www.metaclip.org/}). These in turn are partially based on the VALUE Framework (Gutiérrez et al. 2018)
#' @references
#' Gutiérrez et al, 2018. An intercomparison of a large ensemble of statistical downscaling methods over Europe:
#' Results from the VALUE perfect predictor cross-validation experiment. International Journal of Climatology.
#' https://doi.org/10.1002/joc.5462

#' @export
#' @importFrom igraph make_empty_graph add_vertices add_edges
#' @author J. Bedia

metaclipcc.BiasCorrection <- function(graph,
                                      TrainingGraph,
                                      ReferenceGraph,
                                      ReferenceGraphSpatialExtent = NULL,
                                      ReferenceGraphRectangularGrid = NULL,
                                      BC.method = "EQM",
                                      dc.description = "Bias adjustment of the input data")
{
    BC.method <- match.arg(BC.method, choices = c("EQM", "ISIMIP3"))
    BC.class <- "cal:NonParametricBiasCorrection"
    if (class(graph$graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    if (class(TrainingGraph$graph) != "igraph") stop("Invalid input TrainingGraph (not an 'igraph-class' object)")
    if (class(ReferenceGraph$graph) != "igraph") stop("Invalid input ReferenceGraph (not an 'igraph-class' object)")
    pnode <- graph$parentnodename
    graph <- graph$graph
    # Adding the Calibration node
    cal.node <- paste0("Calibration.", randomName())
    graph <- my_add_vertices(graph,
                             name = cal.node,
                             label = "Calibration",
                             className = "cal:Calibration")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, pnode),
                         getNodeIndexbyName(graph, cal.node)),
                       label = "cal:hadCalibration")
    # Update spatial extent
    if (!is.null(ReferenceGraphSpatialExtent)) {
        if (class(ReferenceGraphSpatialExtent$graph) != "igraph") stop("Invalid \'ReferenceGraphSpatialExtent\' structure")
        spatextent.nodename <- ReferenceGraphSpatialExtent$parentnodename
        graph <- my_union_graph(graph, ReferenceGraphSpatialExtent$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, cal.node),
                             getNodeIndexbyName(graph, spatextent.nodename)),
                           label = "ds:hasHorizontalExtent")
    }
    if (!is.null(ReferenceGraphRectangularGrid)) {
        if (class(ReferenceGraphRectangularGrid$graph) != "igraph") stop("Invalid \'ReferenceGraphRectangularGrid\' structure")
        grid.nodename <- ReferenceGraphRectangularGrid$parentnodename
        graph <- my_union_graph(graph, ReferenceGraphRectangularGrid$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, cal.node),
                             getNodeIndexbyName(graph, grid.nodename)),
                           label = "ds:hasRectangularGrid")
    }
    # Adding the CalibrationMethod node (unlike metaclipR.BiasCorrection, here only know class individuals accepted)
    # A dc:description field is assumed to be introduced
    method.nodename <- paste0("cal:", BC.method)
    graph <- my_add_vertices(graph,
                             name = method.nodename,
                             label = BC.method,
                             className = BC.class,
                             attr = list("dc:description" = dc.description))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, cal.node),
                         getNodeIndexbyName(graph, method.nodename)),
                       label = "cal:withCalibrationMethod")
    # Adding the training Data
    graph <- my_union_graph(graph, TrainingGraph$graph)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, cal.node),
                         getNodeIndexbyName(graph, TrainingGraph$parentnodename)),
                       label = "cal:withTrainingData")
    # Adding the predictand Data
    graph <- my_union_graph(graph, ReferenceGraph$graph)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, cal.node),
                         getNodeIndexbyName(graph, ReferenceGraph$parentnodename)),
                       label = "cal:withReferenceData")
    return(list("graph" = graph, "parentnodename" = cal.node))
}

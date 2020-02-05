##     metaclipcc.HorizontalExtent Construct a directed graph defining a reference SpatialExtent
##
##     Copyright (C) 2020 Santander Meteorology Group (http://www.predictia.es)
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


#' @title Set a reference SpatialExtent
#' @description Creates a graph containing a SpatialExtent definition, that can be used later as spatial reference of
#' subsetting, regridding operations etc. so all steps point to the same reference spatial extent
#' @param region Character string designating a known region.
#' @return A metaclipR list (igraph-class structure + terminal node name)
#' @importFrom igraph make_empty_graph add_vertices
#' @author J Bedia
#' @export


metaclipcc.HorizontalExtent <- function(region) {
    # Comprueba individuos para name
    graph <- make_empty_graph()
    spatextent.nodename <- paste0("ds:", region)
    graph <- add_vertices(graph,
                          nv = 1,
                          name = spatextent.nodename,
                          label = paste(region, "region", sep = "_"),
                          className = "ds:HorizontalExtent")
    return(list("graph" = graph, "parentnodename" = spatextent.nodename))
}



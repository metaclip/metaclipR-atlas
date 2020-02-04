##     metaclipcc.DatasetSubset METACLIP description of a DatasetSubset
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

#' @title Directed metadata graph construction for IPCC Atlas product data sources
#' @description Build a directed metadata graph describing a subset of Dataset
#' @param metaclipcc.Dataset METACLIP description of the Dataset, as returned by \code{\link{metaclipcc.Dataset}}.
#' @param variable Code of the ECV. This must be an individual instance of \code{ds:Variable}.
#' @param Dataset.name Name (label) of the Dataset. This dataset must be included in the internal
#'  lookup table, that can be accessed via \code{\link{showIPCCdatasets}}.
#' @param time.res.orig Temporal resolution of the original data, as downloaded from ESGF. Monthly (\code{"P1M"})for ECV maps,
#'  and daily (\code{"P1D"}) for index calculation.
#' @param ipcc.region Code of the IPCC region. This must be an individual instance of \code{ds:HorizontalExtent}.
#' @param season Season. Integer vector of (correlative) months, e.g.: \code{c(12,1,2)} for DJF (boreal winter).
#' @param years Integer vector of length two with start/end year of the period, e.g.: \code{c(2016,2035)} for the AR5 baseline.
#' @export
#' @importFrom igraph make_empty_graph add_edges
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom metaclipR my_add_vertices getNodeIndexbyName randomName
#' @author J. Bedia

# metaclipcc.Dataset <- metaclipcc.Dataset("CMIP5_CNRM-CERFACS-CNRM-CM5_historical")
# meta <- metaclipcc.DatasetSubset(metaclipcc.Dataset, variable = "ta", years = c(1986,2005))
# graph2json(meta$graph, "/tmp/cnrm.json")

# Dataset.name <- "CMIP5_CNRM-CERFACS-CNRM-CM5_historical"

metaclipcc.DatasetSubset <- function(metaclipcc.Dataset,
                                     Dataset.name,
                                     variable,
                                     time.res.orig = c("P1D", "P1M"),
                                     ipcc.region = "GlobalExtent",
                                     season = 1:12,
                                     years) {
    time.res.orig <- match.arg(time.res.orig, choices = c("P1D", "P1M"), several.ok = FALSE)
    variable <- match.arg(variable, choices = c("tas","tasmax","tasmin","sst","pr","O2","pH"), several.ok = FALSE)
    if (length(years) != 2) stop("Argument \'years\' must be of length two")
    graph <- metaclipcc.Dataset$graph
    parent.node <- metaclipcc.Dataset$parentnodename
    ref <- showIPCCvars(names.only = FALSE)
    if (!variable %in% ref$variable) stop("Invalid variable name. Use \'showIPCCvars()\' to check valid variables")
    ref <- ref[grep(pattern = paste0("^", variable, "$"), x = ref$variable),]
    DatasetSubset.nodename <- paste0("DatasetSubset.", randomName())
    descr <- paste("This step entails extracting a spatio-temporal domain that is a logical subset of the antecedent Dataset for", ref$description)
    graph <- my_add_vertices(graph,
                             name = DatasetSubset.nodename,
                             label = "DatasetSubset",
                             className = "ds:DatasetSubset",
                             attr = list("dc:description" = descr))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, parent.node),
                         getNodeIndexbyName(graph, DatasetSubset.nodename)),
                       label = paste0("ds:hadDatasetSubset"))

    # Variable  ---------------------
    var.nodename <- paste0("ds:", variable)
    graph <- my_add_vertices(graph,
                             name = var.nodename,
                             label = ref$shortname,
                             className = "ds:Variable",
                             attr = list("ds:withUnits" = ref$units,
                                         "ds:hasShortName" = ref$shortname,
                                         "ds:hasVerticalLevel" = ref$vertical))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, var.nodename)),
                       label = "ds:hasVariable")
    # TemporalResolution ---------------------
    timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
    label <- ifelse(time.res.orig == "P1M", "Monthly", "Daily")
    graph <- my_add_vertices(graph,
                             name = timeres.nodename,
                             label = label,
                             className = "ds:TemporalResolution",
                             attr = list("ds:hasTimeStep" = time.res.orig,
                                         "ds:hasCellMethod" = ref$cellfun))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, timeres.nodename)),
                       label = "ds:hasTemporalResolution")
    # SpatialExtent (Horizontal) -------------------------------------
    # Spatial Extent
    ref1 <- showIPCCdatasets(names.only = FALSE)
    if (!Dataset.name %in% ref1$name) stop("Invalid Dataset.name value. Use \'showIPCCdatasets()\' to check dataset availability and spelling")
    # Identify the dataset and initialize a new empty graph
    ref1 <- ref1[grep(Dataset.name, ref1$name),]
    # prefix <- ifelse(ipcc.region == "GlobalExtent", "ds:", "ipcc:")
    # spatextent.nodename <- paste0(prefix, ipcc.region)
    # spatextent.nodename <- paste0("SpatialExtent.", randomName())

    # reg <- showIPCCregions(names.only = FALSE)[showIPCCregions(names.only = FALSE) %>% extract2("HorizontalExtent") %>% grep(pattern = ipcc.region), ]
    # spExtent <- metaclipR.SpatialExtent(region = reg$HorizontalExtent,
    #                                     xmin = reg$xmin,
    #                                     xmax = reg$xmax,
    #                                     ymin = reg$ymin,
    #                                     ymax = reg$ymax,
    #                                     resX = reg$resX,
    #                                     resY = reg$resY)
    if (ref$realm == "atmos") {
        attr.list <- list("ds:xmin" = ref1$xmin.atmos,
                          "ds:xmax" = ref1$xmax.atmos,
                          "ds:ymin" = ref1$ymin.atmos,
                          "ds:ymax" = ref1$ymax.atmos,
                          "ds:hasHorizontalResX" = ref1$resX.atmos,
                          "ds:hasHorizontalResY" = ref1$resX.atmos)
    } else {
        # TODO: Find ocean model resolutions and include in reference table!
        attr.list <- list()
    }
    prefix <- ifelse(ref1$SimulationDomain == "GlobalDomain", "ds:", "ipcc:")
    graph <- my_add_vertices(graph,
                             name = ref1$SimulationDomain,
                             label = ref1$SimulationDomain,
                             className = paste0(prefix, ref1$SimulationDomain),
                             attr = attr.list)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, ref1$SimulationDomain)),
                       label = "ds:hasHorizontalExtent")
    # Spatial Extent (Vertical) --------------------------------------
    vextent.nodename <- paste("VerticalExtent", randomName(), sep = ".")
    graph <- my_add_vertices(graph,
                          name = vextent.nodename,
                          label = ref$vertical,
                          className = "ds:VerticalExtent",
                          attr = list("ds:hasVerticalLevel" = ref$vertical))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, vextent.nodename)),
                       label = "ds:hasVerticalExtent")
    # TemporalPeriod -----------------------------------------------

    # plot(graph)
    # season = c(12,1,2)
    # years = 1986:2005

    if (min(season) < 1 | max(season) > 12) stop("Invalid season definition. See help menu.")
    filter.month <- as.list(season)
    startYear <- years[1]
    endYear <- years[2]
    endMonth <- tail(season, 1)
    if (!identical(season, sort(season))) startYear <- startYear - 1
    if (endMonth == 12) {
        endYear <- endYear + 1
        endMonth <- 1
    }
    season.string <- month.abb[season] %>% paste(collapse = "-")
    startTime <- paste0(startYear, "-", season[1], "-01") %>% as.POSIXlt(tz = "GMT") %>%
        as.POSIXct() %>% format(format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
    endTime <- paste0(endYear, "-", endMonth, "-01") %>% as.POSIXlt(tz = "GMT") %>%
        as.POSIXct() %>% format(format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
    names(filter.month) <- paste("ds:filterMonth", 1:length(filter.month), sep = ".")
    timeper.nodename <- paste("TemporalPeriod", randomName(), sep = ".")
    graph <- my_add_vertices(graph,
                             name = timeper.nodename,
                             label = "Season",
                             className = "ds:TemporalPeriod",
                             attr = c(
                                 list("ds:season" = season.string,
                                      "prov:startedAtTime" = startTime,
                                      "prov:endedAtTime" = endTime),
                                 filter.month))
    graph <- add_edges(graph,
                        c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                          getNodeIndexbyName(graph, timeper.nodename)),
                        label = "ds:hasValidTemporalExtent")
    # # ARGUMENT - COMMAND METADATA ----------------------------------------------
    # if (!disable.command) {
    #     if ("grid" %in% names(arg.list)) arg.list <- arg.list[-grep("grid", names(arg.list))]
    #     graph <- metaclip.graph.Command(graph, package, version, fun, arg.list, DatasetSubset.nodename)
    # }
    return(list("graph" = graph, "parentnodename" = DatasetSubset.nodename))
}
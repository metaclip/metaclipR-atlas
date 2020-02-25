##     metaclipcc.Dataset Define the initial node of a METACLIP graph with data source description
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
#' @description Build a directed metadata graph describing a data source. This is usually the initial
#' step to build METACLIP graphs
#'
#' @param Dataset.name Name (label) of the Dataset. This dataset must be included in the internal
#'  lookup table, that can be accessed via \code{\link{showIPCCdatasets}}.
#' @param RectangularGrid a metaclipR object containing a ds:Rectangular Grid definition
#' @importFrom igraph make_empty_graph add_edges
#' @importFrom magrittr %>%
#' @importFrom metaclipR my_add_vertices getNodeIndexbyName
#' @export
#' @author J. Bedia

# graph <- metaclipcc.Dataset(Dataset.name = "CMIP5_CNRM-CERFACS-CNRM-CM5_historical")
# plot(graph$graph)
# graph2json(graph = graph$graph, output.file = "/tmp/cnrm.json")

metaclipcc.Dataset <- function(Dataset.name, RectangularGrid) {
    ref <- showIPCCdatasets(names.only = FALSE)
    if (!Dataset.name %in% ref$name) stop("Invalid Dataset.name value. Use \'showIPCCdatasets()\' to check dataset availability and spelling")
    if (class(RectangularGrid$graph) != "igraph") stop("Invalid RectangularGrid graph (not an 'igraph-class' object)")
    # Identify the dataset and initialize a new empty graph
    ref <- ref[grep(Dataset.name, ref$name),]
    graph <- make_empty_graph(directed = TRUE)
    # Dataset node
    if (!is.na(ref$classObs)) {
        descr <- paste("A dataset of the", ref$Project, "project containing observed reference records of the",
                       ref$classObs, "class")
        classname <- paste0(ref$vocabulary, ref$classObs)
        graph <- my_add_vertices(graph,
                                 name = Dataset.name,
                                 label = Dataset.name,
                                 className = classname,
                                 attr = list("ds:referenceURL" = ref$doi,
                                             "dc:description" = descr,
                                             "ds:withVersionTag" = ref$SoftwareVersion))
    } else {
        classname <- "ds:MultiDecadalSimulation"
        if (is.na(ref$RCM)) {
            descr <- paste("A dataset of the", ref$Project, "project containing simulations of the",
                           ref$GCM, "GCM for the",
                           ref$Experiment, "experiment")
        } else {
            descr <- paste("A dataset of the", ref$Project, "CMIP5 project containing simulations of the",
                           ref$RCM, "RCM coupled to the",
                           ref$GCM, "GCM for the",
                           ref$Experiment, "experiment")
        }
        graph <- my_add_vertices(graph,
                                 name = Dataset.name,
                                 label = Dataset.name,
                                 className = classname,
                                 attr = list("ds:referenceURL" = ref$doi,
                                             "dc:description" = descr,
                                             "ds:hasRun" = ref$Run))
    }
    # DataProvider
    graph <- my_add_vertices(graph,
                             name = "ipcc:ESGF",
                             label = "ESGF",
                             className = "ds:DataProvider")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, Dataset.name),
                         getNodeIndexbyName(graph, "ipcc:ESGF")),
                       label = "ds:hadDataProvider")
    # ModellingCenter
    ModellingCenter <- ref$ModellingCenter %>% strsplit(split = "-/-", fixed = TRUE) %>% unlist()
    for (i in 1:length(ModellingCenter)) {
        mc.nodename <- paste(ref$vocabulary, ModellingCenter[i], sep = ":")
        graph <- my_add_vertices(graph,
                                 name = mc.nodename,
                                 label = ModellingCenter[i],
                                 className = "ds:ModellingCenter")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, Dataset.name),
                             getNodeIndexbyName(graph, mc.nodename)),
                           label = "ds:hadModellingCenter")
    }
    # Project
    Project <- ref$Project
    project.nodename <- paste(ref$vocabulary, Project, sep = ":")
    graph <- my_add_vertices(graph,
                             name = project.nodename,
                             label = Project,
                             className = "ds:Project")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, Dataset.name),
                         getNodeIndexbyName(graph, project.nodename)),
                       label = "ds:hadProject")
    # Experiment
    Experiment <- ref$Experiment
    if (!is.na(Experiment)) {
        exp.nodename <- paste0("ipcc:", Experiment)
        graph <- my_add_vertices(graph,
                                 name = exp.nodename,
                                 label = Experiment,
                                 className = "ds:Experiment")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, Dataset.name),
                             getNodeIndexbyName(graph, exp.nodename)),
                           label = "ds:hadExperiment")
    }
    ## GCM simulations
    GCM <- ref$GCM
    if (!is.na(GCM)) {
        gcm.nodename <- paste0("ipcc:", GCM)
        RCM <- ref$RCM
        graph <- my_add_vertices(graph,
                                 name = gcm.nodename,
                                 label = GCM,
                                 className = "ds:GCM")
        if (is.na(RCM)) {
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, Dataset.name),
                                 getNodeIndexbyName(graph, gcm.nodename)),
                               label = "ds:hadSimulationModel")
        } else {## RCM simulations
            rcm.nodename <- paste0("ipcc:", RCM)
            graph <- my_add_vertices(graph,
                                     name = rcm.nodename,
                                     label = RCM,
                                     className = "ds:RCM",
                                     attr = list("ds:withSimulationDomain" = ref$SimulationDomain,
                                                 "ds:withVersionTag" = ref$SoftwareVersion))
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, Dataset.name),
                                 getNodeIndexbyName(graph, rcm.nodename)),
                               label = "ds:hadSimulationModel")
            # Driving GCM
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, rcm.nodename),
                                 getNodeIndexbyName(graph, gcm.nodename)),
                               label = "ds:hadDrivingGCM")
        }
    }
    ## Model Grid
    graph <- my_union_graph(graph, RectangularGrid[["graph"]])
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, Dataset.name),
                         getNodeIndexbyName(graph, RectangularGrid[["parentnodename"]])),
                       label = "ds:hasRectangularGrid")
    return(list("graph" = graph, "parentnodename" = Dataset.name))
}

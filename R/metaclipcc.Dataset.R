##     metaclipcc. Dataset Define the initial node of a METACLIP graph with data source description
##
##     Copyright (C) 2019 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Directed metadata graph construction for IPCC Atalas product data sources
#' @description Build a directed metadata graph describing a data source. This is usually the initial 
#' step to build METACLIP graphs
#' 
#' @param Dataset.name Name (label) of the Dataset. There is a number of already known Datasets, pertaining to the User Data Gateway 
#' Public datasets of the climate4R Framework (see References). Type \code{showUDGDatasources()} for details.
#'  If the argument corresponds to any of these named instances, the associated provenance information will be automatically recorded,
#'  and all other arguments can be omitted.
#' @export
#' @importFrom igraph make_empty_graph add_edges 
#' @importFrom magrittr %>% 
#' @importFrom metaclipR my_add_vertices getNodeIndexbyName
#' @author J. Bedia

metaclipcc.Dataset <- function(Dataset.name = NULL) {
    ref <- showIPCCdatasets(names.only = FALSE)
    if (!Dataset.name %in% ref$names) stop("Invalid Dataset.name value. Use \'showIPCCdatasets()\' to check dataset availability and spelling")
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
    # Dataset.name = "CMIP5_CNRM-CERFACS-CNRM-CM5_historical"
    # ### ARREGLAR
    # knownClassIndividuals("Project", vocabulary = "ipcc_terms")
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
    # Identify the dataset and initialize a new empty graph 
    ref <- ref[grep(Dataset.name, ref$name),]
    graph <- make_empty_graph(directed = TRUE)
    # Dataset node
    graph <- my_add_vertices(graph,
                             name = Dataset.name,
                             label = Dataset.name,
                             className = "ds:MultiDecadalSimulation")
    # DataProvider
    graph <- my_add_vertices(graph,
                             name = "ESGF",
                             label = "ESGF",
                             className = "ipcc:ESGF")
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, Dataset.name),
                         getNodeIndexbyName(graph, "ESGF")),
                       label = "ds:hadDataProvider")
    # ModellingCenter 
    ModellingCenter <- ref$ModellingCenter %>% strsplit(split = "-/-", fixed = TRUE) %>% unlist()
    for (i in 1:length(ModellingCenter)) {
        graph <- my_add_vertices(graph,
                                 name = ModellingCenter[i],
                                 label = ModellingCenter[i],
                                 className = "ds:ModellingCenter")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, Dataset.name),
                             getNodeIndexbyName(graph, ModellingCenter[i])),
                           label = "ds:hadModellingCenter")
    }
    # Project
    Project <- ref$Project
    graph <- my_add_vertices(graph,
                             name = Project,
                             label = Project,
                             className = "ds:Project")
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, Dataset.name),
                         getNodeIndexbyName(graph, Project)),
                       label = "ds:hadProject")
    # Experiment
    Experiment <- ref$Experiment
    graph <- my_add_vertices(graph,
                             name = Experiment,
                             label = Experiment,
                             className = "ds:Experiment")
    graph <- add_edges(graph, 
                       c(getNodeIndexbyName(graph, Dataset.name),
                         getNodeIndexbyName(graph, Experiment)),
                       label = "ds:hadExperiment")
    ## GCM simulations
    GCM <- ref$GCM
    RCM <- ref$RCM
    graph <- my_add_vertices(graph,
                             name = GCM,
                             label = GCM,
                             className = "ds:GCM",
                             attr = list("ds:hasRun" = ref$Run))
    if (is.na(RCM)) {
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, Dataset.name),
                             getNodeIndexbyName(graph, GCM)),
                           label = "ds:hadSimulationModel")
    } else {## RCM simulations
        graph <- my_add_vertices(graph,
                                 name = RCM,
                                 label = RCM,
                                 className = "ds:RCM",
                                 attr = list("ds:withSimulationDomain" = ref$SimulationDomain,
                                             "ds:withVersionTag" = ref$SoftwareVersion))
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, Dataset.name),
                             getNodeIndexbyName(graph, RCM)),
                           label = "ds:hadSimulationModel")
        # Driving GCM
        graph <- add_edges(graph, 
                           c(getNodeIndexbyName(graph, RCM),
                             getNodeIndexbyName(graph, GCM)),
                           label = "ds:hadDrivingGCM")
    }
    return(list("graph" = graph, "parentnodename" = Dataset.name))
}

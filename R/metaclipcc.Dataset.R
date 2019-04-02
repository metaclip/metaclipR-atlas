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
#' @importFrom igraph make_empty_graph add_vertices add_edges 
#' @author J. Bedia



# datasets <- read.csv("inst/gcm_table.csv", stringsAsFactors = FALSE)

metaclipcc.Dataset <- function(Dataset.name = NULL) {
    ref <- showIPCCdatasets(names.only = FALSE)
    if (!Dataset.name %in% ref$names) stop("Invalid Dataset.name value. Use \'showIPCCdatasets()\' to check dataset availability and spelling")
    
    Dataset.name = "CMIP5_IPSL-CM5A-MR_historical"
    
    
    ind <- grep(Dataset.name, ref$name)
    
    
    # ?isKnow
    # 
    # know
    # 
    # if (isKnownDatasource) {
    #     message("The Dataset is in the internal reference table: all datasource metadata will be automatically appended")
    #     metadata <- ref[grep(Dataset.name, ref$name), ]
    #     Dataset.subclass <- metadata$DatasetSubclass
    #     ModellingCenter <- metadata$ModellingCenter %>% strsplit(split = ";") %>% unlist()
    #     Project <- metadata$Project
    #     if (DataProvider == "UDG") DataProvider.URL <- metadata$url
    #     RCM <- metadata$RCM
    #     GCM <- metadata$GCM
    #     Run <- metadata$Run
    # }
    # ref <- NULL
    # # Dataset Subclass definition
    # isKnownDataset <- ifelse(Dataset.name %in% suppressMessages(knownClassIndividuals("Dataset")), TRUE, FALSE)
    # if (isKnownDataset) Dataset.subclass <- getIndividualClass(Dataset.name)
    # Dataset.nodename <- ifelse(isKnownDataset, paste0("ds:", Dataset.name), paste0("Dataset.", randomName()))
    # if (!is.character(Dataset.subclass)) stop("A valid 'Dataset.subclass' value is required", call. = FALSE)
    # Dataset.subclass <- match.arg(Dataset.subclass, choices = c("MultiDecadalSimulation",
    #                                                             "ObservationalDataset",
    #                                                             "Reanalysis",
    #                                                             "SeasonalHindcast",
    #                                                             "SeasonalOperationalForecast",
    #                                                             "ShortRangeForecast"))
    # graph <- make_empty_graph(directed = TRUE)
    # # Dataset node ------------------
    # graph <- my_add_vertices(graph,
    #                          nv = 1,
    #                          name = Dataset.nodename,
    #                          label = Dataset.name,
    #                          className = paste0("ds:", Dataset.subclass))
    # # DataProvider node (OPTIONAL) -------------
    # # List of instantiable knownDataProviders
    # if (is.character(DataProvider)) {
    #     if (length(DataProvider) > 1) stop("Only one DataProvider per Dataset is allowed", call. = FALSE)
    #     isKnownDataProvider <- ifelse(DataProvider %in% suppressMessages((knownClassIndividuals("DataProvider"))) | 
    #                                       DataProvider %in% suppressMessages((knownClassIndividuals("ModellingCenter"))), TRUE, FALSE)
    #     DataProvider.nodename <- ifelse(isKnownDataProvider, paste0("ds:", DataProvider), paste0("DataProvider.", randomName())) 
    #     graph <- my_add_vertices(graph,
    #                              name = DataProvider.nodename,
    #                              label = DataProvider,
    #                              className = "ds:DataProvider",
    #                              attr = list("ds:hasMainURL" = DataProvider.URL))
    #     graph <- add_edges(graph, 
    #                        c(getNodeIndexbyName(graph, Dataset.nodename),
    #                          getNodeIndexbyName(graph, DataProvider.nodename)),
    #                        label = "ds:hadDataProvider")
    # }
    # # ModellingCenter OPTIONAL --------------------------------
    # if (is.character(ModellingCenter)) {
    #     for (i in 1:length(ModellingCenter)) {
    #         isKnownModellingCenter <- ifelse(ModellingCenter[i] %in% suppressMessages(knownClassIndividuals("ModellingCenter")), TRUE, FALSE)
    #         ModellingCenter.nodename <- ifelse(isKnownModellingCenter, paste0("ds:", ModellingCenter[i]), paste0("ModellingCenter.", randomName())) 
    #         graph <- my_add_vertices(graph,
    #                                  nv = 1,
    #                                  name = ModellingCenter.nodename,
    #                                  label = ModellingCenter[i],
    #                                  className = "ds:ModellingCenter")
    #         graph <- add_edges(graph, 
    #                            c(getNodeIndexbyName(graph, Dataset.nodename),
    #                              getNodeIndexbyName(graph, ModellingCenter.nodename)),
    #                            label = "ds:hadModellingCenter")
    #     }
    # }
    # # Project OPTIONAL ------------------------------------
    # if (!is.null(Project)) {
    #     isKnownProject <- ifelse(Project %in% suppressMessages(knownClassIndividuals("Project")), TRUE, FALSE)
    #     Project.nodename <- ifelse(isKnownProject, paste0("ds:", Project), paste0("Project.", randomName())) 
    #     graph <- my_add_vertices(graph,
    #                              nv = 1,
    #                              name = Project.nodename,
    #                              label = Project,
    #                              className = "ds:Project")
    #     graph <- add_edges(graph, 
    #                        c(getNodeIndexbyName(graph, Dataset.nodename),
    #                          getNodeIndexbyName(graph, Project.nodename)),
    #                        label = "ds:hadProject")
    # }
    # # Simulation model --------------------------------------
    # rcmdata <- FALSE
    # if (isTRUE(suppressWarnings(is.na(RCM)))) RCM <- NULL
    # if (!is.null(RCM)) {
    #     rcmdata <- TRUE
    #     isKnownRCM <- ifelse(RCM %in% knownClassIndividuals("RCM"), TRUE, FALSE)
    #     RCM.nodename <- ifelse(isKnownRCM, paste0("ds:", RCM), paste0("RCM.", randomName()))
    #     graph <- my_add_vertices(graph,
    #                              nv = 1,
    #                              name = RCM.nodename,
    #                              label = RCM,
    #                              className = "ds:RCM",
    #                              attr = list("ds:hasRun" = Run))
    #     graph <- add_edges(graph, 
    #                        c(getNodeIndexbyName(graph, Dataset.nodename),
    #                          getNodeIndexbyName(graph, RCM.nodename)),
    #                        label = "ds:hadSimulationModel")
    # }
    # if (isTRUE(suppressWarnings(is.na(GCM)))) GCM <- NULL
    # if (!is.null(GCM)) {
    #     isKnownGCM <- ifelse(GCM %in% knownClassIndividuals("GCM"), TRUE, FALSE)
    #     GCM.nodename <- ifelse(isKnownGCM, paste0("ds:", GCM), paste0("GCM.", randomName()))
    #     if (rcmdata) Run <- NA
    #     graph <- my_add_vertices(graph,
    #                              nv = 1,
    #                              name = GCM.nodename,
    #                              label = GCM,
    #                              className = "ds:GCM",
    #                              attr = list("ds:hasRun" = Run))
    #     graph <- if (rcmdata) {
    #         add_edges(graph, 
    #                   c(getNodeIndexbyName(graph, RCM.nodename),
    #                     getNodeIndexbyName(graph, GCM.nodename)),
    #                   label = "ds:hadDrivingGCM")
    #     } else {
    #         op <- ifelse(grepl("^Seasonal", Dataset.subclass), "ds:hadSeasonalForecastingSystem", "ds:hadSimulationModel")
    #         add_edges(graph, 
    #                   c(getNodeIndexbyName(graph, Dataset.nodename),
    #                     getNodeIndexbyName(graph, GCM.nodename)),
    #                   label = op)
    #     }
    # }
    # return(list("graph" = graph, "parentnodename" = Dataset.nodename))
}

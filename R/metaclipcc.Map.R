##     metaclipcc.map METACLIP description of an Atlas map product
##
##     Copyright (C) 2021 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Directed metadata graph construction for AR6 Atlas Map products
#' @description Build a directed metadata graph describing a Map product of the AR6 Interactive Atlas
#' @param project Project. Unused so far. Default to \code{"CMIP5"}.
#' @param variable Code of the input ECV (it can be omitted if \code{climate.index} is indicated).
#'  Current accepted values are restricted to \code{"tas", "meanpr", "TX", "TN", "prsn", "wind"}
#'  and \code{"siconc", "ph", "tos"} for oceanic variables (CMIP6 only).
#' @param climate.index If the map is for a climate index, name of the index. Otherwise NULL (the default). Currently accepted values are restricted to
#' the set of indices to be included in the Atlas, namely:
#' \code{"TXx", "TNn", "Rx1day", "Rx5day", "spi6", "CDD", "tx35", "tx40", "cd", "hdd", "fd"}, as well as the
#' bias adjusted versions of \code{"tx35isimip", "tx40isimip", "fdisimip"}.
#' @param delta Logical. Is it a delta map?. The type of delta displayed can be either \code{"absolute"} or \code{"relative"}.
#  Since metaclipcc v1.0.0 this parameter is internally adjusted as a function of the input variable/index.
#' @param experiment Experiment results displayed in the map. Accepted values are restricted to \code{"historical", "rcp26", "rcp45", "rcp85"},
#' for CORDEX and CMIP5 products, and \code{"historical", "ssp126", "ssp245", "ssp370", "ssp460" and "ssp585"} for CMIP6 products.
#' @param baseline Character string indicating the \code{"start-end"} years of the baseline (historical) period. Accepted values are:
#' \code{"1981-2010"} and \code{"1961-1990"} (WMO standard periods), \code{"1986-2005"} (AR5 period), \code{"1995-2014"} (AR6 period) and \code{"1850-1900"}
#'  (Preindustrial). Internally, there is a tricky part here in some cases, see Details.
#' @param future.period future period. Default to \code{NULL}, for historical maps (i.e., period defined by the \code{baseline} argument). Otherwise, a character string
#'  indicating either the \code{"start-end"} years of the future period or a (GCM-specific) warming level. Current options include the standard AR5 future time slices
#'  for near term, \code{"2021-2040"}, medium term \code{"2041-2060"} and long term \code{"2081-2100"}, and the global
#'  warming levels of +1.5 degC \code{"1.5"}, +2 \code{"2"}, +3 \code{"3"} and +4 \code{"4"}.
#' @param season season. Integer vector of correlative months, in ascending order, encompassing the target season.
#' @param bias.adj.method Default to \code{NULL} and unused. If the map displays a bias-corrected product, a character string idetifying the method. Current accepted values a re \code{"EQM"},
#' for the standard VALUE empirical quantile mapping method. NOTE: since metaclipcc v1.0.0 the parameter is automatically
#' set to "ISIMIP3" when needed as a function of the index name.
#' @param ref.obs.dataset Default to \code{NULL}, and unused unless a \code{bias.adj.method} has been specified.
#'  This is the reference observational dataset to perform the correction. This is an individual that must be defined in the datasource vocabulary,
#'   belonging to either classes \code{ds:ObservationalDataset} or \code{ds:Reanalysis}. Currently accepted values are \code{"W5E5"} and \code{"EWEMBI"}.
#'   Note that the individual instances of the observational reference are assumed to be described in the datasource vocabulary.
#'   NOTE: since metaclipcc v0.3.0 the parameter is
#' set to "W5E5" when needed as a function of the index name.
#' @param proj Map projection string. Accepted values are \code{"Robin"}, \code{"Arctic"}, \code{"Antarctic"} and \code{"Pacific"}
#' for Robinson and WGS84 Arctic/Antarctic Polar stereographic, and Robinson Pacific-centric projections respectively.
#' @param map.bbox Optional. numeric vector of length 4, containing, in this order the \code{c(xmin, ymin, xmax, ymax)} coordinates of the target area
#' zoomed in by the user. If missing, then the HorizontalExtent associated to the \code{project} is assumed.
#' @param uncertainty Uncertainty layer characteristics. Describes different hatched patterns
#' of the output graphical product, depending of the user's choice of visualization. Possible values are \code{NULL}
#' (default), indicating no hatching at all, or \code{"simple"} or \code{"advanced"}.
#' @param test.mode For internal use only. When the test mode is on, only the first two models are used.
#'
#' @details
#'
#' \strong{Baseline period definition}
#'
#' Two of the baseline periods considered (WMO, 1981-2010 and AR6, 1995-2014) go beyond the temporal extent of the historical experiment simulations in AR5, ending in 2005.
#' In this case, the strategy followed in the different IPCC reports is to fill the missing period between 2006 and the end of the baseline with the years of the future
#' simulation used in each case. For example, to compute a RCP 8.5 delta using the WMO baseline, the baseline data for the period 2006-2010 will be extracted from the RCP85
#' simulation, and then concatenated with the 1981-2005 period of the historical simulation in order to complete the baseline period.
#'
#' @author J. Bedia
#'
#' @import metaclipR
#' @importFrom magrittr %>% extract2
#' @importFrom igraph V
#'
#' @export


#TODO:
## Define simulation domains (ds:SpatialExtent individual instances) for CORDEX projects
## Deal with grid info for oceanic variables
## Legend values
## Review hatching criteria


# ## Test area
# project = "CMIP6"
# variable = "tos"
# climate.index = NULL
# delta = TRUE
# experiment = "ssp126" #"ssp126"
# baseline = "1995-2014"
# future.period = "1.5"
# season = 1:12
# bias.adj.method = NULL #"ISIMIP3" # "EQM"
# ref.obs.dataset = NULL #"W5E5" # "EWEMBI"
# proj = "Robin"
# map.bbox = NULL
# test.mode = TRUE
# uncertainty = "simple"
# #
#
# a <- metaclipcc.Map(project = project,
#                     variable = variable,
#                     climate.index = climate.index,
#                     delta,
#                     experiment,
#                     baseline,
#                     future.period,
#                     season,
#                     bias.adj.method = bias.adj.method,
#                     ref.obs.dataset = ref.obs.dataset,
#                     proj = proj,
#                     map.bbox = map.bbox,
#                     test.mode = test.mode)
# a
#
# graph2json(a$graph, output.file = "ignore/prueba2.json")

# End test area

metaclipcc.Map <- function(project = "CMIP5",
                           variable = NULL,
                           climate.index = NULL,
                           delta = FALSE,
                           experiment,
                           baseline,
                           future.period = NULL,
                           season,
                           bias.adj.method = NULL,
                           ref.obs.dataset = NULL,
                           proj,
                           map.bbox = NULL,
                           uncertainty = NULL,
                           test.mode = FALSE) {

    # Fixed parameters *******
    ref.period <- c(1980, 2005) # Used as training period for bias correction
    # time.res.orig <- "P1D"
    # ***********************

    project <- match.arg(project, choices = c("CMIP5", "CMIP6",
                                              "CORDEX-AFR", "CORDEX-ARC", "CORDEX-AUS",
                                              "CORDEX-CAM", "CORDEX-EAS", "CORDEX-NAM",
                                              "CORDEX-SAM", "CORDEX-SEA", "CORDEX-WAS"))
    if (!is.null(variable)) {
        variable <- match.arg(variable, choices = c("tas", "meanpr", "TX", "TN", "prsn", "wind",
                                                    "tos", "ph", "siconc"))
        if (!is.null(climate.index)) {
            climate.index <- NULL
            warning("Variable ", variable, " was first indicated. The \'climate.index\' argument was set to NULL")
        }
    } else {
        if (is.null(climate.index)) stop("Either a variable or a climate index must be supplied")
    }
    if (!is.null(climate.index)) {
        climate.index <- match.arg(climate.index, choices = c("TXx", "TNn",
                                                              "Rx1day", "Rx5day",
                                                              "spi6", "CDD",
                                                              "tx35", "tx40",
                                                              "tx35isimip", "tx40isimip",
                                                              "cd", "hdd",
                                                              "fd", "fdisimip"))
        if (grepl("isimip$", climate.index)) {
            bias.adj.method <- "ISIMIP3"
            ref.obs.dataset <- "W5E5"
        }
    }

    stopifnot(is.logical(delta))

    experiment <- if (project == "CMIP6") {
        match.arg(experiment, choices = c("historical", "ssp126", "ssp245", "ssp370", "ssp460", "ssp585"))
    } else {
        match.arg(experiment, choices = c("historical", "rcp26", "rcp45", "rcp85"))
    }

    if (experiment == "historical") {
        if (isTRUE(delta)) {
            delta <- FALSE
            message("NOTE: \'delta\' argument ignored for the \'experiment=\"historical\"\' setting")
        }
    }

    baseline <- match.arg(baseline, choices = c("1981-2010",
                                                "1961-1990",
                                                "1986-2005",
                                                "1995-2014",
                                                "1850-1900"))

    if (project == "CMIP6") {

        hist.period <- strsplit(baseline, "-") %>% unlist() %>% as.integer()
        fill.period <- NULL

    } else {

        hist.period <- switch(baseline,
                              "1981-2010" = c(1981, 2005),
                              "1961-1990" = c(1961, 1990),
                              "1986-2005" = c(1986, 2005),
                              "1995-2014" = c(1995, 2005),
                              "1850-1900" = c(1850, 1900))

        fill.period <- switch(baseline,
                              "1981-2010" = c(2006, 2010),
                              "1961-1990" = NULL,
                              "1986-2005" = NULL,
                              "1995-2014" = c(2006, 2014),
                              "1850-1900" = NULL)
    }

    if (experiment != "historical" & is.null(future.period)) stop("future.period argument is missing, with no default for ", experiment, " experiment")
    if (!is.null(future.period)) {
        future.period <- match.arg(future.period, choices = c("2021-2040", "2041-2060", "2081-2100",
                                                              "1.5", "2", "3", "4"))
    }

    proj <- match.arg(proj, choices = c("Robin", "Arctic", "Antarctic", "Pacific"))

    if (!is.null(map.bbox)) stopifnot(length(map.bbox) == 4L)

    if (!delta) uncertainty <- NULL
    if (!is.null(uncertainty)) {
        uncertainty <- match.arg(uncertainty, choices = c("simple", "advanced"))
    }

    ref.project <- showIPCCdatasets(names.only = FALSE)[showIPCCdatasets(names.only = FALSE) %>% extract2("Project") %>% grep(pattern = project), ]
    ipcc.region <- ref.project$SimulationDomain %>% unique() ## overwrite later if CORDEX
    # cordex.region <- ref.project$region_name %>% unique()

    ## Reference grids ---------------------------------------------------------
    if (project == "CMIP5") {
        resX <- resY <- 2
        descr <- paste("This is the reference grid used in all CMIP5 map products of",
                       resX, "x", resY, "degree resolution covering the whole globe")
        gridfile.url <- "https://doi.org/10.5281/zenodo.3691645"
        reference.grid <- metaclipR.RectangularGrid(resX = resX,
                                                    resY = resY,
                                                    xmin = -179.25,
                                                    xmax = 179.25,
                                                    ymin = -89.25,
                                                    ymax = 89.25,
                                                    dc.description = descr,
                                                    ref.URL = gridfile.url)
    } else if (project == "CMIP6") {
        resX <- resY <- 1
        descr <- paste("This is the reference grid used in all CMIP6 map products of",
                       resX, "x", resY, "degree resolution covering the whole globe")
        gridfile.url <- "https://doi.org/10.5281/zenodo.3691645"
        reference.grid <- metaclipR.RectangularGrid(resX = resX,
                                                    resY = resY,
                                                    xmin = -179.5,
                                                    xmax = 179.5,
                                                    ymin = -89.5,
                                                    ymax = 89.5,
                                                    dc.description = descr,
                                                    ref.URL = gridfile.url)
    } else {
        resX <- resY <- 0.5
        descr <- paste0("This is the ", project,
                        " reference grid of ", resX," x ", resY,
                        " degree resolution used in all Atlas CORDEX products")
        reference.grid <- metaclipR.RectangularGrid(resX = resX,
                                                    resY = resY,
                                                    xmin = -179.75,
                                                    xmax = 179.75,
                                                    ymin = -89.75,
                                                    ymax = -89.75,
                                                    dc.description = descr)
    }

    ## Reference spatial extents -----------------------------------------------

    reference.extent <- if (grepl("^CORDEX-", project)) {
        metaclipcc.HorizontalExtent(region = NULL,
                                    xmin = ref.project$xmin.atmos,
                                    xmax = ref.project$xmax.atmos,
                                    ymin = ref.project$ymin.atmos,
                                    ymax = ref.project$ymax.atmos,
                                    dc.description = paste("Spatial Extent of the",
                                                           project,
                                                           "simulation domain"))
    } else {
        metaclipcc.HorizontalExtent(region = unique(ref.project$SimulationDomain))
    }

    ## Observational dataset ---------------------------------------------------
    # The observational dataset has a native resolution (obs.spextent)
    # that is then interpolated onto the reference project grid

    if (!is.null(bias.adj.method)) {
        obs.meta <- showIPCCdatasets(names.only = FALSE)[grep(ref.obs.dataset,
                                                              showIPCCdatasets(),
                                                              fixed = TRUE), ]

        obs.spextent <- metaclipcc.HorizontalExtent(region = obs.meta$SimulationDomain)

        descr <- paste("This is the original native grid of", obs.meta$resX.atmos, "x" ,
                       obs.meta$resX.atmos, "of the",
                       ref.obs.dataset, "observational gridded dataset")

        obs.grid <- metaclipR.RectangularGrid(resX = obs.meta$resX.atmos,
                                              resY = obs.meta$resY.atmos,
                                              xmin = obs.meta$xmin.atmos,
                                              xmax = obs.meta$xmax.atmos,
                                              ymin = obs.meta$ymin.atmos,
                                              ymax = obs.meta$ymax.atmos,
                                              dc.description = descr)

        graph.o <- metaclipcc.Dataset(ref.obs.dataset, RectangularGrid = obs.grid, DataProvider = "CDS")
    }

    ## Historical scenarios ----------------------------------------------------

    ls <- showIPCCdatasets(names.only = TRUE)
    hist.list <- ls[which(grepl(paste0("^", project, ".*historical"), ls))]

    if (experiment != "historical") {

        ## Need to filter models, because not all models have all experiments available (e.g. RCP 2.6 is lacking in some models)
        rcp.list <- ls[which(grepl(paste0("^", project, ".*", experiment), ls))]
        hist.list <- gsub(experiment, "historical", rcp.list)
        if (!identical(length(rcp.list), length(hist.list))) {
            stop("historical and future dataset numbers differ")
        }
    }

    pat <- if (project == "CMIP6") {
        "ssp585"
    } else {
        "rcp85"
    }

    rcp85.list <- gsub("historical", pat, hist.list) # For filling historical gap # All models have rcp85 available
    aux <- showIPCCdatasets(names.only = FALSE)

    ## ECV filtering -----------------------------------------------------------

    if (!is.null(climate.index)) {
        ref.vars <- showIPCCvars(names.only = FALSE)[grep(paste0("^", climate.index, "$"), showIPCCvars()), ]
        time.res.orig <- ref.vars$time_step
        vars <- strsplit(ref.vars$inputECV, split = ",") %>% unlist()
    } else {
        ref.vars <- showIPCCvars(names.only = FALSE)[grep(paste0("^", variable, "$"), showIPCCvars()), ]
        time.res.orig <- ref.vars$time_step
        vars <- variable
    }

    ## Observational data ------------------------------------------------------
    if (!is.null(bias.adj.method)) {
        obs.var.graph.list <- lapply(1:length(vars), function(i) {
            graph.o <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph.o,
                                                Dataset.name = ref.obs.dataset,
                                                time.res.orig = time.res.orig,
                                                ipcc.region = obs.spextent,
                                                variable = vars[i],
                                                season = season,
                                                years = ref.period)
            descr <- paste0("The " , ref.obs.dataset, " gridded observations data (",
                            obs.meta$resX.atmos, "x", obs.meta$resY.atmos,
                            " degree resolution) are re-gridded onto the ",
                            resX, "x", resY  ," regular grid of ", project)
            graph.o <- metaclipcc.Regridding(graph = graph.o,
                                             RefSpatialExtent = reference.extent,
                                             RefRectangularGrid = reference.grid,
                                             InterpolationMethod = "bilinear",
                                             dc.description = descr)
            return(graph.o)
        })
    }

    ## EMPIEZA BUCLE EN MODELOS ------------------------------------------------

    ## De este lista se tiran:
    ### 1. modelos que no tienen la target variable (en histórico OR rcp)
    #### 1.1. En el caso de índices, modelos en los que falta algún ECV
    ### 2. modelos que no alcanzan el warming level especificado
    ### 3. modelos de los que no hay dato

    iter <- if (isTRUE(test.mode)) {
        1:2
    } else {
        1:length(hist.list)
    }
    graph.list <- list()
    for (x in iter) {
        ref.model <- aux[grep(hist.list[x], aux$name, ignore.case = TRUE),]
        model.name <- ifelse(grepl("CORDEX-", project), ref.model$RCM, ref.model$GCM)
        if (grepl("^CORDEX-", project)) {
            message("[", Sys.time(), "] Processing ", ref.model$GCM, "-", ref.model$RCM, " model data")
        } else {
            message("[", Sys.time(), "] Processing ", model.name, "_", ref.model$Run, " model data")
        }

        # Domains of 44 and 22 are mixed in the CORDEX ensemble maps
        if (grepl("CORDEX-", project)) ipcc.region <- ref.model$SimulationDomain

        ### Filter missing ECV / Climate Index ---------------------------------------

        if (!is.null(climate.index)) {
            if (ref.model[[climate.index]] != 1) next
        } else {
            if (ref.model[[variable]] != 1) next
        }

        ### Filter missing RCPs OR not reached GWLs ----------------------------

        if (experiment != "historical") {
            pr <- project
            if (grepl("^CORDEX-", project)) pr <- "CMIP5"
            years <- metaclipcc.getFuturePeriod(project = pr,
                                                model = ref.model$GCM,
                                                run = ref.model$Run,
                                                future.period = future.period,
                                                rcp = experiment)
            if (anyNA(years)) next
        }

        ### GCM grid -----------------------------------------------------------

        if (variable %in% c("tos", "ph", "siconc")) {
            descr <- paste("This is the native grid of the ocean model in the", model.name, "simulations")
            gcm.grid <-metaclipcc.OceanGrid(dc.description = descr)
        } else {
            descr <- paste("This is the native grid of", ref.model$resX.atmos,
                           "x", ref.model$resY.atmos, "of the atmospheric variables in the",
                           model.name, "simulations")
            gcm.grid <- metaclipR.RectangularGrid(resX = ref.model$resX.atmos,
                                                  resY = ref.model$resY.atmos,
                                                  xmin = ref.model$xmin.atmos,
                                                  xmax = ref.model$xmax.atmos,
                                                  ymin = ref.model$ymin.atmos,
                                                  ymax = ref.model$ymax.atmos,
                                                  dc.description = descr)
        }

        ## EMPIEZA BUCLE EN VARIABLES ------------------------------------------

        var.graph.list <- lapply(1:length(vars), function(i) {

            ## Historical simulation data --------------------------------------

            graph.hist <- metaclipcc.Dataset(hist.list[x], RectangularGrid = gcm.grid)

            graph.h <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph.hist,
                                                Dataset.name = hist.list[x],
                                                time.res.orig = time.res.orig,
                                                ipcc.region = ipcc.region,
                                                variable = vars[i],
                                                season = season,
                                                years = hist.period)

            ## Filling the gap in historical period with RCP --------------------

            if (!is.null(fill.period)) {
                ref.dataset <- if (experiment == "historical") {
                    aux[grep(rcp85.list[x], aux$name),]
                } else {
                    aux[grep(rcp.list[x], aux$name),]
                }
                graph.rcp <- metaclipcc.Dataset(ref.dataset$name, RectangularGrid = gcm.grid)
                graph2 <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph.rcp,
                                                   Dataset.name = ref.dataset$name,
                                                   variable = vars[i],
                                                   ipcc.region = ipcc.region,
                                                   season = season,
                                                   years = fill.period)

                descr <- paste0("The subperiod ",
                                paste(fill.period[1], fill.period[2], sep = "-"),
                                ", missing from the Historical experiment (1850-2005), is filled with the data from the ",
                                ref.dataset$Experiment, " simulation of the model to complete the baseline period requested (", baseline, ")")
                graph.h <- metaclipR.Binding(graph.list = list(graph.h, graph2),
                                             dim.along = "time",
                                             dc.description = descr)
            }

            ## Bias correction of historical data ------------------------------

            if (!is.null(bias.adj.method)) {

                ## Reference training period for bias correction ---------------

                graph.train <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph.hist,
                                                        ipcc.region = ipcc.region,
                                                        Dataset.name = hist.list[x],
                                                        time.res.orig = time.res.orig,
                                                        variable = vars[i],
                                                        season = season,
                                                        years = ref.period)

                descr <- paste0("In this step the historical time slice is bias-adjusted, using the reference period ",
                                paste(ref.period, collapse = "-"),
                                " as training data, and the ",
                                ref.obs.dataset, " observation data as predictand")

                graph.h <- metaclipcc.BiasCorrection(graph = graph.h,
                                                     TrainingGraph = graph.train,
                                                     ReferenceGraph = obs.var.graph.list[[i]],
                                                     ReferenceGraphSpatialExtent = reference.extent,
                                                     ReferenceGraphRectangularGrid = reference.grid,
                                                     BC.method = bias.adj.method,
                                                     dc.description = descr)
            }

            ## Future simulation data ------------------------------------------

            # Check if the future dataset node already exists
            # (It is TRUE when it has been previously defined for filling the historical gap period)
            graph.r <- NULL
            if (experiment != "historical") {
                rcp.nodename <- paste("ipcc", rcp.list[x], sep = ":")
                is.rcp.already.used <- any(igraph::V(graph.h$graph)$name == rcp.nodename)
                if (!is.rcp.already.used) {
                    graph.rcp <- metaclipcc.Dataset(rcp.list[x], RectangularGrid = gcm.grid)

                }
                graph.r <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph.rcp,
                                                    Dataset.name = rcp.list[x],
                                                    time.res.orig = time.res.orig,
                                                    ipcc.region = ipcc.region,
                                                    variable = vars[i],
                                                    season = season,
                                                    years = years)

                ## Bias correction of future time slice ------------------------

                if (!is.null(bias.adj.method)) {
                    descr <- paste0("In this step the future time slice is bias-adjusted, using the historical experiment simulation as training data, and the ",
                                    ref.obs.dataset, " observation data as predictand")

                    graph.r <- metaclipcc.BiasCorrection(graph = graph.r,
                                                         ReferenceGraphSpatialExtent = reference.extent,
                                                         ReferenceGraphRectangularGrid = reference.grid,
                                                         TrainingGraph = graph.train,
                                                         ReferenceGraph = obs.var.graph.list[[i]],
                                                         dc.description = descr,
                                                         BC.method = bias.adj.method)
                }
            }
            return(list("historical" = graph.h, "future" = graph.r))
        })
        hist.graph.list <- lapply(1:length(var.graph.list), function(j) {
            var.graph.list[[j]][["historical"]]
        })
        names(hist.graph.list) <- vars
        if (!is.null(var.graph.list[[1]][["future"]])) {
            fut.graph.list <- lapply(1:length(var.graph.list), function(j) {
                var.graph.list[[j]][["future"]]
            })
            names(fut.graph.list) <- vars
        }

        ## Climate Index calculation -------------------------------------------

        if (!is.null(climate.index)) {##CI
            graph.h <- metaclipcc.ClimateIndex(graph.list = hist.graph.list,
                                               index.code = climate.index)
            if (experiment != "historical") {
                graph.r <- metaclipcc.ClimateIndex(graph.list = fut.graph.list,
                                                   index.code = climate.index)
            }
        } else {## ECV
            graph.h <- var.graph.list[[1]][["historical"]]
            graph.r <- var.graph.list[[1]][["future"]]
        }

        ## Monthly aggregation -------------------------------------------------
        # Monthly aggregation is done on all ECVs, and some sub-monthly indices

        if (ref.vars$aggr.m != "none") {
            arg.list <- list()
            arg.list$aggr.m$FUN <- ref.vars$aggr.m
            descr <- paste("The historical scenario data are monthly aggregated using the",
                           ref.vars$aggr.m, "cell function")
            graph.h <- metaclipR.Aggregation(graph = graph.h,
                                             disable.command = TRUE,
                                             arg.list = arg.list,
                                             dc.description = descr)
            if (experiment != "historical") {
                descr <- paste("The", experiment , "data are monthly aggregated using the",
                               ref.vars$aggr.m, "cell function")
                graph.r <- metaclipR.Aggregation(graph = graph.r,
                                                 disable.command = TRUE,
                                                 arg.list = arg.list,
                                                 dc.description = descr)
            }
        }

        ## Annual Aggregation --------------------------------------------------

        if (ref.vars$aggr.y != "mean") {
            arg.list <- list()
            arg.list$aggr.y$FUN <- ref.vars$aggr.y
            descr <- paste("The historical data is annually aggregated using the",
                           ref.vars$aggr.y, "cell function")
            graph.h <- metaclipR.Aggregation(graph = graph.h,
                                             disable.command = TRUE,
                                             arg.list = arg.list,
                                             dc.description = descr)
            if (experiment != "historical") {
                descr <- paste("The", experiment , "data is annually aggregated using the",
                               ref.vars$aggr.y, "cell function")
                graph.r <- metaclipR.Aggregation(graph = graph.r,
                                                 disable.command = TRUE,
                                                 arg.list = arg.list,
                                                 dc.description = descr)
            }
        }

        ## Interpolation to reference grid -------------------------------------
        ## The data is already in the reference grid if bias correction has taken place,
        ## because it inherits the grid from the observations, that are previously interpolated

        if (is.null(bias.adj.method)) {
            if (variable %in% c("tos", "ph", "siconc")) {
                descr <- paste0("The historical ", ref.vars$description," (", ref.vars$shortname,") climatology of the ",
                                model.name, " model (in native grid coordinates) is interpolated onto the reference ",
                                project, " grid of ", resX, " x ", resY,
                                " degrees using a first-order conservative method")
            } else {
                descr <- paste0("The historical ", ref.vars$description," (", ref.vars$shortname,") climatology of the ",
                                model.name, " model (" ,
                                ref.model$resX.atmos , " x ",
                                ref.model$resY.atmos, " degrees resolution) is interpolated onto the reference ",
                                project, " grid of ", resX, " x ", resY,
                                " degrees using a conservative method")
            }
            ## A generic conservative remapping is used in CMIPx products (ds:ConservativeRemapping)
            ## In CORDEX, the EURO-CORDEX method is instantiated instead (ds:EUROCordexConservativeRemapping)

            interp.method <- ifelse(grepl("^CORDEX-", project),
                                    "conservative_CORDEX", "conservative")

            graph.h <- metaclipcc.Regridding(graph = graph.h,
                                             RefSpatialExtent = reference.extent,
                                             RefRectangularGrid = reference.grid,
                                             InterpolationMethod = interp.method,
                                             dc.description = descr)
            if (experiment != "historical") {
                if (variable %in% c("tos", "ph", "siconc")) {
                    descr <- paste0("The ", experiment, " ",  ref.vars$description," (", ref.vars$shortname,") climatology of the ",
                                    model.name, " model (in oceanic grid coordinates) is interpolated onto the reference ",
                                    project, " grid of ", resX, " x ", resY,
                                    " degrees using a conservative method")
                } else {
                    descr <- paste0("The ", experiment, " ", ref.vars$description," (", ref.vars$shortname,") climatology of the ",
                                    model.name, " model (" ,
                                    ref.model$resX.atmos , " x ",
                                    ref.model$resY.atmos, " degrees resolution) is interpolated onto the reference ",
                                    project, " grid of ", resX, " x ", resY,
                                    " degrees using a conservative method")
                }
                graph.r <- metaclipcc.Regridding(graph = graph.r,
                                                 RefSpatialExtent = reference.extent,
                                                 RefRectangularGrid = reference.grid,
                                                 InterpolationMethod = interp.method,
                                                 dc.description = descr)
            }
        }

        ########################################################################
        ## Up to this point, the intermediate product is described.
        ## The following steps are undertaken by the Portal.
        ########################################################################

        ## Climatology ---------------------------------------------------------

        arg.list <- list()
        arg.list$clim.fun$FUN <- "mean"
        descr <- paste("The", ref.vars$description,
                       "climatology is calculated for each grid cell, as the mean value for the whole historical period",
                       baseline)
        graph.h <- metaclipR.Climatology(graph = graph.h,
                                         arg.list = arg.list,
                                         disable.command = TRUE,
                                         dc.description = descr)
        if (experiment != "historical") {
            if (future.period == "1.5" | future.period == "2" | future.period == "3" | future.period == "4") {
                fp <- paste0("of +", future.period, " degC Global Warming Level")
            } else {
                fp <- future.period
            }
            descr <- paste0("The \'", ref.vars$description,
                           "\' climatology is calculated for each grid cell, as the mean value for the whole future period ",
                           fp)
            graph.r <- metaclipR.Climatology(graph = graph.r,
                                             arg.list = arg.list,
                                             disable.command = TRUE,
                                             dc.description = descr)
        }

        ## Delta calculation ---------------------------------------------------

        if (isTRUE(delta)) {
            delta.type <- ref.vars$delta_change
            descr <- if (delta.type == "absolute") {
                paste0("The climate change signal is computed, for each grid cell, as the arithmetic difference between the \'",
                       ref.vars$description, "\' (", ref.vars$shortname, ") climatologies of the ", experiment, " and the historical scenarios")
            } else {
                paste0("The climate change signal is computed, for each grid cell, as the ratio (in %) between the \'",
                       ref.vars$description,"\' (", ref.vars$shortname, ") ensemble mean climatologies of the ", experiment, " and the historical scenarios")
            }
            graph <- metaclipcc.Delta(graph = graph.r,
                                      referenceGraph = graph.h,
                                      delta.type = delta.type,
                                      dc.description = descr)
        } else {

            if (experiment == "historical") {
                graph <- graph.h
            } else {
                graph <- graph.r
            }
        }
        graph.list[[x]] <- graph
    }

    ## Filter missing models ---------------------------------------------------

    if (length(graph.list) == 0) {

        if ((ref.vars$variable == "prsn" | ref.vars$variable == "wind" | ref.vars$variable == "spi6" | ref.vars$variable == "siconc" | ref.vars$variable == "ph" | ref.vars$variable == "tos") & (project == "CMIP5")) {
            message("\'", ref.vars$variable, "\' not available for CMIP5: No provenance output was created.")
        } else {
            message("No model reached the +", future.period, " degC global warming level in ", experiment, ": No provenance output was created.")
        }
        graph <- NULL
        map.nodename <- NULL

    } else {

        rm.ind <- which(sapply(graph.list, "is.null") == TRUE)
        if (length(rm.ind) > 0) graph.list <- graph.list[-rm.ind]

        ## Ensemble building -------------------------------------------------------

        if (length(graph.list) > 1L) {
            descr <- "The multi-model ensemble is built by joining each individual model climatology along the new dimension \'member\'"
            graph <- metaclipR.Ensemble(graph.list = graph.list,
                                        disable.command = TRUE,
                                        dc.description = descr)
        } else {## It can happen that one single model remains (e.g. CMIP6 SSP245 +4degC GWL)
            graph <- graph.list[[1]]
            message("One single model. No multi-member ensemble was built")
        }

        ## MAP PRODUCT DESCRIPTION -------------------------------------------------
        ## TODO:Colorbar bounds

        ## Map ---------------------------------------------------------------------
        ### Includes links to a HorizontalExtent and a Projection

        withInput <- graph$parentnodename
        graph <- graph$graph
        map.nodename <- paste("Map", randomName(), sep = ".")
        descr <- "Final map product, consisting of different superposed layers and other graphical elements (legend, title etc.)"
        graph <- add_vertices(graph,
                              nv = 1,
                              name = map.nodename,
                              label = "Map product",
                              className = "go:Map",
                              attr = list( "dc:description" = descr))
        if (is.null(map.bbox)) {
            map.extent <- reference.extent
        } else {
            descr <- "The map horizontal extent is interactively defined by the user through the Atlas Viewer application"
            map.extent <- metaclipcc.HorizontalExtent(xmin = map.bbox[1],
                                                      xmax = map.bbox[3],
                                                      ymin = map.bbox[2],
                                                      ymax = map.bbox[4],
                                                      dc.description = descr)
            graph <- my_union_graph(graph, map.extent$graph)
        }
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, map.extent$parentnodename)),
                           label = "go:hasMapExtent")

        ## Projection --------------------------------------------------------------

        proj.name <- switch(proj,
                            "Robin" = "go:Robinson",
                            "Arctic" = "go:AntarcticPolarStereographic",
                            "Antarctic" = "go:ArcticPolarStereographic",
                            "Pacific" = "go:Robinson-Pacific")

        graph.proj <- metaclipcc.MapProjection(proj = proj.name)
        graph <- my_union_graph(graph, graph.proj$graph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, graph.proj$parentnodename)),
                           label = "go:hasMapProjection")

        ## Heatmap raster ----------------------------------------------------------

        maplayer.nodename <- paste("mapRasterLayer", randomName(), sep = ".")

        descr <- if (!delta) {
            "The ensemble mean is graphically displayed on the map as a raster heatmap layer"
        } else {
            "The ensemble delta change is graphically displayed on the map as a raster heatmap layer"
        }
        graph <- add_vertices(graph,
                              nv = 1,
                              name = maplayer.nodename,
                              label = "Heatmap",
                              className = "go:MapRaster",
                              attr = list("dc:description" = descr))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, withInput),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hadGraphicalRepresentation")
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hasMapLayer")

        ## Land mask (ocean vars only)

        if (variable %in% c("tos", "ph", "siconc")) {
            descr <- "All land areas are masked to display sea-only values"
            refurl <- "https://doi.org/10.5281/zenodo.3691645"
            mask.nodename <- paste("mapLandMask", randomName(), sep = ".")
            graph <- add_vertices(graph,
                                  nv = 1,
                                  name = mask.nodename,
                                  label = "Land Mask",
                                  className = "go:Mask",
                                  attr = list("dc:description" = descr,
                                              "ds:referenceURL" = refurl))
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, maplayer.nodename),
                                 getNodeIndexbyName(graph, mask.nodename)),
                               label = "go:hasMask")

        }

        ## Color palette -----------------------------------------------------------

        # input.ecv <- if (isTRUE(climate.index %in% c("Rx1day", "Rx5day",
        #                                              "DS","SPI6",
        #                                              "SPI12", "SPEI6",
        #                                              "SPEI12", "DF6",
        #                                              "DF12")) | (isTRUE(ref.vars$variable) == "pr"))  {
        #     "pr"
        # } else {
        #     "tas"
        # }
        # div <- ifelse(is.null(delta), FALSE, TRUE)
        # graph.pal <- metaclipcc.ColorPalette(input.ecv = input.ecv, diverging = div)
        # graph <- my_union_graph(graph, graph.pal$graph)
        # graph <- add_edges(graph,
        #                    c(getNodeIndexbyName(graph, maplayer.nodename),
        #                      getNodeIndexbyName(graph, graph.pal$parentnodename)),
        #                    label = "go:hasColorPalette")

        ## Coastline ---------------------------------------------------------------

        maplayer.nodename <- paste("mapLinesLayer", randomName(), sep = ".")
        descr <- "Vector layer. Physical map of coastline boundaries"
        graph <- add_vertices(graph,
                              nv = 1,
                              name = maplayer.nodename,
                              label = "Coastline boundaries",
                              className = "go:MapLines",
                              attr = list("dc:description" = descr,
                                          "go:LineColor" = "hex-5492cd",
                                          "go:LineType" = "solid"))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hasMapLayer")

        ## IPCC regions ------------------------------------------------------------
        # The Region Set is kept generic, since different choices are available through the IA Menu
        # (namely: WGI Reference regions, WGII continental, Monsoons, River Basins and Small Islands )

        maplayer.nodename <- paste("mapLinesLayer", randomName(), sep = ".")
        descr <- "IPCC-AR6 Vector Layer Region Set"
        refurl <- "https://doi.org/10.5281/zenodo.3691645"
        graph <- add_vertices(graph,
                              nv = 1,
                              name = maplayer.nodename,
                              label = "IPCC Region Set",
                              className = "go:MapLines",
                              attr = list("dc:description" = descr,
                                          "ds:referenceURL" = refurl,
                                          "go:LineColor" = "hex-D3D3D3",
                                          "go:LineType" = "solid"))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hasMapLayer")

        # Map hatching -------------------------------------------------------------

        if (!is.null(uncertainty)) {

            if (uncertainty == "simple") {
                descr <- "Model agreement is represented with two categories: No overlay indicates high model agreement, where at least 80% of models agree on sign of change; diagonal lines (/) indicate low model agreement, where fewer than 80% of models agree on sign of change. For more information on the simple approach, please refer to the AR6 WGI Cross-Chapter Box Atlas 1. NOTE: Model agreement is computed at a gridbox level and is not representative of regionally aggregated results over larger regions"
            } else if (uncertainty == "advanced") {
                descr <- "Model agreement is represented using the advanced approach (significant change and agreement) with three categories: No overlay indicates that the change is robust and likely emerges from internal variability (at least 66% of the models show a change greater than the internal-variability threshold and at least 80% of the models agree on the sign of change); diagonal lines (\\\\) indicate no change or no robust change (fewer than 66% of the models show change greater than the internal-variability threshold); crossed lines (X) indicate conflicting signals where at least 66% of the models show change greater than the internal-variability threshold but fewer than 80% of all models agree on the sign of change. For more information on the advanced approach, please refer to the AR6 WGI Cross-Chapter Box Atlas 1. NOTE: Robustness and model agreement are computed at a gridbox level and are not representative of regionally aggregated results over larger regions."
            }

            # Model Consensus

            maplayer.nodename <- paste("mapHatchingLayer", randomName(), sep = ".")
            # refurl <- "https://doi.org/10.1088/1748-9326/aab1b1"
            graph <- add_vertices(graph,
                                  nv = 1,
                                  name = maplayer.nodename,
                                  label = "Model Agreement Layer",
                                  className = "go:Mask",
                                  attr = list("dc:description" = descr,
                                              # "ds:referenceURL" = refurl,
                                              # "go:LineAngle" = -45,
                                              "go:LineColor" = "hex-000000",
                                              "go:LineType" = "solid"))
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, map.nodename),
                                 getNodeIndexbyName(graph, maplayer.nodename)),
                               label = "go:hasMapLayer")
        }
    }
    return(list("graph" = graph, "parentnodename" =  map.nodename))
}





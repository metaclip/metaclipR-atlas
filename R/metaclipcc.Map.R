##     metaclipcc.map METACLIP description of an Atlas map product
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

#' @title Directed metadata graph construction for AR6 Atlas Map products
#' @description Build a directed metadata graph describing a Map product of the AR6 Interactive Atlas
#' @param project Project. Unused so far. Default to \code{"CMIP5"}.
#' @param variable Code of the input ECV (it can be omitted if \code{climate.index} is indicated).
#'  Current accepted values are restricted to \code{"tas", "pr", "tasmax", "tasmin"}.
#' @param climate.index If the map is for a climate index, name of the index Otherwise NULL (the default). Currently accepted values are restricted to
#' the set of indices to be included in the Atlas, namely:
#' \code{"TXx", "TNn", "Rx1day", "Rx5day", "DS", "SPI6", "SPI12", "SPEI6", "SPEI12", "DF6", "DF12", "LFFP", "GDD","T21.5", "TX35", "TX40", "HDD", "FD"}
#' @param delta Type of delta map displayed. This can be either \code{"absolute"} or \code{"relative"}. Default to \code{NULL} meaning that the map is not a delta
#'  but an original magnitude.
#' @param experiment Experiment results displayed in the map. Accepted values are restricted to \code{"historical", "rcp26", "rcp45", "rcp85"}
#' @param baseline Character string indicating the \code{"start-end"} years of the baseline (historical) period. Accepted values are:
#' \code{"1980-2010"} (WMO standard period), \code{"1986-2005"} (AR5 period) or \code{"1995-2014"} (AR6 period). Internally, there is a tricky part here, see Details.
#' @param future.period future period. Default to \code{NULL}, for historical maps (i.e., period defined by the \code{baseline} argument). Otherwise, a character string
#'  indicating either the \code{"start-end"} years of the future period or a (GCM-specific) warming level. Current options include the standard AR5 future time slices
#'  for near term, \code{"2021-2040"}, medium term \code{"2041-2060"} and long term \code{"2081-2100"}, and the global
#'  warming levels of +1.5 degC \code{"1.5"}, +2 \code{"2"} and +3 \code{"3"}.
#' @param season season. Integer vector of correlative months, in ascending order, encompassing the target season.
#' @param bias.adj.method Default to \code{NULL} and unused. If the map displays a bias-corrected product, a character string idetifying the method. Current accepted values a re \code{"EQM"},
#' for the standard VALUE empirical quantile mapping method.
#' @param ref.obs.dataset Default to \code{NULL}, and unused unless a \code{bias.adj.method} has been specified.
#'  This is the reference observational dataset to perform the correction. This is an individual that must be defined in the datasource vocabulary,
#'   belonging to either classes \code{ds:ObservationalDataset} or \code{ds:Reanalysis}. Currently accepted values are \code{"EWEMBI"}.
#' @param proj Map projection string. Accepted values are \code{"Robin"}, \code{"Arctic"} and \code{"Antarctic"},
#' for Robinson and WGS84 Arctic/Antarctic Polar stereographic projections.
#' @param map.bbox Optional. numeric vector of length 4, containing, in this order the \code{c(xmin, ymin, xmax, ymax)} coordinates of the target area
#' zoomed in by the user. If missing, then the HorizontalExtent associated to the \code{project} is assumed.
#'
#' @details
#'
#' \strong{Baseline period definition}
#'
#' Two of the baseline periods considered (WMO, 1981-2010 and AR6, 1995-2014) go beyond the temporal extent of the historical experiment simulations in AR5, ending in 2005.
#' In this case, the strategy followed in the different IPCC reports is to fill the missing period between 2006 and the end of the baseline with the years of the future
#' simulation used in each case. For example, to compute a RCP 8.5 delta using the WMO baseline, the baseline data for the period 2006-2010 will be extracted from the RCP85
#' simulation, and then binded to the 1981-2005 period of the historical simulation in order to complete the baseline period.
#'
#' @author J. Bedia
#'
#' @import metaclipR
#' @importFrom magrittr %>% extract2
#'
#' @export


#TODO:
## Include details on color palettes, range of values etc...
## Define simulation domains (ds:SpatialExtent individual instances) for CORDEX projects
## Deal with grid info for oceanic variables

# project = "CMIP5"
# variable = "tasmax"
# climate.index = "T21.5"
# delta = "absolute"
# experiment = "rcp45"
# baseline = "1986-2005"
# future.period = "2041-2060"
# season = 1:12
# bias.adj.method = NULL # "EQM"
# ref.obs.dataset = NULL # "EWEMBI"
# proj = "Robin"
# map.bbox = c(-10,20,50,60)

metaclipcc.Map <- function(project = "CMIP5",
                           variable = NULL,
                           climate.index = NULL,
                           delta = NULL,
                           experiment,
                           baseline,
                           future.period = NULL,
                           season,
                           bias.adj.method = NULL,
                           ref.obs.dataset = NULL,
                           proj,
                           map.bbox = NULL) {

    # Fixed parameters *******
    ref.period <- c(1980, 2005)
    time.res.orig <- "P1D"
    # ***********************

    project <- match.arg(project, choices = c("CMIP5", "CMIP6", "AFRCORDEX", "ANTCORDEX",
                                              "ARCCORDEX", "AUSCORDEX", "CAMCORDEX",
                                              "CASCORDEX", "EASCORDEX", "EUROCORDEX",
                                              "MEDCORDEX", "MENACORDEX", "NAMCORDEX",
                                              "SAMCORDEX", "SEACORDEX", "WASCORDEX"))
    if (!is.null(variable)) {
        variable <- match.arg(variable, choices = c("tas", "pr", "tasmax", "tasmin"))
        if (!is.null(climate.index)) {
            climate.index <- NULL
            warning("Variable ", variable, " was first indicated. The \'climate.index\' argument was set to NULL")
        }
    } else {
        if (is.null(climate.index)) stop("Either a variable or a climate index must be supplied")
    }
    if (!is.null(climate.index)) {
        climate.index <- match.arg(climate.index, choices = c("TXx", "TNn",
                                                              "Rx1day", "Rx5day", "DS",
                                                              "SPI6", "SPI12", "SPEI6", "SPEI12",
                                                              "DF6", "DF12", "LFFP", "GDD",
                                                              "T21.5", "TX35", "TX40",
                                                              "HDD", "FD"))
    }
    if (!is.null(delta)) {
        delta <- match.arg(delta, choices = c("absolute", "relative"))
    }
    experiment <- match.arg(experiment, choices = c("historical", "rcp26", "rcp45", "rcp85"))
    if (experiment == "historical") {
        if (!is.null(delta)) {
            delta <- NULL
            message("NOTE: \'delta\' argument ignored for the \'experiment=\"historical\"\' setting")
        }
    }
    baseline <- match.arg(baseline, choices = c("1980-2010", "1986-2005", "1995-2014"))
    hist.period <- switch(baseline,
                          "1980-2010" = c(1980, 2005),
                          "1986-2005" = c(1986, 2005),
                          "1995-2014" = c(1995, 2005))
    fill.period <- switch(baseline,
                          "1980-2010" = c(2006, 2010),
                          "1986-2005" = NULL,
                          "1995-2014" = c(2006, 2014))
    if (experiment != "historical" & is.null(future.period)) stop("future.period argument is missing, with no default for ", experiment, " experiment")
    if (!is.null(future.period)) {
        future.period <- match.arg(future.period, choices = c("2021-2040", "2041-2060", "2081-2100",
                                                              "1.5", "2", "3"))
    }

    proj <- match.arg(proj, choices = c("Robin", "Arctic", "Antarctic"))

    if (!is.null(bias.adj.method)) {
        if (is.null(ref.obs.dataset)) stop("A reference observational dataset is required for bias correction", call. = FALSE)
        bias.adj.method <- match.arg(bias.adj.method, choices = "EQM")
        ref.obs.dataset <- match.arg(ref.obs.dataset, choices = "EWEMBI")
    }

    if (!is.null(map.bbox)) stopifnot(length(map.bbox) == 4L)

    ref.project <- showIPCCdatasets(names.only = FALSE)[showIPCCdatasets(names.only = FALSE) %>% extract2("Project") %>% grep(pattern = project), ]
    ipcc.region <- ref.project$SimulationDomain %>% unique()
    cordex.region <- ref.project$region_name %>% unique()

    ## Reference grids ---------------------------------------------------------
    if (project == "CMIP5") {
        resX <- resY <- 2
        descr <- paste("This is the reference grid used in all CMIP5 map products of", resX, "x", resY, "degree resolution covering the whole globe")
        gridfile.url <- "https://github.com/SantanderMetGroup/ATLAS/blob/devel/reference-grids/land_sea_mask_2degree.nc4"
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
        descr <- paste("This is the reference grid used in all CMIP6 map products of", resX, "x", resY, "degree resolution covering the whole globe")
        gridfile.url <- "https://github.com/SantanderMetGroup/ATLAS/blob/devel/reference-grids/land_sea_mask_1degree.nc4"
        reference.grid <- metaclipR.RectangularGrid(resX = resX,
                                                    resY = resY,
                                                    xmin = -179.5,
                                                    xmax = 179.5,
                                                    ymin = -89.5,
                                                    ymax = 89.5,
                                                    dc.description = descr,
                                                    ref.URL = gridfile.url)
    } else {
        resX <- resY <- 0.44
        descr <- paste0("This is the ", project,
                        " reference grid of ", resX," x ", resY, " degree resolution, centered on ",
                        cordex.region, ", used in all Atlas CORDEX products")
        reference.grid <- metaclipR.RectangularGrid(resX = 0.44,
                                                    resY = 0.44,
                                                    xmin = -179.78,
                                                    xmax = 179.78,
                                                    ymin = -89.78,
                                                    ymax = -89.78,
                                                    dc.description = descr)
    }

    ## Reference spatial extents -----------------------------------------------

    reference.extent <- metaclipcc.HorizontalExtent(region = unique(ref.project$SimulationDomain))

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

        graph.o <- metaclipcc.Dataset(ref.obs.dataset, RectangularGrid = obs.grid)
    }

    ## Historical scenarios ----------------------------------------------------

    ls <- showIPCCdatasets(names.only = TRUE)
    hist.list <- ls[which(grepl(paste0("^", project, ".*historical"), ls))]
    if (experiment != "historical") {
        rcp.list <- gsub("historical", experiment, hist.list)
        if (!identical(length(rcp.list), length(hist.list))) {
            stop("historical and future dataset numbers differ")
        }
    }
    rcp85.list <- gsub("historical", "rcp85", hist.list) # For filling historical gap
    aux <- showIPCCdatasets(names.only = FALSE)

    ## ECV filtering -----------------------------------------------------------

    vars <- if (!is.null(climate.index)) {
        ref.vars <- showIPCCvars(names.only = FALSE)[grep(paste0("^", climate.index, "$"), showIPCCvars()), ]
        strsplit(ref.vars$inputECV, split = ",") %>% unlist()
    } else {
        ref.vars <- showIPCCvars(names.only = FALSE)[grep(paste0("^", variable, "$"), showIPCCvars()), ]
        variable
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
    #### 1.1. En el caso de índices, modelos que falta algún ECV
    ### 2. modelos que no alcanzan el warming level especificado
    ### 3. modelos de los que no hay dato

    graph.list <- list()
    # for (x in 1:length(hist.list)) {
    for (x in 1:2) {
        ref.model <- aux[grep(hist.list[x], aux$name, ignore.case = TRUE),]
        message("[", Sys.time(), "] Processing ", ref.model$GCM, " model data")

        ### Filter missing ECV / Climate Index ---------------------------------------

        if (!is.null(climate.index)) {
            if (ref.model[[climate.index]] != 1) next
        } else {
            if (ref.model[[variable]] != 1) next
        }

        ### Filter missing RCPs OR not reached GWLs ----------------------------

        if (experiment != "historical") {
            years <- metaclipcc.getFuturePeriod(project = project,
                                                model = ref.model$GCM,
                                                future.period = future.period,
                                                rcp = experiment)
            if (anyNA(years)) next
        }

        ### GCM grid -----------------------------------------------------------
        descr <- paste("This is the native grid of", ref.model$resX.atmos,
                       "x", ref.model$resY.atmos, "of the atmospheric variables in the",
                       ref.model$GCM, "GCM simulations")

        gcm.grid <- metaclipR.RectangularGrid(resX = ref.model$resX.atmos,
                                              resY = ref.model$resY.atmos,
                                              xmin = ref.model$xmin.atmos,
                                              xmax = ref.model$xmax.atmos,
                                              ymin = ref.model$ymin.atmos,
                                              ymax = ref.model$ymax.atmos,
                                              dc.description = descr)

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
                graph2 <- metaclipcc.Dataset(ref.dataset$name, RectangularGrid = gcm.grid)
                graph2 <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph2,
                                                   Dataset.name = ref.dataset$name,
                                                   variable = vars[i],
                                                   ipcc.region = ipcc.region,
                                                   season = season,
                                                   years = fill.period)
                RCP <- substr(ref.dataset$name, start = nchar(ref.dataset$name) - 4,
                              stop = nchar(ref.dataset$name))
                descr <- paste0("The subperiod ",
                                paste(fill.period[1], fill.period[2], sep = "-"),
                                ", missing from the Historical experiment (1850-2005), is filled with the data from the ",
                                RCP, " simulation of the model to complete the baseline period requested (", baseline, ")")
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

                graph.h <- metaclipR.BiasCorrection(graph = graph.h,
                                                    TrainingGraph = graph.train,
                                                    ReferenceGraph = obs.var.graph.list[[i]],
                                                    disable.command = TRUE,
                                                    ReferenceGraphSpatialExtent = reference.extent,
                                                    ReferenceGraphRectangularGrid = reference.grid,
                                                    BC.method = bias.adj.method,
                                                    dc.description = descr)
            }

            ## Future simulation data ------------------------------------------

            graph.r <- NULL
            if (experiment != "historical") {
                graph.r <- metaclipcc.Dataset(rcp.list[x], RectangularGrid = gcm.grid)
                graph.r <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph.r,
                                                    Dataset.name = rcp.list[x],
                                                    time.res.orig = time.res.orig,
                                                    ipcc.region = ipcc.region,
                                                    variable = vars[i],
                                                    season = season,
                                                    years = years)

                ## Bias correction of future time slice ------------------------

                if (!is.null(bias.adj.method)) {
                    descr <- paste0("In this step the future time slice is bias-adjusted, using the historical experiment simulation as training data, and the ",
                                    ref.obs.dataset,
                                    " observation data as predictand")

                    graph.r <- metaclipR.BiasCorrection(graph = graph.r,
                                                        ReferenceGraphSpatialExtent = reference.extent,
                                                        ReferenceGraphRectangularGrid = reference.grid,
                                                        TrainingGraph = graph.train,
                                                        ReferenceGraph = obs.var.graph.list[[i]],
                                                        dc.description = descr,
                                                        BC.method = bias.adj.method,
                                                        disable.command = TRUE)
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

        if (!is.null(climate.index)) {
            ## Climate Index
            graph.h <- metaclipcc.ClimateIndex(graph.list = hist.graph.list,
                                               index.code = climate.index)
            if (experiment != "historical") {
                graph.r <- metaclipcc.ClimateIndex(graph.list = fut.graph.list,
                                                   index.code = climate.index)
            }
        } else {
            ## ECV
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
            descr <- paste("The historical scenario data is annually aggregated using the",
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

        ## Climatology ---------------------------------------------------------

        arg.list <- list()
        arg.list$clim.fun$FUN <- "mean"
        descr <- paste("The", ref.vars$variable,
                       "climatology is calculated for each grid cell, as the mean value for the whole historical period",
                       baseline)
        graph.h <- metaclipR.Climatology(graph = graph.h,
                                         arg.list = arg.list,
                                         disable.command = TRUE,
                                         dc.description = descr)
        if (experiment != "historical") {
            descr <- paste("The", ref.vars$variable,
                           "climatology is calculated for each grid cell, as the mean value for the whole future period",
                           future.period)
            graph.r <- metaclipR.Climatology(graph = graph.r,
                                             arg.list = arg.list,
                                             disable.command = TRUE,
                                             dc.description = descr)
        }

        ## Interpolation to reference grid -------------------------------------

        ## The data is already in the reference grid if bias correction has taken place,
        ## because it inherits the grid from the observations, that are previously interpolated

        if (is.null(bias.adj.method)) {
            descr <- paste0("The historical ", ref.vars$variable," climatology of the ",
                            ref.model$GCM, " model (" ,
                            ref.model$resX.atmos , " x ",
                            ref.model$resY.atmos, " degrees resolution) is interpolated onto the reference ",
                            project, " grid of ", resX, " x ", resY,
                            " degrees using a conservative method")
            graph.h <- metaclipcc.Regridding(graph = graph.h,
                                             RefSpatialExtent = reference.extent,
                                             RefRectangularGrid = reference.grid,
                                             InterpolationMethod = "conservative",
                                             dc.description = descr)
            if (experiment != "historical") {
                descr <- paste0("The ", experiment, " ", ref.vars$variable, " climatology of the ",
                                ref.model$GCM, " model (" ,
                                ref.model$resX.atmos , " x ",
                                ref.model$resY.atmos, " degrees resolution) is interpolated onto the reference ",
                                project, " grid of ", resX, " x ", resY,
                                " degrees using a conservative method")
                graph.r <- metaclipcc.Regridding(graph = graph.r,
                                                 RefSpatialExtent = reference.extent,
                                                 RefRectangularGrid = reference.grid,
                                                 InterpolationMethod = "conservative",
                                                 dc.description = descr)
            }
        }

        ## Delta calculation ---------------------------------------------------

        if (!is.null(delta)) {
            descr <- if (delta == "absolute") {
                paste("The climate change signal is computed, for each grid cell, as the arithmetic difference between the",
                      ref.vars$variable, "climatologies of the", experiment, "and the historical scenarios")
            } else {
                paste("The climate change signal is computed, for each grid cell, as the ratio (in %) between the",
                      ref.vars$variable, "climatologies of the", experiment, "and the historical scenarios")
            }
            graph <- metaclipcc.Delta(graph = graph.r,
                                      referenceGraph = graph.h,
                                      delta.type = delta, dc.description = descr)
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

    rm.ind <- which(sapply(graph.list, "is.null") == TRUE)
    if (length(rm.ind) > 0) graph.list <- graph.list[-rm.ind]

    ## Ensemble building -------------------------------------------------------

    descr <- "The multi-model ensemble is built by joining each individual model climatology along the new dimension \'member\'"
    graph <- metaclipR.Ensemble(graph.list = graph.list,
                                disable.command = TRUE,
                                dc.description = descr)

    ## MAP PRODUCT DESCRIPTION -------------------------------------------------
    ## Include hatching
    ## Colorbars
    ## IPCC regions layer (referenceURL https://github.com/SantanderMetGroup/ATLAS/tree/master/reference_regions)

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
    map.extent <- if (is.null(map.bbox)) {
        reference.extent
    } else {
        metaclipcc.HorizontalExtent(xmin = map.bbox[1],
                                    xmax = map.bbox[3],
                                    ymin = map.bbox[2],
                                    ymax = map.bbox[4])
    }
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, map.extent$parentnodename)),
                       label = "go:hasMapExtent")

    ## Projection --------------------------------------------------------------

    proj.name <- switch(proj,
           "Robin" = "go:Robin",
           "Arctic" = "go:AntarcticPolarStereographic",
           "Antarctic" = "go:ArcticPolarStereographic")

    graph.proj <- metaclipcc.MapProjection(proj = proj.name)
    graph <- my_union_graph(graph, graph.proj$graph)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, graph.proj$parentnodename)),
                       label = "go:hasMapProjection")

    ## Heatmap raster ----------------------------------------------------------

    maplayer.nodename <- paste("mapRasterLayer", randomName(), sep = ".")

    descr <- if (is.null(delta)) {
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

    ## Coastline ---------------------------------------------------------------

    maplayer.nodename <- paste("mapLinesLayer", randomName(), sep = ".")
    descr <- "Vector layer. Physical map of coastline boundaries"
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "Coastline boundaries",
                          className = "go:MapLines",
                          attr = list("dc:description" = descr,
                                      "go:LineColor" = "grey",
                                      "go:LineType" = "solid"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")

    ## IPCC regions ------------------------------------------------------------

    maplayer.nodename <- paste("mapLinesLayer", randomName(), sep = ".")
    descr <- "IPCC-AR6 World Regions"
    refurl <- "https://github.com/SantanderMetGroup/ATLAS/tree/master/reference_regions"
    graph <- add_vertices(graph,
                          nv = 1,
                          name = maplayer.nodename,
                          label = "IPCC World Regions",
                          className = "go:MapLines",
                          attr = list("dc:description" = descr,
                                      "ds:referenceURL" = refurl,
                                      "go:LineAngle" = -45,
                                      "go:LineColor" = "black",
                                      "go:LineType" = "solid"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, map.nodename),
                         getNodeIndexbyName(graph, maplayer.nodename)),
                       label = "go:hasMapLayer")

    # Map hatching -------------------------------------------------------------

    if (!is.null(delta)) {
        maplayer.nodename <- paste("mapHatchingLayer", randomName(), sep = ".")
        descr <- "Hatched areas in the map indicate a \'weak\' model agreement on the sign of the projected climate change signal (less than 80%, following Nikulin et al. 2018)"
        refurl <- "https://doi.org/10.1088/1748-9326/aab1b1"
        graph <- add_vertices(graph,
                              nv = 1,
                              name = maplayer.nodename,
                              label = "Consensus Hatching",
                              className = "go:Mask",
                              attr = list("dc:description" = descr,
                                          "ds:referenceURL" = refurl))
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, map.nodename),
                             getNodeIndexbyName(graph, maplayer.nodename)),
                           label = "go:hasMapLayer")
    }
    return(list("graph" = graph, "parentnodename" =  map.nodename))
}

# graph2json(graph = graph$graph, output.file = "./ignore/prueba.json")

# graph2json(graph, output.file = "ignore/prueba.json")


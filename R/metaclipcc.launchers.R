#' @title Launch ECV delta map description
#' @description Generates the full graph describing a delta map of an ECV
#' @param project Currently unused. CMIP5 by default.
#' @param baseline Vector of length two indicating the start/end years of the baseline (historical) period.
#'  Default to the AR5 baseline \code{c(1986,2005)}.
#' @param experiment code of the experiment
#' @param future.period future period. Current options include the standard AR5 future time slices.
#' @param variable variable
#' @param proj projection string
#' @param season season
#' @param time.res.orig original temporal resolution of the variable
#' @param delta.type type of delta (absolute or relative)
#' @return A METACLIP object describing the generation of a delta map.
#' @importFrom metaclipR metaclipR.SpatialExtent metaclipR.Aggregation metaclipR.Interpolation metaclipR.Ensemble metaclipR.Climatology
#' @author J. Bedia
#' @export

deltaMap.ECV.ipcc <- function(project = "CMIP5",
                              experiment = "rcp85",
                              future.period = "mid",
                              baseline = c(1986,2005),
                              variable,
                              season,
                              time.res.orig = "P1M",
                              proj = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                              delta.type = "absolute") {
    RCP <- match.arg(experiment, choices = c("rcp45", "rcp85"), several.ok = FALSE)
    future.period <- match.arg(future.period, choices = c("near", "mid", "long"), several.ok = FALSE)
    time.res.orig <- match.arg(time.res.orig, choices = c("P1D", "P1M"), several.ok = FALSE)
    variable <- match.arg(variable, choices = c("ta","tasmax","tasmin","tos","tp","O2","pH"), several.ok = FALSE)
    delta.type <- match.arg(delta.type, choices = c("absolute", "relative"), several.ok = FALSE)
    spExtent <- metaclipR.SpatialExtent(region = "GlobalExtent",
                                        xmin = -180,
                                        xmax = 180,
                                        ymin = -90,
                                        ymax = 90,
                                        proj = proj,
                                        resX = "2 deg",
                                        resY = "2 deg")
    ls <- showIPCCdatasets(names.only = TRUE)
    hist.list <- ls[which(grepl(paste0("^", project, ".*historical"), ls))]
    rcp.list <- gsub("historical", RCP, hist.list)
    if (!identical(length(rcp.list), length(hist.list))) stop("historical and future dataset numbers differ")
    graph.list <- lapply(1:length(hist.list), function(x) {
        aux <- showIPCCdatasets(names.only = FALSE)
        ref.gcm <- aux[grep(hist.list[x], aux$name),]
        graph <- metaclipcc.Dataset(hist.list[x])
        graph <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph, Dataset.name = hist.list[x],
                                          time.res.orig = time.res.orig, variable = variable,
                                          season = season, years = baseline)
        ref <- showIPCCvars(names.only = FALSE)
        ref <- ref[grep(paste0("^",variable,"$"), ref$variable), ]
        arg.list <- NULL
        # if (time.res.orig == "P1M") {
        #     arg.list <- list()
        #     arg.list$aggr.y$FUN <- ref$aggr.y
        # } else {
        #     arg.list <- list()
        #     arg.list$aggr.m$FUN <- ref$aggr.m
        #     arg.list$aggr.y$FUN <- ref$aggr.y
        # }
        # graph <- metaclipR.Aggregation(graph = graph, disable.command = TRUE, arg.list = arg.list,
        #                                dc.description = "Temporal aggregation of the antecedent dataset subset")
        arg.list <- list()
        arg.list$clim.fun$FUN <- "mean"
        graph <- metaclipR.Climatology(graph = graph, arg.list = arg.list, disable.command = TRUE,
                                       dc.description = "The climatology is calculated as the mean value for the given season of the antecedent dataset subset")
        ds <- rcp.list[x]
        graph2 <- metaclipcc.Dataset(ds)
        yrs <- switch(future.period, ## AR5 Periods##
                      "near" = c(2016,2035),
                      "mid" = c(2046,2065),
                      "long" = c(2081,2100))
        graph2 <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph2, Dataset.name = ds,
                                           variable = variable,
                                           season = season, years = yrs)
        # graph2 <- metaclipR.Aggregation(graph = graph2, disable.command = TRUE, arg.list = arg.list,
        #                                 dc.description = "Temporal aggregation of the antecedent dataset subset")
        graph2 <- metaclipR.Climatology(graph = graph2, arg.list = arg.list, disable.command = TRUE,
                                        dc.description = "The climatology is calculated as the mean value for the given season of the antecedent dataset subset")
        graph3 <- metaclipcc.Delta(graph = graph2, referenceGraph = graph, delta.type = delta.type)
        graph2 <- graph <- NULL
        graph3 <- metaclipR.Interpolation(graph = graph3, RefSpatialExtent = spExtent,
                                          disable.command = TRUE,
                                          InterpolationMethod = "bilinear",
                                          dc.description = paste0("The resulting anomalies of this model (of " ,
                                                                  ref.gcm$resX.atmos , " x ", ref.gcm$resY.atmos,
                                                                  " degree native resolution) are re-gridded to the reference 2.0 x 2.0 regular grid prior to ensemble construction"))
    })
    ## ENSEMBLE CONSTRUCTION
    ens.graph <- metaclipR.Ensemble(graph.list = graph.list, disable.command = TRUE,
                                    dc.description = "The multi-model ensemble is built by joining the (regridded) individual member anomalies along the new dimension 'member'")
    al <- list()
    al[['aggr.mem']][['FUN']] <- "mean"
    dc <- "The aggregation of the data along the new dimension 'member' yields one single delta map, that is the average of the (equally-weighted) individual anomalies of each ensemble member"
    ens.graph <- metaclipR.Aggregation(graph = ens.graph, disable.command = TRUE,
                                       arg.list = al, dc.description = dc)
    ## MAP GENERATION
    ens.graph <- metaclipcc.DeltaMap(graph = ens.graph, proj = proj, spatial.ref =  spExtent)
    return(ens.graph)
}


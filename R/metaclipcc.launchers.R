#' @title Launch ECV anomaly map description
#' @description Generates the full graph describing an anomaly map of an ECV
#' @param project Currently unused. CMIP5 by default.
#' @param experiment code of the experiment
#' @param future.period future period
#' @param variable variable
#' @param proj projection string
#' @param season season
#' @param time.res.orig original temporal resolution of the variable
#' @param anomaly.type type of anomaly (absolute or relative)
#' @return A METACLIP object describing the generation of an anomaly map.
#' @importFrom metaclipR metaclipR.SpatialExtent metaclipR.Aggregation metaclipR.Interpolation metaclipR.Ensemble
#' @author J. Bedia
#' @export

anomalyMap.ECV.ipcc <- function(project = "CMIP5",
                                experiment = c("rcp45", "rcp85"),
                                future.period = c("near", "mid", "long"),
                                variable, season,
                                time.res.orig = c("P1D", "P1M"),
                                proj = "EPSG:54030",
                                anomaly.type = c("absolute", "relative")) {
    RCP <- match.arg(experiment, choices = c("rcp45", "rcp85"), several.ok = FALSE)
    future.period <- match.arg(future.period, choices = c("near", "mid", "long"), several.ok = FALSE)
    time.res.orig <- match.arg(time.res.orig, choices = c("P1D", "P1M"), several.ok = FALSE)
    variable <- match.arg(variable, choices = c("ta","tasmax","tasmin","tos","tp","O2","pH"), several.ok = FALSE)
    anomaly.type <- match.arg(anomaly.type, choices = c("absolute", "relative"), several.ok = FALSE)
    spExtent <- metaclipR.SpatialExtent(region = "GlobalExtent",
                                        xmin = -180,
                                        xmax = 180,
                                        ymin = -90,
                                        ymax = 90,
                                        proj = proj,
                                        resX = "1.5deg",
                                        resY = "1.5deg")
    ls <- showIPCCdatasets(names.only = TRUE)
    hist.list <- ls[which(grepl(paste0("^", project, ".*historical"), ls))]
    rcp.list <- gsub("historical", RCP, hist.list)
    graph.list <- lapply(1:length(hist.list), function(x) {
        graph <- metaclipcc.Dataset(hist.list[x])
        graph <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph, Dataset.name = hist.list[x],
                                          time.res.orig = time.res.orig, variable = variable,
                                          season = season, years = c(1986,2005))
        ref <- showIPCCvars(names.only = FALSE)
        ref <- ref[grep(paste0("^",variable,"$"), ref$variable), ]
        if (time.res.orig == "P1M") {
            arg.list <- list(aggr.y = list(FUN = ref$aggr.y))
        } else {
            arg.list <- list(aggr.m = list(FUN = ref$aggr.m),
                             aggr.y = list(FUN = ref$aggr.y))
        }
        graph <- metaclipR.Aggregation(graph = graph, disable.command = TRUE, arg.list = arg.list)
        ds <- rcp.list[x]
        graph2 <- metaclipcc.Dataset(ds)
        yrs <- switch(future.period, ## AR5 Periods##
                      "near" = c(2016,2035),
                      "mid" = c(2046,2065),
                      "long" = c(2081,2100))
        graph2 <- metaclipcc.DatasetSubset(metaclipcc.Dataset = graph2, Dataset.name = ds,
                                           variable = variable,
                                           season = season, years = yrs)
        graph2 <- metaclipR.Aggregation(graph = graph2, disable.command = TRUE, arg.list = arg.list)
        graph3 <- metaclipcc.AnomalyCalculation(graph = graph2, referenceGraph = graph, anomaly.type = "absolute")
        graph2 <- graph <- NULL
        graph3 <- metaclipR.Interpolation(graph = graph3, RefSpatialExtent = spExtent,
                                          disable.command = TRUE,
                                          InterpolationMethod = "bilinear")

    })
    ## ENSEMBLE CONSTRUCTION
    ens.graph <- metaclipR.Ensemble(graph.list = graph.list, disable.command = TRUE)
    ## MAP GENERATION
    ens.graph <- metaclipcc.AnomalyMap(graph = ens.graph, proj = proj, spatial.ref =  spExtent)
    return(ens.graph)
}


#' @title Color Palette
#' @description Creates a sinle node graph that defines a go:ColorPalette-class object
#' @param input.ecv Input ECV determining the character of the palette.
#' Essentially, two different schemes will be used for either precipitation or temperature-related variables
#' @param diverging Logical. If TRUE, a diverging palette (for deltas). Otherwise a sequential palette is assumed.
#' @keywords internal
#' @importFrom metaclipR my_add_vertices
#' @importFrom igraph make_empty_graph
#' @export
#' @author J Bedia

metaclipcc.ColorPalette <- function(input.ecv, diverging) {
    vn <- paste("ColorPalette", randomName(), sep = ".")
    if (isTRUE(diverging)) {
        cn <- "go:DivergingColorPalette"
        lab <- "Diverging Color Palette"
        if (input.ecv == "pr") {
            attr.list <- list("go:hasColorSet" = "#543005,#8c510a,#bf812d,#dfc27d,#f6e8c3,#f5f5f5,#c7eae5,#80cdc1,#35978f,#01665e,#003c30",
                              "ds:referenceURL" = "http://colorbrewer2.org/#type=diverging&scheme=BrBG&n=11")
        } else {
            attr.list <- list("go:hasColorSet" = "#67001f,#b2182b,#d6604d,#f4a582,#fddbc7,#f7f7f7,#d1e5f0,#92c5de,#4393c3,#2166ac,#053061",
                              "ds:referenceURL" = "http://colorbrewer2.org/#type=diverging&scheme=RdBu&n=11")
        }
    } else {
        cn <- "go:SequentialColorPalette"
        lab <- "Sequential Color Palette"
        if (input.ecv == "pr") {
            attr.list <- list("go:hasColorSet" = "eff3ff,#c6dbef,#9ecae1,#6baed6,#4292c6,#2171b5,#084594",
                              "ds:referenceURL" = "http://colorbrewer2.org/#type=sequential&scheme=Blues&n=7")
        } else {
            attr.list <- list("go:hasColorSet" = "#fee5d9,#fcbba1,#fc9272,#fb6a4a,#ef3b2c,#cb181d,#99000d",
                              "ds:referenceURL" = "http://colorbrewer2.org/#type=sequential&scheme=Reds&n=7")
        }
    }
    graph <- make_empty_graph()
    graph <- my_add_vertices(graph,
                             name = vn,
                             label = lab,
                             className = cn,
                             attr = attr.list)
    return(list(graph = graph, parentnodename = vn))
}


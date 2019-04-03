#' @title Table of IPCC Datasets
#' @description Shows the internal table with IPCC Datasets and relevant metadata
#' @param names.only Logical flag indicating that only the internal dataset names are shown. Default to \code{TRUE}. Otherwise, the full metadata table is shown
#' @export
#' @author J Bedia
#' @return Either a vector with all dataset names (when \code{names.only = TRUE}) or a \code{data.frame} with all the associated metadata.
#' @keywords internal
#' @export
#' @importFrom utils read.csv
#' @examples showIPCCdatasets()

showIPCCdatasets <- function(names.only = TRUE) {
    out <- read.csv(file.path(find.package("metaclipcc"), "gcm_table.csv"),
             stringsAsFactors = FALSE, na.strings = "")
    if (isTRUE(names.only)) out <- out$name
    return(out)
}

#' @title Table of IPCC Atlas Variables
#' @description Shows the internal table with IPCC Variables and relevant metadata
#' @param names.only Logical flag indicating that only the variable names are shown. Default to \code{TRUE}. Otherwise, the full metadata table is shown
#' @export
#' @author J Bedia
#' @return Either a vector with all dataset names (when \code{names.only = TRUE}) or a \code{data.frame} with all the associated metadata.
#' @keywords internal
#' @export
#' @importFrom utils read.csv
#' @examples showIPCCvars()

showIPCCvars <- function(names.only = TRUE) {
    out <- read.csv(file.path(find.package("metaclipcc"), "var_table.csv"),
                    stringsAsFactors = FALSE, na.strings = "")
    if (isTRUE(names.only)) out <- out$variable
    return(out)
}
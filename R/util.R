##     util.R METACLIP internal utlities for metaclipcc package
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

#' @title Update reference dataset table
#' @description Update var/index availability in master table from a curated reference JSON
#' @param projects projects vector string (several allowed). Currently just \code{"CMIP5"} and \code{"CMIP6"}.
#' @param out.file Full path to output file table. Default to "/tmp/dataset_table.csv". See Details.
#' @details The function will replace the appropriate fields within a copy of the master table dataset_table.csv
#' in the inst folder, and store it in the target directory (writing permission is assumed).
#'
#' Note that the dataset_table.csv in the inst folder will have to be replaced manually afterwards, and the package
#' compiled for a new updated version.
#'
#' The reference JSON storing the curated version of the IA ensemble members is at
#' \url{https://gitlab.predictia.es/ipcc/data-homogenizers/-/blob/master/total_members_curated.json}.
#' An up-to-date copy of this file is stored in 'inst/total_members_curated.json'.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>% %<>%
#' @importFrom utils write.table
#' @keywords internal
#' @author J. Bedia

update_dataset_table <- function(projects =  c("CMIP5", "CMIP6"),
                                 out.file = "/tmp/dataset_table.csv") {

    projects <- match.arg(projects, choices = c("CMIP5", "CMIP6"), several.ok = TRUE)

    # Import master lookup table
    master.table <- showIPCCdatasets(names.only = FALSE)

    # Import Master JSON
    json <- system.file("total_members_curated.json",
                        package = "metaclipcc") %>% fromJSON(flatten = TRUE)

    for (k in 1:length(projects)) {
        project <- projects[k]
        # Names in dataset master table (inst/dataset_table.csv)
        if (project == "CMIP6") {
            vars <- c("meanpr","tas", "TN","TX","wind","prsn",
                      "TXx", "TNn","Rx1day","Rx5day","CDD", "spi6",
                      "tx35", "tx35isimip", "tx40", "tx40isimip",
                      "hdd", "fd", "fdisimip", "cd") # The last one is cdd in JSON

            # Solo para CMIP6, prsn y wind vienen de Amon. Tb hay de oceano.
            var.realm <- c("Atmos", "Atmos", "Atmos", "Atmos", "Amon", "Amon",
                           "Atmos", "Atmos", "Atmos", "Atmos", "Atmos",
                           "Atmos", "Atmos", "Atmos", "Atmos", "Atmos",
                           "Atmos", "Atmos", "Atmos", "Atmos")

            json.proj <- gsub("Atmos",
                              "CMIP6",
                              var.realm) %>% gsub("Amon",
                                                  "CMIP6Amon", .) %>% gsub("Omon",
                                                                           "CMIP6Omon", .)

        } else if (project == "CMIP5") {
            vars <- c("meanpr","tas", "TN","TX",
                      "TXx", "TNn","Rx1day","Rx5day","CDD",
                      "tx35", "tx35isimip", "tx40", "tx40isimip",
                      "hdd", "fd", "fdisimip", "cd") # The last one is cdd in JSON

            json.proj <- rep("CMIP5", length(vars))

        }

        # Filter entries by project
        ind <- which(master.table$Project == project)
        master.aux <- master.table[ind, ]

        # Loop over variables/scenarios
        for (i in 1:length(vars)) {

            # Filter JSON by project
            json.ref <- json.proj[i]
            json.aux <- json[[json.ref]]

            # Replace cdd by cd in JSON
            names(json.aux) %<>% gsub("cdd", "cd", .)
            var <- vars[i]
            message("[", Sys.time(), "] Processing \'", var, "\'")

            # Loop over scenarios
            scenarios <- names(json.aux[[var]])
            for (j in 1:length(scenarios)) {
                scen <- scenarios[j]
                message("\t -----", scen)
                # Curated members
                curated.models <- json.aux[[var]][[scen]]
                ind1 <- grep(scen, master.aux$Experiment, ignore.case = TRUE)
                master.models <- paste(master.aux[ind1, "GCM"], master.aux[ind1, "Run"], sep = "_")
                if (length(curated.models) == 0) { # Empty scenario
                    avail <- rep(0L, length(master.models))
                } else {
                    avail <- vapply(1:length(master.models), FUN.VALUE = integer(1L), function(x) {
                        grepl(master.models[x], curated.models, ignore.case = TRUE) %>% any() %>% as.integer()
                    })
                }
                master.aux[ind1,var] <- avail
            }
        }
        master.table[ind, ] <- master.aux
    }
    message("*******************************************************************\nThe master table records for ",
            paste(projects, collapse = ", "),
            " have been updated and stored at:\n",
            out.file,
            "\n*******************************************************************")
    write.table(master.table, file = out.file,  row.names = FALSE, sep = ",")
}

#
# update_dataset_table()
# update_dataset_table(project = "CMIP6")


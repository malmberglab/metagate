#
#  MetaGate
# 
#  R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Eivind Heggernes Ask, University Of Oslo and Oslo University Hospital
# 
#  License terms and conditions:
#  https://github.com/malmberglab/metagate/blob/main/LICENSE.pdf
#  


#' metagate
#'
#' @name metagate
#' @docType package
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @import shiny 
#' @importFrom shinyjs useShinyjs runjs addClass reset
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput renderDT
#' @importFrom plyr rbind.fill
#' @importFrom reshape2 melt
#' @importFrom XML xmlTreeParse xmlValue xmlRoot xpathApply xmlGetAttr saveXML free
#' @importFrom dunn.test dunn.test
#' @importFrom parallel makeCluster clusterExport parLapplyLB detectCores stopCluster
#' @importFrom pbapply pboptions pblapply
#' @import flowCore
#' @import flowWorkspace
#' @importFrom CytoML open_flowjo_xml fj_ws_get_samples flowjo_to_gatingset
#' @importFrom bit bit as.bit
#' @importFrom openxlsx write.xlsx read.xlsx createStyle
#' @importFrom viridis scale_color_viridis scale_fill_viridis



.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n",
                        "Welcome to metagate ", packageDescription("metagate", fields = "Version"), "!\n",
                        "\n",
                        "Visit metagate.malmberglab.com for instructions.\n",
                        "License terms and conditions:\n",
                        "https://github.com/malmberglab/metagate/blob/main/LICENSE.pdf\n\n",
                        "Start the user interface by entering: run_metagate()\n"
  )
}

.onLoad <- function(...) {
  shiny::addResourcePath("www", system.file("www", package = "metagate"))
}

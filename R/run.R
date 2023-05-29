#' Run shiny app
#'
#' This function launches the graphical user interface in the web browser.

#' @param debug Run metagate in debug mode. If `TRUE`, more information will be shown in the R console, and raw data 
#'   can be accessed in the "Debug" menu inside MetaGate. Defaults to `getOption("metagate.debug")`, if set, or `FALSE` 
#'   if not.
#' @param modules Character vector with names of custom analysis modules that should be loaded. Defaults to
#'   `getOption("metagate.modules")`.
#' @param populations The number of populations that can be defined when importing data. Setting a high number may
#'   impact performance. Defaults to `getOption("metagate.populations")`, if set, or `256` if not.
#' @param groups The number of groups that can be defined during analysis. Setting a high number may impact 
#'   performance. Defaults to `getOption("metagate.groups")`, if set, or `20` if not.
#' @param asterisk_limits Numerical vector of cut-offs for displaying p values as asterisks. A p value will be shown as
#'   n asterisks if below or equal to the n'th value of this vector. Defaults to 
#'   `getOption("metagate.asterisk_limits")`, if set, or `c(0.05, 0.01, 0.001, 0.0001)` if not.
#' @param posthoc_limit In statistical comparisons of more than two groups, post-hoc testing will only be performed if
#'   initial analysis of variance yields a p value below or equal to this limit. Defaults to 
#'   `getOption("metagate.posthoc_limit")`, or `0.05` if not.
#'   if set, or `0.05` if not.
#' @param port Shiny web server port. If not set, a random port is used.
#' @param file MetaGate file to load automatically upon launch. Defaults to getOption("metagate.file").
#' @param disable Character vector with names of features that should be disabled. Defaults to 
#'   getOption("metagate.disable"). Could include the following:
#'   * `open_saved`: User will not be allowed to load saved projects.
#'   * `import_flowjo`: Creating new projects based on FlowJo workspaces will not be allowed.
#'   * `import_cytobank`: Creating new projects based on Cytobank gating files will not be allowed.
#'   * `merge_projects`: Disable project merging feature.
#'   * `save_project`: User will not be allowed to save projects as MetaGate files.
#'   * `close_project`: User will not be allowed to close the current project.
#' @keywords metagate
#' @export
#' @examples
#' \dontrun{
#' # Launch MetaGate with default settings
#' run_metagate()
#' 
#' # Automatically load a project upon MetaGate launch, and allow only analysis 
#' # within this project:
#' run_metagate(file = "/path/to/my/metagate/file.metagate",
#'              disable = c("open_saved", "import_flowjo", "import_cytobank",
#'                          "merge_projects", "save_project", "close_project"))
#' }


run_metagate <- function(debug           = getOption("metagate.debug", FALSE),
                         modules         = getOption("metagate.modules"),
                         populations     = getOption("metagate.populations", 256),
                         groups          = getOption("metagate.groups", 20),
                         asterisk_limits = getOption("metagate.asterisk_limits", c(0.05, 0.01, 0.001, 0.0001)),
                         posthoc_limit   = getOption("metagate.posthoc_limit", 0.05),
                         port            = NULL,
                         file            = getOption("metagate.file"),
                         disable         = getOption("metagate.disable")) {

  options(list(
    metagate.current_debug           = debug,
    metagate.current_modules         = modules,
    metagate.current_populations     = populations,
    metagate.current_groups          = groups,
    metagate.current_asterisk_limits = asterisk_limits,
    metagate.current_posthoc_limit   = posthoc_limit,
    metagate.current_file            = file,
    metagate.current_disable         = disable
  ))

  return(shiny::shinyApp(ui      = shiny_ui,
                         server  = shiny_server,
                         options = list(port = port)))
}


setting <- function(name) {
  settings <- list("allowed_cytobank_versions" = c("2.0"),
                   "project_file_extension"    = "metagate")

  value <- getOption(paste0("metagate.current_", name))

  if (is.null(value)) {
    value <- settings[[name]]
  }

  if (name == "modules") {
    return(c("barplot", "heatmap", "volcano", value))
  } else {
    return(value)
  }
}


allow <- function(feature) {
  disable <- setting("disable")
  return(is.null(disable) || !(feature %in% disable))
}

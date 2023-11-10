#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


shiny_server <- function(input, output, session) {
  

  # ==  SETTINGS  ============================================================
  
  options(stringsAsFactors     = FALSE,
          scipen               = 20,
          shiny.maxRequestSize = 8 * 1024^3,
          shiny.reactlog       = FALSE)



  # ==  APPLICATION STATE  ===================================================

  #   Can be one of the following: "", "project_loaded", "gate_file_loaded", "populations_defined", "fcs_uploaded"
  session$userData$application_state <- reactiveVal("")

 

  # ==  DATA STORAGE  ========================================================

  # gate_data
  #   List of gates
  #   id        Int. Unique identifier.
  #   name      String. Name given in FlowJo or Cytobank
  #   formatted_name  String. Formatted name.
  session$userData$gate_data <- reactiveVal(data.frame())


  # parameter_data
  # List of parameters found in FCS files
  # name           String. Parameter name (description in FCS)
  # found          Int. Number of files that have this parameter
  # missing        Int. Number of files in which this parameter is not found
  # total          Int. Total number of files
  # missing_files  String. A list of file names where this parameter is not found, separated by ", "
  # mean           Boolean. Whether mean is calculated for this parameter.
  # median         Boolean. Whether median is calculated for this parameter.
  # geomean        Boolean. Whether geometric mean is calculated for this parameter.
  # transform      String. "none", "arcsinh" or "flowjo"
  # cofactor       Int.
  session$userData$parameter_data <- reactiveVal(data.frame())


  # population_data
  # List of populations defined by the user
  # name   String.
  # query  String. Gate names followed by + or -. Separated by " ".
  session$userData$population_data <- reactiveVal(data.frame())


  # fcs_data
  # List of FCS files included in the project
  # name         String. File name.
  # FIL          String. $FIL from FCS file.
  # cytometer    String. From FCS file.
  # FCS_version  String. E.g. "3.0"
  # date         String. From FCS file.
  # sampleID     Int. If project is based on FlowJo workspace, this refers to FlowJo sampleID. Otherwise, arbitrary
  #              unique ID.
  # events       Int. Number of events.
  # missing      Int. Number of missing readouts.
  session$userData$fcs_data <- reactiveVal(data.frame())


  # stat_data
  # The actual data.
  # Columns are named by fcs_data$name
  # Row names are readout names.
  session$userData$stat_data <- reactiveVal(matrix(nrow = 0, ncol = 0))


  # meta_data
  # The meta data that the user can edit.
  # Columns are meta variables. Must contain "file", which refers to the FCS file name (not $FIL) of this sample.
  # Rows: one for each sample.
  session$userData$meta_data <- reactiveVal(data.frame())


  # project_data
  # Named list that stores some basic information about the project and some user settings.
  session$userData$project_data <- reactiveVal(list())


  # group_data
  # Saved groups. During analysis, group data is typically fecthed directly from the text/select fields.
  # id
  # name
  # query
  session$userData$group_data <- reactiveVal(list())



  # ==  IMPORT REACTIVES  ====================================================

  session$userData$import_gate_data             <- reactiveVal(data.frame())  # set before gate_file_loaded
  session$userData$import_gatingML              <- reactiveVal(NULL)          # set before gate_file_loaded (Cytobank)
  session$userData$import_flowjo_file_names     <- reactiveVal(NULL)          # set before gate_file_loaded (FlowJo)
  session$userData$import_flowjo_workspace_path <- reactiveVal(NULL)          # set before gate_file_loaded (FlowJo)
  session$userData$import_import_file_name      <- reactiveVal(NULL)          # set before gate_file_loaded
  session$userData$import_population_data       <- reactiveVal(data.frame())  # set before populations_defined
  session$userData$import_parameter_list        <- reactiveVal(data.frame())  # set before fcs_uploaded
  session$userData$import_fcs_data              <- reactiveVal(data.frame())  # set before fcs_uploaded




  # ==  OTHER SHARED REACTIVES  ==============================================

  # session$userData$group_data() will be updated from the inputs on the Group tab if this is set to TRUE.
  # Is set to TRUE when the Group tab is loaded, or when meta_data is uploaded.
  session$userData$group_editor_changed <- reactiveVal(FALSE)
  session$userData$group_editor_loaded  <- reactiveVal(FALSE)

  session$userData$group_name_list     <- reactive({ get_group_name_list(session$userData$group_data()) })
  session$userData$readout_types       <- reactive({ get_readout_types(session$userData$stat_data()) })
  session$userData$readout_populations <- reactive({ get_readout_populations(session$userData$stat_data()) })
  session$userData$count_data          <- reactive({
    get_absolute_counts(stat_data    = session$userData$stat_data(),
                        meta_data    = session$userData$meta_data(),
                        project_data = session$userData$project_data())
  })



  # ==  UI FUNCTIONS  ========================================================

  ui_base_functions <- c("basic", "start", "group", "help", "import", "import_flowjo", "import_cytobank",
                         "import_populations", "import_fcs", "import_parameters", "meta", "project")

  for (fun in c(ui_base_functions, setting("modules"))) {
    do.call(paste0(fun, "_ui"), list(input, output, session))
  }


  # ==  LOAD PROJECT  ========================================================

  if (!is.null(setting("file"))) {
    file <- load_metagate_file(setting("file"))
  
    if (is.null(file$error)) {
      session$userData$gate_data(file$data$gate_data)
      session$userData$parameter_data(file$data$parameter_data)
      session$userData$population_data(file$data$population_data)
      session$userData$fcs_data(file$data$fcs_data)
      session$userData$stat_data(file$data$stat_data)
      session$userData$meta_data(file$data$meta_data)
      session$userData$project_data(file$data$project_data)
      session$userData$group_data(file$data$group_data)
      session$userData$group_editor_loaded(FALSE)
      session$userData$application_state("project_loaded")
    } else {
      stop("Could not load file: ", file$error)
    }
  }


}


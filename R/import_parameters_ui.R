import_parameters_ui <- function(input, output, session) {
  
  output$import_parameters_ui <- renderUI({
    tagList(
      h2("Choose markers"),

      p("Select which markers you want to include in your analysis and how values should be transformed.",
        style = "margin-bottom: 24px"),

      fluidRow(class = "marker_selection_header",
        column(width = 1, ""),
        column(width = 3, label("Name",
                                "This is the name that will be displayed in all plots and tables. Therefore, it is",
                                "recommended to make them short and clear.\n\nIf you give two parameters the same",
                                "name, they will be treated as one parameter.")),
        column(width = 2, "Marker"),
        column(width = 2, "Tag"),
        column(width = 1, label("Samples", "In how many samples was the parameter found.")),
        column(width = 1, label("Transform",
                                "If you select a transformation method, data will be transformed before mean, median",
                                "and geometric mean values are calculated. If Linear is selected, no transformation",
                                "will be applied.\n\nTransformation will not affect gating.")),
        column(width = 1, label("Cofactor", "Cofactor for arcsinh transformation (transformed = asinh(x/cofactor)).")),
        column(width = 1, label("Apply", "Apply these transformation settings to all parameters."))
      ),

      uiOutput("import_marker_selection_ui"),

      fluidRow(style = "margin-top: 35px",
        column(width = 3,
          label("Event limit",
                "If a population has fewer events than this limit, no readouts will be calculated in this population."),
          numericInput(inputId = "import_event_limit",
                       label   = NULL,
                       value   = 50,
                       min     = 1,
                       max     = NA,
                       step    = 1,
                       width   = "100%")
        ), # /column
        column(width = 3,
          label("Cores",
                "The maximum number of cores used for calculations.",
                "The default setting is half of the number of available cores."),
          sliderInput(inputId = "import_cores",
                      label   = NULL,
                      value   = ceiling(parallel::detectCores() / 2),
                      max     = parallel::detectCores(),
                      min     = 1,
                      step    = 1,
                      width   = "100%")
        ), # /column
        column(width = 3,
          label("Progress indicator",
                "A progress indicator will be displayed in the R console, but calculation will be slower."),
          checkboxInput(inputId = "import_progress_bar",
                        label   = "Show progress indicator",
                        value   = FALSE)
        ) # /column
      ), # /fluidRow
      actionButton(inputId = "import_parameters_back_button",
                   label   = "Back",
                   icon    = icon("arrow-left", lib = "glyphicon"),
                   style   = "margin: 20px 0 20px 0"),

      actionButton(inputId = "import_start_parsing_button",
                   label   = "Start parsing",
                   icon    = icon("ok", lib = "glyphicon"),
                   style   = "margin: 20px 0 20px 8px",
                   class   = "main_button")
    )
  })
  

  output$import_marker_selection_ui <- renderUI({
    parameter_list <- session$userData$import_parameter_list()

    return(do.call(tagList, lapply(row.names(parameter_list), function(row) {
      fluidRow(class = "marker_selection_body",
        column(width = 1, class = "marker_selection_included_column",
          checkboxInput(inputId = paste0("import_marker_selection_included_", row),
                        label   = NULL,
                        value   = TRUE)
        ), # /column
        column(width = 3,
          textInput(inputId = paste0("import_marker_selection_title_", row),
                    label   = NULL,
                    value   = parameter_list[row, "title"])
        ), # /column
        column(width = 2,
          tags$span(parameter_list[row, "desc"])
        ), # /column
        column(width = 2,
          tags$span(parameter_list[row, "name"])
        ), # /column
        column(width = 1,
          HTML(paste0("<span class='help-text' title='",
                      if (parameter_list[row, "missing_files"] == "") {
                        "This marker was found in all samples"
                      } else {
                        paste0("This marker was not found in the following ", parameter_list[row, "missing"],
                               " samples:\n\n", parameter_list[row, "missing_files"])
                      },
                      "', style='font-weight: bold; color: ",
                      if (parameter_list[row, "missing"] == 0) { "green" } else { "red" }, "'>",
                      parameter_list[row, "found"], " of ", parameter_list[row, "total"], "</span>"))
        ), # /column
        column(width = 1, class = "marker_selection_transform",
          selectInput(inputId   = paste0("import_marker_selection_transform_", row),
                      label     = NULL,
                      choices   = c("Linear"         = "none",
                                    "Arcsinh"        = "arcsinh",
                                    "FlowJo logicle" = "flowjo"),
                      selected  = "none",
                      multiple  = FALSE,
                      selectize = FALSE,
                      width     = "100%")
        ), # /column
        column(width = 1, class = "marker_selection_cofactor",
          numericInput(inputId = paste0("import_marker_selection_cofactor_", row),
                       label   = NULL,
                       value   = 5,
                       min     = 0.01,
                       max     = NA,
                       step    = 1,
                       width   = "100%")
        ), # /column
        column(width = 1,
          tags$span("Apply to all",
                    class = "marker_selection_apply",
                    onclick = paste0("marker_selection_apply('", row, "')"))
        ) # /column
      )
    })))
  })


  observeEvent(input$import_parameters_back_button, {
    session$userData$application_state("populations_defined")

    session$userData$import_parameter_list(data.frame())
    session$userData$import_fcs_data(data.frame())
  })



    # Start the FCS file parsing
  observeEvent(input$import_start_parsing_button, {
    showModal(modalDialog(p("Please wait while your FCS are parsed. ",
                            "Parsing time depends on your computer as well as the number of samples, ",
                            "populations, markers and cells."),
                          title     = "Parsing files",
                          footer    = NULL,
                          easyClose = FALSE))

    parameter_list <- session$userData$import_parameter_list()

    for (row in row.names(parameter_list)) {
      parameter_list[row, "title"]     <- input[[paste0("import_marker_selection_title_", row)]]
      parameter_list[row, "mean"]      <- input[[paste0("import_marker_selection_included_", row)]]
      parameter_list[row, "median"]    <- input[[paste0("import_marker_selection_included_", row)]]
      parameter_list[row, "geomean"]   <- input[[paste0("import_marker_selection_included_", row)]]
      parameter_list[row, "transform"] <- input[[paste0("import_marker_selection_transform_", row)]]
      parameter_list[row, "cofactor"]  <- if (parameter_list[row, "transform"] == "arcsinh") {
                                            as.numeric(input[[paste0("import_marker_selection_cofactor_", row)]])
                                          } else {
                                            NA
                                          }
    }

    duplicate_marker_names <- import_check_parameter_list_duplicates(parameter_list)

    if (length(duplicate_marker_names) > 0) {
      removeModal()
      show_error_modal(single   = "The following marker names are used for two or more markers in some files:",
                       multiple = duplicate_marker_names)
    } else {
      result <- fcs_parse_files(gate_data             = session$userData$import_gate_data(),
                                gatingML              = session$userData$import_gatingML(),
                                flowjo_workspace_path = session$userData$import_flowjo_workspace_path(),
                                fcs_data              = session$userData$import_fcs_data(),
                                parameter_list        = parameter_list,
                                population_data       = session$userData$import_population_data(),
                                event_limit           = input$import_event_limit,
                                gate_file             = session$userData$import_import_file_name(),
                                n_cores               = min(detectCores(), input$import_cores,
                                                            length(input$import_fcs_upload$name)),
                                show_progress_bar     = input$import_progress_bar)

      if (!is.null(result$errors)) {
        removeModal()
        show_error_modal(single   = paste("An unexpected error occurred while parsing the files below.",
                                          "This could happen if you use FCS files that come directly from the",
                                          "cytometer. If this is the case, try starting your analysis with FCS files ",
                                          "exported from FlowJo or Cytobank."),
                         multiple = result$errors)
      } else {
        session$userData$stat_data(result$stat_data)
        session$userData$fcs_data(result$fcs_data)
        session$userData$meta_data(result$meta_data)
        session$userData$parameter_data(parameter_list)
        session$userData$project_data(result$project_data)
        session$userData$gate_data(session$userData$import_gate_data())
        session$userData$population_data(session$userData$import_population_data())
        session$userData$group_data(list(list(id = 1, name = "All samples", query = NULL)))
      
        removeModal()
        session$userData$group_editor_loaded(FALSE)
        session$userData$application_state("project_loaded")
    
        # Reset all reactives
        session$userData$import_gate_data(data.frame())
        session$userData$import_gatingML(NULL)
        session$userData$import_flowjo_file_names(NULL)
        session$userData$import_flowjo_workspace_path(NULL)
        session$userData$import_import_file_name(NULL)
        session$userData$import_population_data(data.frame())
        session$userData$import_parameter_list(data.frame())
        session$userData$import_fcs_data(data.frame())
    
        if ("messages" %in% names(result) && length(result$messages) > 0) {
          showModal(modalDialog(p("FCS file processing completed, but some warnings occurred. ",
                                  "A possible reason for this is that the FlowJo/Cytobank file does not match the ",
                                  "FCS files or that some gates were not defined for all samples."),
                                p("The project can still be analyzed, but keep in mind that some populations will ",
                                  "be missing for some samples."),
                                tags$ul(lapply(lapply(result$messages, HTML), tags$li)),
                                title  = "Processing completed with warnings",
                                footer = modalButton("Close")))
        }
      }
    }
  })

}
project_ui <- function(input, output, session) {

  # ==  RENDER UI  ===========================================================

  output$project_ui <- renderUI({
    tabs <- list(
      tabPanel("Summary",     tableOutput("project_summary_table")),
      tabPanel("Parameters",  uiOutput("project_parameter_ui")),
      tabPanel("FCS files",   uiOutput("project_fcs_ui")),
      tabPanel("Populations", uiOutput("project_population_ui")),
      tabPanel("Gates",       uiOutput("project_gate_ui"))
    )

    if (setting("debug"))
      tabs <- c(tabs, list(tabPanel("Debug", br(), uiOutput("project_debug_output"))))


    button_row <- fluidRow(
      column(width = 6,
        if (!allow("save_project") && !allow("close_project")) {
          p(style = "font-style: italic", HTML(
            "<strong>MetaGate is currently running in demonstration mode.</strong><br>",
            "This means that you are not allowed to create or load other projects."))
        } else {
          tagList(
            if (allow("save_project")) {
              actionButton(inputId = "project_save",
                           label   = "Save project",
                           icon    = icon("floppy-disk", lib = "glyphicon"),
                           style   = "margin-right: 8px")
            },
            if (allow("close_project")) {
              actionButton(inputId = "project_close",
                           label   = "Close project",
                           icon    = icon("off", lib = "glyphicon"))
            }
          )
        }
      ),
      column(width = 6, style = "text-align: right; margin-bottom: 28px",
        actionButton(inputId = "project_change_name",
                     label   = "Change project name",
                     icon    = icon("pencil", lib = "glyphicon"),
                     style   = "margin-right: 8px")
      ) 
    )

    tagList(
      uiOutput("project_top_ui"),
      button_row,
      do.call(tabsetPanel, tabs)
    )
  })


  output$project_top_ui <- renderUI({
    time_saved <- project_get_data(session = session, name = "time_saved")
    if (!is.null(time_saved) && is.na(time_saved)) {
      div(class = "message_warning message_warning_top",
          p("You have not saved your project yet. It is recommended that you save the project as soon as possible."))
    }
  })


  output$project_parameter_ui <- renderUI({
    parameter_table <- tableOutput("project_parameter_table")
    gate_files <- unique(session$userData$parameter_data()$gate_file)
    if (!is.null(gate_files) && length(gate_files) > 1) {
      return(tagList(
        selectInput(inputId   = "project_parameter_gate_file",
                    label     = "Show parameters for gate file:",
                    choices   = gate_files,
                    multiple  = FALSE,
                    selectize = TRUE),
        parameter_table
      ))
    } else {
      return(parameter_table)
    }
  })


  output$project_fcs_ui <- renderUI({
    fcs_table <- tableOutput("project_fcs_table")

    gate_files <- unique(session$userData$fcs_data()$gate_file)
    if (!is.null(gate_files) && length(gate_files) > 1) {
      return(tagList(
        selectInput(inputId   = "project_fcs_gate_file",
                    label     = "Show FCS files for gate file:",
                    choices   = gate_files,
                    multiple  = FALSE,
                    selectize = TRUE),
        fcs_table
      ))
    } else {
      return(fcs_table)
    }
  })


  output$project_population_ui <- renderUI({
    population_table <- tableOutput("project_population_table")
    gate_files <- unique(session$userData$population_data()$gate_file)
    if (!is.null(gate_files) && length(gate_files) > 1) {
      return(tagList(
        selectInput(inputId   = "project_population_gate_file",
                    label     = "Show populations for gate file:",
                    choices   = gate_files,
                    multiple  = FALSE,
                    selectize = TRUE),
        population_table
      ))
    } else {
      return(population_table)
    }
  })


  output$project_gate_ui <- renderUI({
    gate_table <- tableOutput("project_gate_table")
    gate_files <- unique(session$userData$gate_data()$gate_file)
    if (!is.null(gate_files) && length(gate_files) > 1) {
      return(tagList(
        selectInput(inputId   = "project_gate_gate_file",
                    label     = "Show gates for gate file:",
                    choices   = gate_files,
                    multiple  = FALSE,
                    selectize = TRUE),
        gate_table
      ))
    } else {
      return(gate_table)
    }
  })


  output$project_debug_output <- renderUI({
    format_debug_output <- function(data) {
      return(tags$pre(paste(capture.output(print(data)), collapse = "\n")))
    }

    tabsetPanel(
      tabPanel("Gates",            format_debug_output(session$userData$gate_data())),
      tabPanel("Parameters",       format_debug_output(session$userData$parameter_data())),
      tabPanel("Populations",      format_debug_output(session$userData$population_data())),
      tabPanel("FCS files",        format_debug_output(session$userData$fcs_data())),
      tabPanel("Statistics",       downloadButton(outputId = "project_debug_download_data",
                                                  label    = "Download percent, mean, median, geometric mean data"),
                                   br(), br(),
                                   downloadButton(outputId = "project_debug_download_count_data",
                                                  label    = "Download absolute count data")),
      tabPanel("Meta data",        format_debug_output(session$userData$meta_data())),
      tabPanel("Project settings", format_debug_output(session$userData$project_data())),
      tabPanel("Groups",           format_debug_output(session$userData$group_data()))
    )
  })


  output$project_population_table <- renderTable({
    population_data <- session$userData$population_data()
    if ("gate_file" %in% names(population_data)) {
      population_data <- population_data[population_data$gate_file == input$project_population_gate_file, ]
    }
    return(project_create_population_table(population_data = population_data))
  }, sanitize.text.function = function(x) x)



  # ==  RENDER TABLE  ========================================================
  
  output$project_summary_table <- renderTable({
    return(project_create_summary_table(project_data = session$userData$project_data()))
  }, colnames = FALSE, rownames = TRUE)
  
  
  output$project_parameter_table <- renderTable({
    parameter_data <- session$userData$parameter_data()

    if ("gate_file" %in% names(parameter_data)) {
      parameter_data <- parameter_data[parameter_data$gate_file == input$project_parameter_gate_file, ]
    }
    return(project_create_parameter_table(parameter_data = parameter_data))
  })
  

  output$project_fcs_table <- renderTable({
    fcs_data <- session$userData$fcs_data()
    if ("gate_file" %in% names(fcs_data)) {
      fcs_data <- fcs_data[fcs_data$gate_file == input$project_fcs_gate_file, ]
    }
    return(project_create_fcs_table(fcs_data      = fcs_data,
                                    readout_count = nrow(session$userData$stat_data())))
  })
  

  output$project_gate_table <- renderTable({
    gate_data <- session$userData$gate_data()
    if ("gate_file" %in% names(gate_data)) {
      gate_data <- gate_data[gate_data$gate_file == input$project_gate_gate_file, ]
    }
    return(project_create_gate_table(gate_data = gate_data))
  })



  # ==  DOWNLOAD HANDLERS  ===================================================

  output$project_save_download <- downloadHandler(
    filename = function() {
      name <- input$project_save_name
      if (nchar(name) < 1)
        name <- "untitled"

      return(paste0(name, ".", setting("project_file_extension")))
    },
    content = function(file) {
      project_set_data(session = session, name = "time_saved", value = Sys.time())
      project_set_data(session = session, name = "version_saved",
                       value = packageDescription("metagate", fields = "Version"))
  
      save <- list(gate_data       = session$userData$gate_data(),
                   parameter_data  = session$userData$parameter_data(),
                   population_data = session$userData$population_data(),
                   fcs_data        = session$userData$fcs_data(),
                   stat_data       = session$userData$stat_data(),
                   meta_data       = session$userData$meta_data(),
                   project_data    = session$userData$project_data(),
                   group_data      = session$userData$group_data())

      save(save, file = file)
  
      removeModal()
      showModal(modalDialog("Your project file was successfully downloaded.",
                            title  = "Save project",
                            footer = actionButton(inputId = "project_save_download_ok", label = "Close")))
    }
  )


  output$project_debug_download_data <- downloadHandler(
    filename = function() { paste0(project_get_name(session), ".xlsx")},
    content = function(file) {
      download_xlsx_file(as.data.frame(session$userData$stat_data()), file = file, row_names = TRUE)
    }
  )


  output$project_debug_download_count_data <- downloadHandler(
    filename = function() { paste0(project_get_name(session), " counts.xlsx")},
    content = function(file) {
      download_xlsx_file(as.data.frame(session$userData$count_data()), file = file, row_names = TRUE)
    }
  )


  # ==  OBSERVE EVENT  =======================================================

  observeEvent(input$project_close, {
    showModal(modalDialog("Are you sure you want to close this project? All unsaved changes will be lost.",
                          title  = "Close project",
                          footer = tagList(modalButton("Cancel"),
                                           actionButton(inputId = "project_close_ok",
                                                        label = "Close the project"))))
  })
  
  observeEvent(input$project_close_ok, {
    session$userData$gate_data(data.frame())
    session$userData$parameter_data(data.frame())
    session$userData$population_data(data.frame())
    session$userData$fcs_data(data.frame())
    session$userData$stat_data(matrix(nrow = 0, ncol = 0))
    session$userData$meta_data(data.frame())
    session$userData$project_data(list())
    session$userData$group_data(list())
    session$userData$application_state("")
    removeModal()
  })

  observeEvent(input$project_change_name, {
    showModal(project_create_name_modal(project_get_name(session)))
  })
  
  
  observeEvent(input$project_change_name_ok, {
    new_name <- input$project_change_name_input
    if (nchar(new_name) > 2) {
      project_set_data(session = session, name = "project_name", value = new_name)
      removeModal()
    } else {
      showModal(project_create_name_modal(project_get_name(session), failed = TRUE))
    }
  })
  
  
  # Download project file
  observeEvent(input$project_save, {
    showModal(project_create_save_modal(project_get_name(session)))
  })


  observeEvent(input$project_save_download_ok, {
    removeModal()
  })

}





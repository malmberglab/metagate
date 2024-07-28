#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


start_ui <- function(input, output, session) {

  # ==  RENDER UI  ===========================================================

  output$start_ui <- renderUI({
    ui <- div(class = "welcome_text",
              h2("Welcome to MetaGate!"),
              p("To get started, you need to import data from either Cytobank or FlowJo."),
              p("Please select your preferred platform below, and follow the instructions."))

    if (allow("import_flowjo")) {
      ui <- tagList(ui, actionButton(inputId = "import_select_flowjo",
                                     label   = "Create new project (FlowJo)",
                                     icon    = icon("plus-sign", lib = "glyphicon"),
                                     class   = "large_button"))
    }

    if (allow("import_cytobank")) {
      ui <- tagList(ui, actionButton(inputId = "import_select_cytobank",
                                     label   = "Create new project (Cytobank)",
                                     icon    = icon("plus-sign", lib = "glyphicon"),
                                     class   = "large_button"))
    }

    if (allow("open_saved")) {
      ui <- tagList(ui,
        div(class = "load_item",
            tags$h3("Load project"),
            fileInput(inputId  = "load_file_upload",
                      label    = "Select a MetaGate file to load a project:",
                      accept   = c("application/x-gzip", paste0(".", setting("project_file_extension"))),
                      multiple = FALSE))
      )
    }

    if (allow("merge_projects")) {
      ui <- tagList(ui,
        div(class = "load_item",
            tags$h3("Merge projects"),
            fileInput(inputId  = "merge_files_upload",
                      label    = "Select multiple MetaGate files to merge into one file:",
                      accept   = c("application/x-gzip", paste0(".", setting("project_file_extension"))),
                      multiple = TRUE))
      )
    }

    return(ui)      
  })




  # ==  OBSERVE EVENT  =======================================================

  observeEvent(input$import_select_flowjo, {
    session$userData$application_state("flowjo_selected")
  })


  observeEvent(input$import_select_cytobank, {
    session$userData$application_state("cytobank_selected")
  })


  # Upload project file and load project
  observeEvent(input$load_file_upload, {
    if (!allow("open_saved"))
      return(NULL)

    file <- load_metagate_file(input$load_file_upload$datapath)
  
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
      shinyjs::reset("load_file_upload")
      show_error_modal(single = file$error)
    }
  })


  observeEvent(input$merge_files_upload, {
    error <- NULL
  
    paths <- input$merge_files_upload$datapath
    shinyjs::reset("merge_files_upload")

    if (length(paths) < 2) {
      show_error_modal(single = "You need to select at least two files.")
      return(NULL)
    }

    if (!all(endsWith(paths, paste0(".", setting("project_file_extension"))))) {
      show_error_modal(single = paste0("One or more of the selected files are not .",
                                       setting("project_file_extension"), " files."))
      return(NULL)
    }

    show_processing_modal(message = "Please wait while project are being merged.")

    result <- merge_projects(paths)

    if (!is.null(result$error)) {
      if (length(result$error) > 1) {
        show_error_modal(multiple = result$error)
      } else {
        show_error_modal(single = result$error)
      }
    } else {
      ui_merged_file(result$merged)

      if (!is.null(result$messages) && length(result$messages) > 0) {
        messages <- do.call(tags$ul, lapply(result$messages, function(x) { tags$li(x) }))
      } else {
        messages <- NULL
      }

      showModal(modalDialog(
        p("The project file merge was successful!", style = "font-weight: bold"),
        messages,
        title  = "Merge successful",
        footer = downloadButton(outputId = "merge_save_file",
                                label    = "Save merged project file")
      ))
    }
  })


  output$merge_save_file <- downloadHandler(
    filename = function() {
      return(paste0("Merged project", ".", setting("project_file_extension")))
    },
    content = function(file) {
      save <- ui_merged_file()
      ui_merged_file(NULL)
      save(save, file = file)
      removeModal()
    }
  )


  ui_merged_file <- reactiveVal()

}

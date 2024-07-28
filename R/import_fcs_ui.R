#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


import_fcs_ui <- function(input, output, session) { 

  output$import_fcs_ui <- renderUI({
    tagList(
      h2("Upload FCS files"),

      p("Select all the FCS files that you want to include in your project. Make sure that you select all files at ",
        "once, as you will not be able to upload more files later.",
        style = "margin-bottom: 16px"),

      fileInput(inputId  = "import_fcs_upload",
                label    = "Select all FCS files:",
                multiple = TRUE,
                width    = "100%",
                accept   = c(".fcs", ".fcs3")),

      actionButton(inputId = "import_fcs_back_button",
                   label   = "Back",
                   icon    = icon("arrow-left", lib = "glyphicon"))
    )
  })



  # ==  OBSERVE EVENT  =======================================================

  # Upload FCS files and check them
  observeEvent(input$import_fcs_upload, {
    show_processing_modal("Checking FCS files",
                          "Please wait while the FCS files are checked.")

    shinyjs::reset("import_fcs_upload")

    fcs_files <- input$import_fcs_upload
    result <- fcs_check_files(fcs_files         = fcs_files,
                              flowjo_file_names = session$userData$import_flowjo_file_names())

    if (is.null(result$error)) {
      session$userData$import_fcs_data(result$fcs_data)
      session$userData$import_parameter_list(result$parameter_list)
      removeModal()
      session$userData$application_state("fcs_uploaded")
    } else {
      removeModal()
      show_error_modal(single = result$error)
    }
  })


  observeEvent(input$import_fcs_back_button, {
    session$userData$application_state("gate_file_loaded")
  })

}
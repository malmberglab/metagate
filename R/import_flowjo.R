import_flowjo_ui <- function(input, output, session) { 

  output$import_flowjo_ui <- renderUI({
    tagList(
      h2("Create project based on FlowJo gating"),
      actionButton(inputId = "import_flowjo_back_button",
                   label   = "Back",
                   icon    = icon("arrow-left", lib = "glyphicon"),
                   style   = "margin: 7px 0 3px"),

      tags$ol(class = "numbered_instructions",
              tags$li(tags$span("1"),
                      HTML("<p>Make sure you are using FlowJo vX.</p>",
                           "<p>Older versions of FlowJo are not supported by MetaGate.</p>")),
              tags$li(tags$span("2"),
                      HTML("<p>Import your FCS files into an empty FlowJo workspace and gate on your desired ",
                           "<em>base</em> population.</p>",
                           "<p>This population could be for example live, intact, single cells.</p>",
                           "<p>MetaGate works best if you remove debris, doublets and other events you do not ",
                           "need in your analysis. While this is not strictly necessary, it is strongly ",
                           "recommended, as it will improve performance and convenience during your analysis.",
                           "If you still want to skip this, jump straight to step 5.</p>")),
              tags$li(tags$span("3"),
                      HTML("<p>Export this population so that you get a set of new FCS files.</p>",
                           "<p>If you are working with FACS data, perform any compensation matrix adjustments ",
                           "prior to exporting, and select \"All <strong>compensated</strong> parameters\" ",
                           "when exporting.</p>")),
              tags$li(tags$span("4"),
                      HTML("<p>Import your new FCS files into a new FlowJo workspace.</p>")),
              tags$li(tags$span("5"),
                      HTML("<p>Set all the gates that you want in your analysis.</p>",
                           "<p>At a later stage, you will be allowed to combine gates to define ",
                           "<em>populations</em>, so it is not necessary to create gating hierarchies in ",
                           "FlowJo. Additionally, MetaGate will automatically create negative (NOT) gates for ",
                           "all your gates.",
                           "<p style=\"font-style: italic\">Example: Let us say you want \"T cells\" as a ",
                           "population in your analysis, and that you want this to be CD3+ CD14- CD19- CD56-.",
                           "Instead of sequentially sub-gating on all these markers in FlowJo, you can create ",
                           "only the following four gates in FlowJo: \"CD3+\", \"CD14+\", \"CD19+\" and ",
                           "\"CD56+\". Then, MetaGate will later let you define \"T cells\" using combinations ",
                           "of these gates.</p>")),
              tags$li(tags$span("6"),
                      HTML("<p>Make sure that gates have the same names in all samples.</p>",
                           "<p>MetaGate will exclude gates that are not set for all samples. After you upload the",
                           "FlowJo workspace, MetaGate will give you an overview of the gates that are not set for",
                           "all samples.</p>")),
              tags$li(tags$span("7"),
                      HTML("<p>Save the FlowJo workspace.</p>",
                           "<p>Since the gates you set in this file will be the basis for all downstream ",
                           "analysis, it is recommended that you keep a non-editable copy of this in case you ",
                           "want to go back and look at the gates later.</p>")),
              tags$li(tags$span("8"),
                      fileInput(inputId  = "flowjo_file_upload",
                                label    = "Select your FlowJo workspace file (.wsp)",
                                accept   = c("text/xml", ".wsp"),
                                multiple = FALSE,
                                width    = "600px"))
      ) # /ol
    ) # /tagList
  })



  # ==  OBSERVE EVENT  =======================================================


  # Upload workspace file from FlowJo
  observeEvent(input$flowjo_file_upload, {
    if (!allow("import_flowjo"))
      return(NULL)

    show_processing_modal("Parsing FlowJo workspace file",
                          "Please wait while the FlowJo workspace file is being parsed.")
  
    parsed <- flowjo_parse(input$flowjo_file_upload$datapath)

    if (is.null(parsed$error)) {
      session$userData$import_gate_data(parsed$gate_data)
      session$userData$import_flowjo_file_names(parsed$file_names)
      session$userData$import_flowjo_workspace_path(input$flowjo_file_upload$datapath)
      session$userData$import_import_file_name(input$flowjo_file_upload$name)
      
      title <- paste0(nrow(parsed$gate_data), " gates and ", nrow(parsed$file_names), " samples found")
      content <- p(strong("MetaGate found", nrow(parsed$gate_data), "gates that are set for all",
                   nrow(parsed$file_names), "samples."))

      if (!is.null(parsed$missing_gates)) {
        flowjo_missing_gates_overview(parsed$missing_gates)

        content <- tagList(content,
                           hr(),
                           h4("Warning: Missing gates"),
                           p(nrow(parsed$missing_gates), " gates were not set for all samples",
                             "and these will be excluded from the analysis."),
                           p("For a complete overview of missing gates, download the file below and open in Microsoft",
                             "Excel or similar."),
                           downloadButton(outputId = "import_flowjo_missing_gates_download",
                                          label    = "Download missing gates overview"))
      }

      if (nrow(parsed$gate_data) > setting("populations")) {
        content <- tagList(content,
                           hr(),
                           h4("Warning: More gates than allowed populations"),
                           p("The FlowJo workspace has more gates than the maximum number of populations allowed by",
                             "MetaGate", paste0("(", setting("populations"), ")."), "Only the", setting("populations"),
                             "first gates will be added as populations."),
                           p("To allow more populations in MetaGate, start MetaGate using the following code:"),
                           pre("run_metagate(populations = 256)"))
      }

      footer <- tagList(actionButton(inputId = "import_flowjo_reject_button",
                                     label   = "Back",
                                     icon    = icon("arrow-left", lib = "glyphicon")),
                        actionButton(inputId = "import_flowjo_accept_button",
                                     label   = "Continue",
                                     icon    = icon("ok", lib = "glyphicon"),
                                     class   = "main_button"))

      removeModal()
      showModal(modalDialog(title = title, footer = footer, content))
    } else {
      shinyjs::reset("flowjo_file_upload")
      removeModal()

      show_error_modal(single = parsed$error)
    }
  })

  flowjo_missing_gates_overview <- reactiveVal(NULL)

  output$import_flowjo_missing_gates_download <- downloadHandler(
    filename = function() { paste0("Missing gates.xlsx")},
    content = function(file) {
      download_xlsx_file(flowjo_missing_gates_overview(), file, row_names = TRUE)
    }
  )


  observeEvent(input$import_flowjo_accept_button, {
    removeModal()
    session$userData$application_state("gate_file_loaded")
  })


  observeEvent(input$import_flowjo_reject_button, {
    session$userData$import_gate_data(NULL)
    session$userData$import_flowjo_file_names(NULL)
    session$userData$import_flowjo_workspace_path(NULL)
    session$userData$import_import_file_name(NULL)

    removeModal()
    shinyjs::reset("flowjo_file_upload")
  })


  observeEvent(input$import_flowjo_back_button, {
    session$userData$application_state("")
  })



 }
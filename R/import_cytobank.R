#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


import_cytobank_ui <- function(input, output, session) { 

  output$import_cytobank_ui <- renderUI({
    tagList(
      h2("Create project based on Cytobank gating"),
      actionButton(inputId = "import_cytobank_back_button",
                   label   = "Back",
                   icon    = icon("arrow-left", lib = "glyphicon"),
                   style   = "margin: 7px 0 3px"),

      tags$ol(class = "numbered_instructions",
              tags$li(tags$span("1"),
                      HTML("<p>Upload your FCS files to Cytobank.</p>")),
              tags$li(tags$span("2"),
                      HTML("<p>Gate on your desired <em>base</em> population.</p>",
                           "<p>This population could be for example live, intact, single cells.</p>",
                           "<p>MetaGate works best if you remove debris, doublets and other events you do not ",
                           "need in your analysis. While this is not strictly necessary, it is strongly ",
                           "recommended, as it will improve performance and convenience during your analysis. ",
                           "If you still want to skip this, jump straight to step 5.</p>")),
              tags$li(tags$span("3"),
                      HTML("<p>Export this population to a new experiment.</p>",
                           "<p>To do this, select the base population you just created in the Working ",
                           "Illustration. Then, select \"Split files by population\" from the \"Action\" menu. ",
                           "This will create a new experiment.</p>")),
              tags$li(tags$span("4"),
                      HTML("<p>Open the experiment you just created.</p>")),
              tags$li(tags$span("5"),
                      HTML("<p>Set all the gates you need for your analysis.</p>",
                           "<p>MetaGate will only care about <em>gates</em>, not <em>populations</em>.",
                           "You will be allowed to create populations based on your gates later.</p>",
                           "<p>MetaGate will automatically create negative gates (NOT gates) for all your ",
                           "gates, so if it not required by your analysis, there is no need to create both ",
                           "negative and positive gates.</p>")),
              tags$li(tags$span("6"),
                      HTML("<p>Download all FCS files.</p>",
                           "<p>To do this, select \"Export\" > \"Download files\" from the \"Actions\" ",
                           "menu.</p>",
                           "<p>You will upload these files to MetaGate at a later stage.</p>",
                           "<p>Note that these FCS files will only consist of the base population you gated on. ",
                           "Therefore, you cannot use your original FCS files in MetaGate.</p>")),
              tags$li(tags$span("7"),
                      HTML("<p>Download the GatingML file.</p>",
                           "<p>To do this, select \"Export\" > \"Export gating-ML\" from the \"Actions\" ",
                           "menu.</p>",
                           "<p>This is a small file that contains information about the gates you have set.</p>",
                           "<p>It is recommended that you keep a secure copy of this file. By keeping and not ",
                           "editing this file, you can go back to check your gates at later stages in the ",
                           "analysis.</p>")),
              tags$li(tags$span("8"),
                      fileInput(inputId  = "cytobank_file_upload",
                                label    = "Select your Cytobank GatingML file (.xml)",
                                accept   = c("text/xml", ".xml"),
                                multiple = FALSE,
                                width    = "600px"))) # /ol
    ) # /tagList
  })



  # ==  OBSERVE EVENT  =======================================================


  # Upload gatingML file from Cytobank
  observeEvent(input$cytobank_file_upload, {
    if (!allow("import_cytobank"))
      return(NULL)

    show_processing_modal("Parsing gatingML file",
                          "Please wait while the gatingML file is being parsed.")
  
    parsed <- cytobank_parse(path = input$cytobank_file_upload$datapath)
  
    if (is.null(parsed$error)) {
      session$userData$import_gate_data(parsed$gate_data)
      session$userData$import_gatingML(parsed$gatingML)
      session$userData$import_import_file_name(input$cytobank_file_upload$name)
  
      removeModal()
      session$userData$application_state("gate_file_loaded")
    } else {
      shinyjs::reset("cytobank_file_upload")
      removeModal()
      show_error_modal(single = parsed$error)
    }
  })


  observeEvent(input$import_cytobank_back_button, {
    session$userData$application_state("")
  })



 }
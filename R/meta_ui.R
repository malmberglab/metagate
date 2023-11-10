#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


meta_ui <- function(input, output, session) {
    
  # ==  RENDER UI  ===========================================================

  output$meta_ui <- renderUI({

    if (ncol(session$userData$meta_data()) < 2) {
      summary_ui <- div(class = "help_box",
                        h4("You have not yet uploaded any meta data"),
                        p("To be able to create groups and analyze your samples, you need to assign some meta data ",
                          "to each sample. Meta data could for example be clinical data (e.g. diagnosis or treatment ",
                          "outcome) or experimental conditions (e.g. timepoint or stimulation type). Follow these ",
                          "steps to add meta data to your project:"),
                        tags$ol(tags$li("Click the \"Download Excel file template\" button above to download a meta ",
                                        "data file template."),
                                tags$li("Open the file in any software that can read .xlsx files (e.g. Microsoft ",
                                        "Excel). In this file, each row represents one sample (FCS file), while each ",
                                        "column represents a meta variable."),
                                tags$li("The column called \"file\" contains the FCS file names of your samples. Do ",
                                        "not edit this, as this is the only way the software knows what meta data ",
                                        "belongs to which sample."),
                                tags$li("Add one new column for each meta variable (e.g. gender, diagnosis, ",
                                        "timepoint."),
                                tags$li("For each variable, fill in data for each sample."),
                                tags$li("Save your .xlsx file, and upload it by clicking \"Upload Excel meta file\" ",
                                        "above."),
                                tags$li("If you later want to add or change the meta data, just repeat the steps ",
                                        "above.")))
    } else {
      summary_ui <- tableOutput("meta_summary_table")
    }

    return(tagList(
      tags$style(HTML(meta_css)),
      fluidRow(style = "margin-bottom: 28px",
        column(width = 6,
          downloadButton(outputId = "meta_file_download",
                         label   = "Download Excel file template",
                         style   = "margin-right: 8px"),

          actionButton(inputId = "meta_file_upload_modal",
                       label   = "Upload Excel meta file",
                       icon    = icon("open", lib = "glyphicon"))
        ),
        column(width = 6, style="text-align: right",
          actionButton(inputId = "meta_counts",
                       label   = "Set up absolute counts",
                       icon    = icon("stats", lib="glyphicon")),
          actionButton(inputId = "meta_settings",
                       label   = "Settings",
                       icon    = icon("cog", lib="glyphicon")) 
        )
      ), # /fluidRow

      tabsetPanel(
        tabPanel("Summary", summary_ui),
        tabPanel("Samples", DT::DTOutput("meta_sample_table"))
      ) # /tabsetPanel
    ))
  })

  meta_css <- "
div#meta_sample_table {
  font-size: 90%;
}
  "



  # ==  RENDER TABLE  ========================================================


  output$meta_summary_table <- renderTable({
    df <- meta_create_summary_table(session$userData$meta_data())
    data.frame("Variable name" = df$name,
               "Type"          = df$type,
               "Options"       = df$options,
               check.names     = FALSE)
  }, sanitize.text.function = function(x) x)


  output$meta_sample_table <- DT::renderDT({
    session$userData$meta_data()
  }, rownames = FALSE, selection = "none", options = list(pageLength = 20), filter = "top", style = "bootstrap")



  # ==  DOWNLOAD HANDLERS  ===================================================

  output$meta_file_download <- downloadHandler(
    filename = "Meta data.xlsx",
    content = function(file) {
      download_xlsx_file(session$userData$meta_data(), file = file)
    }
  )



  # ==  OBSERVE EVENT  =======================================================

  observeEvent(input$meta_file_upload_modal, {
    showModal(modalDialog(
      p("Please select an Excel file with meta data. There are two ways to create this file:"),
      tags$ul(style = "margin-bottom: 30px",
        tags$li(tags$strong("Use a template (recommended): "), "Download a template by pressing the ",
                tags$em("Download Excel file template"), " on the ", tags$em("Meta data"), " page. Add new columns ",
                "for your variables, e.g. \"Gender\" or \"Diagnosis\". The \"file\" column identifies each sample, ",
                "so do not edit this."),
        tags$li(tags$strong("Write manually: "), "Make a Excel file where each row represents a sample and each ",
                "column represents a variable. Create a column named \"file\" and fill in the FCS file names for ",
                "each sample.")
      ),
      fileInput(inputId  = "meta_file_upload",
                label    = "Select Excel file:",
                multiple = FALSE,
                accept   = xlsx_mime_types(),
                width    = "100%"),
      title  = "Upload meta data file",
      footer = modalButton("Cancel")
    ))
  })
  
  
  observeEvent(input$meta_file_upload, {
    result <- meta_parse_file(old_meta_data = session$userData$meta_data(),
                              path          = input$meta_file_upload$datapath,
                              group_data    = session$userData$group_data())
    
    if (is.null(result$errors)) {
      # Update color/annotation variables
      color_variables <- intersect(project_get_data(session = session, name = "meta_variables_color"),
                                   result$new_variables)
      project_set_data(session = session,
                       name    = "meta_variables_color",
                       value   = c(color_variables, result$added_variables))

      # Update group definition variables
      group_variables <- intersect(project_get_data(session = session, name = "meta_variables_group"),
                                   result$new_variables)
      project_set_data(session = session,
                       name    = "meta_variables_group",
                       value   = c(group_variables, result$added_group_variables))

      session$userData$meta_data(result$new_meta_data)

      if ("new_group_data" %in% names(result) && !is.null(result$new_group_data)) {
        session$userData$group_data(result$new_group_data)
      }
      
      session$userData$group_editor_loaded(FALSE)

      removeModal()

      if (is.null(result$warnings)) {
        dialog_content <- p("The meta data was loaded successfully.")
      } else {
        dialog_content <- tagList(p("The meta data was loaded, but the following warnings were issued:"),
                                  tags$ul(lapply(result$warnings, tags$li)))
      }
      showModal(modalDialog(dialog_content,
                            title  = "Meta data uploaded",
                            footer = actionButton(inputId = "meta_file_upload_close", label = "Close")))
    } else {
      removeModal()
      show_error_modal(multiple = result$errors)
    }
  })


  observeEvent(input$meta_file_upload_close, {
    removeModal()
    updateNavbarPage(session  = session,
                     inputId  = "main_menu",
                     selected = "Meta data")
  })


  observeEvent(input$meta_settings, {
    choices <- setdiff(names(session$userData$meta_data()), "file")

    choices_with_none <- c(list("(None)" = "-"), setNames(as.list(choices), choices))

    showModal(modalDialog(
      selectInput(inputId   = "meta_variables_group",
                  label     = "Variables for group definition:",
                  choices   = choices,
                  selected  = project_get_data(session = session, name = "meta_variables_group"),
                  multiple  = TRUE,
                  selectize = TRUE,
                  width     = "100%"),

      p(class = "input_description",
        "Variables that can be used to define groups. Existing groups will be affected if you",
        "remove a variable that is used for defining these groups."),

      selectInput(inputId   = "meta_variables_color",
                  label     = "Annotation variables:", 
                  choices   = choices,
                  selected  = project_get_data(session = session, name = "meta_variables_color"),
                  multiple  = TRUE,
                  selectize = TRUE,
                  width     = "100%"),

      p(class = "input_description",
        "Variables that can be used to annotate samples in dot plots or volcano plots."),

      selectInput(inputId   = "meta_variables_pairing",
                  label     = "Pairing variables:",
                  choices   = choices,
                  selected  = project_get_data(session = session, name = "meta_variables_pairing"),
                  multiple  = TRUE,
                  selectize = TRUE,
                  width     = "100%"),

      p(class = "input_description",
        "Variables that can be used to pair samples. Example: If your dataset contains two timepoints for multiple",
        "patients, select a variable uniquely identifying each patient. This will let MetaGate know which samples",
        "to pair."),

      selectInput(inputId   = "meta_variables_panel",
                  label     = "Panel variable:",
                  choices   = choices_with_none,
                  selected  = project_get_data(session = session, name = "meta_variables_panel"),
                  multiple  = FALSE,
                  selectize = TRUE,
                  width     = "50%"),

      p(class = "input_description",
        "If you have run multiple panels, there will be multiple samples per individual in your project. For",
        "readouts that are only found in one panel, this will not cause any problems. However, if the same readout",
        "is found in multiple panels, you risk including the individuals multiple times in your visualizations",
        "or statistical calculations. To avoid this, create a meta data variable that specifies which panel is",
        "used for which FCS file (e.g. \"Panel 1\", \"Panel 2\", etc.). Then, choose this variable as the \"Panel",
        "variable\". Be aware that all samples that are not assigned to a panel will be ignored. For each readout",
        "individually, MetaGate chooses the panel with the most available samples, then alphabetically."),

      title  = "Meta data settings",
      footer = tagList(modalButton("Cancel"), actionButton(inputId = "meta_settings_apply", label = "Apply changes"))
    ))
  })
  
  
  observeEvent(input$meta_settings_apply, {
    project_set_data(session = session, name = "meta_variables_group", value = input$meta_variables_group)
    project_set_data(session = session, name = "meta_variables_color", value = input$meta_variables_color)
    project_set_data(session = session, name = "meta_variables_pairing", value = input$meta_variables_pairing)

    panel_variable <- input$meta_variables_panel
    if (panel_variable == "-") panel_variable <- NA
    project_set_data(session = session, name = "meta_variables_panel", value = panel_variable)

    check_variables <- meta_check_variables(meta_variables = input$meta_variables_group,
                                            group_data     = session$userData$group_data())

    session$userData$group_editor_loaded(FALSE)

    if (!is.null(check_variables$new_group_data))
      session$userData$group_data(check_variables$new_group_data)

    if (length(check_variables$missing) > 0) {
      warnings <- sapply(check_variables$missing, function(group) {
        paste0("The definition of the group \"", group$group, "\" was changed because the following variables are no ",
               "longer available for group creation: ", paste(group$variables, collapse = ", "))
      })

      dialog_content <- tagList(
        p("The changes were applied, but the following warnings were issued:"),
        tags$ul(lapply(warnings, tags$li))
      )
    } else {
      dialog_content <- "The changes were successfully applied."
    }

    removeModal()
    showModal(modalDialog(dialog_content,
                          title  = "Changes applied",
                          footer = actionButton(inputId = "meta_settings_close", label = "Close")))
  })


  observeEvent(input$meta_settings_close, {
    removeModal()
    updateNavbarPage(session = session, inputId = "main_menu", selected = "Meta data")
  })



  observeEvent(input$meta_counts, {
    current <- project_get_data(session = session, name = "meta_variables_count")

    meta_data <- session$userData$meta_data()
    meta_variables <- names(meta_data)[sapply(meta_data, class) %in% c("integer", "numeric")]

    populations <- get_readout_populations(session$userData$stat_data())

    table <- do.call(tagList, lapply(1:5, function(i) {
      if (!is.null(current) && nrow(current) >= i && current[i, "variable"] %in% meta_variables
        && current[i, "population"] %in% populations) {
        selected_variable <- current[i, "variable"]
        selected_population <- current[i, "population"]
      } else {
        selected_variable <- "(none)"
        selected_population <- "(none)"
      }

      fluidRow(
        column(6, selectInput(inputId   = paste0("meta_counts_", i, "_variable"),
                              label     = NULL,
                              choices   = c("(none)", meta_variables),
                              selected  = selected_variable,
                              multiple  = FALSE,
                              selectize = TRUE,
                              width     = "100%")),
        column(6, selectInput(inputId   = paste0("meta_counts_", i, "_population"),
                              label     = NULL,
                              choices   = c("(none)", populations),
                              selected  = selected_population,
                              multiple  = FALSE,
                              selectize = TRUE,
                              width     = "100%"))
      )
    }))

    showModal(modalDialog(
      p("On each row, select a population and a meta data variable containing absolute counts for that population.",
        style = "margin-bottom: 24px"),
      fluidRow(class = "table_header",
        column(6, tags$h4("Absolute count variable", style="margin: 0 0 14px 0")),
        column(6, tags$h4("Corresponding population", style="margin: 0 0 14px 0"))
      ),
      table,
      title  = "Absolute count set-up",
      footer = tagList(modalButton("Cancel"), actionButton(inputId = "meta_counts_apply", label = "Apply changes"))
    ))
  })
  
  
  observeEvent(input$meta_counts_apply, {
    new <- do.call(rbind, lapply(1:5, function(i) {
      variable <- input[[paste0("meta_counts_", i, "_variable")]]
      population <- input[[paste0("meta_counts_", i, "_population")]]

      if (variable == "(none)" || population == "(none)") {
        return(NULL)
      } else {
        return(data.frame(variable = variable, population = population))
      }
    }))

    if (anyDuplicated(new$variable)) {
      show_error_modal("Settings could not be changed because one variable was assigned to multiple populations.")
    } else if (anyDuplicated(new$population)) {
      show_error_modal("Settings could not be changed because one population was assigned to multiple count variables.")
    } else {
      if (!is.null(new) && nrow(new) < 1) {
        new <- NULL
      }
      
      project_set_data(session = session,
                       name    = "meta_variables_count",
                       value   = new)

      removeModal()
      showModal(modalDialog("Changes in absolute count calculations were successfully applied.",
                            title  = "Changes applied",
                            footer = actionButton(inputId = "meta_counts_close", label = "Close")))
    }    
  })


  observeEvent(input$meta_counts_close, {
    removeModal()
  })

}


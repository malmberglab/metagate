#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


heatmap_ui <- function(input, output, session) {
  
  ui_selected_readouts <- callModule(module   = readout_ui,
                                     id       = "heatmap",
                                     multiple = TRUE)



  # ==  RENDER UI  ===========================================================

  output$heatmap_ui <- renderUI({
    tagList(
      uiOutput("heatmap_top_ui"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("heatmap-readout_ui")
        ),
        mainPanel(
          uiOutput("heatmap_top_controls_ui"),
          uiOutput("heatmap_messages_ui"),
          uiOutput("heatmap_plot_ui")
        )
      ),
      tabsetPanel(
        tabPanel("Plot settings", uiOutput("heatmap_plot_settings_ui")),
        tabPanel("Statistics",    tableOutput("heatmap_statistics_table")),
        tabPanel("Samples",       tableOutput("heatmap_samples_table")),
        tabPanel("Export", uiOutput("heatmap_export_ui")),
        selected = "Plot settings"
      )
    )
  })


  output$heatmap_top_ui <- renderUI({
    if (length(session$userData$group_name_list()) < 1) {
      div(class = "message_warning message_warning_top",
          p("You need to set up one or more groups to create heatmaps. ", 
            "Select Groups in the main menu and define some groups."))
    }
  })


  output$heatmap_top_controls_ui <- renderUI({
    tagList(
      fluidRow(
        column(width = 8,
          uiOutput("heatmap_group_select_ui")
        ),
        column(width = 4,
          div(class = "heatmap_comparison_controls", style = "display: none",
            label("Pairing",
                  "Select a meta variable to use for sample pairing.",
                  "Enabling sample pairing will affect the choice of statistical test."),
            selectInput(inputId   = "heatmap_pairing",
                        label     = NULL,
                        choices   = c("None", project_get_data(session, "meta_variables_pairing")),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          )
        )
      ),
      fluidRow(
        div(class = "heatmap_twogroups_controls", style = "display: none",
          column(width = 3,
            label("Plot type",
                  "Perform comparison of groups or display them side by side?"),
            selectInput(inputId   = "heatmap_type",
                        label     = NULL,
                        choices   = c("Comparison", "Display both groups"),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          )
        ),
        column(width = 3,
          label("Mean/median",
                "When generating a readout value for each group, should mean or median be applied?"),
          selectInput(inputId   = "heatmap_mean",
                      label     = NULL,
                      choices   = c("Mean", "Median"),
                      multiple  = FALSE,
                      selectize = TRUE,
                      width     = "100%")
        ),
        div(class = "heatmap_comparison_controls", style="display: none",
          column(width = 3,
            label("Comparison",
                  "The comparison that is reflected in the heatmap colors."),
            selectInput(inputId   = "heatmap_comparison",
                        label     = NULL,
                        choices   = c("log2 fold change", "Absolute change", "p values"),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          ),
          column(width = 3,
            label("p value adj.",
                  "Select method for p value adjustment."),
            selectInput(inputId   = "heatmap_p_adjust",
                        label     = NULL,
                        choices   = p_adjust_methods(),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          )
        )
      )
    )
  })


  output$heatmap_group_select_ui <- renderUI({
    tagList(
      label("Groups",
            "Select exactly two groups to do comparison. Selecting 1 or >2 groups will display group values side by",
            "side and not allow statistical comparison."),
      selectInput(inputId   = "heatmap_groups",
                  label     = NULL,
                  choices   = session$userData$group_name_list(),
                  multiple  = TRUE,
                  selectize = TRUE,
                  width     = "100%")
    )
  })


  output$heatmap_messages_ui <- renderUI({
    return(display_messages(ui_heatmap_messages()))
  })


  output$heatmap_plot_ui <- renderUI({
    req(input$heatmap_plot_height)
    req(input$heatmap_plot_width)
    
    plotOutputWithLoader(outputId = "heatmap_plot",
                         width    = paste0(input$heatmap_plot_width, "px"),
                         height   = paste0(input$heatmap_plot_height, "px"))
  })


  output$heatmap_plot_settings_ui <- renderUI({
    fluidRow(
      column(width = 3,
        sliderInput(inputId = "heatmap_plot_width",
                    label   = "Plot width:",
                    min     = 200,
                    max     = 1200,
                    value   = 800,
                    step    = 5,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%"),
        sliderInput(inputId = "heatmap_plot_height",
                    label   = "Plot height:",
                    min     = 200,
                    max     = 1200,
                    value   = 500,
                    step    = 10,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%")
      ),
      column(width = 3,
        div(class = "heatmap_noncomparison_controls", style = "display: none",
          colorSelectizeInput(inputId  = "heatmap_plot_intensity_colors",
                              label    = "Intensity colors (select 2 or more):",
                              selected = c("black", "yellow"),
                              maxItems = 10),
          selectInput(inputId   = "heatmap_plot_color_transform",
                      label     = "Transform color scale:",
                      choices   = list("None"  = "none", "Logarithmic"  = "log"),
                      multiple  = FALSE,
                      selectize = TRUE,
                      width     = "100%")
        ),
        div(class = "heatmap_colored_controls", style = "display: none",
          div(class = "heatmap_comparison_controls", style = "display: none",
            colorSelectizeInput(inputId  = "heatmap_plot_comparison_colors",
                                label    = "Comparison colors (select 3):",
                                selected = c("blue", "gray85", "red"),
                                maxItems = 3)
          ),
          tags$label("Manual upper color limit:"),
          fluidRow(
            column(width = 2,
              checkboxInput(inputId = "heatmap_plot_manual_upper_limit",
                            label   = NULL,
                            value   = FALSE)),
            column(width = 10,
              numericInput(inputId = "heatmap_plot_manual_upper_limit_value",
                           label = NULL,
                           value = 100))
          ),
          tags$label("Manual lower color limit:"),
          fluidRow(
            column(width = 2, 
              checkboxInput(inputId = "heatmap_plot_manual_lower_limit",
                            label   = NULL,
                            value   = FALSE)),
            column(width = 10,
              numericInput(inputId = "heatmap_plot_manual_lower_limit_value",
                           label   = NULL,
                           value   = 0))
          )
        )
      ),
      column(width = 3,
        sliderInput(inputId = "heatmap_plot_text_size",
                    label   = "Text size:",
                    min     = 1,
                    max     = 30,
                    value   = 15,
                    step    = 0.5,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%"),
        selectInput(inputId  = "heatmap_plot_x_axis_position",
                    label    = "X axis labels:",
                    choices  = c("Top", "Bottom"),
                    selected ="Top",
                    multiple = FALSE,
                    width    = "100%"),
        checkboxInput(inputId = "heatmap_plot_x_axis_angled",
                      label   = "Angled x axis labels",
                      value   = TRUE)
      ),
      column(width = 3,
        checkboxInput(inputId = "heatmap_plot_show_axis_titles",
                      label   = "Show axis titles",
                      value   = FALSE),
        actionLink(inputId = "heatmap_swap_axes",
                   label   = "Swap X and Y axes",
                   style   = "display: block; padding-bottom: 15px")
      )
    )
  })


  output$heatmap_export_ui <- renderUI({
    tagList(
      downloadButton(outputId = "heatmap_export_pdf",
                     label    = "Download PDF plot"),
      br(), br(),
      downloadButton(outputId = "heatmap_export_xlsx",
                     label    = "Download data as Excel file")
    )
  })


  # ==  RENDER TABLE  ========================================================

  output$heatmap_samples_table <- renderTable({
    ui_heatmap_samples_table()
  })
  

  output$heatmap_statistics_table <- renderTable({
    ui_heatmap_statistics_table()
  }, digits = 8)



  # ==  RENDER PLOT  =========================================================

  output$heatmap_plot <- renderPlot({
    ui_heatmap_plot()
  })



  # ==  DOWNLOAD HANDLERS  ===================================================

  output$heatmap_export_pdf <- downloadHandler(
    filename = function() { "Heatmap.pdf" },
    content = function(file) {
      pdf(NULL)
      ggsave(filename    = file, 
             plot        = ui_heatmap_plot(),
             width       = input$heatmap_plot_width * 0.015,
             height      = input$heatmap_plot_height * 0.015,
             useDingbats = FALSE)
      dev.off()
    }
  )
  

  output$heatmap_export_xlsx <- downloadHandler(
    filename = function() { "Heatmap data.xlsx" },
    content = function(file) {
      download_xlsx_file(ui_heatmap_statistics_table(), file = file)
    }
  )



  # ==  OBSERVE EVENT  =======================================================

  # Store the list of selected groups
  observeEvent(input$heatmap_groups, {
    session$userData$heatmap_selected_groups(input$heatmap_groups)
  })


  # Update the list of selected groups
  observeEvent(input$main_menu, {
    if (input$main_menu == "Heatmap") {
      selected <- session$userData$heatmap_selected_groups()
      updateSelectInput(session  = session,
                        inputId  = "heatmap_groups",
                        selected = selected[selected %in% session$userData$group_name_list()])
    }
  })


  # Clear groups and readouts when a new project is loaded
  observeEvent(session$userData$application_state(), {
    if (session$userData$application_state() == "project_loaded") {
      session$userData$heatmap_selected_groups(NULL)
      ui_selected_readouts(NULL)
    }
  })


  observeEvent(input$heatmap_swap_axes, {
    ui_heatmap_readout_x_axis(!ui_heatmap_readout_x_axis())
  })


  observe({
    if (ui_heatmap_is_comparison()) {
      shinyjs::runjs("$('.heatmap_comparison_controls').show()")
      shinyjs::runjs("$('.heatmap_noncomparison_controls').hide()")
    } else {
      shinyjs::runjs("$('.heatmap_comparison_controls').hide()")
      shinyjs::runjs("$('.heatmap_noncomparison_controls').show()")
    }
  })


  observeEvent(input$heatmap_groups, {
    if (length(input$heatmap_groups) == 2) {
      shinyjs::runjs("$('.heatmap_twogroups_controls').show()")
    } else {
      shinyjs::runjs("$('.heatmap_twogroups_controls').hide()")
    }
  })


  observe({
    if (ui_heatmap_is_comparison() && input$heatmap_comparison == "p values") {
      shinyjs::runjs("$('.heatmap_colored_controls').hide()")
    } else {
      shinyjs::runjs("$('.heatmap_colored_controls').show()")
    }
  })



  # ==  REACTIVE  ============================================================

  session$userData$heatmap_selected_groups <- reactiveVal()

  ui_heatmap_messages <- reactiveVal()

  ui_heatmap_readout_x_axis <- reactiveVal(TRUE)


  ui_heatmap_is_paired <- reactive({
    ui_heatmap_is_comparison() && input$heatmap_pairing != "None"
  })
  

  ui_heatmap_is_comparison <- reactive({
    input$heatmap_type == "Comparison" && length(input$heatmap_groups) == 2
  })


  ui_heatmap_groups <- reactive({
    as.numeric(input$heatmap_groups)
  })


  ui_heatmap_samples <- reactive({
    if (length(ui_selected_readouts()) < 1)
      return(list(data = NULL, error = "No readouts selected."))

    if (length(ui_heatmap_groups()) < 1)
      return(list(data = NULL, error = "No groups selected."))

    samples <- get_samples_for_groups(meta_data      = session$userData$meta_data(),
                                      group_data     = session$userData$group_data(),
                                      group_ids      = ui_heatmap_groups(),
                                      pair_by        = if (ui_heatmap_is_paired()) input$heatmap_pairing else NULL,
                                      color_by       = NULL,
                                      panel_variable = project_get_data(session, "meta_variables_panel"))

    if (is.null(samples)) {
      return(list(data = NULL, error = "No samples found."))
    } else if (!is.null(samples$error)) {
      return(list(data = NULL, error = samples$error))
    } else {
      return(list(data = samples$data, error = NULL))
    }
  })
  
  
  ui_heatmap_samples_table <- reactive({
    create_samples_table(samples = ui_heatmap_samples()$data,
                         pair_by = if(ui_heatmap_is_paired()) input$heatmap_pairing else NULL)
  })
  
  
  ui_heatmap_data <- reactive({
    samples <- ui_heatmap_samples()

    if (!is.null(samples$error))
      return(list(data = NULL, error = samples$error))

    readouts <- ui_selected_readouts()

    if (is.null(readouts))
      return(list(data = NULL, error = "No readouts selected"))

    if (ui_heatmap_is_comparison()) {
      if (length(unique(samples$data$group_id)) < 2) {
        missing_groups <- setdiff(ui_heatmap_groups(), unique(samples$data$group_id))
      } else {
        heatmap_data <- get_multiple_readout_comparison_data(samples         = samples$data,
                                                             readouts        = readouts,
                                                             stat_data       = session$userData$stat_data(),
                                                             count_data      = session$userData$count_data(),
                                                             paired_groups   = ui_heatmap_is_paired(),
                                                             mean_type       = input$heatmap_mean,
                                                             p_adjust_method = input$heatmap_p_adjust)

        missing_groups <- heatmap_data$missing_groups
      }

      if (length(missing_groups) == 2) {
        return(list(data = NULL, error = "No data found for any of the groups."))
      } else if (length(missing_groups) == 1) {
        missing_group_name <- get_group_names_for_ids(session$userData$group_data(), missing_groups)
        return(list(data = NULL, error = paste0("No data found for the <em>", missing_group_name, "</em> group.")))
      }
    } else {
      heatmap_data <- get_multiple_readout_data(samples       = samples$data,
                                                readouts      = readouts,
                                                stat_data     = session$userData$stat_data(),
                                                count_data    = session$userData$count_data(),
                                                mean_type     = input$heatmap_mean)

      if (length(unique(heatmap_data$data$readout_prefix)) > 1) {
        return(list(data = NULL, error = paste0("Please select only one readout type. You have currently selected: ",
                                                paste(unique(heatmap_data$data$readout_prefix), collapse = ", "))))
      }
    }

    return(list(data = heatmap_data$data, error = NULL))
  })
  
  
  ui_heatmap_statistics_table <- reactive({
    if (ui_heatmap_is_comparison()) {
      create_multiple_readout_comparison_statistics_table(data       = ui_heatmap_data()$data,
                                                          group_data = session$userData$group_data(),
                                                          groups     = ui_heatmap_groups(),
                                                          mean_type  = input$heatmap_mean)
    } else {
      create_multiple_readout_statistics_table(data      = ui_heatmap_data()$data,
                                               mean_type = input$heatmap_mean)
    }
  })
  
  
  ui_heatmap_plot <- reactive({
    ui_heatmap_messages(NULL)
    messages <- NULL

    heatmap_data <- ui_heatmap_data()

    if (is.null(heatmap_data$data)) {
      if (is.null(heatmap_data$error)) {
        ui_heatmap_messages("ERROR: No data found.")
      } else {
        ui_heatmap_messages(paste0("ERROR: ", heatmap_data$error))
      }
      return(NULL)
    }

    if (!ui_heatmap_is_comparison()) {
      missing_groups <- get_group_names_for_ids(session$userData$group_data(),
                                                setdiff(input$heatmap_groups, heatmap_data$data$group_id))
      if (length(missing_groups) == 1) {
        messages <- c(messages, paste0("No data found for the <em>", missing_groups, "</em> group."))
      } else if (length(missing_groups) > 1) {
        messages <- c(messages, paste0("No data found for the following groups: <em>",
                                       paste(missing_groups, collapse = ", "), "</em>."))
      }
    }

    if (ui_heatmap_is_comparison()) {
      colors <- input$heatmap_plot_comparison_colors
      if (length(colors) != 3) {
        ui_heatmap_messages("ERROR: Exactly three colors should be set for comparison heatmaps.")
        return(NULL)
      }
    } else {
      colors <- input$heatmap_plot_intensity_colors
      if (length(colors) < 2) {
        ui_heatmap_messages("ERROR: You need to set more than one color.")
        return(NULL)
      }
    }

    result <- create_heatmap_plot(data               = heatmap_data$data,
                                  group_data         = session$userData$group_data(),
                                  groups             = ui_heatmap_groups(),
                                  readouts           = ui_selected_readouts(),
                                  is_paired          = ui_heatmap_is_paired(),
                                  is_comparison      = ui_heatmap_is_comparison(),
                                  heatmap_type       = input$heatmap_type,
                                  readout_x_axis     = ui_heatmap_readout_x_axis(),
                                  heatmap_comparison = input$heatmap_comparison,
                                  mean_type          = input$heatmap_mean,
                                  lower_limit        = if (input$heatmap_plot_manual_lower_limit)
                                                         input$heatmap_plot_manual_lower_limit_value else NA,
                                  upper_limit        = if (input$heatmap_plot_manual_upper_limit)
                                                         input$heatmap_plot_manual_upper_limit_value else NA,
                                  colors             = colors,
                                  color_transform    = input$heatmap_plot_color_transform,
                                  show_axis_titles   = input$heatmap_plot_show_axis_titles,
                                  x_axis_angled      = input$heatmap_plot_x_axis_angled,
                                  x_axis_position    = input$heatmap_plot_x_axis_position,
                                  text_size          = input$heatmap_plot_text_size)

    ui_heatmap_messages(c(messages, result$messages))

    return(result$plot)
  })


}
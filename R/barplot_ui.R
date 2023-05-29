barplot_ui <- function(input, output, session) {

  ui_selected_readouts <- callModule(module               = readout_ui,
                                     id                   = "barplot",
                                     multiple             = FALSE,
                                     multiple_populations = TRUE)



  # ==  RENDER UI  ===========================================================
  
  output$barplot_ui <- renderUI({
    tagList(
      uiOutput("barplot_top_ui"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("barplot-readout_ui")
        ),
        mainPanel(
          uiOutput("barplot_top_controls_ui"),
          uiOutput("barplot_messages_ui"),
          uiOutput("barplot_plot_ui")
        )
      ),
      tabsetPanel(
        tabPanel("Plot settings",   uiOutput("barplot_plot_settings_ui")),
        tabPanel("Statistics",      uiOutput("barplot_statistics_ui")),
        tabPanel("Samples",         tableOutput("barplot_samples_table")),
        tabPanel("Export",          uiOutput("barplot_export_ui")),
        selected = "Plot settings"
      )
    )
  })


  output$barplot_top_ui <- renderUI({
    if (length(session$userData$group_name_list()) < 1) {
      div(class = "message_warning message_warning_top",
        p("You need to set up one or more groups to perform any analysis.",
          "Select Groups in the main menu and define some groups."))
    }
  })


  output$barplot_top_controls_ui <- renderUI({
    tagList(
      fluidRow(
        column(8,
          uiOutput("barplot_group_select_ui")
        ),
        column(4,
          label("Plot type"),
          selectInput(inputId   = "barplot_plot_type",
                      label     = NULL,
                      choices   = c("Bar plot", "Dot plot", "Box plot"),
                      selected  = "Bar plot",
                      multiple  = FALSE,
                      selectize = TRUE,
                      width     = "100%")
        )
      ), # /fluidRow

      fluidRow(
        column(width = 3,
          div(class = "barplot_group_by_controls", style = "display: none",
            label("Group by",
                  "Should data in the plot be grouped by populations or by sample groups?"),
            selectInput(inputId   = "barplot_group_by",
                        label     = NULL,
                        choices   = list("Sample groups" = "groups",
                                         "Populations"   = "populations"),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          )
        ), # /column

        column(width = 3,
          div(class = "barplot_groups_pairing_controls", style = "display: none",
            label("Pair groups by",
                  "Select a meta variable to use for sample pairing.",
                  "Some samples might get removed to ensure complete pairs if pairing is enabled."),
            selectInput(inputId   = "barplot_groups_pairing",
                        label     = NULL,
                        choices   = c("None", project_get_data(session, "meta_variables_pairing")),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          )
        ), # /column

        column(width = 3,
          div(class = "barplot_dot_single_readout_controls", style = "display: block",
            label("Dot color",
                  "Select a meta variable to use for coloring of the dots in the plot."),
            selectInput(inputId   = "barplot_dot_color",
                        label     = NULL,
                        choices   = c("None", project_get_data(session, "meta_variables_color")),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          ), # /div

          div(class = "barplot_populations_pairing_controls", style = "display: none",
            label("Pair populations",
                  "Should values from different populations be paired?",
                  "Some samples might get removed to ensure complete pairs if pairing is enabled."),
            selectInput(inputId   = "barplot_populations_pairing",
                        label     = NULL,
                        choices   = c("Disabled", "Enabled"),
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          ) # /div
        ), # /column

        column(width = 3,
          div(class = "barplot_dot_controls", style = "display: block",
            label("Mean bar",
                  "Select which value should be indicated by the mean bar."),
            selectInput(inputId    = "barplot_dot_mean",
                        label      = NULL,
                        choices    = list("None"   = "none",
                                          "Mean"   = "mean",
                                          "Median" = "median"),
                        selected   = "median",
                        multiple   = FALSE,
                        selectize  = TRUE,
                        width      = "100%")
          ), # /div

          div(class = "barplot_bar_controls", style = "display: none",
            label("Bar type",
                  "Select bar plot type and values for error bars."),
            selectInput(inputId   = "barplot_plot_bar_type",
                        label     = NULL,
                        choices   = list("Median with dots" = "median_dots",
                                         "Median (IQR)"     = "median_IQR",
                                         "Median (range)"   = "median_range",
                                         "Median"           = "median",
                                         "Mean with dots"   = "mean_dots",
                                         "Mean (SD)"        = "mean_sd",
                                         "Mean (SE)"        = "mean_se",
                                         "Mean (IQR)"       = "mean_IQR",
                                         "Mean (95%)"       = "mean_95",
                                         "Mean (range)"     = "mean_range",
                                         "Mean"             = "mean"),
                        selected  = "median_dots",
                        multiple  = FALSE,
                        selectize = TRUE,
                        width     = "100%")
          ) # /div
        ) # /column
      ) # /fluidRow
    )
  })


  output$barplot_group_select_ui <- renderUI({
    tagList(
      label("Groups",
            "Select one or more groups for comparison.", 
            "The number of selected groups will decide the type of statistical test performed."),
      selectInput(inputId   = "barplot_groups",
                  label     = NULL,
                  choices   = session$userData$group_name_list(),
                  multiple  = TRUE,
                  selectize = TRUE,
                  width     = "100%")
    )
  })


  output$barplot_messages_ui <- renderUI({
    return(display_messages(ui_barplot_messages()))
  })


  output$barplot_plot_ui <- renderUI({
    req(input$barplot_plot_width)
    req(input$barplot_plot_height)

    plotOutputWithLoader(outputId = "barplot_plot",
                         width    = paste0(input$barplot_plot_width, "px"),
                         height   = paste0(input$barplot_plot_height, "px"))
  })


  output$barplot_statistics_ui <- renderUI({
    tagList(
      fluidRow(
        column(6,
          radioButtons(inputId = "barplot_statistics_comparison",
                       label   = NULL,
                       choices = list("Compare groups"      = "groups",
                                      "Compare populations" = "populations"),
                       inline  = TRUE)
        ),
        column(6,
          uiOutput("barplot_statistics_p_adjust_ui")
        )
      ), # /fluidRow
      uiOutput("barplot_statistics_table_ui")
    ) 
  })


  output$barplot_statistics_p_adjust_ui <- renderUI({
    selectInput(inputId   = "barplot_p_adjust",
                label     = NULL,
                choices   = p_adjust_methods(),
                multiple  = FALSE,
                selectize = TRUE,
                width     = "100%")
  })


  output$barplot_statistics_table_ui <- renderUI({
    if (input$barplot_statistics_comparison == "populations") {
      tagList(
        if ("barplot_p_adjust" %in% names(input)
            && input$barplot_p_adjust != "none"
            && ui_barplot_readout_count() > 2
            && ui_barplot_group_count() > 1) {
          div(class = "message_warning",
            p("Be aware that p value adjustment is only performed within each sample group."))
        } else {
          NULL
        },
        tableOutput("barplot_population_statistics_table")
      )
    } else {
      tagList(
        if ("barplot_p_adjust" %in% names(input)
            && input$barplot_p_adjust != "none"
            && ui_barplot_group_count() > 2
            && ui_barplot_readout_count() > 1) {
          div(class="message_warning",
            p("Be aware that p value adjustment is only performed within each population."))
        } else {
          NULL
        },
        tableOutput("barplot_group_statistics_table")
      )
    }
  })


  output$barplot_plot_settings_ui <- renderUI({
    fluidRow(
      column(width = 3,
        tags$script(HTML("barplot_set_auto_width = function() {
          Shiny.onInputChange('barplot_set_auto_width', document.getElementById('barplot_plot_ui').clientWidth)
        }")),
        HTML("<a href=\"#\" onclick=\"barplot_set_auto_width()\" style=\"float: right\">Fit to screen</a>"),

        sliderInput(inputId = "barplot_plot_width",
                    label   = "Plot width:",
                    min     = 100,
                    max     = 2000,
                    value   = 500,
                    step    = 10,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%"),

        sliderInput(inputId = "barplot_plot_height",
                    label   = "Plot height:",
                    min     = 100,
                    max     = 2000,
                    value   = 400,
                    step    = 10,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%"),
        sliderInput(inputId = "barplot_plot_x_angle",
                    label   = "Group label angle:",
                    min     = 0,
                    max     = 90,
                    value   = 45,
                    step    = 15,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%")
      ), # /column

      column(width = 3,
        tags$label("Manual upper limit:"),

        fluidRow(
          column(width = 2,
            checkboxInput(inputId = "barplot_plot_manual_upper_limit",
                          label    = NULL,
                          value    = FALSE)
          ),

          column(width = 10,
            numericInput(inputId = "barplot_plot_manual_upper_limit_value",
                         label   = NULL,
                         value   = 100)
          )
        ), # /fluidRow

        tags$label("Manual lower limit:"),

        fluidRow(
          column(width = 2,
            checkboxInput(inputId = "barplot_plot_manual_lower_limit",
                          label   = NULL,
                          value   = FALSE)
          ),

          column(width = 10,
            numericInput(inputId = "barplot_plot_manual_lower_limit_value",
                         label   = NULL,
                         value   = 0)
          )
        ), # /fluidRow

        selectInput(inputId   = "barplot_plot_transform",
                    label     = "Transform Y axis:",
                    choices   = list("None"  = "none", "Logarithmic" = "log"),
                    multiple  = FALSE,
                    selectize = TRUE,
                    width     = "100%")

      ), # /column

      column(width = 3,

        div(class = "barplot_dot_style_controls", style = "display: block",
          sliderInput(inputId = "barplot_plot_dot_point_size",
                      label   = "Point size:",
                      min     = 0,
                      max     = 10,
                      value   = 3,
                      step    = 0.5,
                      round   = TRUE,
                      ticks   = TRUE,
                      animate = FALSE,
                      width   = "100%"),

          selectInput(inputId   = "barplot_plot_dot_point_shape",
                      label     = "Point type:",
                      choices   = list("Filled dot" = "16",
                                       "Circle"     = "1",
                                       "X"          = "4",
                                       "+"          = "3"),
                      multiple  = FALSE,
                      selectize = TRUE),

          div(class = "barplot_unpaired_controls", style = "display: block",
            sliderInput(inputId = "barplot_plot_dot_jitter",
                        label   = "Jitter:",
                        min     = 0,
                        max     = 0.5,
                        value   = 0.05,
                        step    = 0.01,
                        round   = TRUE,
                        ticks   = TRUE,
                        animate = FALSE,
                        width   = "100%"),

            actionLink(inputId = "barplot_plot_dot_jitter_change",
                       label   = "Change jitter")
          ), # /div.barplot_unpaired_controls

          div(class = "barplot_paired_controls", style = "display: none",
            sliderInput(inputId = "barplot_plot_dot_line_width",
                        label   = "Line width:",
                        min     = 0,
                        max     = 5,
                        value   = 1,
                        step    = 0.1,
                        round   = TRUE,
                        ticks   = TRUE,
                        animate = FALSE,
                        width   = "100%")
          ) # /div.barplot_paired_controls
        ), # /div.barplot_dot_style_controls

        div(class = "barplot_box_controls", style = "display: none",
          checkboxInput(inputId = "barplot_plot_box_varwidth",
                        label   = "Proportional width")
        ) # /div.barplot_box_controls

      ), # /column

      column(width = 3,
        colorSelectizeInput(inputId = "barplot_discrete_colors",
                            label    = "Custom colors:",
                            selected = NULL),
        div(class = "barplot_dot_controls", style = "display: block",
          sliderInput(inputId = "barplot_plot_dot_mean_bar_size",
                      label   = "Mean bar size:",
                      min     = 0,
                      max     = 50,
                      value   = 20,
                      step    = 1,
                      round   = TRUE,
                      ticks   = TRUE,
                      animate = FALSE,
                      width   = "100%"),
          div(class = "barplot_paired_controls", style = "display: none",
            sliderInput(inputId = "barplot_plot_dot_mean_line_width",
                        label   = "Mean line width:",
                        min     = 0,
                        max     = 10,
                        value   = 2,
                        step    = 0.1,
                        round   = TRUE,
                        ticks   = TRUE,
                        animate = FALSE,
                        width   = "100%")
          ) # /div.barplot_paired_controls
        ) # /div.barplot_dot_controls
      ) # /column
    ) # /fluidRow
  })


  output$barplot_export_ui <- renderUI({
    tagList(
      downloadButton(outputId = "barplot_export_pdf",
                     label    = "Download PDF plot"),
      br(), br(),
      downloadButton(outputId = "barplot_export_xlsx",
                     label    = "Download statistics Excel file")
    )
  })



  # ==  RENDER TABLE  ========================================================

  output$barplot_samples_table <- renderTable({
    ui_barplot_samples_table()
  }, digits = 8)


  output$barplot_group_statistics_table <- renderTable({
    ui_barplot_group_statistics_table()
  }, digits = 8)


  output$barplot_population_statistics_table <- renderTable({
    ui_barplot_population_statistics_table()
  }, digits = 8)



  # ==  RENDER PLOT  =========================================================

  output$barplot_plot <- renderPlot({
    ui_barplot_plot()
  })



  # ==  DOWNLOAD HANDLERS  ===================================================

  output$barplot_export_pdf <- downloadHandler(
    filename = function() { paste0(ui_barplot_readout_name(), ".pdf") },
    content = function(file) {
      pdf(NULL)
      ggsave(filename    = file,
             plot        = ui_barplot_plot(),
             width       = 9 * input$barplot_plot_width / 600,
             height      = 9 * input$barplot_plot_height / 600,
             useDingbats = FALSE)
      dev.off()
    }
  )
  

  output$barplot_export_xlsx <- downloadHandler(
    filename = function() { paste0(ui_barplot_readout_name(), ".xlsx")},
    content = function(file) {
      samples_table <- ui_barplot_samples_table()
      names(samples_table)[names(samples_table) == "Value"] <- ui_barplot_readout_name()

      groups <- ui_barplot_group_statistics_table()
      populations <- ui_barplot_population_statistics_table()

      workbook <- list("Samples" = samples_table)
      if (!is.null(groups)) workbook <- c(workbook, list("Group comparison" = groups))
      if (!is.null(populations)) workbook <- c(workbook, list("Population comparison" = populations))

      download_xlsx_file(workbook, file = file)
    }
  )



  # ==  OBSERVE EVENT  =======================================================
  

  # Store the list of selected groups
  observeEvent(input$barplot_groups, {
    session$userData$barplot_selected_groups(input$barplot_groups)
  })


  # Update the list of selected groups
  observeEvent(input$main_menu, {
    if (input$main_menu == "Bar plot") {
      selected <- session$userData$barplot_selected_groups()
      updateSelectInput(session  = session,
                        inputId  = "barplot_groups",
                        selected = selected[selected %in% session$userData$group_name_list()])
    }
  })


  # Clear groups and readouts when a new project is loaded
  observeEvent(session$userData$application_state(), {
    if (session$userData$application_state() == "project_loaded") {
      session$userData$barplot_selected_groups(NULL)
      ui_selected_readouts(NULL)
    }
  })


  # Toggle controls for multiple readouts
  observe({
    if (ui_barplot_readout_count() > 1 && ui_barplot_group_count() > 1) {
      shinyjs::runjs("$('.barplot_group_by_controls').show()")
    } else {
      shinyjs::runjs("$('.barplot_group_by_controls').hide()")
    }
  })


  # Toggle controls for pairing populations
  observe({
    if (ui_barplot_readout_count() == 2) {
      shinyjs::runjs("$('.barplot_populations_pairing_controls').show()")
    } else {
      shinyjs::runjs("$('.barplot_populations_pairing_controls').hide()")
    }
  })


  # Toggle controls for pairing groups
  observe({
    if (ui_barplot_group_count() == 2) {
      shinyjs::runjs("$('.barplot_groups_pairing_controls').show()")
    } else {
      shinyjs::runjs("$('.barplot_groups_pairing_controls').hide()")
    }
  })


  # Toggle controls for single readut plots with "Dot plot" type
  observe({
    if (ui_barplot_readout_count() == 1
        && "barplot_plot_type" %in% names(input)
        && input$barplot_plot_type == "Dot plot") {
      shinyjs::runjs("$('.barplot_dot_single_readout_controls').show()")
    } else {
      shinyjs::runjs("$('.barplot_dot_single_readout_controls').hide()")
    }
  })


  # Toggle p adjustment controls
  observe({
    if ("barplot_statistics_comparison" %in% names(input)) {
      if ((input$barplot_statistics_comparison == "populations" && ui_barplot_readout_count() > 2)
          || (input$barplot_statistics_comparison == "groups" && ui_barplot_group_count() > 2) ) {
        shinyjs::runjs("$('#barplot_statistics_p_adjust_ui').show()")
      } else {
        shinyjs::runjs("$('#barplot_statistics_p_adjust_ui').hide()")
      }
    }
  })


  # Toggle pairing controls
  observe({
    if (input$barplot_plot_type == "Dot plot"
        && ((ui_barplot_group_count() == 1 && ui_barplot_populations_are_paired())
            || (ui_barplot_readout_count() == 1 && ui_barplot_groups_are_paired()))) {
      shinyjs::runjs("$('.barplot_paired_controls').show()")
      shinyjs::runjs("$('.barplot_unpaired_controls').hide()")
    } else {
      shinyjs::runjs("$('.barplot_paired_controls').hide()")
      shinyjs::runjs("$('.barplot_unpaired_controls').show()")
    }
  })

  
  # React to selected plot type and change controls
  observeEvent(input$barplot_plot_type, {
    if (input$barplot_plot_type == "Dot plot") {
      shinyjs::runjs("$('.barplot_dot_controls').show()")
      shinyjs::runjs("$('.barplot_dot_style_controls').show()")
      shinyjs::runjs("$('.barplot_box_controls').hide()")
      shinyjs::runjs("$('.barplot_bar_controls').hide()")
    } else if (input$barplot_plot_type == "Box plot") {
      shinyjs::runjs("$('.barplot_dot_controls').hide()")
      shinyjs::runjs("$('.barplot_dot_style_controls').hide()")
      shinyjs::runjs("$('.barplot_box_controls').show()")
      shinyjs::runjs("$('.barplot_bar_controls').hide()")
    } else if (input$barplot_plot_type == "Bar plot") {
      shinyjs::runjs("$('.barplot_dot_controls').hide()")
      shinyjs::runjs("$('.barplot_dot_style_controls').hide()")
      shinyjs::runjs("$('.barplot_box_controls').hide()")
      shinyjs::runjs("$('.barplot_bar_controls').show()")
    }
  })


  observe({
    if (!is.null(input$barplot_plot_type) && input$barplot_plot_type == "Bar plot") {
      if (endsWith(input$barplot_plot_bar_type, "_dots")) {
        shinyjs::runjs("$('.barplot_dot_style_controls').show()")
      } else {
        shinyjs::runjs("$('.barplot_dot_style_controls').hide()")
      }
    }
  })


  observeEvent(input$barplot_plot_dot_jitter_change, {
    ui_barplot_jitter_seed(sample(1:1000, 1))  
  })


  observeEvent(input$barplot_set_auto_width, {
    updateSliderInput(session = session,
                      inputId = "barplot_plot_width",
                      value   = input$barplot_set_auto_width)
  })



  # ==  REACTIVE  ============================================================

  session$userData$barplot_selected_groups <- reactiveVal()

  ui_barplot_messages <- reactiveVal()

  ui_barplot_jitter_seed <- reactiveVal(1)


  ui_barplot_data <- reactive({
    if (is.null(ui_selected_readouts())) {
      return(list(data = NULL, error = "No readouts selected."))
    }

    if (length(input$barplot_groups) < 1) {
      return(list(data = NULL, error = "No groups selected."))
    }

    samples <- get_samples_for_groups(meta_data      = session$userData$meta_data(),
                                      group_data     = session$userData$group_data(),
                                      group_ids      = input$barplot_groups,
                                      pair_by        = if (ui_barplot_groups_are_paired()) input$barplot_groups_pairing
                                                       else NULL,
                                      color_by       = if (input$barplot_dot_color != "None") input$barplot_dot_color
                                                       else NULL,
                                      panel_variable = project_get_data(session, "meta_variables_panel"))

    if (is.null(samples)) {
      return(list(data = NULL, error = "No samples found."))
    } else if (!is.null(samples$error)) {
      return(list(data = NULL, error = samples$error))
    }

    barplot_data <- get_single_readout_data(samples            = samples$data,
                                            readouts           = ui_selected_readouts(),
                                            stat_data          = session$userData$stat_data(),
                                            count_data         = session$userData$count_data(),
                                            paired_groups      = ui_barplot_groups_are_paired(),
                                            paired_populations = ui_barplot_populations_are_paired())

    return(list(data = barplot_data$data, panel = barplot_data$panel, error = NULL))
  })
  
  
  ui_barplot_readout_name <- reactive({
    barplot_get_readout_name(ui_selected_readouts())
  })


  ui_barplot_readout_type <- reactive({
    barplot_get_readout_type(ui_selected_readouts())
  })
  
  
  ui_barplot_groups_are_paired <- reactive({
    input$barplot_groups_pairing != "None" && ui_barplot_group_count() == 2
  })


  ui_barplot_populations_are_paired <- reactive({
    input$barplot_populations_pairing == "Enabled" && ui_barplot_readout_count() == 2
  })


  ui_barplot_readout_count <- reactive({
    length(ui_selected_readouts())
  })


  ui_barplot_group_count <- reactive({
    length(input$barplot_groups)
  })


  ui_barplot_plot <- reactive({
    ui_barplot_messages(NULL)
    messages <- NULL

    barplot_data <- ui_barplot_data()

    if (is.null(barplot_data$data)) {
      if (is.null(barplot_data$error)) {
        ui_barplot_messages("ERROR: No data found.")
      } else {
        ui_barplot_messages(paste0("ERROR: ", barplot_data$error))
      }
      return(NULL)
    }

    if (!is.null(barplot_data$panel) && !is.na(barplot_data$panel)) {
      messages <- c(messages, paste0("Showing data from panel: ", barplot_data$panel))
    }

    missing_groups <- get_group_names_for_ids(session$userData$group_data(),
                                              setdiff(input$barplot_groups, barplot_data$data$group_id))
    if (length(missing_groups) == 1) {
      messages <- c(messages, paste0("No data found for the <em>", missing_groups, "</em> group."))
    } else if (length(missing_groups) > 1) {
      messages <- c(messages, paste0("No data found for the following groups: <em>",
                                     paste(missing_groups, collapse = ", "), "</em>."))
    }
    
    result <- barplot_create_plot(data                = barplot_data$data,
                                  readout_name        = ui_barplot_readout_name(),
                                  readout_type        = ui_barplot_readout_type(),
                                  paired_groups       = ui_barplot_groups_are_paired(),
                                  paired_populations  = ui_barplot_populations_are_paired(),
                                  plot_type           = input$barplot_plot_type,
                                  group_by            = input$barplot_group_by,
                                  upper_limit         = if (input$barplot_plot_manual_upper_limit)
                                                          input$barplot_plot_manual_upper_limit_value else NA,
                                  lower_limit         = if (input$barplot_plot_manual_lower_limit)
                                                          input$barplot_plot_manual_lower_limit_value else NA,
                                  transform           = input$barplot_plot_transform,
                                  discrete_colors     = input$barplot_discrete_colors,
                                  dot_color           = input$barplot_dot_color,
                                  dot_mean            = input$barplot_dot_mean,
                                  dot_point_size      = input$barplot_plot_dot_point_size,
                                  dot_point_shape     = input$barplot_plot_dot_point_shape,
                                  dot_line_width      = input$barplot_plot_dot_line_width,
                                  dot_jitter_seed     = ui_barplot_jitter_seed(),
                                  dot_jitter_width    = input$barplot_plot_dot_jitter,
                                  dot_mean_bar_size   = input$barplot_plot_dot_mean_bar_size,
                                  dot_mean_line_width = input$barplot_plot_dot_mean_line_width,
                                  box_varwidth        = input$barplot_plot_box_varwidth,
                                  bar_type            = input$barplot_plot_bar_type,
                                  x_angle             = input$barplot_plot_x_angle)
    
    ui_barplot_messages(c(messages, result$messages))
    
    return(result$plot)
  })


  ui_barplot_samples_table <- reactive({
    create_samples_table(samples  = ui_barplot_data()$data,
                         color_by = input$barplot_dot_color,
                         pair_by  = input$barplot_groups_pairing,
                         percent  = ui_barplot_readout_type() == "Percent")
  })
  
  
  ui_barplot_group_statistics_table <- reactive({
    p_adjust_method <- if (is.null(input$barplot_p_adjust)) "none" else input$barplot_p_adjust

    barplot_create_group_statistics_table(data            = ui_barplot_data()$data,
                                          paired          = ui_barplot_groups_are_paired(),
                                          percent         = ui_barplot_readout_type() == "Percent",
                                          p_adjust_method = p_adjust_method)
  })


  ui_barplot_population_statistics_table <- reactive({
    p_adjust_method <- if (is.null(input$barplot_p_adjust)) "none" else input$barplot_p_adjust

    barplot_create_population_statistics_table(data            = ui_barplot_data()$data,
                                               paired          = ui_barplot_populations_are_paired(),
                                               percent         = ui_barplot_readout_type() == "Percent",
                                               p_adjust_method = p_adjust_method)
  })

}


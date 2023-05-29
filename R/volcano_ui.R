volcano_ui <- function(input, output, session) {
  
  ui_selected_readouts <- callModule(module   = readout_ui,
                                     id       = "volcano",
                                     multiple = TRUE)



  # ==  RENDER UI  ===========================================================

  output$volcano_ui <- renderUI({
    tagList(
      tags$style(HTML(volcano_css)),
      uiOutput("volcano_top_ui"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("volcano-readout_ui")
        ),
        mainPanel(
          uiOutput("volcano_top_controls_ui"),
          uiOutput("volcano_messages_ui"),
          uiOutput("volcano_plot_ui")
        )
      ),
      tabsetPanel(
        tabPanel("Plot settings", uiOutput("volcano_plot_settings_ui")),
        tabPanel("Statistics",    tableOutput("volcano_statistics_table")),
        tabPanel("Samples",       tableOutput("volcano_samples_table")),
        tabPanel("Export",        uiOutput("volcano_export_ui")),
        selected = "Plot settings"
      )
    )
  })


  volcano_css <- "
.plot_hover {
  position: absolute;
  z-index: 100;
  padding: 3px 7px;
  font-size: 13px;
  margin: 0;
  background: #fff1b1;
  border: 1px #ffdd3d solid;
  color: #303030;
}
  "


  output$volcano_top_ui <- renderUI({
    if (length(session$userData$group_name_list()) < 1) {
      div(class = "message_warning message_warning_top", p(
            "You need to set up at least two groups to create volcano plots. ",
            "Select Groups in the main menu and define some groups."))
    }
  })


  output$volcano_top_controls_ui <- renderUI({
    tagList(
      fluidRow(
        column(width = 8,
          uiOutput("volcano_group_select_ui")
        ), # / column
        column(width = 4,
          label("Pairing",
                "Select a meta variable to use for sample pairing.",
                "Enabling sample pairing will affect the choice of statistical test."),
          selectInput(inputId   = "volcano_pairing",
                      label     = NULL,
                      choices   = c("None", project_get_data(session, "meta_variables_pairing")),
                      multiple  = FALSE,
                      selectize = TRUE,
                      width     = "100%")
        ) # /column
      ),
      fluidRow(
        column(width = 4,
          label("Mean/median",
                "When generating a readout value for each group, should mean or median be applied?"),
          selectInput(inputId   = "volcano_mean",
                      label     = NULL,
                      choices   = c("Mean", "Median"),
                      multiple  = FALSE,
                      selectize = TRUE,
                      width     = "100%")
        ), # /column
        column(width = 4,
          label("Comparison",
                "log2 fold change: log2 of the mean/median of group 2 values divided by mean/median of group 1 values.",
                "\n\nAbsolute change: Mean/median of group 2 values minus mean/median of group 1 values."),
          selectInput(inputId   = "volcano_comparison",
                      label     = NULL,
                      choices   = c("log2 fold change", "Absolute change"),
                      multiple  = FALSE,
                      selectize = TRUE,
                      width     = "100%")
        ), # /column
        column(width = 4,
          label("p value adj.",
                "Select method for p value adjustment."),
          selectInput(inputId   = "volcano_p_adjust",
                      label     = NULL,
                      choices   = p_adjust_methods(),
                      multiple  = FALSE,
                      selectize = TRUE,
                      width     = "100%")
        ) # /column
      ) # /fluidRow
    )
  })


  output$volcano_group_select_ui <- renderUI({
    tagList(
      fluidRow(
        column(width = 6,
          label("Group 1"),
          selectizeInput(inputId  = "volcano_group_1",
                         label    = NULL,
                         choices  = session$userData$group_name_list(),
                         selected = 1,
                         multiple = FALSE,
                         width    = "100%")
        ), column(width = 6,
          label("Group 2"),
          selectizeInput(inputId  = "volcano_group_2",
                         label    = NULL,
                         choices  = session$userData$group_name_list(),
                         selected = 2,
                         multiple = FALSE,
                         width    = "100%")
        )
      )
    )
  })


  output$volcano_messages_ui <- renderUI({
    display_messages(ui_volcano_messages())
  })


  output$volcano_plot_ui <- renderUI({
    req(input$volcano_plot_height)
    req(input$volcano_plot_width)
    
    div(style = "position: relative",
        plotOutputWithLoader("volcano_plot",
                             width  = paste0(input$volcano_plot_width, "px"),
                             height = paste0(input$volcano_plot_height, "px"),
                             hover  = hoverOpts("volcano_plot_hover", delay = 100, delayType = "debounce")
        ),
        uiOutput("volcano_plot_hover_ui"))
  })


  output$volcano_plot_hover_ui <- renderUI({
    hover <- input$volcano_plot_hover
    hover_text <- ui_volcano_hover_text()

    if (is.null(hover_text))
      return(NULL)

    hover_text <- strsplit(ui_volcano_hover_text(), "\n")[[1]]
    
    div(class = "plot_hover",
        style = paste0("left:", hover$coords_css$x + 5, "px; top:", hover$coords_css$y + 5, "px;"),
        HTML("<strong>", hover_text[1], "</strong><br>",
             paste0(hover_text[2:length(hover_text)], collapse = "<br>")))
  })


  output$volcano_plot_settings_ui <- renderUI({
    fluidRow(
      column(width = 3,
        sliderInput(inputId = "volcano_plot_width",
                    label   = "Plot width:",
                    min     = 200,
                    max     = 1200,
                    value   = 800,
                    step    = 5,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%"),

        sliderInput(inputId = "volcano_plot_height",
                    label   = "Plot height:",
                    min     = 200,
                    max     = 1200,
                    value   = 500,
                    step    = 10,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%")
      ), # /column
      column(width = 3,
        tags$label("Manual upper X limit:"),
        fluidRow(
          column(width = 2,
            checkboxInput(inputId = "volcano_plot_manual_x_upper_limit",
                          label   = NULL,
                          value   = FALSE)),
          column(width = 10,
            numericInput(inputId = "volcano_plot_manual_x_upper_limit_value",
                         label   = NULL,
                         value   = 5))
        ), # /fluidRow
        tags$label("Manual lower X limit:"),
        fluidRow(
          column(width = 2,
            checkboxInput(inputId = "volcano_plot_manual_x_lower_limit",
                          label   = NULL,
                          value   = FALSE)),
          column(width = 10,
            numericInput(inputId = "volcano_plot_manual_x_lower_limit_value",
                         label   = NULL,
                         value   = -5))
        ), # /fluidRow
        tags$label("Manual p value limit:"),
        fluidRow(
          column(width = 2,
            checkboxInput(inputId = "volcano_plot_manual_y_upper_limit",
                          label   = NULL,
                          value   = FALSE)),
          column(width = 10,
            numericInput(inputId = "volcano_plot_manual_y_upper_limit_value",
                         label   = NULL,
                         value   = 0.05,
                         min     = 0,
                         max     = 1))
        ) # /fluidRow
      ), # /column
      column(width = 3,
        checkboxInput(inputId = "volcano_plot_annotation",
                      label   = "Show annotations",
                      value   = FALSE),
        sliderInput(inputId = "volcano_plot_annotation_size",
                    label   = "Annotation size:",
                    min     = 0,
                    max     = 15,
                    value   = 5,
                    step    = 1,
                    round   = TRUE,
                    ticks   = TRUE,
                    animate = FALSE,
                    width   = "100%"),
        selectInput(inputId   = "volcano_plot_y_axis_label_type",
                    label     = "Y axis labels:",
                    choices   = list("E notation" = "E", "Superscript" = "superscript", "Decimal" = "decimal"),
                    multiple  = FALSE,
                    selectize = TRUE,
                    width     = "100%")
      ), # /column
      column(width = 3,
        numericInput(inputId = "volcano_plot_annotation_limit_p",
                     label   = "Annotation p value limit (Y axis):",
                     value   = 0.05),
        numericInput(inputId = "volcano_plot_annotation_limit_change",
                     label   = "Annotation change limit (X axis):",
                     value   = 0)
      )
    ) # /fluidRow
  })


  output$volcano_export_ui <- renderUI({
    tagList(
      downloadButton(outputId = "volcano_export_pdf",
                     label    = "Download PDF plot"),
      br(), br(),
      downloadButton(outputId = "volcano_export_xlsx",
                     label    = "Download data as Excel file")
    )
  })



  # ==  RENDER TABLE  ========================================================

  output$volcano_samples_table <- renderTable({
    ui_volcano_samples_table()
  })
  

  output$volcano_statistics_table <- renderTable({
    ui_volcano_statistics_table()
  }, digits = 8)



  # ==  RENDER PLOT  =========================================================

  output$volcano_plot <- renderPlot({
    ui_volcano_plot()
  })



  # ==  DOWNLOAD HANDLERS  ===================================================

  output$volcano_export_pdf <- downloadHandler(
    filename = function() { "Volcano.pdf" },
    content = function(file) {
      pdf(NULL)
      ggsave(filename    = file,
             plot        = ui_volcano_plot(),
             width       = 9 * input$volcano_plot_width / 600,
             height      = 9 * input$volcano_plot_height / 600,
             useDingbats = FALSE)
      dev.off()
    }
  )
  
  
  output$volcano_export_xlsx <- downloadHandler(
    filename = function() { "Volcano data.xlsx" },
    content = function(file) {
      download_xlsx_file(ui_volcano_statistics_table(), file = file)
    }
  )



  # ==  OBSERVE EVENT  =======================================================
  
  session$userData$volcano_selected_groups <- reactiveVal()


  # Store the list of selected groups
  observeEvent(input$volcano_group_1, {
    session$userData$volcano_selected_groups(c(input$volcano_group_1, input$volcano_group_2))
  })


  observeEvent(input$volcano_group_2, {
    session$userData$volcano_selected_groups(c(input$volcano_group_1, input$volcano_group_2))
  })


  # Update the list of selected groups
  observeEvent(input$main_menu, {
    if (input$main_menu == volcano_module_name()) {
      choices <- session$userData$group_name_list()
      selected <- session$userData$volcano_selected_groups()
      if (length(selected) < 2)
        selected <- choices[1:2]
      
      updateSelectInput(session  = session,
                        inputId  = "volcano_group_1",
                        choices  = choices,
                        selected = selected[1][selected[1] %in% choices])
      updateSelectInput(session  = session,
                        inputId  = "volcano_group_2",
                        choices  = choices,
                        selected = selected[2][selected[2] %in% choices])
    }
  })


  # Clear groups and readouts when a new project is loaded
  observeEvent(session$userData$application_state(), {
    if (session$userData$application_state() == "project_loaded") {
      session$userData$volcano_selected_groups(NULL)
      ui_selected_readouts(NULL)
    }
  })



  # ==  REACTIVE  ============================================================

  ui_volcano_messages <- reactiveVal()


  ui_volcano_is_paired <- reactive({
    input$volcano_pairing != "None"
  })


  ui_volcano_groups <- reactive({
    as.numeric(c(input$volcano_group_1, input$volcano_group_2))
  })
  

  ui_volcano_samples <- reactive({
    if (length(ui_selected_readouts()) < 1)
      return(list(data = NULL, error = "No readouts selected."))

    groups <- ui_volcano_groups()

    if (length(groups) != 2)
      return(list(data = NULL, error = "Exactly two groups should be selected."))

    if (groups[1] == groups[2])
      return(list(data = NULL, error = "A group cannot be compared with itself. Select two different groups."))

    samples <- get_samples_for_groups(meta_data  = session$userData$meta_data(),
                                      group_data = session$userData$group_data(),
                                      group_ids  = groups,
                                      pair_by    = if (ui_volcano_is_paired()) input$volcano_pairing else NULL,
                                      color_by   = NULL,
                                      panel_variable = project_get_data(session, "meta_variables_panel"))

    if (is.null(samples)) {
      return(list(data = NULL, error = "No samples found."))
    } else if (!is.null(samples$error)) {
      return(list(data = NULL, error = samples$error))
    } else {
      return(list(data = samples$data, error = NULL))
    }
  })
  
  
  ui_volcano_samples_table <- reactive({
    create_samples_table(samples   = ui_volcano_samples()$data,
                         pair_by   = if (ui_volcano_is_paired()) input$volcano_pairing else NULL)
  })
  

  ui_volcano_data <- reactive({
    samples <- ui_volcano_samples()

    if (!is.null(samples$error))
      return(list(data = NULL, error = samples$error))

    readouts <- ui_selected_readouts()

    if (is.null(readouts))
      return(list(data = NULL, error = "No readouts selected."))

    if (length(unique(samples$data$group_id)) < 2) {
      missing_groups <- setdiff(ui_volcano_groups(), unique(samples$data$group_id))
    } else {
      volcano_data <- get_multiple_readout_comparison_data(samples         = samples$data,
                                                           readouts        = readouts,
                                                           stat_data       = session$userData$stat_data(),
                                                           count_data      = session$userData$count_data(),
                                                           paired_groups   = ui_volcano_is_paired(),
                                                           mean_type       = input$volcano_mean,
                                                           p_adjust_method = input$volcano_p_adjust)

      missing_groups <- volcano_data$missing_groups
    }

    if (length(missing_groups) == 2) {
      return(list(data = NULL, error = "No data found for any of the groups."))
    } else if (length(missing_groups) == 1) {
      missing_group_name <- get_group_names_for_ids(session$userData$group_data(), missing_groups)
      return(list(data = NULL, error = paste0("No data found for the <em>", missing_group_name, "</em> group.")))
    }

    return(list(data = volcano_data$data, error = NULL))
  })  
  
  
  ui_volcano_statistics_table <- reactive({
    create_multiple_readout_comparison_statistics_table(data       = ui_volcano_data()$data,
                                                        group_data = session$userData$group_data(),
                                                        groups     = ui_volcano_groups(),
                                                        mean_type  = input$volcano_mean)
  })
  
  
  ui_volcano_plot <- reactive({
    ui_volcano_messages(NULL)
    messages <- NULL

    volcano_data <- ui_volcano_data()

    if (is.null(volcano_data$data)) {
      if (is.null(volcano_data$error)) {
        ui_volcano_messages("ERROR: No data found.")
      } else {
        ui_volcano_messages(paste0("ERROR: ", volcano_data$error))
      }
      return(NULL)
    }

    result <- create_volcano_plot(data                    = volcano_data$data,
                                  group_data              = session$userData$group_data(),
                                  groups                  = ui_volcano_groups(),
                                  readouts                = ui_selected_readouts(),
                                  comparison              = input$volcano_comparison,
                                  mean_type               = input$volcano_mean,
                                  x_lower_limit           = if (input$volcano_plot_manual_x_lower_limit)
                                                              input$volcano_plot_manual_x_lower_limit_value else NA,
                                  x_upper_limit           = if (input$volcano_plot_manual_x_upper_limit)
                                                              input$volcano_plot_manual_x_upper_limit_value else NA,
                                  y_upper_limit           = if (input$volcano_plot_manual_y_upper_limit)
                                                              input$volcano_plot_manual_y_upper_limit_value else NA,
                                  annotation              = input$volcano_plot_annotation,
                                  annotation_limit_p      = input$volcano_plot_annotation_limit_p,
                                  annotation_limit_change = input$volcano_plot_annotation_limit_change,
                                  annotation_size         = input$volcano_plot_annotation_size,
                                  y_axis_label_type       = input$volcano_plot_y_axis_label_type)

    ui_volcano_messages(c(messages, result$messages))

    return(result$plot)
  })


  ui_volcano_hover_data <- reactive({
    get_volcano_hover_data(data       = ui_volcano_data()$data,
                           comparison = input$volcano_comparison)
  })


  ui_volcano_hover_text <- reactive({
    volcano_hover_data <- ui_volcano_hover_data()
    if (is.null(volcano_hover_data))
      return(NULL)

    point <- nearPoints(df        = volcano_hover_data,
                        coordinfo = input$volcano_plot_hover,
                        xvar      = "x",
                        yvar      = "y",
                        maxpoints = 1,
                        addDist   = TRUE)

    if (nrow(point) < 1) {
      return(NULL)
    } else {
      return(point[1, "text"])
    }
  })

}


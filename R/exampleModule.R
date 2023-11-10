#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


exampleModule_module_name <- function() {
	return("Example module")
}


exampleModule_ui <- function(input, output, session) {
  ui_selected_readouts <- callModule(module = readout_ui, id = "exampleModule")

  output$exampleModule_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        uiOutput("exampleModule-readout_ui")
      ),
      mainPanel(
        selectInput(inputId   = "exampleModule_groups",
                    label     = "Groups:",
                    choices   = session$userData$group_name_list(),
                    multiple  = TRUE,
                    selectize = TRUE,
                    width     = "100%"),
        tabsetPanel(
          tabPanel("Histogram", plotOutput("exampleModule_plot"),),
          tabPanel("Table", tableOutput("exampleModule_table"))
        )
      )
    )
  })

  exampleModule_data <- reactive({
    readout <- ui_selected_readouts()
    if (is.null(readout))
      return(NULL)

    groups <- input$exampleModule_groups
    if (is.null(groups))
      return(NULL)

    samples <- get_samples_for_groups(meta_data      = session$userData$meta_data(),
                                      group_data     = session$userData$group_data(),
                                      group_ids      = groups,
                                      panel_variable = project_get_data(session, "meta_variables_panel"))

    if (is.null(samples) || is.null(samples$data))
      return(NULL)

    data <- get_single_readout_data(samples    = samples$data,
                                    readouts   = readout,
                                    stat_data  = session$userData$stat_data(),
                                    count_data = session$userData$count_data())

    if (is.null(data) || is.null(data$data))
      return(NULL)

    return(data$data)
  })

  output$exampleModule_plot <- renderPlot({
    data <- exampleModule_data()
    if (is.null(data))
      return(NULL)

    plot <- ggplot(data, aes(x = value, color = group_name)) + geom_density() + theme_classic()

    return(plot)
  })

  output$exampleModule_table <- renderTable({
    data <- exampleModule_data()
    if (is.null(data))
      return(NULL)

    return(data)
  }, digits = 8)
}
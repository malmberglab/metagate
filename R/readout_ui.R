readout_ui <- function(input, output, session, multiple = FALSE, multiple_populations = FALSE) {

  # ==  RENDER UI  ===========================================================

  output$readout_ui <- renderUI({
    if (multiple) {
      tagList(
        uiOutput(session$ns("readout_type_ui")),
        uiOutput(session$ns("readout_select_multiple_ui")),
        actionButton(inputId = session$ns("readout_add"),
                     label   = "Add readouts"),
        hr(),
        uiOutput(session$ns("readout_count_ui")),
        uiOutput(session$ns("readout_list")),
        actionLink(inputId = session$ns("readout_remove_selected"),
                   label   = "Remove selected",
                   style   = "margin-right: 14px"),
        actionLink(inputId = session$ns("readout_remove_all"),
                   label   = "Remove all")
      )
    } else {
      tagList(
        uiOutput(session$ns("readout_type_ui")),
        uiOutput(session$ns("readout_select_single_readout_ui")),
        uiOutput(session$ns("readout_select_single_population_ui"))
      )
    }
    
  })


  output$readout_type_ui <- renderUI({
    readout_types <- session$userData$readout_types()

    if (!is.null(session$userData$count_data())) {
      readout_types <- c(readout_types, "Absolute count")
    }

    selectInput(inputId  = session$ns("readout_type"),
                label    = "Marker type:",
                choices  = readout_types,
                width    = "100%",
                multiple = FALSE)
  })


  output$readout_select_single_readout_ui <- renderUI({
    if (is.null(input$readout_type)) 
      return(NULL)

    readouts <- readouts_for_type(input$readout_type)

    selected <- NULL
    if (!is.null(isolate(selected_readouts()))) {
      old <- strsplit(isolate(selected_readouts())[1], "___", fixed = TRUE)[[1]]
      if (old[2] %in% readouts$markers) {
        selected <- old[2]
      }
    }

    if (input$readout_type == "Percent") {
      marker_label <- "Marker/population:"
    } else if (input$readout_type == "Absolute count") {
      marker_label <- "Population:"
    } else {
      marker_label <- "Marker:"
    }

    selectInput(inputId   = session$ns("readout_marker"),
                label     = marker_label,
                choices   = setNames(paste0(input$readout_type, "___", readouts$markers), readouts$markers),
                selected  = paste0(input$readout_type, "___", selected),
                width     = "100%",
                multiple  = FALSE,
                selectize = TRUE)
  })


  output$readout_select_single_population_ui <- renderUI({
    if (is.null(input$readout_type))
      return(NULL)

    readouts <- readouts_for_type(input$readout_type)

    selected <- NULL
    if (!is.null(isolate(selected_readouts()))) {
      old <- sapply(strsplit(isolate(selected_readouts()), "___", fixed = TRUE), "[[", 3)
      selected <- old[old %in% readouts$populations]
    }

    if (input$readout_type != "Absolute count" && multiple_populations && multiple_populations_active()) {
      if (input$readout_type == "Percent") {
        population_label <- "as percentages of populations:"
      } else {
        population_label <- "in populations:"
      }

      ui <- tagList(
        actionLink(inputId = session$ns("readout_use_single_population"),
                   label   = "Use single",
                   style   = "float: right"),

        selectInput(inputId   = session$ns("readout_multiple_populations"),
                    label     = population_label,
                    choices   = readouts$populations,
                    selected  = selected,
                    width     = "100%",
                    multiple  = TRUE,
                    selectize = TRUE)
      )
    } else {
      if (input$readout_type == "Percent") {
        population_label <- "as a percentage of population:"
      } else if (input$readout_type == "Absolute count") {
        population_label <- "Based on absolute counts of:"
      } else {
        population_label <- "in population:"
      }

      ui <- selectInput(inputId   = session$ns("readout_single_population"),
                        label     = population_label,
                        choices   = readouts$populations,
                        selected  = selected[1],
                        width     = "100%",
                        multiple  = FALSE,
                        selectize = TRUE)

      if (input$readout_type != "Absolute count" && multiple_populations) {
        ui <- tagList(
          actionLink(inputId = session$ns("readout_use_multiple_populations"),
                     label   = "Use multiple",
                     style   = "float: right"),
          ui
        )
      }
    }

    return(ui)
  })


  output$readout_select_multiple_ui <- renderUI({
    if (is.null(input$readout_type))
      return(NULL)

    selected_readouts <- selected_readouts()

    readouts <- readouts_for_type(input$readout_type)

    if (input$readout_type == "Percent") {
      marker_label <- "Marker/population:"
      population_label <- "as percentages of populations:"
    } else if (input$readout_type == "Absolute count") {
      marker_label <- "Population:"
      population_label <- "Based on absolute counts of:"
    } else {
      marker_label <- "Marker:"
      population_label <- "in populations:"
    }
  
    return(tagList(
      selectInput(inputId   = session$ns("readout_marker"),
                  label     = marker_label,
                  choices   = readouts$markers,
                  width     = "100%",
                  multiple  = TRUE,
                  selectize = TRUE),

      selectInput(inputId   = session$ns("readout_population"),
                  label     = population_label,
                  choices   = readouts$populations,
                  width     = "100%",
                  multiple  = input$readout_type != "Absolute count",
                  selectize = TRUE)))
  })


  output$readout_count_ui <- renderUI({
    return(div(style="float: right", paste(length(selected_readouts()), "readouts")))
  })


  output$readout_list <- renderUI({
    readout_ids <- selected_readouts()
    if (length(readout_ids) > 0) {
      readout_names <- format_readout_names(readout_ids = readout_ids)
      choices <- as.list(setNames(readout_ids, readout_names))
    } else {
      choices <- NULL
    }
    
    return(selectInput(inputId   = session$ns("readout_list"),
                       label     = "Included readouts:",
                       choices   = choices,
                       width     = "100%",
                       multiple  = TRUE,
                       selectize = FALSE))
  })


  observe({
    if (multiple || is.null(input$readout_marker)) {
      return(NULL)
    }

    has_multiple_populations <- multiple_populations_active() && input$readout_type != "Absolute count"
    
    if (has_multiple_populations && is.null(input$readout_multiple_populations)) {
      selected_readouts(NULL)
    } else {
      type <- input$readout_type

      marker_split <- strsplit(input$readout_marker, "___", fixed = TRUE)[[1]]
      if (marker_split[1] != type) {
        return(NULL)
      }

      marker <- marker_split[2]

      if (has_multiple_populations) {
        populations <- input$readout_multiple_populations
      } else {
        populations <- input$readout_single_population
      }

      selected_readouts(paste0(type, "___", marker, "___", populations))
    }
  })


  readouts_for_type <- function(type) {
    if (type == "Absolute count") {
      data <- session$userData$count_data()
    } else {
      data <- session$userData$stat_data()
    }
    
    #if (type == "Absolute count") {
    #  all_populations <- get_readout_populations(stat_data)
    #  readout_populations <- project_get_data(session = session, name = "meta_variables_count")$population
#
    #  include <- sapply(all_populations, function(population1) {
    #    for (population2 in readout_populations) {
    #      readout <- paste0("Percent___", population2, "___", population1)
    #      if (readout %in% row.names(stat_data) == FALSE) {
    #        return(FALSE)
    #      }
#
    #      values <- setdiff(stat_data[readout, ], NA)
    #      if (length(values) > 0 && all(values == 1)) {
    #        return(TRUE)
    #      }
    #    }
    #    return(FALSE)
    #  })
#
    #  readout_markers <- all_populations[include]
    #} else {
      readouts <- row.names(data)
      readouts <- readouts[startsWith(readouts, paste0(type, "___"))]

      split <- strsplit(readouts, "___", fixed = TRUE)
      readout_markers <- unique(sapply(split, '[', 2))
      readout_markers <- readout_markers[readout_markers != "Bulk"]
    
      readout_populations <- unique(sapply(split, '[', 3))
    #}

    return(list(markers     = readout_markers,
                populations = readout_populations))
  }



  # ==  OBSERVE EVENT  =======================================================

  observeEvent(input$readout_use_single_population, {
    multiple_populations_active(FALSE)
  })


  observeEvent(input$readout_use_multiple_populations, {
    multiple_populations_active(TRUE)
  })


  observeEvent(input$readout_add, {
    if (is.null(input$readout_population)) {
      populations <- "Bulk"
    } else {
      populations <- input$readout_population
    }
  
    new <- paste0(input$readout_type, "___",
                  rep(input$readout_marker, each = length(populations)), "___",
                  populations)
    readout_ids <- intersect(unique(c(selected_readouts(), new)), c(row.names(session$userData$stat_data()),
                                                                    row.names(session$userData$count_data())))
    
    readout_names <- format_readout_names(readout_ids = readout_ids)
  
    selected_readouts(readout_ids)
  })
  

  observeEvent(input$readout_remove_selected, {
    all <- selected_readouts()
    if (length(all) < 1)
      return()
  
    remove <- input$readout_list
    new <- all[all %in% remove == FALSE]
  
    selected_readouts(new)
  })
  

  observeEvent(input$readout_remove_all, {
    selected_readouts(NULL)
  })



  # ==  REACTIVE  ============================================================

  selected_readouts <- reactiveVal()

  multiple_populations_active <- reactiveVal(FALSE)



  # ==  MODULE RETURN  =======================================================

  return(selected_readouts)

}




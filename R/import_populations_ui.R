import_populations_ui <- function(input, output, session) {

  output$import_populations_ui <- renderUI({
    population_data <- session$userData$import_population_data()
    gate_data       <- session$userData$import_gate_data()

    # If user clicked the "back" button to get to this page
    populations_are_defined <- nrow(population_data) > 0
    
    head <- list(h2("Define populations"),
                 p(style = "margin-bottom: 20px",
                   paste0("Use these gates to define the populations that you want to use for downstream analysis. ",
                          "Click the button below when you are done.")),

                 div(style = "float: right",
                     actionButton(inputId = "import_populations_upload_modal",
                                  label   = "Import",
                                  icon    = icon("open", lib = "glyphicon"),
                                  style   = "margin-right: 8px"),

                     downloadButton(outputId = "import_populations_download",
                                    label    = "Export",
                                    style    = "margin-right: 8px"),

                     actionButton(inputId = "import_populations_combinatorial_modal",
                                  label   = "Combinatorial",
                                  icon    = icon("list-alt", lib = "glyphicon"),
                                  style   = "margin-right: 8px"),

                     actionButton(inputId = "import_populations_reset",
                                  label   = "Reset",
                                  icon    = icon("refresh", lib = "glyphicon"),
                                  style   = "margin-right: 8px"),

                     actionButton(inputId = "import_populations_empty",
                                  label   = "Empty list",
                                  icon    = icon("remove-sign", lib = "glyphicon"))),

                 div(actionButton(inputId = "import_populations_back_button",
                                  label   = "Back",
                                  icon    = icon("arrow-left", lib = "glyphicon"),
                                  style   = "margin-right: 8px"),

                     actionButton(inputId = "import_populations_defined_button",
                                  label   = "I am done defining my populations",
                                  icon    = icon("ok", lib = "glyphicon"),
                                  class   = "main_button")),

                 fluidRow(column(width = 3,
                                 tags$h4(style = "margin: 22px 0 14px 0",
                                         label("Name",
                                               "Create a name for the population you are defining. The names will be",
                                               "shown in plots and tables, so it is recommended to make them short and",
                                               "clear."))),
                          column(width = 9,
                                 tags$h4(style = "margin: 22px 0 14px 0",
                                         label("Definition",
                                               "Define which cells should be included in the population by combining",
                                               "gates.")))))

    rows <- lapply(1:setting("populations"), function(i) {
      if (populations_are_defined) {
        name <- population_data[i, "name"]
        selected <- import_parse_population_query(population_data[i, "query"])
      } else if (i <= nrow(gate_data)) {
        name <- gate_data[i, "short_name"]
        selected <- gate_data[i, "name"]
      } else {
        name <- ""
        selected <- NULL
      }

      choices <- as.list(setNames(paste0(c("", "NOT "), rep(gate_data$name, each = 2)),
                                  paste0(c("", "NOT "), rep(gate_data$formatted_name, each = 2))))

      name_input <- tags$input(id    = paste0("population_name_", i),
                               type  = "text",
                               class = "population_name_input",
                               value = name)

      query_input <- import_population_query_selectize(inputId  = paste0("population_query_", i),
                                                       label    = NULL,
                                                       choices  = choices,
                                                       selected = selected)
      return(fluidRow(column(width = 3, name_input),
                      column(width = 9, query_input)))
    })

    return(do.call(tagList, c(head, rows)))
  })



  import_population_query_selectize <- function(inputId, label, choices, selected) {
    render <- I('{
      item: function(item, escape) {
        return "<div class=\'population_selectize_" +
               (item.value.startsWith("NOT ") ? "neg" : "pos") + "\'>" + escape(item.label) + "</div>"
      },
      option: function(item, escape) {
        return "<div class=\'population_selectize_" +
               (item.value.startsWith("NOT ") ? "neg" : "pos") + "\'>" + escape(item.label) + "</div>"
      }
    }')

    return(selectizeInput(inputId  = inputId,
                          label    = label,
                          choices  = choices,
                          selected = selected,
                          width    = "100%",
                          multiple = TRUE,
                          options  = list(render = render)))
  }



  # ==  DOWNLOAD HANDLERS  ===================================================

  output$import_populations_download <- downloadHandler(
    filename = "Populations.xlsx",
    content  = function(file) {
      population_count <- setting("populations")

      names <- sapply(1:population_count, function(i) { input[[paste0("population_name_", i)]] })
      
      definitions <- sapply(1:population_count, function(i) {
        gates <- input[[paste0("population_query_", i)]]
        if(length(gates) < 1) {
          return("")
        } else {
          return(paste(gates, collapse=" "))
        }
      })

      download_xlsx_file(data.frame(Name = names, Definition = definitions), file = file)
    }
  )



  # ==  OBSERVE EVENT  =======================================================

  # Show population upload modal
  observeEvent(input$import_populations_upload_modal, {
    showModal(modalDialog(p("Select a population file that you have exported from another project."),
                          checkboxInput(inputId = "import_populations_upload_remove_old",
                                        label   = "Remove existing populations before adding new",
                                        value   = TRUE,
                                        width   = "100%"),
                          fileInput(inputId  = "import_populations_upload",
                                    label    = NULL,
                                    multiple = FALSE,
                                    accept   = xlsx_mime_types(),
                                    width    = "100%"),
                          title  = "Upload populations",
                          footer = modalButton("Cancel")))
  })


  # Upload population file
  observeEvent(input$import_populations_upload, {
    gate_data <- session$userData$import_gate_data()

    file_path <- input$import_populations_upload$datapath

    population_file <- read_xlsx_file(file_path)
    if (is.null(population_file)) {
      show_error_modal("Unable to read Excel file.")
      return()
    }
  
    file_check <- import_check_population_file(population_file, experiment_gates = gate_data$name)
    if (!is.null(file_check$errors)) {
      show_error_modal(multiple = file_check$errors)
      return()
    }
    
    populations      <- file_check$populations
    population_count <- setting("populations")
    warning_messages <- file_check$warnings
  
    if (isolate(input$import_populations_upload_remove_old)) {
      for (i in 1:population_count) {
        updateTextInput(session = session,
                        inputId = paste0("population_name_", i),
                        value   = "")
        updateSelectInput(session  = session,
                          inputId  = paste0("population_query_", i),
                          selected = "")
      }
  
      empty_slots <- 1:population_count
    } else {
      empty_slots <- which(sapply(1:population_count, function(i) {
        input[[paste0("population_name_", i)]] == "" && length(input[[paste0("population_query_", i)]]) < 1
      }))
    }
  
    # Check if all new populations will fit into the table.
    if (length(empty_slots) < sum(populations$valid)) {
      missing_slots <- sum(populations$valid) - length(empty_slots)
      remove_populations <- row.names(populations) %in% row.names(tail(populations[populations$valid,],
                                                                       missing_slots))
      populations[remove_populations, "valid"] <- FALSE
      for (population_name in populations[remove_populations, "Name"]) {
        warning_messages <- c(warning_messages, paste0("The population ", population_name, " was not added because ",
                                                       "you have exceeded the maximum number of populations in ",
                                                       "your project (", population_count, ")"))
      }
    }
  
    # Populate table with new populations
    for (population in row.names(populations[populations$valid,])) {
      slot <- empty_slots[1]
      empty_slots <- empty_slots[-1]

      updateTextInput(session = session,
                      inputId = paste0("population_name_", slot),
                      value   = populations[population, "Name"])

      updateSelectInput(session  = session,
                        inputId  = paste0("population_query_", slot),
                        selected = import_parse_population_query(populations[population, "Definition"]))
    }
  
    # Remove upload modal
    removeModal()
  
    if (length(warning_messages) < 1) {
      warning_list <- NULL
    } else {
      warning_list <- tags$ul(lapply(lapply(warning_messages, HTML), tags$li))
    }
  
    showModal(modalDialog(tagList(p(style = "font-weight: bold",
                                    sum(populations$valid), " of ", nrow(populations), " populations were added."),
                                  warning_list),
                          title  = "Upload populations",
                          footer = tagList(modalButton("Close"))
    ))
  })


  # Show combinatorial populations modal
  observeEvent(input$import_populations_combinatorial_modal, {
    showModal(import_populations_combinatorial_modal(session$userData$import_gate_data()))
  })


  import_populations_combinatorial_modal <- function(gate_data, selected_gates = NULL, selected_base_population = NULL,
                                                     base_population_name = NULL, error_message = NULL) {
    all_gates <- setNames(gate_data$name, gate_data$formatted_name)
    add_not <- function(x) paste0(rep(c("", "NOT "), length(x)), rep(x, each = 2))
    all_gates_with_not <- setNames(add_not(gate_data$name), add_not(gate_data$formatted_name))

    error_text <- if (is.null(error_message)) NULL else p(error_message, style="font-weight: bold; color: #ff0000")

    max_populations <- floor(log2(setting("populations")))

    return(modalDialog(p("MetaGate will create populations for all possible combinations of the gates you select ",
                         "below. The \"base population\" will be used for all the created populations."),
                       error_text,

                       selectizeInput(inputId = "import_population_combinatorial_modal_gates",
                                      label    = "Gates:",
                                      choices  = all_gates,
                                      selected = selected_gates,
                                      width    = "100%",
                                      multiple = TRUE,
                                      options  = list(maxItems = max_populations)),

                       import_population_query_selectize(inputId  = "import_population_combinatorial_modal_base",
                                                         label    = "Base population:",
                                                         choices  = all_gates_with_not,
                                                         selected = selected_base_population),

                       textInput(inputId = "import_population_combinatorial_modal_base_name",
                                 label   = "Base population name: (will be appended to all population names)",
                                 value   = base_population_name,
                                 width   = "100%"),
                       
                       title = "Create combinatorial populations",
                       footer = tagList(modalButton("Cancel"),
                                        actionButton(inputId = "import_populations_combinatorial_modal_create",
                                                     label   = "Create populations",
                                                     icon    = icon("ok", lib = "glyphicon")))
    ))
  }


  # Create combinatorial populations
  observeEvent(input$import_populations_combinatorial_modal_create, {
    gate_data <- session$userData$import_gate_data()

    gates                <- input$import_population_combinatorial_modal_gates
    gates_short          <- sapply(gates, function(gate) { gate_data[gate_data$name == gate, "short_name"] })
    base_population      <- input$import_population_combinatorial_modal_base
    base_population_name <- input$import_population_combinatorial_modal_base_name

    if (nchar(base_population_name) > 0)
      base_population_name <- paste0(" ", base_population_name)

    error <- NULL

    if (length(gates) < 2)
      error <- "You have to select at least two gates to create combinatorial populations."

    if (is.null(error)) {
      empty_slots <- which(sapply(1:setting("populations"), function(i) {
        input[[paste0("population_name_", i)]] == "" && length(input[[paste0("population_query_", i)]]) < 1
      }))

      if (length(empty_slots) < 2^length(gates)) {
        error <- paste0("The combinatorial populations could not be created because you exceeded the maximum ",
                        "number of populations in your project (", setting("populations"), "). ",
                        "Try removing some existing populations or selected fewer gates below.")
      }
    }

    if (is.null(error)) {
      combinations <- function(gates, gate_options) {
        strings <- NULL
        for (gate in gates) {
          strings <- paste(strings, rep(x          = gate_options(gate),
                                        each       = (2^which(gates==gate)) / 2,
                                        length.out = 2^length(gates)))
        }
        return(strings)
      }

      gates_stripped <- sapply(gates_short, function(gate) {
        stripped <- gsub("(\\+|\\-)$", "", gate)
        if (sum(stripped == gsub("(\\+|\\-)$", "", gates_short)) > 1) {
          return(gate)
        } else {
          return(stripped)
        }
      })

      populations <- data.frame(name  = paste0(combinations(gates        = gates_stripped,
                                                            gate_options = function(x) paste0(x, c("+", "-"))),
                                               base_population_name),
                                query = paste(combinations(gates        = gates,
                                                           gate_options = function(x) paste0(c("", "NOT "), x)),
                                              paste(base_population, collapse = " ")))

      for (slot in empty_slots) {
        i <- which(empty_slots == slot)

        updateTextInput(session = session,
                        inputId = paste0("population_name_", slot),
                        value   = populations[i, "name"])

        updateSelectInput(session  = session,
                          inputId  = paste0("population_query_", slot),
                          selected = import_parse_population_query(populations[i, "query"]))
      }
    }

    if (is.null(error)) {
      modal <- modalDialog(p(2^length(gates), " new populations were successfully created."),
               title  = "Create combinatorial populations",
               footer = modalButton("Close"))
    } else {
      modal <- import_populations_combinatorial_modal(
                                         gate_data = gate_data,
                                         selected_gates = input$import_population_combinatorial_modal_gates,
                                         selected_base_population = input$import_population_combinatorial_modal_base,
                                         base_population_name = input$import_population_combinatorial_modal_base_name,
                                         error_message = error)
    }

    showModal(modal)
  })


  # Button: Reset population table.
  observeEvent(input$import_populations_reset, {
    gate_data <- session$userData$import_gate_data()
  
    for (i in 1:setting("populations")) {
      if (i <= nrow(gate_data)) {
        short_name <- gate_data[i, "short_name"]
      } else {
        short_name <- ""
      }

      if (i <= nrow(gate_data)) {
        name <- gate_data[i, "name"]
      } else {
        name <- ""
      }
  
      updateTextInput(session = session,
                      inputId = paste0("population_name_", i),
                      value   = short_name)

      updateSelectInput(session  = session,
                        inputId  = paste0("population_query_", i),
                        selected = name)
    }
  })
  

  # Button: Empty population table.
  observeEvent(input$import_populations_empty, {
    for (i in 1:setting("populations")) {
      updateTextInput(session = session,
                      inputId = paste0("population_name_", i),
                      value   = "")

      updateSelectInput(session  = session,
                        inputId  = paste0("population_query_", i),
                        selected = "")
    }
  })


  # Check populations
  observeEvent(input$import_populations_defined_button, {
    gate_data <- session$userData$import_gate_data()

    population_count <- setting("populations")

    population_names <- sapply(1:population_count, function(i) {
      input[[paste0("population_name_", i)]]
    })

    populations <- do.call(rbind, lapply(1:population_count, function(i) {
      gates <- input[[paste0("population_query_", i)]]

      if (is.null(gates)) {
        query <- ""
        query_formatted <- ""
      } else {
        query <- paste(gates, collapse = " ")

        gates_formatted <- sapply(gates, function(gate) {
          if (startsWith(gate, "NOT ")) {
            paste0("NOT ", gate_data[gate_data$name == substring(gate, 5), "formatted_name"])
          } else {
            gate_data[gate_data$name == gate, "formatted_name"]
          }
        })

        query_formatted <- gsub("\\", "", gates_formatted, fixed = TRUE)
        query_formatted <- paste(c("", query_formatted), collapse = "\\")
      }

      return(data.frame(name             = input[[paste0("population_name_", i)]],
                        query            = query,
                        query_formatted  = query_formatted,
                        stringsAsFactors = FALSE))
    }))

    result <- import_parse_populations(populations)

    if (length(result$errors) < 1) {
      session$userData$import_population_data(result$populations)
      session$userData$application_state("populations_defined")
    } else {
      show_error_modal(multiple = result$errors)
    }
  })


  observeEvent(input$import_populations_back_button, {
    if (!is.null(session$userData$import_gatingML())) {
      session$userData$application_state("cytobank_selected")
    } else {
      session$userData$application_state("flowjo_selected")
    }

    session$userData$import_gate_data(data.frame())
    session$userData$import_gatingML(NULL)
    session$userData$import_flowjo_file_names(NULL)
    session$userData$import_flowjo_workspace_path(NULL)
    session$userData$import_import_file_name(NULL)
    session$userData$import_population_data(data.frame())
  })



}
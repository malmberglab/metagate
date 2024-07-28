#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


group_ui <- function(input, output, session) {
    
  # ==  RENDER UI  ===========================================================

  output$group_ui <- renderUI({
    head <- list(uiOutput("group_top_message_ui"),
                 fluidRow(column(width = 3,
                                 tags$h4("Group names", style="margin: 0 0 14px 0")),
                          column(width = 9,
                                 tags$h4("Group definitions", style="margin: 0 0 14px 0"))))

    rows <- lapply(1:setting("groups"), function(i) {
      fluidRow(column(width = 3, 
                      tags$input(id = paste0("group_name_", i), type = "text", class = "group_name_input")),
               column(width = 9,
                      selectInput(inputId   = paste0("group_query_", i),
                                  label     = NULL,
                                  choices   = NULL,
                                  width     = "100%",
                                  selectize = TRUE,
                                  multiple  = TRUE)))
    })

    return(do.call(tagList, c(head, rows)))
  })


  output$group_top_message_ui <- renderUI({
    if (ncol(session$userData$meta_data()) < 2) {
      div(class="message_warning message_warning_top",
          p("You need to import some meta data before you can create any groups!"))
    }
  })
  


  # ==  OBSERVE EVENT  =======================================================

  observeEvent(input$main_menu, {
    if (session$userData$application_state() == "project_loaded") {
      if (input$main_menu == "Groups") {
        if (!session$userData$group_editor_loaded()) {

          # The Groups tab is selected for the first time, update inputs from session$userData$group_data():
          show_processing_modal("Loading", "Please wait while group settings are loaded.")
          group_data <- session$userData$group_data()
          if (length(group_data) > 0) {
            if (length(group_data) < setting("groups")) {
              group_data <- c(group_data, lapply((length(group_data)+1):setting("groups"),
                                                 function(i) { list(id = i, name = NULL, query = NULL) }))
            }

            for (id in 1:setting("groups")) {
              updateTextInput(session = session,
                              inputId = paste0("group_name_", id),
                              value   = group_data[[id]]$name)

              updateSelectInput(session  = session,
                                inputId  = paste0("group_query_", id),
                                choices  = group_all_options(),
                                selected = group_data[[id]]$query)
            }
          }
          session$userData$group_editor_loaded(TRUE)
          removeModal()
        }

        # As the Groups tab is selected, groups might have been changed. Set group_editor_changed() to TRUE so that
        # group_data() will be updated when moving away from the Groups tab:
        session$userData$group_editor_changed(TRUE)
      } else {
        if (session$userData$group_editor_loaded() && session$userData$group_editor_changed()) {

          # When moving away from the Groups tab, update session$userData$group_data() by reading the inputs on the
          # Groups tab:
          group_data <- lapply(1:setting("groups"), function(id) {
            list(id    = id,
                 name  = input[[paste0("group_name_", id)]],
                 query = input[[paste0("group_query_", id)]])
          })
          session$userData$group_data(group_data)
          session$userData$group_editor_changed(FALSE)
        }
      }
    }
  })



  # ==  REACTIVE  ============================================================

  group_all_options <- reactive({
    if (session$userData$application_state() == "project_loaded") {
      variables <- project_get_data(session, "meta_variables_group")
      if (length(variables) < 1)
        return(NULL)
  
      meta_data <- session$userData$meta_data()
  
      options <- unlist(lapply(variables, function(variable) {
        values <- meta_data[, variable]
        values[is.na(values) | values == ""] <- "(none)"
        return(paste0(variable, ": ", unique(values)))
      }))
  
      return(options)
    }
  })


}
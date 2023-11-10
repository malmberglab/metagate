#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


basic_ui <- function(input, output, session) {

  # ==  RENDER UI  ===========================================================

  output$basic_ui <- renderUI({
    state <- session$userData$application_state()

    if (state == "project_loaded") {
      analysis_module_tabs <- lapply(setting("modules"), function(module) {
        tabPanel(do.call(paste0(module, "_module_name"), list()),
                 id = paste0("tab_", module),
                 uiOutputWithLoader(paste0(module, "_ui")))
      })

      tabs <- c(list(tabPanel("Project",     id = "tab_project",     uiOutputWithLoader("project_ui")),
                     tabPanel("Meta data",   id = "tab_meta",        uiOutputWithLoader("meta_ui")),
                     tabPanel("Groups",      id = "tab_group",       uiOutputWithLoader("group_ui"))),
                analysis_module_tabs,
                list(tabPanel("Help",        id = "tab_help",        uiOutput("help_ui"))))

      do.call("navbarPage",
              c(tabs, list(
              position    = "fixed-top",
              title       = uiOutput("project_name_ui"),
              windowTitle = "MetaGate",
              id          = "main_menu")))
      
    } else if (state %in% c("fcs_uploaded", "populations_defined", "gate_file_loaded",
                            "flowjo_selected", "cytobank_selected")) {
      navbarPage(position    = "fixed-top",
                 title       = "",
                 windowTitle = "MetaGate",
                 
                 tabPanel("New project", id = "tab_import", uiOutput("import_ui")),
                 tabPanel("Help",        id = "tab_help",   uiOutput("help_ui")))
    } else {
      navbarPage(position    = "fixed-top",
                 title       = "",
                 windowTitle = "MetaGate",
                 
                 tabPanel("Start",       id = "tab_start",  uiOutput("start_ui")),
                 tabPanel("Help",        id = "tab_help",   uiOutput("help_ui")))
    }
  })

  
  output$project_name_ui <- renderUI({
    project_get_name(session)
  })



  # ==  OBSERVE EVENT  =======================================================
  
  observeEvent(input$keyboard_pressed, {
    shortcuts <- list("P" = "Project",
                      "M" = "Meta data",
                      "G" = "Groups",
                      "H" = "Help")
    
    if (input$keyboard_pressed %in% names(shortcuts)) {
      updateNavbarPage(session  = session,
                       inputId  = "main_menu",
                       selected = shortcuts[[input$keyboard_pressed]])
    }
  })

}

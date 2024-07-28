#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


help_ui <- function(input, output, session) {
  
  # ==  RENDER UI  ===========================================================

  output$help_ui <- renderUI({
    status_text <- paste0("You are running MetaGate version ",
                          packageDescription("metagate", fields = "Version"), " ")

    if (setting("debug"))
      status_text <- paste0(status_text, "in debug mode ")

    status_text <- paste0(status_text, "computer with ", parallel::detectCores(), " available cores.")


    ui <- tagList(p(HTML("For more information about how to use MetaGate,<br>please visit ",
                         "<a href=\"http://metagate.malmberglab.com\" target=\"_blank\"> ",
                         "<strong>metagate.malmberglab.com</strong></a>")),

                  h3("Citation"),
                  p("Please cite the following article:"),
                  p(HTML("<em>Ask EH, Tschan-Plessl A, Hoel HJ, Kolstad A, Holte H, Malmberg KJ. <strong>MetaGate: Interactive analysis of high-dimensional cytometry data with metadata integration.</strong> Patterns. 2024 May;100989. <a href=\"https://doi.org/10.1016/j.patter.2024.100989\" target=\"_blank\">https://doi.org/10.1016/j.patter.2024.100989</a>.</em>")),

                  fluidRow(
                    column(6, h3("Technical information"), tableOutput("help_technical_table")),
                    column(6, h3("Settings"), tableOutput("help_settings_table"))
                  ))

    return(ui)
  })


  output$help_technical_table <- renderTable({
    t(data.frame(
      "MetaGate version" = packageDescription("metagate", fields = "Version"),
      "R version"        = R.Version()$version.string,
      "Operating system" = paste0(osVersion, " (", R.Version()$platform, ")"),
      "Available cores"  = parallel::detectCores(),
      "Shiny version"    = packageDescription("shiny", fields = "Version"),
      check.names = FALSE
    ))
  }, colnames = FALSE, rownames = TRUE)

  output$help_settings_table <- renderTable({
    t(data.frame(
      "Debug mode"             = if (setting("debug")) "enabled" else "disabled",
      "Loaded modules"         = paste(setting("modules"), collapse = ", "),
      "Population limit"       = setting("populations"),
      "Group limit"            = setting("groups"),
      "Asterisk limits"        = paste(setting("asterisk_limits"), collapse = ", "),
      "Post-hoc p value limit" = setting("posthoc_limit"),
      check.names = FALSE
    ))
  }, colnames = FALSE, rownames = TRUE)

}
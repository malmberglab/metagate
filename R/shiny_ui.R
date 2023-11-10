#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


shiny_ui <- function() {
  return(tagList(
    tags$head(tags$script(src = "www/misc.js")),
    tags$head(tags$script(src = "www/marker_selection.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")),
    tags$html(id = "metagate"),

    useShinyjs(),
    
    uiOutput("basic_ui"),
    
    div(class = "not-running", 
        div(h3("MetaGate stopped running"),
            p("This could be caused by an error or by intentionally shutting down metagate in the R console. ",
              "Please refer to the R console to see possible error messages."),
            p("To restart metagate, go to the R console, press ESC on your keyboard, then enter run_metagate()")))
  ))
}

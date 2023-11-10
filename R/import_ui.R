#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


import_ui <- function(input, output, session) { 

  # ==  RENDER UI  ===========================================================

  output$import_ui <- renderUI({
    state <- session$userData$application_state()

    if (state == "fcs_uploaded") {
      uiOutputWithLoader("import_parameters_ui")
    } else if (state == "populations_defined") {
      uiOutputWithLoader("import_fcs_ui")
    } else if (state == "gate_file_loaded") {
      uiOutputWithLoader("import_populations_ui")
    } else if (state == "flowjo_selected") {
      uiOutputWithLoader("import_flowjo_ui")
    } else if (state == "cytobank_selected") {
      uiOutputWithLoader("import_cytobank_ui")
    }
  })

}


cytobank_parse <- function(path) {
  gatingML <- tryCatch(suppressMessages(CytoML:::read.gatingML.cytobank(path)),
                       warning = function(w) { msg("Warning reading GatingML file: ", w$message); return(NULL) },
                       error = function(e) { msg("Error reading GatingML file: ", e$message); return(NULL) })
  if (is.null(gatingML)) {
    return(list(error = "Unable to read Cytobank GatingML file."))
  }

  for (node in names(gatingML@nodeData@data)) {
    name <- gatingML@nodeData@data[[node]]$popName
    if (grepl("/", name)) {
      gatingML@nodeData@data[[node]]$popName <- gsub("/", ":", name)
    }
  }

  nodes <- flowWorkspace::getNodes(gatingML)
  gate_data <- data.frame(id = seq_along(nodes), cytobank_name = names(nodes))
  gate_data$path <- sapply(gate_data$cytobank_name, function(node) {
    path <- ""
    while (length(node) > 0) {
      path <- paste0("/", gsub("/", ":", nodes[node]), path)
      node <- flowWorkspace::getParent(gatingML, node)
    }
    return(path)
  })
  gate_data$name <- gsub(" ", "_", gate_data$path)
  gate_data$formatted_name <- gsub("/", " => ", gsub("^/", "", gate_data$name))
  gate_data$short_name <- nodes

  return(list(error     = NULL,
              gate_data = gate_data,
              gatingML  = gatingML))
}


cytobank_create_gate_matrix <- function(cs, gate_data, gatingML, event_count) {
  gs <- flowWorkspace::GatingSet(cs)
  gs <- flowWorkspace::compensate(gs, gatingML)
  transformations <- CytoML:::getTransformations.graphGML(gatingML)
  gs <- flowWorkspace::transform(gs, transformations)
  
  suppressMessages(CytoML:::gating_graphGML(gatingML, gs, transformations))

  gate_matrix <- sapply(gate_data$name, function(gate) {
    tryCatch({ suppressWarnings({
      path <- gate_data$path[gate_data$name == gate]
      bit::as.bit(flowWorkspace::gh_pop_get_indices(obj = gs[[1]], y = path))
    }) }, error = function(e) NULL)
  }, simplify = FALSE, USE.NAMES = TRUE)

  missing_gates <- names(gate_matrix)[sapply(gate_matrix, is.null)]

  for (gate in missing_gates) {
    gate_matrix[[gate]] <- bit::bit(event_count)
  }

  return(list(gate_matrix   = gate_matrix,
              missing_gates = missing_gates))
}


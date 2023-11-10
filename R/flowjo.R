#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


# Parse a workspace file from FlowJo
flowjo_parse <- function(path) {
  wsp <- tryCatch({
    CytoML::open_flowjo_xml(path, sample_names_from = "sampleNode")
  }, warning = function(w) w, error = function(e) e)

  if (!is(wsp, "flowjo_workspace")) {
    if ("message" %in% names(wsp))
      msg(wsp$message)
    
    return(list(error = "Could not load FlowJo workspace."))
  }

  xml <- XML::xmlParse(path)

  if (flowjo_workspace_is_rescaled(xml)) {
    return(list(error = HTML("Could not load FlowJo workspace because a rescaling divider has been set in FlowJo. This 
typically happens if the FCS files originate from a YETI instrument.</p><p>Information about how to change this 
can be found in the <a href=\"https://docs.flowjo.com/flowjo/workspaces-and-samples/flowjo-and-your-cytometer/
ws-instrumentation/\" target=\"_blank\">FlowJo documentation</a>.</p><p>In FlowJo, set the <em>rescaling divider
</em> to 1 and create a new workspace.")))
  }

  file_names <- flowjo_get_file_names(xml)

  XML::free(xml)
    
  if (is.null(file_names) || !is.data.frame(file_names))
    return(list(error = "Unable to read FCS file paths from FlowJo workspace file."))
  
  if (anyDuplicated(file_names$name))
    return(list(error = "FlowJo workspace contains multiple samples with the same FCS file name."))

  nodes_for_samples <- tryCatch({
    lapply(seq_along(file_names$sampleID), function(i) {
      progress_indicator(i, length(file_names$sampleID), "Parsing FlowJo workspace...", TRUE)
      gh <- flowjo_gh_for_sampleID(wsp, as.character(file_names$sampleID[i]))
      nodes <- setdiff(flowWorkspace::gs_get_pop_paths(gh, path = "full"), "root")
      rm(gh)

      return(nodes)
    })
  }, error = function(e) e)

  if ("error" %in% class(nodes_for_samples)) {
    error_text <- "Unable to parse FlowJo workspace."

    if ("message" %in% names(nodes_for_samples)) {
      msg("flowjo_parse: ", nodes_for_samples$message)
      if (nodes_for_samples$message == "Invalid sample group index.") {
        error_text <- paste("Unable to parse FlowJo workspace because there are no gates.",
                            "Please set at least one gate and try again.")
      } else if (nodes_for_samples$message %in% c("'names' attribute [4] must be the same length as the vector [1]",
                                                  "No samples in this workspace to parse!")) {
        error_text <- paste("Unable to parse FlowJo workspace because there are no samples or no gates.",
                            "Please add at least one sample and try again.")
      } else if (startsWith(nodes_for_samples$message, "Duplicated GUIDs detected within group:")) {
        error_text <- paste("Unable to parse FlowJo workspace because there seem to be duplicated samples.",
                            "Please make sure that each FCS file is included only once.")
      }
    }

    return(list(error = error_text))
  }

  all_nodes <- unique(unlist(nodes_for_samples))
  common_nodes <- Reduce(intersect, nodes_for_samples)
  missing_nodes <- setdiff(all_nodes, common_nodes)

  if (length(common_nodes) < 1) {
    return(list(error = "No gates were found in all samples."))
  }

  if (length(missing_nodes) > 0) {
    missing_nodes_df <- as.data.frame(do.call(cbind, lapply(nodes_for_samples, function(nodes_for_sample) {
      ifelse(missing_nodes %in% nodes_for_sample, "", "MISSING")
    })))
    names(missing_nodes_df) <- file_names$name
    row.names(missing_nodes_df) <- missing_nodes
  } else {
    missing_nodes_df <- NULL
  }

  gh <- flowjo_gh_for_sampleID(wsp, file_names$sampleID[1])
  node_names <- data.frame(node       = flowWorkspace::gs_get_pop_paths(gh, path = "full"),
                           node_short = flowWorkspace::gs_get_pop_paths(gh, path = "auto"))
  node_names <- node_names[node_names$node %in% common_nodes, ]

  gate_data <- data.frame(id               = 1:length(node_names$node),
                          flowjo_name      = node_names$node,
                          name             = gsub(" ", "_", node_names$node),
                          formatted_name   = flowjo_format_gate_names(node_names$node),
                          short_name       = flowjo_format_gate_names(node_names$node_short),
                          stringsAsFactors = FALSE)

  return(list(error         = NULL,
              missing_gates = missing_nodes_df,
              gate_data     = gate_data,
              file_names    = file_names))
}


flowjo_format_gate_names <- function(names) {
  return(gsub("/", " => ", gsub("^/", "", names)))
}


flowjo_gh_for_sampleID <- function(wsp, sampleID) {

  invisible(capture.output(suppressWarnings({
    samples <- CytoML::fj_ws_get_samples(wsp, 1)
    subset <- samples[samples$sampleID == sampleID, "name"]
  
    gs_template <- CytoML::flowjo_to_gatingset(wsp,
                                               name                = 1,
                                               subset              = subset,
                                               execute             = FALSE,
                                               additional.sampleID = TRUE)
  
    template_sampleIDs <- as.numeric(sapply(flowWorkspace::sampleNames(gs_template),
                                            function(x) tail(strsplit(x, "_")[[1]], 2)[1]))
  })))
  
  gs_template_index <- which(template_sampleIDs == sampleID)

  if (length(gs_template_index) < 1) {
    stop("No gating hierarchies found for sampleID: ", sampleID)
  } else if (length(gs_template_index) > 1) {
    stop("Multiple gating hierarchies found for sampleID: ", sampleID)
  }

  return(gs_template[[gs_template_index]])
}


flowjo_create_gate_matrix <- function(cs, sampleID, gate_data, flowjo_workspace_path, event_count) {
  suppressMessages({
      wsp <- CytoML::open_flowjo_xml(flowjo_workspace_path)
      gh <- flowjo_gh_for_sampleID(wsp, sampleID)

      channel_names <- names(flowWorkspace::markernames(cs[[1]]))
      for (channel_name in channel_names[grepl("^<(.*)>$", channel_names)]) {
        flowWorkspace::cf_rename_channel(cs[[1]], channel_name, gsub("^<(.*)>$", "[\\1]", channel_name))
      }

      gs <- flowWorkspace::gh_apply_to_cs(gh, cs, compensation_source = "sample")
  })

  rm(wsp, gh)

  gate_matrix <- sapply(gate_data$name, function(gate) {
    ind <- try(flowWorkspace::gh_pop_get_indices(obj = gs[[1]], y = gate_data[gate_data$name == gate, "flowjo_name"]))
    if (is.logical(ind)) {
      return(as.bit(ind))
    } else {
      return(NULL)
    }
  }, simplify = FALSE, USE.NAMES = TRUE)

  rm(gs)

  missing_gates <- names(gate_matrix)[sapply(gate_matrix, is.null)]

  for (gate in missing_gates) {
    gate_matrix[[gate]] <- bit(event_count)
  }

  return(list(gate_matrix   = gate_matrix, 
              missing_gates = missing_gates))
}


flowjo_workspace_is_rescaled <- function(xml) {
  return(any(unlist(XML::xpathApply(xml, "/Workspace/Cytometers/Cytometer", function(s) {
    XML::xmlGetAttr(s, "linearRescale") != "1" || XML::xmlGetAttr(s, "logRescale") != "1"
  }))))
}

flowjo_get_file_names <- function(xml) {
  datasets <- XML::xpathApply(xml, "/Workspace/SampleList/Sample/DataSet",
                              function(s) {
                                list(sampleID = as.numeric(XML::xmlGetAttr(s, "sampleID")),
                                     name = basename(URLdecode(XML::xmlGetAttr(s, "uri"))))
                              })

  return(tryCatch(do.call(rbind.data.frame, datasets), error = function(e) { msg(e$message); return(NULL) }))
}

#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


load_metagate_file <- function(path) {
  if (!endsWith(path, paste0(".", setting("project_file_extension")))) {
    return(list(error = paste0("The selected file is not a .", setting("project_file_extension"), " file")))
  }

  if (tryCatch({ load(path) },
               warning = function(w) { return("warning") },
               error = function(error) { return("error") }) != "save") {
    return(list(error = "Could not open file"))
  }

  required_components <- c("gate_data", "parameter_data", "population_data", "fcs_data", "stat_data", "meta_data",
                           "project_data", "group_data")

  # Legacy file support:
  required_components <- setdiff(required_components, "gate_data")
  
  missing_components <- required_components[required_components %in% names(save) == FALSE]
  if (length(missing_components) > 0) {
    msg("load_metagate_file: File lacks the following components: ", paste(missing_components, collapse = ", "))
    return(list(error = "Could not open file"))
  }

  return(list(data = save, error = NULL))
}


project_create_name_modal <- function(name, failed = FALSE) {
  modalDialog(title = "Change project name",
              textInput("project_change_name_input", "New project name:", value = name, width = "100%"),
              if (failed) div("Name must be at least 3 characters long.", style = "font-weight: bold; color: red"),
              footer = tagList(modalButton("Cancel"),
                               actionButton("project_change_name_ok", "Change name")))
}


project_create_save_modal <- function(name) {
  modalDialog(title = "Save project",
              textInput("project_save_name", "Please enter a file name:", value = name, width = "100%"),
              footer = tagList(modalButton("Cancel"),
                               downloadButton(outputId = "project_save_download", label = "Save")))
}


project_create_summary_table <- function(project_data) {
  if (length(project_data) < 1)
    return(NULL)

  if (!is.na(project_data$time_created)) {
    created <- format(project_data$time_created, format = "%a %d. %B %Y, %H:%M:%S")
  } else {
    created <- "Unknown date"
  }
  if (!is.null(project_data$version_created) && !is.na(project_data$version_created)) {
    created <- paste0(created, " (using MetaGate version ", project_data$version_created, ")")
  } else {
    created <- paste0(created, " (using unknown MetaGate version)")
  }

  if (!is.na(project_data$time_saved)) {
    saved <- format(project_data$time_saved, format = "%a %d. %B %Y, %H:%M:%S")
    if (!is.null(project_data$version_saved) && !is.na(project_data$version_saved)) {
      saved <- paste0(saved, " (with MetaGate version ", project_data$version_saved, ")")
    }
  } else {
    saved <- "Not yet saved"
  }
  

  if (length(project_data$population_size_limit) > 1) {
    population_size_limit <- paste(sapply(seq_along(project_data$population_size_limit), function(i) {
      paste0(project_data$gate_file[i], ": ", project_data$population_size_limit[i], " events")
    }), collapse = ", ")
  } else if (is.na(project_data$population_size_limit)) {
    population_size_limit <- "Unknown"
  } else {
    population_size_limit <- paste(project_data$population_size_limit, "events")
  }

  df <- data.frame("Project name:"          = project_data$project_name,
                   "Created:"               = created,
                   "Last saved:"            = saved,
                   "Gate files:"            = paste(project_data$gate_file, collapse = ", "),
                   "Number of FCS files:"   = paste(project_data$fcs_file_count, "files"),
                   "Number of gates:"       = paste(project_data$gate_count, "gates"),
                   "Number of readouts:"    = paste(project_data$readout_count, "readouts"),
                   "Population size limit:" = population_size_limit,
                   check.names = FALSE)

  return(t(df))
}


project_create_parameter_table <- function(parameter_data) {
  if (nrow(parameter_data) < 1) {
    return(NULL)
  }

  readout_types <- intersect(c("mean", "median", "geomean"), names(parameter_data))
  included <- rep(FALSE, nrow(parameter_data))
  for (readout_type in readout_types) {
    included <- included | parameter_data[, readout_type]
  }

  parameter_data <- parameter_data[included, ]

  return(data.frame("Title"          = parameter_data$title,
                    "Marker"         = parameter_data$desc,
                    "Tag"            = parameter_data$name,
                    "Samples"        = paste0(parameter_data$found, " of ", parameter_data$total),
                    "Transformation" = parameter_data$transform,
                    "Cofactor"       = parameter_data$cofactor,
                    check.names      = FALSE))
}


project_create_fcs_table <- function(fcs_data, readout_count) {
  if (nrow(fcs_data) < 1)
    return(NULL)

  return(data.frame("Name"             = fcs_data$name,
                    "Events"           = fcs_data$events,
                    "Missing readouts" = paste0(round(fcs_data$missing / readout_count * 100, 1),
                                                " % (", fcs_data$missing, " readouts)"),
                    "Compensated"      = ifelse(fcs_data$compensate, "Yes", "No"),
                    "Instrument"        = fcs_data$cytometer,
                    "Date"             = fcs_data$date,
                    "$FIL"             = fcs_data$FIL,
                    check.names        = FALSE))
}


project_create_population_table <- function(population_data) {
  population_data$query_html <- sapply(population_data$query_formatted, function(x) {
    if (!startsWith(x, "\\")) {
      return(x)
    }

    x <- substring(x, 2)

    parts <- sapply(strsplit(x, "\\", fixed = TRUE)[[1]], function(gate) {
      if (startsWith(gate, "NOT ")) {
        class <- "neg"
      } else {
        class <- "pos"
      }
      paste0("<span class=\"gate_definition_", class, "\">", gate, "</span>")
    })

    return(paste0(parts, collapse = ""))
  })
  return(data.frame("Name"       = population_data$name,
                    "Definition" = population_data$query_html))
}


project_create_gate_table <- function(gate_data) {
  return(data.frame("ID"             = gate_data$id,
                    "Name"           = gate_data$name,
                    "Formatted name" = gate_data$formatted_name,
                    check.names      = FALSE))
}


project_get_data <- function(session, name) {
  project_data <- session$userData$project_data()
  if (length(project_data) < 1 || name %in% names(project_data) == FALSE) {
    return(NULL)
  } else {
    return(project_data[[name]])
  }
}


project_set_data <- function(session, name, value) {
  project_data <- session$userData$project_data()
  project_data[[name]] <- value
  session$userData$project_data(project_data)
}


project_get_name <- function(session) {
  name <- project_get_data(session, "project_name")
  if (is.null(name) || is.na(name)) {
    return("")
  } else {
    return(name)
  }
}


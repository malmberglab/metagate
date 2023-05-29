merge_projects <- function(paths) {
  messages <- NULL

  projects <- lapply(paths, load_metagate_file)
  has_error <- sapply(projects, function(project) { !is.null(project$error) })

  if (any(has_error)) {
    errors <- sapply(which(has_error), function(i) {
      paste0("Error when reading ", basename(paths[i]), ": ", projects[[i]]$error)
    })
    return(list(error = errors))
  }

  projects <- lapply(projects, "[[", "data")


  # Get gate file names
  gate_file_names <- sapply(projects, function(project) {
    gate_file <- project$project_data$gate_file

    if (length(gate_file) > 1) {
      return(NA)
    } else {
      return(gate_file)
    }
  })

  all_gate_file_names <- unlist(sapply(projects, function(project) { project$project_data$gate_file }))

  if (any(duplicated(gate_file_names[!is.na(gate_file_names)])) || any(duplicated(all_gate_file_names))) {
    return(list(error = "Some gate files have identical names."))
  }


  all_fcs_names <- unlist(sapply(projects, function(project) { project$fcs_data$name }))
  if (any(duplicated(all_fcs_names))) {
    return(list(error = paste0("Some FCS files have identical names: ",
                               paste(all_fcs_names[duplicated(all_fcs_names)], collapse = ", "))))
  }


  merged <- list()

  # Merge gate_data
  merged$gate_data <- merge_project_df(lapply(projects, "[[", "gate_data"), gate_file_names)

  # Merge parameter_data
  merged$parameter_data <- merge_project_df(lapply(projects, "[[", "parameter_data"), gate_file_names)

  # Merge population_data
  merged$population_data <- merge_project_df(lapply(projects, "[[", "population_data"), gate_file_names)

  # Merge fcs_data
  merged$fcs_data <- merge_project_df(lapply(projects, "[[", "fcs_data"), gate_file_names)

  # Merge stat_data
  merged$stat_data <- merge_stat_data(lapply(projects, "[[", "stat_data"))

  # Merge meta_data
  merged$meta_data <- merge_project_df(lapply(projects, "[[", "meta_data"), gate_file_names, replace_missing = "")
  
  # Create new project data
  event_limits <- unlist(lapply(projects, function(project) { project$project_data$population_size_limit }))
  merged$project_data <- list("project_name"              = "Untitled merged project",
                              "time_created"              = Sys.time(),
                              "time_saved"                = Sys.time(),
                              "version_created"           = packageDescription("metagate", fields = "Version"),
                              "version_saved"             = packageDescription("metagate", fields = "Version"),
                              "gate_file"                 = all_gate_file_names,
                              "fcs_file_count"            = ncol(merged$stat_data),
                              "gate_count"                = nrow(merged$gate_data),
                              "readout_count"             = nrow(merged$stat_data),
                              "population_size_limit"     = event_limits,
                              "meta_variables_group"      = NULL,
                              "meta_variables_color"      = NULL,
                              "meta_variables_pairing"    = NULL,
                              "meta_variables_panel"      = NA)

  # Remove group data
  merged$group_data <- list(list(id = 1, name = "All samples", query = NULL))

  return(list(merged = merged, messages = messages, error = NULL))
}


merge_project_df <- function(dfs, gate_file_names, replace_missing = NA) {
  columns <- unique(unlist(lapply(dfs, names)))

  merged <- do.call(rbind, lapply(seq_along(gate_file_names), function(i) {
    df <- dfs[[i]]

    missing_columns <- setdiff(columns, names(df))
    if (length(missing_columns) > 0) {
      df[, missing_columns] <- replace_missing
    }

    if ("gate_file" %in% names(df) == FALSE) {
      df$gate_file <- gate_file_names[i]
    }

    return(df)
  }))

  return(merged)
}


merge_stat_data <- function(dfs) {
  readouts <- unique(unlist(lapply(dfs, row.names)))

  merged <- do.call(cbind, lapply(dfs, function(df) {
    missing_readouts <- setdiff(readouts, row.names(df))
    if (length(missing_readouts) > 0) {
      missing_data <- matrix(rep(NA, length(missing_readouts) * ncol(df)), ncol = ncol(df))
      row.names(missing_data) <- missing_readouts
      colnames(missing_data) <- colnames(df)
      df <- rbind(df, missing_data)
    }
    
    return(df[readouts, ])
  }))

  return(merged)
}




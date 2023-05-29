fcs_check_files <- function(fcs_files, flowjo_file_names = NULL) {
  file_count <- nrow(fcs_files)

  if (anyDuplicated(fcs_files$name))
    return(list(error = "All FCS files must have unique file names."))

  fcs_files$version <- fcs_get_FCS_version(fcs_files$datapath)

  if (any(is.na(fcs_files$version))) {
    return(list(error = HTML(paste0("The following files are not valid FCS files:</p><ul><li>",
                                    paste(fcs_files[is.na(fcs_files$version), "name"], collapse = "</li><li>"),
                                    "</li></ul><p>Please try uploading your FCS files again."))))
  }

  not_compatible_files <- fcs_files[fcs_files$version %in% c("2.0", "3.0", "3.1") == FALSE,]

  if (nrow(not_compatible_files) > 0) {
    return(list(error = HTML(paste0("Supported FCS versions are 3.0 and 3.1. ",
                                    "The following files are not supported:</p><ul><li>",
                                    paste(paste0(not_compatible_files$name, " (FCS version ",
                                                 not_compatible_files$version, ")"),
                                          collapse = "</li><li>"),
                                    "</li></ul><p>Please try converting to FCS 3.0 or 3.1."))))
  }

  fcs_file_data <- lapply(1:nrow(fcs_files), function(i) {
    fcs <- tryCatch(flowCore::read.FCS(fcs_files[i, "datapath"], which.lines = 1,
                                       truncate_max_range = FALSE, emptyValue = FALSE),
                    warning = function(w) w, error = function(e) e)

    if (is(fcs, "warning") || is(fcs, "error"))
      return(fcs)

    compensate <- length(intersect(c("SPILL", "$SPILLOVER", "$SPILL", "$COMP"), names(flowCore::keyword(fcs)))) > 0

    list(fcs_data = list(name        = fcs_files[i, "name"],
                         path        = fcs_files[i, "datapath"],
                         FIL         = fcs_get_keyword(fcs, "$FIL"),
                         cytometer   = fcs_get_keyword(fcs, "$CYT"),
                         FCS_version = fcs_files[i, "version"],
                         date        = fcs_get_keyword(fcs, "$DATE"),
                         compensate  = compensate),
         parameters = data.frame("name"  = as.character(flowCore::parameters(fcs)$name),
                                 "desc"  = ifelse(is.na(flowCore::parameters(fcs)$desc),
                                                  "",
                                                  as.character(flowCore::parameters(fcs)$desc)),
                                 "order" = 1:nrow(flowCore::parameters(fcs)),
                                 row.names = NULL,
                                 stringsAsFactors = FALSE))
  })

  samples_with_errors <- unlist(lapply(1:nrow(fcs_files), function(i) {
    if (is(fcs_file_data[[i]], "warning") || is(fcs_file_data[[i]], "error")) {
      file_name <- fcs_files[i, "name"]
      msg("Error while checking ", file_name, ": ", fcs_file_data[[i]]$message)
      return(file_name)
    } else {
      return(NULL)
    }
  }))

  if (length(samples_with_errors) > 0) {
    return(list(error = HTML(paste0("The following FCS files could not be loaded: </p><ul><li>",
                                    paste(samples_with_errors, collapse = "</li><li>"),
                                    "</li></ul><p>Please try uploading your FCS files again."))))
  }

  fcs_data <- do.call(rbind.data.frame, lapply(fcs_file_data, "[[", "fcs_data"))

  
  if (!is.null(flowjo_file_names)) {
    missing_samples <- fcs_data[fcs_data$name %in% flowjo_file_names$name == FALSE, "name"]
    if (length(missing_samples) > 0)
      return(list(error = HTML(paste0("The following files were not found in the FlowJo workspace: </p><ul><li>",
                                      paste(missing_samples, collapse = "</li><li>"),
                                      "</li></ul><p>Please try uploading your FCS files again."))))
  
    fcs_data$sampleID <- sapply(fcs_data$name,
                                function(name) { flowjo_file_names[flowjo_file_names$name == name, "sampleID"] })
  } else {
    fcs_data$sampleID <- 1:nrow(fcs_data)
  }


  parameter_list <- do.call(rbind, lapply(fcs_file_data, "[[", "parameters"))
  parameter_list <- parameter_list[order(parameter_list$order),]
  parameter_list <- unique(parameter_list[, c("name", "desc")])

  parameter_list <- cbind(parameter_list, as.data.frame(t(sapply(1:nrow(parameter_list), function(i) {
                      missing <- which(sapply(1:file_count, function(file) {
                        sum(fcs_file_data[[file]]$parameters$name == parameter_list[i, "name"]
                            & fcs_file_data[[file]]$parameters$desc == parameter_list[i, "desc"]) < 1 }))

                      if (parameter_list[i, "desc"] == "" || parameter_list[i, "name"] == parameter_list[i, "desc"]) {
                        title <- parameter_list[i, "name"]
                      } else {
                        title <- paste0(parameter_list[i, "desc"], " (", parameter_list[i, "name"], ")")
                      }
  
                      return(c("title"         = as.character(title),
                               "found"         = file_count - length(missing),
                               "missing"       = length(missing),
                               "total"         = file_count,
                               "files"         = paste(fcs_files[row.names(fcs_files) %in% missing == FALSE, "name"],
                                                 collapse = "\n"),
                               "missing_files" = if (length(missing) == 0) "" else paste(fcs_files[missing, "name"],
                                                                                         collapse = "\n")))
                      }))))

  row.names(parameter_list) <- NULL

  return(list(
    error          = NULL,
    parameter_list = parameter_list,
    fcs_data       = fcs_data
  ))
}


fcs_get_FCS_version <- function(paths) {
  sapply(paths, function(path) {
    if (!file.exists(path))
      return(NA)

    f <- file(path, open = "rb")
    version <- readChar(f, 6)
    close(f)

    if (substr(version, 1, 3) == "FCS") {
      return(substring(version, 4))
    } else {
      return(NA)
    }
  })
}


fcs_get_keyword <- function(fcs, keyword) {
  if (keyword %in% names(flowCore::keyword(fcs))) {
    return(as.character(flowCore::keyword(fcs, keyword)))
  } else {
    return("")
  }
}


fcs_parse_files <- function(gate_data, gatingML, flowjo_workspace_path, fcs_data, parameter_list,
                            population_data, event_limit, gate_file, n_cores, show_progress_bar) {

  message("Parsing ", nrow(fcs_data), " files on ", n_cores, " cores...", sep = "")
  parse_start_time <- Sys.time()

  parse_function <- function(file_number) {
    tryCatch(metagate:::fcs_parse_one_file(file_number           = file_number,
                                           gate_data             = gate_data,
                                           gatingML              = gatingML,
                                           flowjo_workspace_path = flowjo_workspace_path,
                                           fcs_data              = fcs_data,
                                           parameter_list        = parameter_list,
                                           population_data       = population_data,
                                           event_limit           = event_limit),
             error = function(e) e)
  }

  if (n_cores < 2 || nrow(fcs_data) < 2) {
    if (show_progress_bar) {
      parsed <- pbapply::pblapply(X = 1:nrow(fcs_data), FUN = parse_function)
    } else {
      parsed <- lapply(1:nrow(fcs_data), parse_function)
    }
  } else {
    if (setting("debug")) {
      cl <- parallel::makeCluster(n_cores, useXDR = FALSE, methods = FALSE, outfile = "")
    } else {
      cl <- parallel::makeCluster(n_cores, useXDR = FALSE, methods = FALSE)
    }
    
    parallel::clusterExport(cl, varlist = c("gate_data", "gatingML", "flowjo_workspace_path",
                                            "fcs_data", "parameter_list", "population_data", "event_limit"),
                            envir = environment())

    if (show_progress_bar) {
      pbapply::pboptions(use_lb = TRUE)
      parsed <- pbapply::pblapply(cl = cl, X = 1:nrow(fcs_data), FUN = parse_function)
    } else {
      parsed <- parallel::parLapplyLB(cl = cl, X = 1:nrow(fcs_data), fun = parse_function)
    }

    parallel::stopCluster(cl)
  }

  samples_with_errors <- unlist(lapply(1:nrow(fcs_data), function(file_number) {
    if (!is(parsed[[file_number]], "data.frame")) {
      file_name <- fcs_data[file_number, "name"]
      if ("message" %in% names(parsed[[file_number]])) {
        msg("Error while parsing ", file_name, ": ", parsed[[file_number]]$message)
      }

      return(file_name)
    } else {
      return(NULL)
    }
  }))

  if (length(samples_with_errors) > 0) {
    return(list(errors = samples_with_errors))
  }

  
  stat_data <- plyr::rbind.fill(parsed)
  message("Done parsing files in ", metagate:::run_time_formatted(parse_start_time), "\n", sep = "")
  
  fcs_names              <- stat_data$FCS_NAME
  fcs_events             <- stat_data$FCS_EVENTS
  messages               <- stat_data$FCS_WARNINGS
  
  stat_data$FCS_NAME     <- NULL
  stat_data$FCS_EVENTS   <- NULL
  stat_data$FCS_WARNINGS <- NULL
  
  stat_data <- t(stat_data)
  colnames(stat_data) <- fcs_names
  missing_values <- apply(stat_data, 2, function(x) { sum(is.na(x))})

  fcs_data$events     <- sapply(fcs_data$name, function(name) { fcs_events[which(fcs_names == name)] })
  fcs_data$missing    <- sapply(fcs_data$name, function(name) { missing_values[which(fcs_names == name)] })
  fcs_data$path       <- NULL
  fcs_data$local_path <- NULL

  project_data <- list("project_name"              = "Untitled project",
                       "time_created"              = Sys.time(),
                       "time_saved"                = NA,
                       "version_created"           = packageDescription("metagate", fields = "Version"),
                       "version_saved"             = NA,
                       "gate_file"                 = gate_file,
                       "fcs_file_count"            = ncol(stat_data),
                       "gate_count"                = nrow(gate_data),
                       "readout_count"             = nrow(stat_data),
                       "population_size_limit"     = event_limit,
                       "meta_variables_group"      = NULL,
                       "meta_variables_color"      = NULL,
                       "meta_variables_pairing"    = NULL,
                       "meta_variables_panel"      = NA)

  return(list(errors       = NULL,
              stat_data    = stat_data,
              fcs_data     = fcs_data,
              meta_data    = data.frame(file = fcs_names, stringsAsFactors = FALSE),
              project_data = project_data,
              messages     = as.vector(unlist(sapply(messages, strsplit, split = "\n", fixed = TRUE)))))
}


fcs_parse_one_file <- function(file_number, gate_data, gatingML, flowjo_workspace_path,
                               fcs_data, parameter_list, population_data, event_limit) {
  messages <- NULL

  fcs_name <- fcs_data[file_number, "name"]

  cs <- flowWorkspace::load_cytoset_from_fcs(files = fcs_data[file_number, "path"],
                                             truncate_max_range = FALSE, emptyValue = FALSE)
  sampleNames(cs) <- fcs_name

  intensity_matrix <- fcs_create_intensity_matrix(fcs            = cs[[1]],
                                                  parameter_list = parameter_list,
                                                  compensate     = fcs_data[file_number, "compensate"])
  event_count <- nrow(intensity_matrix)

  if (is.null(gatingML)) {
    gate_matrix <- flowjo_create_gate_matrix(cs                    = cs,
                                             sampleID              = fcs_data[file_number, "sampleID"],
                                             gate_data             = gate_data,
                                             flowjo_workspace_path = flowjo_workspace_path,
                                             event_count           = event_count)
  } else {
    gate_matrix <- cytobank_create_gate_matrix(cs          = cs,
                                               gate_data   = gate_data,
                                               gatingML    = gatingML,
                                               event_count = event_count)
  }

  rm(cs)
  

  if ("missing_gates" %in% names(gate_matrix)) {
    messages <- c(messages,
                  sapply(gate_matrix$missing_gates,
                         function(gate) paste("Unable to apply gate", gate, "on file", fcs_name)))
    ignored_populations <- c(FALSE, fcs_ignored_populations(population_data = population_data,
                                                            ignored_gates   = gate_matrix$missing_gates))
  } else {
    ignored_populations <- rep(FALSE, nrow(population_data) + 1)
  }

  parameter_list <- do.call(rbind, lapply(colnames(intensity_matrix), function(title) {
    parameter_list[parameter_list$title == title & grepl(fcs_name, parameter_list$file, fixed = TRUE),][1,]
  }))
  row.names(parameter_list) <- NULL

  population_definitions <- fcs_create_population_definitions(population_data = population_data,
                                                              gate_names      = gate_data$name)

  readout_values <- calculate_readouts(gate_matrix        = gate_matrix$gate_matrix,
                                       intensity_matrix   = intensity_matrix,
                                       populations        = population_definitions,
                                       ignore_populations = ignored_populations,
                                       calculate_mean     = parameter_list$mean,
                                       calculate_median   = parameter_list$median,
                                       calculate_geomean  = parameter_list$geomean,
                                       event_limit        = event_limit)

  rm(gate_matrix)
  rm(intensity_matrix)
  rm(population_definitions)

  df <- data.frame(matrix(readout_values, nrow = 1))
  colnames(df)    <- fcs_generate_readout_names(populations = population_data, parameter_list = parameter_list)
  df$FCS_NAME     <- fcs_name
  df$FCS_EVENTS   <- event_count
  df$FCS_WARNINGS <- if (length(messages) > 0) paste(messages, collapse = "\n") else ""

  return(df)
}


fcs_generate_readout_names <- function(populations, parameter_list) {
  population_names <- c("Bulk", populations$name)
  readouts <- c(
    paste0("Percent___", population_names),
    paste0("Mean___", parameter_list$title[parameter_list$mean]),
    paste0("Median___", parameter_list$title[parameter_list$median]),
    paste0("Geometric mean___", parameter_list$title[parameter_list$geomean])
  )
  return(paste0(rep(readouts, length(population_names)), "___",
                rep(population_names, each = length(readouts))))
}


fcs_create_intensity_matrix <- function(fcs, parameter_list, compensate) {
  fcs_parameters <- flowCore::pData(flowCore::parameters(fcs))
  fcs_parameters[is.na(fcs_parameters$desc), "desc"] <- ""

  # Apply compensation based on the spillover values in the FCS file
  if (compensate) {
    spillover <- flowCore::keyword(fcs, intersect(c("SPILL", "$SPILLOVER", "$SPILL", "$COMP"),
                                                  names(flowCore::keyword(fcs)))[1])[[1]]
    fcs <- flowCore::compensate(fcs, spillover)
  }

  intensity_matrix <- as.matrix(flowCore::exprs(fcs))

  # Apply transformation
  intensity_matrix <- sapply(1:ncol(intensity_matrix), function(i) {
    q <- parameter_list$name == fcs_parameters[i, "name"] & parameter_list$desc == fcs_parameters[i, "desc"]
    if (parameter_list[q, "transform"] == "arcsinh") {
      asinh(intensity_matrix[, i] / parameter_list[q, "cofactor"])
    } else if (parameter_list[q, "transform"] == "flowjo") {
      flowWorkspace::flowjo_biexp()(intensity_matrix[, i])
    } else {
      intensity_matrix[, i]
    }
  })

  # Set the user-defined marker names
  colnames(intensity_matrix) <- sapply(1:ncol(intensity_matrix), function(i) {
    parameter_list[parameter_list$name == fcs_parameters[i, "name"]
                   & parameter_list$desc == fcs_parameters[i, "desc"], "title"]
  })

  return(intensity_matrix)
}


calculate_readouts <- function(gate_matrix, intensity_matrix, populations, ignore_populations, calculate_mean,
                               calculate_median, calculate_geomean, event_limit) {
  event_count <- nrow(intensity_matrix)
  
  population_matrix <- lapply(1:nrow(populations), function(population) {
    if (population %in% which(ignore_populations)) {
      return(NA)
    }
    
    population_definition <- populations[population, ]
    included <- !bit(event_count)
    
    for (gate in seq_along(population_definition)) {
      gate_value <- population_definition[gate]
      if (gate_value == 1) {
        included <- included & gate_matrix[[gate]]
      } else if (gate_value == -1) {
        included <- included & !gate_matrix[[gate]]
      }
    }
    
    return(included)
  })
  
  values <- lapply(seq_along(population_matrix), function(population) {
    population_event_count <- sum(population_matrix[[population]])
    if (is.na(population_event_count) || population_event_count < event_limit) {
      return(rep(NA, length(population_matrix) + sum(calculate_mean) + sum(calculate_median) + sum(calculate_geomean)))
    }
    
    population_intensity_matrix <- intensity_matrix[as.logical(population_matrix[[population]]),]
    
    percentage_values <- sapply(seq_along(population_matrix), function(i) {
      sum(population_matrix[[population]] & population_matrix[[i]]) / population_event_count
    })
    mean_values <- colSums(population_intensity_matrix[, calculate_mean]) / population_event_count
    median_values <- apply(population_intensity_matrix[, calculate_median], 2, function(x) {
      half <- (population_event_count + 1) %/% 2
      if (population_event_count %% 2 == 1) {
        sort(x, partial = half)[half]
      } else {
        mean(sort(x, partial = half + 0:1)[half + 0:1])
      }
    })
    geomean_values <- apply(population_intensity_matrix[, calculate_geomean], 2, function(x) {
      if (min(x) <= 0) return(NA)
      exp(sum(log(x)) / population_event_count)
    })
    
    return(c(percentage_values, mean_values, median_values, geomean_values))
  })
  
  return(unlist(values))
}


fcs_ignored_populations <- function(population_data, ignored_gates) {
  as.vector(sapply(population_data$name, function(population) {
    query <- import_parse_population_query(population_data[population_data$name == population, "query"])
    gates <- gsub("^NOT ", "", query)
    return(sum(gates %in% ignored_gates) > 0)
  }))
}


fcs_create_population_definitions <- function(population_data, gate_names) {
  population_definitions <- matrix(rep(0, (1 + nrow(population_data)) * length(gate_names)), ncol = length(gate_names))
  colnames(population_definitions) <- gate_names
  row.names(population_definitions) <- c("Bulk", population_data$name)

  for (population in population_data$name) {
    query_chunks <- import_parse_population_query(population_data[population_data$name == population, "query"])
    for (chunk in query_chunks) {
      if (startsWith(chunk, "NOT ")) {
        population_definitions[population, gsub("^NOT ", "", chunk)] <- -1
      } else {
        population_definitions[population, chunk] <- 1
      }
    }
  }

  return(population_definitions)
}





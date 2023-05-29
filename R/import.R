import_parse_population_query <- function(query) {
  query <- gsub("\\s+", " ", query)
  query <- gsub("^\\s+|\\s+$", "", query)
  query <- gsub("NOT ", "NOT___", query)
  query <- strsplit(query, " ", fixed = TRUE)[[1]]
  query <- gsub("NOT___", "NOT ", query)
  query
}


# Parse uploaded population file and return population data frame.
import_check_population_file <- function(population_file, experiment_gates) {
  errors           <- NULL
  warning_messages <- NULL
  populations      <- NULL

  if (all(c("Name", "Definition") %in% names(population_file)) == FALSE) {
    errors <- paste0("The population file could not be imported because the file does not have columns ",
                     "named \"Name\" and \"Definition\".")
  } else {
    populations <- population_file[!is.na(population_file$Name) & population_file$Name != "",]

    if (nrow(populations) < 1) {
      errors <- "The population file could not be imported because the file does not contain any populations."
    } else {
      populations$valid <- TRUE

      for (row in row.names(populations)) {
        population_gates <- import_parse_population_query(populations[row, "Definition"])
        gates_not_in_experiment <- population_gates[gsub("^NOT ", "", population_gates) %in% experiment_gates == FALSE]
  
        if (length(gates_not_in_experiment) > 0) {
          warning_reasons <- sapply(gates_not_in_experiment, function(gate) {
            paste0("the gate ", gsub("^NOT ", "", gate), " is not in your project")
          })
          warning_messages <- c(warning_messages, paste0("The population ", populations[row, "Name"],
                                                         " was not added because:<ul><li>",
                                                         paste(warning_reasons, collapse = "</li><li>"),
                                                         "</li></ul>"))
          populations[row, "valid"] <- FALSE
        }
      }
    }
  }

  return(list(errors      = errors,
              warnings    = warning_messages,
              populations = populations))
}


import_parse_populations <- function(populations) {
  errors <- NULL

  for (row in row.names(populations)) {
    gates_stripped <- gsub("^NOT ", "", import_parse_population_query(populations[row, "query"]))
    duplicated_gates <- unique(gates_stripped[duplicated(gates_stripped)])
    for (duplicated_gate in duplicated_gates) {
      errors <- c(errors, paste0("The \"", populations[row, "name"], "\" population is defined as both ",
                                 duplicated_gate, " and NOT ", duplicated_gate, "."))
    }
  }
  
  for (missing_name_query in populations[gsub(" ", "", populations$name) == "" & populations$query != "", "query"]) {
    errors <- c(errors, paste0("The population with the definition \"", missing_name_query, "\" does not have a name."))
  }
  
  for (missing_query_name in populations[gsub(" ", "", populations$name) != "" & populations$query == "", "name"]) {
    errors <- c(errors, paste0("The \"", missing_query_name, "\" population does not have a definition."))
  }
  
  populations <- populations[gsub(" ", "", populations$name) != "" & populations$query != "",]
  
  for (duplicate_name in unique(populations[duplicated(populations$name), "name"])) {
    errors <- c(errors, paste0("There are two or more populations with the same name: \"", duplicate_name, "\""))
  }
  
  for (duplicate_query in unique(populations[duplicated(populations$query), "query"])) {
    errors <- c(errors, paste0("The following populations have the same definition: \"",
                               paste(populations[populations$query == duplicate_query, "name"], collapse = "\", \""),
                               "\""))
  }
  
  if ("Bulk" %in% populations$name)
    errors <- c(errors, paste0("The population name \"Bulk\" cannot be used. ",
                               "A \"Bulk\" population will be created automatically by the system."))
  
  if (length(errors) < 1 & nrow(populations) < 1)
    errors <- c(errors, "You have not defined any populations.")

  return(list(errors     = errors,
             populations = populations))
}


import_check_parameter_list_duplicates <- function(parameter_list) {
  titles_with_several_parameters <- unique(parameter_list$title[duplicated(parameter_list$title)])
  
  if (length(titles_with_several_parameters) < 1)
    return(NULL)

  duplicates <- NULL

  for (title in titles_with_several_parameters) {
    files <- strsplit(paste(parameter_list[parameter_list$title == title, "files"], collapse = "\n"),
                      split = "\n", fixed = TRUE)[[1]]
    if (anyDuplicated(files))
      duplicates <- c(duplicates, title)
  }

  return(duplicates)
}





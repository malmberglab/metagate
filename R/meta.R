meta_create_summary_table <- function(meta_data) {
  variables <- setdiff(names(meta_data), "file")

  optionTexts <- sapply(variables, function(variable) {
    values <- meta_data[, variable]
    if (is.numeric(values)) {
      return(paste0("<i>",
                    "Mean: ", round(mean(values, na.rm = TRUE), 2), ", ",
                    "median: ", round(median(values, na.rm = TRUE), 2), ", ",
                    "min: ", round(min(values, na.rm = TRUE), 2), ", ",
                    "max: ", round(max(values, na.rm = TRUE), 2), ". ",
                    "Missing values: n=", sum(is.na(values)),
                    "</i>"))
    } else {
      values[values == "" | values == "N/A" | is.na(values)] <- "N/A"
      if (length(unique(values)) > 8) {
        return(paste0("<i>", length(unique(values)), " different values</i>"))
      } else {
        options <- sapply(unique(values), function(option) {
          paste0(option, " <i>(n=", sum(values == option), ", ",
                 round(sum(values == option) / length(values) * 100, 1), "%)</i>")
        })
        return(paste(options, collapse = "<br>"))
      }
      
    }
  })

  types <- sapply(variables, function(variable) {
    if (is.numeric(meta_data[, variable])) {
      return("numerical")
    } else {
      return("categorical")
    }
  })

  return(data.frame(name = variables, type = types, options = optionTexts))
}


meta_parse_file <- function(old_meta_data, path, group_data) {
  errors   <- NULL
  warnings <- NULL

  new_meta_data <- read_xlsx_file(path)

  if (is.null(new_meta_data) || !is(new_meta_data, "data.frame")) {
    errors <- "Unable to read the uploaded file. Are you sure this is an Excel file with the .xlsx file extension?"
  } else if ("file" %in% names(new_meta_data) == FALSE) {
    errors <- "The file you uploaded does not contain a \"file\" column."
  }
  
  if (is.null(errors)) {
    #names(new_meta_data) <- gsub(".", "_", names(new_meta_data), fixed = TRUE)

    # Check if any of the files listed in the meta data are not in the project
    files_not_in_project <- new_meta_data[new_meta_data$file %in% old_meta_data$file == FALSE, "file"]
    for (file in files_not_in_project) {
      errors <- c(errors, paste0("Found a sample that is not in your project: ", file))
    }

    # Check for duplicates
    duplicate_samples <- unique(new_meta_data$file[duplicated(new_meta_data$file)])
    for (file in duplicate_samples) {
      errors <- c(errors, paste0("Found a sample that is listed more than once in the meta data: ", file))
    }
  }

  if (!is.null(errors)) {
    return(list(errors = errors))
  } else {
    # Check if any files are missing in the meta data
    files_not_in_meta <- old_meta_data[old_meta_data$file %in% new_meta_data$file == FALSE, "file"]
    if (length(files_not_in_meta) > 0) {
      warnings <- c(warnings, paste0("The file you uploaded does not contain all files/samples, so ",
                                     length(files_not_in_meta), " samples are now without meta data."))

      new_meta_data <- rbind(new_meta_data,
                             cbind(data.frame(file = files_not_in_meta),
                                   do.call(data.frame, sapply(setdiff(vars, "file"),
                                                              function(var) { rep("", length(files_not_in_meta)) },
                                                              simplify = FALSE, USE.NAMES = TRUE))))
    }

    # Check if all variables used for group definitions are in the new meta data
    check_variables <- meta_check_variables(names(new_meta_data), group_data)
    if (length(check_variables$missing) > 0) {
      warnings <- c(warnings, sapply(check_variables$missing, function(group) {
        paste0("The definition of the group \"", group$group,
               "\" was changed because the following variables are no longer in the meta data: ",
               paste(group$variables, collapse = ", "))
      }))
    }

    added_variables <- setdiff(names(new_meta_data), names(old_meta_data))
    if (length(added_variables) > 0) {
      is_group_variable <- sapply(added_variables, function(variable) {
        !is.numeric(new_meta_data[, variable]) && length(unique(new_meta_data[, variable])) <= 8
      })
      added_group_variables <- added_variables[is_group_variable]
    } else {
      added_group_variables <- NULL
    }
    

    return(list(errors                = errors,
                warnings              = warnings,
                new_meta_data         = new_meta_data,
                old_variables         = setdiff(names(old_meta_data), "file"),
                new_variables         = setdiff(names(new_meta_data), "file"),
                added_variables       = added_variables,
                added_group_variables = added_group_variables,
                new_group_data        = check_variables$new_group_data))
  }
}


meta_check_variables <- function(meta_variables, group_data) {
  missing <- list()
  new_group_data <- group_data
  for (group_id in 1:length(group_data)) {
    query <- group_data[[group_id]]$query
    if (length(query) > 0) {
      variables_in_group <- unique(sapply(strsplit(query, ": ", fixed = TRUE), "[", 1))
      variables_missing <- variables_in_group[variables_in_group %in% meta_variables == FALSE]
      if (length(variables_missing) > 0) {
        missing[[length(missing)+1]] <- list(group = group_data[[group_id]]$name, variables = variables_missing)
        new_group_data[[group_id]]$query <- query[!startsWith(query, paste0(variables_missing, ": "))]
      }
    }
  }

  if (identical(group_data, new_group_data))
    new_group_data <- NULL

  return(list(new_group_data = new_group_data,
              missing        = missing))
}




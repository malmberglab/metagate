#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


get_samples_for_groups <- function(meta_data, group_data, group_ids, pair_by = NULL, color_by = NULL,
                                   panel_variable = NULL) {
  if (length(group_ids) < 1) {
    return(NULL)
  }

  if (!is.null(panel_variable) && !is.na(panel_variable) && panel_variable %in% names(meta_data)) {
    panels <- setdiff(unique(meta_data[, panel_variable]), NA)
    results <- lapply(panels, function(panel) {
      panel_data <- get_samples_for_groups_panel(meta_data  = meta_data[meta_data[, panel_variable] == panel, ],
                                                 group_data = group_data,
                                                 group_ids  = group_ids,
                                                 pair_by    = pair_by,
                                                 color_by   = color_by)
      
      if (is.null(panel_data)) {
        return(list(data = NULL, error = NULL))
      } else if (!is.null(panel_data$error)) {
        return(list(data = NULL, error = panel_data$error))
      } else {
        panel_data$data$panel <- panel
        return(list(data = panel_data$data, error = NULL))
      }
    })

    all_samples <- lapply(results, "[[", "data")
    
    all_errors <- paste(unique(unlist(sapply(results, "[[", "error"))), collapse = "\n")
    if (all_errors == "")
      all_errors <- NULL

    if (any(sapply(all_samples, is.data.frame))) {
      return(list(data = do.call(rbind, all_samples), error = all_errors))
    } else {
      return(list(error = all_errors))
    }
    
  } else {
    return(get_samples_for_groups_panel(meta_data  = meta_data,
                                        group_data = group_data,
                                        group_ids  = group_ids,
                                        pair_by    = pair_by,
                                        color_by   = color_by))
  }
}


get_samples_for_groups_panel <- function(meta_data, group_data, group_ids, pair_by, color_by) {
  if (!is.null(pair_by)) {
    if (length(group_ids) != 2) {
      return(NULL)
    }

    samples <- list()
    for (group_id in as.character(group_ids)) {
      samples[[group_id]] <- get_meta_for_group(meta_data = meta_data, group_data = group_data, group_id = group_id)

      if (pair_by %in% names(samples[[group_id]]) == FALSE) {
        return(list(error = "Sample pairing error."))
      }

      samples[[group_id]] <- samples[[group_id]][!is.na(samples[[group_id]][, pair_by])
                                                 & samples[[group_id]][, pair_by] != "(none)", ]

      if (anyDuplicated(samples[[group_id]][, pair_by])) {
        return(list(error = paste0("Sample pairing error: Multiple samples with same pairing variable value in group. ",
                                   "Please check your meta data.")))
      }

      row.names(samples[[group_id]]) <- as.character(samples[[group_id]][, pair_by])
    }

    keep <- Reduce(intersect, sapply(samples, "[", pair_by))

    for (group_id in as.character(group_ids)) {
      samples[[group_id]] <- samples[[group_id]][as.character(keep), ]
    }

    if (length(unique(sapply(samples, "[", pair_by))) != 1) {
      return(list(error = "Sample pairing error."))
    }

    if (anyDuplicated(unlist(sapply(samples, "[[", "file")))) {
      return(list(error = "Sample pairing error: Groups have overlapping samples."))
    }
  } else {
    samples <- sapply(as.character(group_ids),
                      get_meta_for_group,
                      meta_data  = meta_data,
                      group_data = group_data,
                      simplify   = FALSE,
                      USE.NAMES  = TRUE)
  }
  
  samples_group_id <- as.vector(unlist(sapply(names(samples),
                                              function(group_id) { rep(group_id, nrow(samples[[group_id]])) })))
  samples_file <- unlist(sapply(samples, "[", "file"))

  group_name_list <- get_group_name_list(group_data)
  samples_group_name <- unlist(sapply(samples_group_id, function(x) { names(which(group_name_list == x ))}))
  if (length(samples_group_name) != length(samples_group_id)) {
    return(NULL)
  }

  df <- data.frame(group_id = samples_group_id, group_name = samples_group_name, file = samples_file)

  if (!is.null(pair_by)) {
    df$pairing <- unlist(sapply(samples, "[", pair_by))
  }

  if (!is.null(color_by)) {
    df$color <- unlist(sapply(samples, "[", color_by))
  }

  if (nrow(df) < 1) {
    return(NULL)
  }

  return(list(data = df, error = NULL))
}


get_meta_for_group <- function(meta_data, group_data, group_id) {
  if (is.null(group_id) || group_id == "") {
    return(NULL)
  }

  char_columns <- sapply(meta_data, is.character)
  meta_data[, char_columns][is.na(meta_data[char_columns]) | meta_data[, char_columns] == ""] <- "(none)"

  group_meta <- meta_data

  query <- group_data[[as.numeric(group_id)]]$query

  if (length(query) < 1) {
    return(group_meta)
  }

  variables <- unique(sapply(strsplit(query, ": ", fixed = TRUE), '[', 1))
  for (variable in variables) {
    query_variable <- query[startsWith(query, paste0(variable, ": "))]
    options <- substring(query_variable, nchar(variable)+3)
    group_meta <- group_meta[!is.na(group_meta[, variable]) & group_meta[, variable] %in% options,]
  }

  return(group_meta)
}


get_group_name_list <- function(group_data) {
  if (length(group_data) > 0) {
    ids <- sapply(group_data, "[[", "id")
    names <- unlist(sapply(group_data, "[[", "name"))
    keep <- !is.null(names) & names != ""
    if (sum(keep) > 0)
      return(as.list(setNames(ids[keep], names[keep])))
  }
  return(NULL)
}


get_group_names_for_ids <- function(group_data, ids) {
  all_ids <- sapply(group_data, "[[", "id")

  return(sapply(ids, function(id) {
    index <- which(all_ids == id)

    if (length(index) != 1)
      return(NA)

    return(group_data[[index]]$name)
  }))
}




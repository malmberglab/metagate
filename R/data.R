#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


# Data for bar plots
get_single_readout_data <- function(samples, readouts, stat_data, count_data, paired_groups = FALSE,
                                    paired_populations = FALSE) {
  if (is.null(samples) || nrow(samples) < 1) {
    return(NULL)
  }

  if (any(startsWith(readouts, "Absolute count___"))) {
    stat_data <- rbind(stat_data, count_data)
  }

  if (!all(readouts %in% row.names(stat_data))) {
    return(NULL)
  }
  
  # Get values
  if (length(readouts) == 1) {
    samples$value <- as.numeric(stat_data[readouts, samples$file])
  } else {
    samples <- do.call(rbind, lapply(readouts, function(readout) {
      ret <- samples
      ret$population <- strsplit(readout, "___", fixed = TRUE)[[1]][3]
      ret$value <- as.numeric(stat_data[readout, ret$file])
      return(ret)
    }))
  }

  panel <- choose_panel_for_samples(samples)
  samples <- panel$samples
  chosen_panel <- panel$chosen_panel

  # Set values for incomplete sample pairs or population pairs to NA
  if (paired_populations || paired_groups) {
    repeat {
      na_rows <- which(is.na(samples$value))

      for (na_row in na_rows) {
        if (paired_populations) {
          samples[samples$group_id == samples[na_row, "group_id"]
                  & samples$file == samples[na_row, "file"], "value"] <- NA
        }
        
        if (paired_groups) {
          if ("population" %in% names(samples)) {
            samples[samples$population == samples[na_row, "population"]
                    & samples$pairing == samples[na_row, "pairing"], "value"] <- NA
          } else {
            samples[samples$pairing == samples[na_row, "pairing"], "value"] <- NA
          }
        }
      }

      if (length(na_rows) == sum(is.na(samples$value))) break
    }
  }

  # Remove samples with missing data
  samples <- samples[!is.na(samples$value), ]

  if (nrow(samples) < 1)
    return(NULL)

  return(list(data = samples, panel = chosen_panel))
}


# Data for non-comparison heatmaps
get_multiple_readout_data <- function(samples, readouts, stat_data, count_data, mean_type) {
  if (is.null(samples) || length(readouts) <  1) {
    return(NULL)
  }

   if (any(startsWith(readouts, "Absolute count___"))) {
    stat_data <- rbind(stat_data, count_data)
  }

  group_ids <- unique(samples$group_id)
  group_names <- sapply(group_ids, function(x) samples[samples$group_id == x, "group_name"][1])

  df <- do.call(rbind, lapply(readouts, function(readout) {
    readout_samples <- samples
    readout_samples$value <- as.numeric(stat_data[readout, readout_samples$file])

    panel <- choose_panel_for_samples(readout_samples)
    readout_samples <- panel$samples
    chosen_panel <- panel$chosen_panel

    data.frame(readout    = readout,
               panel      = chosen_panel,
               group_id   = group_ids,
               group_name = group_names,
               value      = sapply(group_ids, function(group_id) {
                 values <- readout_samples[readout_samples$group_id == group_id, "value"]
                 if (mean_type == "Median") {
                   median(values, na.rm = TRUE)
                 } else {
                   mean(values, na.rm = TRUE)
                 }
               }))
  }))

  df <- add_readout_columns_to_data(df)

  return(list(data = df))
}


# Data for heatmaps and volcano plots
get_multiple_readout_comparison_data <- function(samples, readouts, stat_data, count_data, paired_groups, mean_type,
                                                 p_adjust_method) {
  if (is.null(samples) || length(readouts) <  1 || length(unique(samples$group_id)) != 2) {
    return(NULL)
  }

  if (any(startsWith(readouts, "Absolute count___"))) {
    stat_data <- rbind(stat_data, count_data)
  }

  group_ids <- unique(samples$group_id)
  group_names <- sapply(group_ids, function(x) samples[samples$group_id == x, "group_name"][1])

  df <- do.call(rbind, lapply(readouts, function(readout) {
    readout_samples <- samples
    readout_samples$value <- as.numeric(stat_data[readout, readout_samples$file])

    panel <- choose_panel_for_samples(readout_samples)
    readout_samples <- panel$samples
    chosen_panel <- panel$chosen_panel

    readout_samples <- lapply(1:2, function(i) {
      readout_samples[readout_samples$group_id == group_ids[i], ]
    })

    if (paired_groups) {
      remove <- is.na(readout_samples[[1]]$value) | is.na(readout_samples[[2]]$value)
      readout_samples <- lapply(readout_samples, function(x) x[!remove, ])

      if (!all(readout_samples[[1]]$pairing == readout_samples[[2]]$pairing)) {
        return(list(error = "Pairing failed"))
      }
    } else {
      readout_samples <- lapply(readout_samples, function(x) x[!is.na(x$value), ])
    }

    means <- lapply(readout_samples, function(x) {
      if (nrow(x) < 1) {
        return(NA)
      } else if (mean_type == "Median") {
        median(x$value)
      } else {
        mean(x$value)
      }
    })

    log2_fold_change <- if (!any(is.na(means)) && all(means > 0)) {
      log2(means[[2]] / means[[1]])
    } else {
      NA
    }

    if (all(sapply(readout_samples, nrow) > 0)) {
      test <- suppressWarnings(wilcox.test(readout_samples[[1]]$value, readout_samples[[2]]$value,
                                           paired = paired_groups))
      p      <- test$p.value
      p_test <- test$method
    } else {
      p      <- NA
      p_test <- NA
    }

    return(data.frame(readout          = readout,
                      panel            = chosen_panel,
                      group_1_mean     = means[[1]],
                      group_2_mean     = means[[2]],
                      group_1_n        = nrow(readout_samples[[1]]),
                      group_2_n        = nrow(readout_samples[[2]]),
                      absolute_change  = means[[2]] - means[[1]],
                      log2_fold_change = log2_fold_change,
                      p                = p,
                      p_test           = p_test))
  }))

  if (p_adjust_method != "none") {
    df$p_adj <- p.adjust(df$p, method = p_adjust_method)
    df$p_adjust_method <- p_adjust_methods(method = p_adjust_method)
  }

  df <- add_readout_columns_to_data(df)

  missing_groups <- group_ids[sapply(1:2, function(i) { all(is.na(df[, paste0("group_", i, "_mean")])) })]

  return(list(data = df, missing_groups = missing_groups))
}


choose_panel_for_samples <- function(samples) {
  if ("panel" %in% names(samples)) {
    panels <- unique(samples$panel)

    # Order by number of non-NA data points, then alphabetically
    value_count_for_panels <- sapply(panels, function(panel) {
      sum(!is.na(samples[!is.na(samples$panel) & samples$panel == panel, "value"]))
    })

    chosen_panel <- panels[value_count_for_panels == max(value_count_for_panels)]
    chosen_panel <- chosen_panel[order(chosen_panel)][1]

    samples <- samples[!is.na(samples$panel) & samples$panel == chosen_panel, ]      
  } else {
    chosen_panel <- NA
  }

  return(list(samples = samples, chosen_panel = chosen_panel))
}


add_readout_columns_to_data <- function(data) {
  readout_split           <- strsplit(data$readout, "___", fixed = TRUE)
  data$readout_prefix     <- sapply(readout_split, "[", 1)
  data$readout_value      <- paste0(sapply(readout_split, "[", 1), "___", sapply(readout_split, '[', 2))
  data$readout_value      <- gsub("___", " ", gsub("Percent___", "% ", data$readout_value))
  data$readout_population <- sapply(readout_split, "[", 3)
  data$readout_name       <- format_readout_names(readout_ids = data$readout)
  return(data)
}


get_absolute_counts <- function(stat_data, meta_data, project_data) {
  count_variables <- project_data$meta_variables_count
  if (is.null(count_variables) || nrow(count_variables) < 1) {
    return(NULL)
  }

  counts <- sapply(count_variables$population, function(population) {
    sapply(colnames(stat_data), function(file) {
      as.numeric(meta_data[meta_data$file == file, count_variables$variable[count_variables$population == population]])
    })
  }, simplify = FALSE, USE.NAMES = TRUE)

  matching_readouts <- grep(paste0("^Percent___(.*)___(\\Q",
                                   paste(count_variables$population, collapse = "\\E|\\Q"), "\\E)$"),
                            row.names(stat_data), value = TRUE)

  count_data <- do.call(rbind, sapply(matching_readouts, function(readout) {
    populations <- strsplit(readout, "___")[[1]][2:3]
    reverse_values <- setdiff(stat_data[paste0("Percent___", populations[2], "___", populations[1]), ], NA)
    if (length(reverse_values) > 0 && all(reverse_values == 1)) {
      return(as.numeric(stat_data[readout, ]) * counts[[populations[2]]])
    } else {
      return(NULL)
    }
  }, simplify = FALSE, USE.NAMES = TRUE))

  colnames(count_data) <- colnames(stat_data)
  rownames(count_data) <- gsub("^Percent___", "Absolute count___", rownames(count_data))
  
  return(count_data)
}



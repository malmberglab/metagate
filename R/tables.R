create_samples_table <- function(samples, pair_by = NULL, color_by = NULL, color_data = NULL, excluded = NULL,
                                 percent = FALSE) {
  if (is.null(samples)) {
    return(NULL)
  }

  df <- data.frame(Sample = samples$file,
                   Group  = samples$group_name)

  if ("value" %in% names(samples)) {
    df$Value <- if (percent) format_percentages(samples$value) else samples$value
  }

  if (!is.null(color_by)) {
    if (!is.null(color_data)) {
      df[, color_by] <- color_data
    } else if ("color" %in% names(samples)) {
      df[, color_by] <- samples$color
    }
  }

  if (!is.null(pair_by) && "pairing" %in% names(samples)) {
    df[, pair_by] <- samples$pairing
  }

  if ("population" %in% names(samples)) {
    df[, "Population"] <- samples$population
    df <- df[, c("Population", setdiff(names(df), "Population"))]
  }

  if (!is.null(excluded)) {
    df[, "Excluded"] <- ifelse(excluded, "Excluded", "")
  }

  return(df)
}


create_multiple_readout_statistics_table <- function(data, mean_type) {
  if (is.null(data))
    return(NULL)

  if (length(unique(data$group_id)) > 1) {
    df <- data.frame(Readout    = data$readout_name,
                     Group      = data$group_name)
  } else {
    df <- data.frame(Readout    = data$readout_value,
                     Population = data$readout_population)
  }

  if (!all(is.na(data$panel)))
    df$Panel <- data$panel

  df$value <- ifelse(data$readout_prefix == "Percent", format_percentages(data$value), data$value)

  names(df)[names(df) == "value"] <- mean_type
  
  return(df)
}


create_multiple_readout_comparison_statistics_table <- function(data, group_data, groups, mean_type) {
  if (is.null(data))
    return(NULL)

  means <- lapply(1:2, function(i) {
    values <- data[, paste0("group_", i, "_mean")]
    ifelse(data$readout_prefix == "Percent", format_percentages(values), values)
  })

  df <- data.frame(Readout    = data$readout_value,
                   Population = data$readout_population)
  if (!all(is.na(data$panel)))
    df$Panel <- data$panel
  df[, paste(group_data[[groups[1]]]$name, tolower(mean_type))] <- means[[1]]
  df[, paste(group_data[[groups[2]]]$name, tolower(mean_type))] <- means[[2]]
  df[, paste(group_data[[groups[1]]]$name, "n")]                <- data$group_1_n
  df[, paste(group_data[[groups[2]]]$name, "n")]                <- data$group_2_n
  df[, "log2 fold change"]                                      <- data$log2_fold_change
  df[, "Absolute change"]                                       <- data$absolute_change
  df[, "p value"]                                               <- data$p

  if ("p_adj" %in% names(data))
    df[, "p value (adj.)"]                                      <- data$p_adj

  df[, "Test"]                                                  <- data$p_test

  if ("p_adjust_method" %in% names(data))
    df[, "Adjustment"]                                          <- data$p_adjust_method

  return(df)
}


format_percentages <- function(values) {
  return(ifelse(is.na(values), NA, paste(round(values * 100, 2), "%")))
}

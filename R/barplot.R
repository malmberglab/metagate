barplot_module_name <- function() {
  return("Bar plot")
}


barplot_get_readout_name <- function(readout) {
  readout_parts <- strsplit(readout[1], "___", fixed = TRUE)[[1]]
  
  if (readout_parts[1] == "Percent") {
    name <- "%"
  } else if (readout_parts[1] == "Absolute count") {
    name <- "Absolute count of"
  } else {
    name <- readout_parts[1]
  }
  
  name <- paste(name, readout_parts[2])
  
  if (readout_parts[3] != "Bulk" && length(readout) == 1 && readout_parts[1] != "Absolute count") {
    name <- paste(name, "in", readout_parts[3])
  }
  
  return(name)
}


barplot_get_readout_type <- function(readout) {
  strsplit(readout[1], "___", fixed = TRUE)[[1]][1]
}


barplot_create_plot <- function(data, readout_name, readout_type, paired_groups, paired_populations, plot_type,
                                group_by, upper_limit, lower_limit, transform, discrete_colors, dot_color, dot_mean,
                                dot_point_size, dot_point_shape, dot_line_width, dot_jitter_seed, dot_jitter_width,
                                dot_mean_bar_size, dot_mean_line_width, box_varwidth, bar_type, x_angle) {
  if (is.null(data) || nrow(data) < 1) {
    return(NULL)
  }

  messages <- NULL

  discrete_scale <- function(aes, limits, ...) {
    if (is.null(labels)) {
      labels <- limits
    }

    if (is.null(discrete_colors)) {
      do.call(paste0("scale_", aes, "_viridis"), c(list(...), list(limits = limits, discrete = TRUE)))
    } else {
      do.call(paste0("scale_", aes, "_manual"),
              c(list(...), list(limits = limits, values = rep_len(discrete_colors, length(limits)))))
    }
  }

  group_ids <- unique(data$group_id)
  group_names <- sapply(group_ids, function(x) { data[data$group_id == x, "group_name"][1] })
  populations <- if ("population" %in% names(data)) unique(data$population) else NULL


  # Adjust limits if values are percentages
  if (readout_type == "Percent") {
    lower_limit <- lower_limit / 100
    upper_limit <- upper_limit / 100
  }

  if (!is.na(lower_limit) && !is.na(upper_limit) && lower_limit > upper_limit) {
    return(list(messages = "ERROR: Lower Y axis limit is higher than upper Y axis limit.", plot = NULL))
  }

  hidden_values <- 0

  only_summarized_data <- plot_type  ==  "Bar plot" && !(!is.na(bar_type) && endsWith(bar_type, "_dots"))

  # Check if there are values below the lower limit
  if (!is.na(lower_limit) && !only_summarized_data) {
    outside_y_axis_low <- data$value < lower_limit
    if (sum(outside_y_axis_low) > 0) {
      hidden_values <- hidden_values + sum(outside_y_axis_low)
      messages <- c(messages, paste0(sum(outside_y_axis_low),
                                     " samples are not shown because values were below the lower Y axis limit."))
    }
  }

  # Check if there are values above the upper limit
  if (!is.na(upper_limit) && !only_summarized_data) {
    outside_y_axis_high <- data$value > upper_limit
    if (sum(outside_y_axis_high) > 0) {
      hidden_values <- hidden_values + sum(outside_y_axis_high)
      messages <- c(messages, paste0(sum(outside_y_axis_high),
                                     " samples are not shown because values were above the upper Y axis limit."))
    }
  }

  # Check again if there are any data, in case the Y axis limits removed all values
  if (hidden_values == nrow(data)) {
    return(list(messages = "ERROR: All data points are outside the Y axis.", plot = NULL))
  }

  if (transform == "log") {
    remove <- data$value <= 0
    if (sum(remove) == nrow(data)) {
      return(list(messages = paste0("ERROR: All samples were removed because their values are below or equal to zero, ",
                                    "and therefore cannot be log transformed."), plot = NULL))
    } else if (sum(remove) > 0) {
      data <- data[!remove, ]
      messages <- c(messages, paste0(sum(remove),
                                     " samples are removed because values are below or equal to zero and log ",
                                     "transformation is enabled"))
    }

    if (!is.na(lower_limit) && lower_limit <= 0) {
      return(list(messages = "ERROR: Lower Y axis limit must be above 0 when log transformation is enabled",
                  plot = NULL))
    }
  }

  
  if (is.na(lower_limit)) {
    if (min(data$value) >= 0 && transform != "log") {
      lower_limit <- 0
    } else {
      lower_limit <- min(data$value)
    }
  }

  if (is.na(upper_limit)) {
    upper_limit <- max(data$value)
  }

  if (lower_limit == min(data$value) && transform != "log") {
    lower_limit <- lower_limit - (upper_limit - lower_limit) * 0.05
  }

  if (upper_limit == max(data$value)) {
    upper_limit <- upper_limit + (upper_limit - lower_limit) * 0.05
  }


  has_multiple_groups <- length(unique(data$group_id)) > 1
  has_multiple_populations <- "population" %in% names(data) && length(unique(data$population)) > 1

  if (has_multiple_populations && (!has_multiple_groups | group_by == "populations")) {
    group_by <- "populations"
  } else {
    group_by <- "groups"
  }

  if (plot_type == "Dot plot") {

    if (group_by == "populations") {
      if (has_multiple_groups) {
        p <- ggplot(data, aes(x = factor(population, populations), y = value,
                              color = factor(group_id, group_ids),
                              group = factor(group_id, group_ids))) +
          discrete_scale("color", limits = group_ids, labels = group_names, name = "Group")
        dot_mean_bar_color <- "black"
        geom_point_position <- position_jitterdodge(jitter.width = dot_jitter_width, dodge.width  = 0.75,
                                                    seed = dot_jitter_seed)
      } else {
        if (dot_color == "None") {
          p <- ggplot(data, aes(x = factor(population, populations), y = value, group = 1))
          dot_mean_bar_color <- "red"
        } else {
          p <- ggplot(data, aes(x = factor(population, populations), y = value, color = color, group = 1)) +
            labs(color = dot_color)
          if (is.numeric(data$color)) {
            p <- p + scale_color_viridis()
          } else {
            p <- p + discrete_scale("color", limits = as.character(unique(data$color)))
          }
          dot_mean_bar_color <- "black"
        }

        if (paired_populations) {
          p <- p + geom_line(aes(group = file), size = dot_line_width, alpha = 1)
          geom_point_position <- "identity"
        } else {
          geom_point_position <- position_jitter(width = dot_jitter_width, height = 0, seed = dot_jitter_seed)
        }
      }
    } else if (group_by == "groups") {
      if (has_multiple_populations) {
        p <- ggplot(data, aes(x = factor(group_id, group_ids), y = value,
                              color = factor(population, populations),
                              group = factor(population, populations))) +
          discrete_scale("color", limits = populations, name = "Population")
        dot_mean_bar_color <- "black"
        geom_point_position <- position_jitterdodge(jitter.width = dot_jitter_width, dodge.width  = 0.75,
                                                    seed = dot_jitter_seed)
      } else {
        if (dot_color == "None") {
          p <- ggplot(data, aes(x = factor(group_id, group_ids), y = value, group = 1))
          dot_mean_bar_color <- "red"
        } else {
          p <- ggplot(data, aes(x = factor(group_id, group_ids), y = value, color = color, group = 1)) +
            labs(color = dot_color)
          if (is.numeric(data$color)) {
            p <- p + scale_color_viridis()
          } else {
            p <- p + discrete_scale("color", limits = as.character(unique(data$color)))
          }
          dot_mean_bar_color <- "black"
        }

        if (paired_groups) {
          p <- p + geom_line(aes(group = pairing), size = dot_line_width, alpha = 1)
          geom_point_position <- "identity"
        } else {
          geom_point_position <- position_jitter(width = dot_jitter_width, height = 0, seed = dot_jitter_seed)
        }
      }
    }

    p <- p + geom_point(size = dot_point_size, shape = as.numeric(dot_point_shape), position = geom_point_position)
    
    if (dot_mean %in% c("mean", "median")) {
      p <- p + stat_summary(fun = dot_mean, geom = "point", shape = 95, size = dot_mean_bar_size,
                            color = dot_mean_bar_color, position = position_dodge(0.75))

      if ((group_by == "groups" && !has_multiple_populations && paired_groups)
          || (group_by == "populations" && !has_multiple_groups && paired_populations)) {
        p <- p + stat_summary(fun = dot_mean, geom = "line", size = dot_mean_line_width, color = dot_mean_bar_color)
      }
    }

  } else if (plot_type == "Box plot") {

    if (group_by == "populations") {
      if (has_multiple_groups) {
        p <- ggplot(data, aes(x = factor(population, populations), y = value, fill = factor(group_id, group_ids))) +
          discrete_scale("fill", limits = group_ids, labels = group_names, name = "Group")
      } else {
        p <- ggplot(data, aes(x = factor(population, populations), y = value))
      }
    } else if (group_by == "groups") {
      if (has_multiple_populations) {
        p <- ggplot(data, aes(x = factor(group_id, group_ids), y = value, fill = factor(population, populations))) +
          discrete_scale("fill", limits = populations, name = "Population")
      } else {
        p <- ggplot(data, aes(x = factor(group_id, group_ids), y = value))
      }
    }

    if ((group_by == "populations" && has_multiple_groups) || (group_by == "groups" && has_multiple_populations)) {
      p <- p + geom_boxplot(varwidth = box_varwidth, color = "black")
    } else {
      p <- p + geom_boxplot(varwidth = box_varwidth, color = "black", fill = "gray80")
    }
    

  } else if (plot_type == "Bar plot") {

    if (group_by == "populations") {
      if (has_multiple_groups) {
        p <- ggplot(data, aes(x = factor(population, populations), y = value, fill = factor(group_id, group_ids))) +
          discrete_scale("fill", limits = group_ids, labels = group_names, name = "Group")
      } else {
        p <- ggplot(data, aes(x = factor(population, populations), y = value))
      }
    } else if (group_by == "groups") {
      if (has_multiple_populations) {
        p <- ggplot(data, aes(x = factor(group_id, group_ids), y = value, fill = factor(population, populations))) +
          discrete_scale("fill", limits = populations, name = "Population")
      } else {
        p <- ggplot(data, aes(x = factor(group_id, group_ids), y = value))
      }
    }

    bar_position <- position_dodge(0.8)

    bar_type <- strsplit(bar_type, split = "_")[[1]]
    bar_mean <- bar_type[1]
    bar_error <- if (length(bar_type) == 2) bar_type[2] else NA

    if (has_multiple_groups && has_multiple_populations) {
      p <- p + stat_summary(fun = bar_mean, geom = "bar", position = bar_position, width = 0.7, color = "black")
    } else {
      p <- p + stat_summary(fun = bar_mean, geom = "bar", position = bar_position, fill = "gray80", color = "gray40",
                            width = 0.7)
    }

    if (!is.na(bar_error)) {
      if (bar_error == "dots") {
        if (has_multiple_populations && has_multiple_groups) {
          geom_point_position <- position_jitterdodge(jitter.width = dot_jitter_width, dodge.width  = 0.75,
                                                      seed = dot_jitter_seed)
        } else {
          geom_point_position <- position_jitter(width = dot_jitter_width, height = 0, seed = dot_jitter_seed)
        }
        p <- p + geom_point(size = dot_point_size, shape = as.numeric(dot_point_shape), position = geom_point_position)
      } else {
        errorbar <- function(x, mean, error) {
          y <- do.call(mean, list(x))
          if (error == "sd") {
            sd <- sd(x)
            return(data.frame(y = y, ymin = y - sd, ymax = y + sd))
          } else if (error == "se") {
            se <- sd(x) / sqrt(length(x))
            return(data.frame(y = y, ymin = y - se, ymax = y + se))
          } else if (error == "IQR") {
            quantiles <- quantile(x, c(0.25, 0.75))
            return(data.frame(y = y, ymin = quantiles[1], ymax = quantiles[2]))
          } else if (error == "95") {
            quantiles <- quantile(x, c(0.025, 0.975))
            return(data.frame(y = y, ymin = quantiles[1], ymax = quantiles[2]))
          } else if (error == "range") {
            return(data.frame(y = y, ymin = min(x), ymax = max(x)))
          }
        }
  
        p <- p + stat_summary(fun.data = errorbar, geom = "errorbar", position = bar_position,
                              fun.args = list(mean = bar_mean, error = bar_error), width = 0.4)
      }
    }

  } else {

    return(list(messages = "ERROR: Unknown plot type.", plot = NULL))

  }

  p <- p + coord_cartesian(ylim = c(lower_limit, upper_limit)) +
    ylab(readout_name)

  if (group_by == "groups")
    p <- p + scale_x_discrete(limits = group_ids, labels = group_names)
  
  y_labels <- if (readout_type == "Percent") scales::percent else waiver()
  p <- p + scale_y_continuous(labels = y_labels, expand = c(0, 0),
                              trans = if (transform == "log") "log10" else "identity")
  p <- p + barplot_theme(x_angle = x_angle)

  return(list(messages = messages, plot = p))
}


barplot_create_group_statistics_table <- function(data, paired, percent, p_adjust_method) {
  if (length(unique(data$group_id)) < 2)
    return(NULL)

  if ("population" %in% names(data)) {
    df <- do.call(rbind, lapply(unique(data$population), function(population) {
      population_df <- barplot_create_group_statistics_df(data            = data[data$population == population, ],
                                                          paired          = paired,
                                                          p_adjust_method = p_adjust_method)
      population_df$Population <- population
      population_df <- population_df[, c("Population", setdiff(names(population_df), "Population"))]
    }))
  } else {
    df <- barplot_create_group_statistics_df(data            = data,
                                             paired          = paired,
                                             p_adjust_method = p_adjust_method)
  }

  if (percent) {
    for (column in c("Group A mean", "Group A median", "Group B mean", "Group B median")) {
      df[, column] <- format_percentages(df[, column])
    }
  }

  return(df)
}


barplot_create_population_statistics_table <- function(data, paired, percent, p_adjust_method) {
  if (length(unique(data$population)) < 2)
    return(NULL)

  if (length(unique(data$group_id)) > 1) {
    df <- do.call(rbind, lapply(unique(data$group_id), function(group) {
      group_df <- barplot_create_population_statistics_df(data            = data[data$group_id == group, ],
                                                          paired          = paired,
                                                          p_adjust_method = p_adjust_method)
      group_df$Group <- data[data$group_id == group, "group_name"][1]
      group_df <- group_df[, c("Group", setdiff(names(group_df), "Group"))]
    }))
  } else {
    df <- barplot_create_population_statistics_df(data            = data,
                                                  paired          = paired,
                                                  p_adjust_method = p_adjust_method)
  }

  if (percent) {
    for (column in c("Pop. A mean", "Pop. A median", "Pop. B mean", "Pop. B median")) {
      df[, column] <- format_percentages(df[, column])
    }
  }

  return(df)
}


barplot_create_group_statistics_df <- function(data, paired, p_adjust_method) {
  stats <- barplot_perform_statistical_comparison(values          = data$value,
                                                  groups          = data$group_id,
                                                  paired          = paired,
                                                  p_adjust_method = p_adjust_method)
  
  group_names <- function(ids) {
    sapply(ids, function(id) {
      if (id == 0) "All" else data[data$group_id == id, "group_name"][1]
    })
  }

  df <- data.frame("Group A"        = group_names(stats$group1_id),
                   "Group B"        = group_names(stats$group2_id),
                   "Group A mean"   = stats$group1_mean,
                   "Group B mean"   = stats$group2_mean,
                   "Group A median" = stats$group1_median,
                   "Group B median" = stats$group2_median,
                   "Group A n"      = stats$group1_n,
                   "Group B n"      = stats$group2_n,
                   "p value"        = stats$p,
                   "Sign."          = p_to_asterisks(stats$p),
                   "Test"           = stats$test,
                   check.names      = FALSE)

  if ("p_adj" %in% names(stats)) {
    df <- cbind(df, data.frame("p value (adj.)" = stats$p_adj,
                               "Sign. (adj.)"   = p_to_asterisks(stats$p_adj),
                               "p value adj."   = stats$p_adjust_method,
                               check.names      = FALSE))
  }
  
  return(df)
}


barplot_create_population_statistics_df <- function(data, paired, p_adjust_method) {
  stats <- barplot_perform_statistical_comparison(values          = data$value,
                                                  groups          = data$population,
                                                  paired          = paired,
                                                  p_adjust_method = p_adjust_method)

  df <- data.frame("Population A"        = ifelse(stats$group1_id == 0, "All", stats$group1_id),
                   "Population B"        = ifelse(stats$group2_id == 0, "All", stats$group2_id),
                   "Pop. A mean"         = stats$group1_mean,
                   "Pop. B mean"         = stats$group2_mean,
                   "Pop. A median"       = stats$group1_median,
                   "Pop. B median"       = stats$group2_median,
                   "Pop. A n"            = stats$group1_n,
                   "Pop. B n"            = stats$group2_n,
                   "p value"             = stats$p,
                   "Sign."               = p_to_asterisks(stats$p),
                   "Test"                = stats$test,
                   check.names           = FALSE)

  if ("p_adj" %in% names(stats)) {
    df <- cbind(df, data.frame("p value (adj.)" = stats$p_adj,
                               "Sign. (adj.)"   = p_to_asterisks(stats$p_adj),
                               "p value adj."   = stats$p_adjust_method,
                               check.names      = FALSE))
  }
  
  return(df)
}


barplot_perform_statistical_comparison <- function(values, groups, paired = FALSE, p_adjust_method = "none") {
  group_ids <- unique(groups)

  if (length(group_ids) < 2) {
    return(NULL)
  } else if (length(group_ids) == 2) {
    test <- suppressWarnings(wilcox.test(values[groups == group_ids[1]],
                                         values[groups == group_ids[2]],
                                         paired = paired))

    df <- data.frame(p    = test$p.value,
                     test = test$method)
    for (i in seq_along(group_ids)) {
      df[, paste0("group", i, "_id")]     <- group_ids[i]
      df[, paste0("group", i, "_mean")]   <- mean(values[groups == group_ids[i]])
      df[, paste0("group", i, "_median")] <- median(values[groups == group_ids[i]])
      df[, paste0("group", i, "_n")]      <- sum(groups == group_ids[i])
    }
  } else {
    kruskal_test <- kruskal.test(x = values, g = as.factor(groups))

    if (is.na(kruskal_test$p.value) || kruskal_test$p.value > setting("posthoc_limit")) {
      df <- data.frame(group1_id     = 0,
                       group2_id     = 0,
                       group1_mean   = NA,
                       group2_mean   = NA,
                       group1_median = NA,
                       group2_median = NA,
                       group1_n      = length(values),
                       group2_n      = length(values),
                       p             = kruskal_test$p.value,
                       test          = kruskal_test$method)

      if (p_adjust_method != "none") {
        df$p_adj           <- NA
        df$p_adjust_method <- NA
      }
    } else {
      invisible(capture.output(dunn_test <- dunn.test::dunn.test(x = values, g = as.factor(groups),
                                                                 method = p_adjust_method, altp = TRUE)))
      df <- data.frame(group1_id = c(0, unlist(lapply(strsplit(dunn_test$comparisons, " - ", fixed = TRUE), '[[', 1))),
                       group2_id = c(0, unlist(lapply(strsplit(dunn_test$comparisons, " - ", fixed = TRUE), '[[', 2))),
                       p         = c(kruskal_test$p.value, dunn_test$altP),
                       test      = c(kruskal_test$method, rep("Dunn's test", length(dunn_test$altP))))

      df <- cbind(df, do.call(rbind, lapply(1:nrow(df), function(row) {
        group1_id <- df[row, "group1_id"]
        group2_id <- df[row, "group2_id"]

        if (group1_id == 0) {
          data.frame(group1_mean   = NA,
                     group2_mean   = NA,
                     group1_median = NA,
                     group2_median = NA,
                     group1_n      = length(values),
                     group2_n      = length(values))
        } else {
          data.frame(group1_mean   = mean(values[groups == group1_id]),
                     group2_mean   = mean(values[groups == df[row, "group2_id"]]),
                     group1_median = median(values[groups == df[row, "group1_id"]]),
                     group2_median = median(values[groups == df[row, "group2_id"]]),
                     group1_n      = sum(groups == df[row, "group1_id"]),
                     group2_n      = sum(groups == df[row, "group2_id"]))
        }
      })))
      
      if (p_adjust_method != "none") {
        df$p_adj           <- c(NA, dunn_test$altP.adjust)
        df$p_adjust_method <- c(NA, rep(p_adjust_methods(method = p_adjust_method), length(dunn_test$altP)))
      }
    }
  }

  return(df)
}


barplot_theme <- function(x_angle = 90) {
  if (x_angle == 0) {
    x_hjust <- 0.5
    x_vjust <- 1
  } else if (x_angle == 15) {
    x_hjust <- 1
    x_vjust <- 1
  } else if (x_angle == 30) {
    x_hjust <- 1
    x_vjust <- 1
  } else if (x_angle == 45) {
    x_hjust <- 1
    x_vjust <- 1
  } else if (x_angle == 60) {
    x_hjust <- 1
    x_vjust <- 1
  } else if (x_angle == 75) {
    x_hjust <- 1
    x_vjust <- 1
  } else if (x_angle == 90) {
    x_hjust <- 1
    x_vjust <- 0.5
  }

  return(theme(
    panel.background      = element_blank(),
    axis.ticks.x          = element_blank(),
    axis.ticks.y          = element_line(size = 0.5, linetype = "solid", color = "black"),
    axis.text.x           = element_text(size = 18, color = "black", angle = x_angle, hjust = x_hjust, vjust = x_vjust,
                                         margin = margin(t = 8, r = 0, b = 0, l = 0)),
    axis.text.y           = element_text(size = 18, color = "black"),
    axis.title.x          = element_blank(),
    axis.title.y          = element_text(size = 18 , color = "black", margin = margin(0, 10, 0, 0)),
    axis.line.x           = element_line(size = 0.5, linetype = "solid", color = "black"),
    axis.line.y           = element_line(size = 0.5, linetype = "solid", color = "black"),
    plot.title            = element_blank(),
    legend.key            = element_blank(),
    legend.key.size       = unit(2, "lines"),
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.title          = element_text(size = 18, face = "bold"),
    legend.text           = element_text(size = 18),
    legend.position       = "right",
    legend.box.margin     = margin(12, 12, 8, 12),
    plot.margin           = margin(12, 0, 0, 2)
  ))
}


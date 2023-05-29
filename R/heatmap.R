heatmap_module_name <- function() {
  return("Heatmap")
}


create_heatmap_plot <- function(data, group_data, groups, readouts, is_paired, is_comparison, heatmap_type,
                                readout_x_axis, heatmap_comparison, mean_type, lower_limit, upper_limit, colors,
                                color_transform, show_axis_titles, x_axis_angled, x_axis_position, text_size) {
  if (length(readouts) < 1 || length(groups) < 1 || is.null(data) || nrow(data) < 1) {
    return(NULL)
  }

  messages <- NULL

  if (!is_comparison && !is.na(lower_limit) && lower_limit <= 0 && !is.null(color_transform) &&
      color_transform == "log") {
    return(list(messages = "ERROR: Lower limit should be above 0 when log transform is enabled.", plot = NULL))
  }

  if (!is.na(lower_limit) && !is.na(upper_limit) && lower_limit >= upper_limit) {
    return(list(messages = "ERROR: Upper limit should be higher than lower limit.", plot = NULL))
  }

  if (!is_comparison && length(groups) > 1) {
    if (readout_x_axis) {
      p <- ggplot(data, aes(x    = factor(readout_name, unique(readout_name)),
                            y    = factor(group_name, rev(unique(group_name))),
                            fill = value)) +
             xlab("Readouts") +
             ylab("Groups")
    } else {
      p <- ggplot(data, aes(x    = factor(group_name, unique(group_name)),
                            y    = factor(readout_name, rev(unique(readout_name))),
                            fill = value)) +
             xlab("Groups") +
             ylab("Readouts")
    }
  } else {
    if (length(groups) == 2) {
      if (heatmap_comparison == "Absolute change") {
        data$value <- data$absolute_change
        z_scale_name <- paste0("Absolute\nchange\nin ", tolower(mean_type))
      } else if (heatmap_comparison == "p values") {
        asterisk_colors <- c("****"  = "#fc644d",
                             "***"   = "#f2a466",
                             "**"    = "#eddc88",
                             "*"     = "#eee8c1",
                             "ns."   = "#e5e5e5",
                             "* "    = "#c1e8ee",
                             "** "   = "#88dced",
                             "*** "  = "#66a4f2",
                             "**** " = "#4d64fc")
        
        data$p_plot <- if ("p_adj" %in% names(data)) data$p_adj else data$p
        data[is.na(data$p_plot), "p_plot"] <- 1
        data$value <- p_to_asterisks(data$p_plot)
        group_1_higher <- grepl("*", data$value, fixed = TRUE) & data$group_1_mean > data$group_2_mean
        data[group_1_higher, "value"] <- paste0(data[group_1_higher, "value"], " ")
        data$value <- factor(data$value, levels = names(asterisk_colors))
        z_scale_name <- "p values\nfor change"
      } else {
        data$value <- data$log2_fold_change
        z_scale_name <- paste0("log2\nfold change\nin ", tolower(mean_type))
      }
    }

    if (readout_x_axis) {
      p <- ggplot(data, aes(x    = factor(readout_value, unique(readout_value)),
                            y    = factor(readout_population, rev(unique(readout_population))),
                            fill = value)) +
             xlab("Readouts") +
             ylab("Populations")
    } else {
      p <- ggplot(data, aes(x    = factor(readout_population, unique(readout_population)),
                            y    = factor(readout_value, rev(unique(readout_value))),
                            fill = value)) +
             xlab("Populations") +
             ylab("Readouts")
    }
  }

  p <- p + geom_tile(color = "white")

  if (is_comparison) {
    if (heatmap_comparison == "p values") {
      
      labels <- names(asterisk_colors)
      labels[1] <- paste0(labels[1], " (more in ", group_data[[groups[2]]]$name, ")")
      labels[9] <- paste0(labels[9], " (more in ", group_data[[groups[1]]]$name, ")")
      p <- p + scale_fill_manual(values = asterisk_colors,
                                 breaks = names(asterisk_colors),
                                 labels = labels,
                                 name   = paste0(z_scale_name, "\n"),
                                 drop   = FALSE)
    } else {
      auto_limit <- max(0.1, max(abs(data$value), na.rm = TRUE))
      if (is.na(upper_limit)) upper_limit <- auto_limit
      if (is.na(lower_limit)) lower_limit <- -auto_limit

      breaks <- c(ceiling(lower_limit * 10), 0, floor(upper_limit * 10)) / 10
      labels <- breaks
      labels[1] <- paste0(labels[1], " (more in ", group_data[[groups[1]]]$name, ")")
      labels[3] <- paste0(labels[3], " (more in ", group_data[[groups[2]]]$name, ")")
      p <- p + scale_fill_gradient2(low      = colors[1],
                                    mid      = colors[2],
                                    high     = colors[3],
                                    midpoint = 0,
                                    limits   = c(lower_limit, upper_limit),
                                    name     = paste0(z_scale_name, "\n"),
                                    breaks   = breaks,
                                    labels   = labels)
    }
  } else {
    readout_type <- data$readout_prefix[1]

    if (is.na(lower_limit)) {
      if (!is.null(color_transform) && color_transform == "log") {
        lower_limit <- NA
      } else {
        lower_limit <- 0
      }
    }

    if (readout_type == "Percent") {
      lower_limit <- lower_limit / 100
      upper_limit <- upper_limit / 100

      plot_labels <- scales::percent
    } else {
      plot_labels <- waiver()
    }
    
    if (readout_type == "Percent") {
      z_scale_name <- paste0(mean_type, " of\n% positive")
    } else if (readout_type == "Mean") {
      z_scale_name <- paste0(mean_type, " of\nmean\nintensity")
    } else if (readout_type == "Median") {
      z_scale_name <- paste0(mean_type, " of\nmedian\nintensity")
    } else {
      z_scale_name <- paste0(mean_type, " of\n", readout_type)
    }

    scale_fill_gradientn_args <- list(colors = colors,
                                      name   = paste0(z_scale_name, "\n"),
                                      limits = c(lower_limit, upper_limit),
                                      labels = plot_labels)

    if (!is.null(color_transform) && color_transform == "log") {
      scale_fill_gradientn_args <- c(scale_fill_gradientn_args, list(trans = "log10"))
    }

    p <- p + do.call(scale_fill_gradientn, scale_fill_gradientn_args)
  }

  if (!show_axis_titles) {
    p <- p + xlab(NULL) + ylab(NULL)
  }

  p <- p + theme_classic() +
           heatmap_theme(text_size = text_size, x_axis_angled = x_axis_angled, x_axis_top = x_axis_position == "Top") +
           scale_x_discrete(position = if (x_axis_position == "Top") "top" else "bottom") +
           coord_fixed(ratio = 1)
  
  return(list(messages = messages, plot = p))
}


heatmap_theme <- function(text_size = 15, x_axis_angled = TRUE, x_axis_top = TRUE) {
  x_axis_angle <- if (x_axis_angled) 45 else 90
  x_axis_vjust <- if (x_axis_angled) { if (x_axis_top) 0 else 1 } else 0.5

  return(theme(
    axis.line            = element_blank(), 
    axis.ticks           = element_blank(),
    axis.title.x         = element_text(size = text_size, face = "bold", margin = margin(25, 0, 25, 0)),
    axis.title.y         = element_text(size = text_size, face = "bold", margin = margin(0, 25, 0, 0)),
    axis.text.x          = element_text(color = "black", size = text_size, angle = x_axis_angle,
                                        hjust = 1, vjust = x_axis_vjust),
    axis.text.x.top      = element_text(hjust = 0, vjust = x_axis_vjust),
    axis.text.y          = element_text(color = "black", size = text_size),
    legend.justification = "top",
    legend.title         = element_text(size = text_size, face = "bold"),
    plot.title           = element_blank(),
    legend.text          = element_text(size = text_size)
  ))
}


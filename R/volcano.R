#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


volcano_module_name <- function() {
  return("Volcano plot")
}


create_volcano_plot <- function(data, group_data, groups, readouts, comparison, mean_type, x_lower_limit,
                                x_upper_limit, y_upper_limit, annotation, annotation_limit_p, annotation_limit_change,
                                annotation_size, signline, signline_limit_p, signline_color, signline_type,
                                y_axis_label_type) {
  if (length(readouts) < 1 || length(groups) < 1 || is.null(data) || nrow(data) < 1)
    return(NULL)
  
  messages <- NULL

  if (comparison == "Absolute change") {
    data$value   <- data$absolute_change
    x_scale_name <- paste0("Absolute\nchange\nin ", tolower(mean_type))
    x_axis_name  <- paste0(group_data[[groups[2]]]$name, " – ", group_data[[groups[1]]]$name)
  } else {
    data$value   <- data$log2_fold_change
    x_scale_name <- paste0("log2\nfold change\nin ", tolower(mean_type))
    x_axis_name  <- paste0(group_data[[groups[2]]]$name, " / ", group_data[[groups[1]]]$name)
  }

  if ("p_adj" %in% names(data)) {
    data$p_plot <- data$p_adj
  } else {
    data$p_plot <- data$p
  }

  # Check readouts with missing data
  na_readouts <- is.na(data$p_plot) | is.na(data$value)
  if (sum(na_readouts) > 0) {
    remove_names <- data[na_readouts, "readout_name"]
    data <- data[!na_readouts,]
    messages <- c(messages, paste0("Removed the following readouts because sufficient data was not available: ",
                                   paste(remove_names, collapse = ", ")))
  }

  if (nrow(data) < 1)
    return(list(messages = c(messages, "ERROR: No data to plot.", plot = NULL)))

  # X axis
  x_auto_limit <- max(0.1, max(abs(data$value), na.rm = TRUE))
  if (is.na(x_upper_limit))
    x_upper_limit <- x_auto_limit
  if (is.na(x_lower_limit))
    x_lower_limit <- -x_auto_limit

  outside_x_axis_high <- data$value > x_upper_limit
  if (sum(outside_x_axis_high) > 0) {
    data <- data[!outside_x_axis_high,]
    messages <- c(messages, paste0(sum(outside_x_axis_high),
                                   " readouts were removed because values were above the upper X axis limit."))
  }

  outside_x_axis_low <- data$value < x_lower_limit
  if (sum(outside_x_axis_low) > 0) {
    data <- data[!outside_x_axis_low,]
    messages <- c(messages, paste0(sum(outside_x_axis_low),
                                   " readouts were removed because values were below the lower X axis limit."))
  }

  # Y axis
  if (!is.na(y_upper_limit)) {
    if (y_upper_limit == 0) {
      y_upper_limit <- NA
    } else if (y_upper_limit < 0) {
      y_upper_limit <- NA
      messages <- c(messages, "The p value limit was ignored because it was set to a value below 0")
    } else if (y_upper_limit > 1) {
      y_upper_limit <- NA
      messages <- c(messages, "The p value limit was ignored because it was set to a value above 1")
    } else {
      y_upper_limit <- -log10(y_upper_limit)
      outside_y_axis_high <- -log10(data$p_plot) > y_upper_limit
      if (sum(outside_y_axis_high) > 0) {
        data <- data[!outside_y_axis_high,]
        messages <- c(messages, paste0(sum(outside_y_axis_high),
                                       " readouts were removed because values were above the upper Y axis limit."))
      }
    }
  } else {
    y_upper_limit <- max(2, ceiling(-log10(min(abs(data$p_plot), na.rm = TRUE))))
  }

  if (nrow(data) < 1)
    return(list(messages = c(messages, "ERROR: All data points are outside the axes.", plot = NULL)))

  y_axis_name <- if ("p_adj" %in% names(data)) "Adjusted p value" else "p value"

  p <- ggplot(data, aes(x     = value,
                        y     = -log10(p_plot),
                        color = value)) +
    xlab(x_axis_name) +
    ylab(y_axis_name)


  if (signline) {
    if (is.null(signline_color) || signline_color == "") signline_color <- "gray50"
    p <- p + geom_hline(yintercept = -log10(signline_limit_p),
                        color      = signline_color,
                        size       = 0.7,
                        linetype   = signline_type)
  }

  p <- p + geom_point()

  y_labels <- if (y_axis_label_type == "superscript") {
    c("1", parse(text = paste0("10^-", 1:20)))
  } else if (y_axis_label_type == "decimal") {
    sapply(0:20, function(x) format(10^-x, scientific = FALSE))
  } else {
    c("1", paste0("1e-", 1:20))
  }

  breaks <- c(ceiling(x_lower_limit * 10), 0, floor(x_upper_limit * 10)) / 10
  labels <- breaks
  labels[1] <- paste0(labels[1], " (more in ", group_data[[groups[1]]]$name, ")")
  labels[3] <- paste0(labels[3], " (more in ", group_data[[groups[2]]]$name, ")")
  p <- p + scale_color_gradient2(low      = "blue",
                                 mid      = "grey50",
                                 high     = "red",
                                 midpoint = 0,
                                 limits   = c(x_lower_limit, x_upper_limit),
                                 name     = paste0(x_scale_name, "\n"),
                                 breaks   = breaks,
                                 labels   = labels) +
    scale_x_continuous(limits = c(x_lower_limit, x_upper_limit)) +
    scale_y_continuous(breaks = 0:20,
                       labels = y_labels,
                       limits = c(0, y_upper_limit))

  if (annotation) {
    p <- p + ggrepel::geom_text_repel(subset(data,
                                             p_plot <= annotation_limit_p & abs(value) >= annotation_limit_change),
                                      mapping = aes(label = readout_name),
                                      size          = annotation_size,
                                      color         = "black",
                                      box.padding   = unit(0.8, "lines"),
                                      point.padding = unit(0.5, "lines"))
  }
  
  p <- p + volcano_theme()

  return(list(messages = messages, plot = p))
}


get_volcano_hover_data <- function(data, comparison) {
  if (is.null(data) || nrow(data) < 1)
    return(NULL)

  text <- format_readout_names(readout_ids = data$readout)

  if (comparison == "Absolute change") {
    text <- paste0(text, "\nAbsolute change: ", round(data$absolute_change, 2))
    x    <- data$absolute_change
  } else {
    text <- paste0(text, "\nlog2 fold change: ", round(data$log2_fold_change, 2))
    x    <- data$log2_fold_change
  }

  p_value_text <- function(p) {
    ifelse(p < 0.0001, "p < 0.0001", paste0("p = ", round(p, 4)))
  }

  if ("p_adj" %in% names(data)) {
    text <- paste0(text, "\n", p_value_text(data$p_adj), " (adjusted)")
    y    <- -log10(data$p_adj)
  } else {
    text <- paste0(text, "\n", p_value_text(data$p))
    y    <- -log10(data$p)
  }

  return(data.frame(x    = x,
                    y    = y,
                    text = text))
}



volcano_theme <- function() {
  return(theme(
    panel.background = element_blank(),
    axis.text.x      = element_text(size = 18, color = "black"),
    axis.text.y      = element_text(size = 18, color = "black"),
    axis.title.x     = element_text(size = 18, face = "plain", margin = margin(20, 0, 0, 0)),
    axis.title.y     = element_text(size = 18, face = "plain", margin = margin(0, 20, 0, 0)),
    axis.line.x      = element_blank(),
    axis.line.y      = element_blank(),
    axis.ticks       = element_line(size = 0.5, linetype = "solid", color = "black"),
    plot.title       = element_blank(),
    panel.border     = element_rect(fill = NA, size = 1, color = "black"),
    legend.title     = element_text(size = 18, face = "bold"),
    legend.text      = element_text(size = 18),
    aspect.ratio     = 1
  ))
}


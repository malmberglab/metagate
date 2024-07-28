#
#  MetaGate: R package for visualization and statistical analysis of cytometry data.
#  Copyright (C) 2023 Malmberg Lab, University of Oslo. See LICENSE.md
#  


p_adjust_methods <- function(method = NULL) {
  p_adjust_methods <- list("No p value adjustment"                     = "none",
                           "Holm"                                      = "holm",
                           "Hochberg"                                  = "hochberg",
                           "Bonferroni"                                = "bonferroni",
                           "Benjamini-Hochberg (false discovery rate)" = "BH",
                           "Benjamini-Yekutieli"                       = "BY")
  if (!is.null(method)) {
    return(names(p_adjust_methods)[p_adjust_methods == method])
  } else {
    return(p_adjust_methods)
  }
}


msg <- function(...) {
  if (setting("debug")) {
    message(format(Sys.time(), "%H:%M:%S"), "  ", paste(..., sep = ""), sep = "")
  }
}


uiOutputWithLoader <- function(...) {
  shinycssloaders::withSpinner(uiOutput(...),
                               type  = 7,
                               color = "#2b999e")
}
plotOutputWithLoader <- function(...) {
  shinycssloaders::withSpinner(plotOutput(...),
                               type    = 7,
                               color   = "#2b999e",
                               hide.ui = FALSE)
}


progress_indicator <- function(i, total, message = "Processing...", only_percent = FALSE) {
  cat("\r", message, " ", sep = "")

  if (i == total) {
    cat("Done!\n")
  } else if (only_percent) {
    cat(round(i/total*100), "%", sep = "")
  } else {
    cat(i, " of ", total, " (", round(i/total*100), "%)", sep = "")
  }
}


label <- function(...) {
  strings <- c(...)
  if (length(strings) < 1) {
    return(NULL)
  } else if (length(strings) == 1) {
    return(tags$label(paste0(strings[1], ":")))
  } else {
    return(HTML(paste0('<label class="help-text" title="', paste(strings[2:length(strings)], collapse = " "), '">',
                       strings[1], ':</label>')))
  }
}


format_time <- function(x) {
  h <- floor(x / 3600)
  m <- floor((x - h * 3600) / 60)
  s <- round(x - h * 3600 - m * 60, 1)
  if (h == 0 && m == 0) {
    return(paste0(s, " seconds"))
  } else if (h == 0) {
    return(paste0(m, " minutes and ", s, " seconds"))
  } else {
    return(paste0(h, " hours, ", m, " minutes and ", s, " seconds"))
  }
}


run_time_formatted <- function(start) {
  format_time(difftime(Sys.time(), start, units = "secs"))
}


p_to_asterisks <- function(p) {
  limits <- setting("asterisk_limits")

  return(sapply(p, function(x) {
    if (is.na(x))
      return("")

    asterisks <- sum(x <= limits)

    if (asterisks == 0) {
      return("ns.")
    } else {
      return(strrep("*", asterisks))
    }
  }))
}

p_to_string <- function(p, digits = 4) {
  return(sapply(p, function(x) {
    if (is.na(x)) {
      return("")
    } else if (x < 10^(-digits)) {
      return(paste0("< ", format(10^(-digits), scientific = FALSE)))
    } else {
      return(as.character(format(round(x, digits), scientific = FALSE)))
    }
  }))
}


show_error_modal <- function(single = NULL, multiple = NULL, title = "Error") {
  if (is.null(single) && is.null(multiple)) {
    content <- p("An error occurred")
  } else if (!is.null(single) & is.null(multiple)) {
    content <- p(single)
  } else {
    header <- if (is.null(single)) "The following errors occurred:" else single
    content <- tagList(p(header),
               tags$ul(lapply(lapply(multiple, HTML), tags$li)))
  }

  showModal(modalDialog(title  = title,
                        footer = modalButton("Close"),
                        content))
}


show_processing_modal <- function(title = "Processing", message = "Please wait") {
  showModal(modalDialog(title = title, HTML(message), footer = NULL, easyClose = FALSE))
}


download_xlsx_file <- function(data, file, row_names = FALSE) {
  msg("Downloading ", file, " with data of class ", class(data))
  openxlsx::write.xlsx(x           = data,
                       file        = file,
                       headerStyle = openxlsx::createStyle(textDecoration = "BOLD"),
                       creator     = paste("MetaGate", packageDescription("metagate", fields = "Version")),
                       rowNames    = row_names)
}

read_xlsx_file <- function(file) {
  tryCatch(openxlsx::read.xlsx(file, check.names = FALSE, sep.names = " "),
           warning = function(w) { msg("Warning reading XLSX file: ", w$message); return(NULL) },
           error = function(e) { msg("Error reading XLSX file: ", e$message); return(NULL) })
}

xlsx_mime_types <- function() {
  return("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
}


display_messages <- function(messages) {
    if (length(messages) > 0) {
      is_error <- startsWith(messages, "ERROR:")
      warnings <- messages[!is_error]
      errors <- gsub("^ERROR:( )?", "", messages[is_error])

      return(tagList(
        if (length(errors) > 0) div(class="message_error", lapply(errors, function(x) p(HTML(x)))),
        if (length(warnings) > 0) div(class="message_warning", lapply(warnings, function(x) p(HTML(x))))
      ))
    }
}

string_pad <- function(x, length, pad = " ", right = FALSE) {
  sapply(x, function(string) {
    if (nchar(string) >= length) {
      return(string)
    }

    padding <- paste(rep_len(strsplit(pad, "")[[1]], length - nchar(string)), collapse = "")
    if (right) {
      return(paste0(string, padding))
    } else {
      return(paste0(padding, string))
    }
  })
}

col2hex <- function(col) {
    rgb <- col2rgb(col)
    hex <- string_pad(as.character.hexmode(rgb), length = 2, pad = "0")
    return(paste0("#", paste(hex, collapse = "")))
}


colorSelectizeInput <- function(inputId, label = NULL, selected = NULL, maxItems = NULL, width = "100%") {
  render <- I('{
    item: function(item, escape) {
      return "<div style=\'background-color: " + item.value + "\'>" + escape(item.label) + "</div>";
    },
    option: function(item, escape) {
      return "<div style=\'background-color: " + item.value + "\'>" + escape(item.label) + "</div>";
    }
  }')

  options <- list(render = render)

  if (!is.null(maxItems) && maxItems > 1) {
    options$maxItems <- maxItems
  }

  if (!is.null(selected)) {
    if (is.null(maxItems) || maxItems != 1) {
      selected <- as.list(sapply(selected, col2hex))
      } else {
        options$onInitialize <- I(paste0("function() { this.setValue('", col2hex(selected), "') }"))
        selected <- NULL
      }
  }

  selectizeInput(inputId  = inputId,
                 label    = label,
                 choices  = as.list(sapply(colors(), col2hex)),
                 selected = selected,
                 multiple = is.null(maxItems) || maxItems != 1,
                 width    = width,
                 options  = options)
}

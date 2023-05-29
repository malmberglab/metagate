format_readout_names <- function(readout_ids) {
  readout_ids <- gsub("^Absolute count___(.*)___(.*)$", "Absolute count of \\1", readout_ids)
  readout_ids <- gsub("Percent___", "% ", readout_ids, fixed = TRUE)
  readout_ids <- gsub("Mean___", "Mean ", readout_ids, fixed = TRUE)
  readout_ids <- gsub("Median___", "Median ", readout_ids, fixed = TRUE)
  readout_ids <- gsub("Geometric Mean___", "Geo. mean ", readout_ids, fixed = TRUE)
  readout_ids <- gsub("___", " in ", readout_ids, fixed = TRUE)
  readout_ids <- gsub(" in Bulk", "", readout_ids, fixed = TRUE)
  return(readout_ids)
}

get_readout_types <- function(stat_data) {
  readouts <- row.names(stat_data)
  split <- strsplit(readouts, "___", fixed = TRUE)
  return(unique(sapply(split, '[', 1)))
}


get_readout_populations <- function(stat_data) {
  readouts <- row.names(stat_data)
  readouts <- readouts[startsWith(readouts, "Percent___")]
  split <- strsplit(readouts, "___", fixed = TRUE)
  return(unique(sapply(split, '[', 3)))
}
#' Get the Directory of the Current File
#'
#' This function retrieves the directory of the currently running script. It first 
#' attempts to get the file location from the command-line arguments (for non-interactive 
#' sessions). If unavailable, it falls back to retrieving the file path from RStudio's 
#' source editor context (for interactive RStudio sessions).
#'
#' @return A character string representing the directory path of the current file.
#' If the function is run in an interactive R session without an active file, 
#' it may return an empty string.
#' @importFrom tibble enframe
#' @importFrom tidyr separate
#' @importFrom dplyr filter pull
#' @importFrom rstudioapi getSourceEditorContext
#' @export
getCurrentFileLocation <- function() {
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = 'right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  
  return(dirname(this_file))
}


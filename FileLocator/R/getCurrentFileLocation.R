#' Get the Directory of the Current File
#'
#' This function retrieves the directory of the currently running script. It first 
#' attempts to get the file location from the command-line arguments (for non-interactive 
#' sessions). Next, it attemps to retrieve the file path from RStudio's 
#' source editor context (for interactive RStudio sessions).  If that is unavailable,
#' it returns a warning and falls back to the current working directory.  
#'
#' @return A character string representing the directory path of the current file.
#' If the function is run in an interactive R session without an active file, 
#' it may return an empty string.
#' @importFrom tibble enframe
#' @importFrom tidyr separate
#' @importFrom dplyr filter pull %>%
#' @importFrom rstudioapi getSourceEditorContext
#' @export
getCurrentFileLocation <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  
  # Case 1: Running via Rscript with --file=
  if (length(file_arg) > 0) {
    file_path <- sub("^--file=", "", file_arg)
    return(dirname(normalizePath(file_path)))
  }
  
  # Case 2: Running in RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    context_path <- tryCatch(
      rstudioapi::getSourceEditorContext()$path,
      error = function(e) NULL
    )
    if (!is.null(context_path) && nzchar(context_path)) {
      return(dirname(normalizePath(context_path)))
    }
  }
  
  # Case 3: Plain R GUI or source() without a file — fallback
  message("Falling back to current working directory.")
  return(getwd())
}

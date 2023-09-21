list_racmoM_variables <- function(print = TRUE) {
  #' Return the available monthly RACMO variables
  #'
  #' @description Which monthly RACMO variables are available? It simply reads
  #'   the names of the files as created by [configure_polarcm()] (and thus
  #'   only accounts for the files present when that function was initially
  #'   called).
  #'
  #' @param print BINARY: Should the variables be printed out? Can be suppressed
  #'   for use inside other functions.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  token <- configure_polarcm()

  # Use available names
  varNames <- token$varNames$racmoM
  if (isTRUE(print)) {cat("\nracmoM Variables:\n"); print(varNames)}
  return(invisible(varNames))
}

list_racmoD_variables <- function(print = TRUE) {
  #' Return the available daily RACMO variables
  #'
  #' @description Which daily RACMO variables are available? It simply reads the
  #'   names of the files as created by [configure_polarcm()] (and thus only
  #'   accounts for the files present when that function was initially called).
  #' @inheritParams list_racmoM_variables
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  token <- configure_polarcm()

  # Use available names
  varNames <- token$varNames$racmoD
  if (isTRUE(print)) {cat("\nracmoD Variables:\n"); print(varNames)}
  return(invisible(varNames))
}

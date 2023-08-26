list_racmoM_variables <- function(print = TRUE) {
  #' Return the available monthly RACMO variables
  #'
  #' @description Which monthly RACMO variables are available? It simply reads
  #'   the names of the files as created by [configure_racmoR()] (and thus
  #'   only accounts for the files present when that function was initially
  #'   called).
  #'
  #' @param print BINARY: Should the variables be printed out? Can be suppressed
  #'   for use inside other functions.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Use available names
  racmoVars <- configure_racmoR()$racmoVars$racmoM |>
    as.data.frame() |>
    `colnames<-`("Monthly Variables")
  if (isTRUE(print)) print(racmoVars)
  return(invisible(racmoVars))
}

list_racmoD_variables <- function(print = TRUE) {
  #' Return the available daily RACMO variables
  #'
  #' @description Which daily RACMO variables are available? It simply reads the
  #'   names of the files as created by [configure_racmoR()] (and thus only
  #'   accounts for the files present when that function was initially called).
  #'
  #' @param print BINARY: Should the variables be printed out? Can be suppressed
  #'   for use inside other functions.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Use available names
  racmoVars <- configure_racmoR()$racmoVars$racmoD |>
    as.data.frame() |>
    `colnames<-`("Daily Variables")
  if (isTRUE(print)) print(racmoVars)
  return(invisible(racmoVars))
}

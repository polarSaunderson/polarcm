# Useful function here
# These are all taken from domR, v0.0.1.10

##
`%notIn%` <- function(x, y) {
  # Code -----------------------------------------------------------------------
  match(x, y, nomatch = 0) == 0
}

##
set_if_null <- function(x, defaultValue) {
  # Code -----------------------------------------------------------------------
    if (is.null(x)) x <- defaultValue
    return(x)
}

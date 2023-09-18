  # Useful function here
  `%notIn%` <- function(x, y) {    # copied from domR so no need to install it
    match(x, y, nomatch = 0) == 0
  }

  set_if_null <- function(x, defaultValue) {
      if (is.null(x)) x <- defaultValue
      return(x)
  }

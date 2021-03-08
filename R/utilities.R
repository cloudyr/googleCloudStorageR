#' base R safe rbind
#' 
#' Send in a list of data.fames with different column names
#' 
#' @return one data.frame
#' a safe rbind for variable length columns
#' @noRd
my_reduce_rbind <- function(x){
  classes <- lapply(x, inherits, what = "data.frame")
  stopifnot(all(unlist(classes)))
  
  # all possible names
  df_names <- Reduce(union, lapply(x, names))
  
  df_same_names <- lapply(x, function(y){
    missing_names <- setdiff(df_names,names(y))
    num_col <- length(missing_names)
    if(num_col > 0){
      missing_cols <- vapply(missing_names, function(i) NA, NA, USE.NAMES = TRUE)
      
      new_df <- data.frame(matrix(missing_cols, ncol = num_col))
      names(new_df) <- names(missing_cols)
      y <- cbind(y, new_df, row.names = NULL)
    }
    
    y[, df_names]
    
  })

  Reduce(rbind, df_same_names)
}

#' if argument is NULL, no line output
#'
#' @keywords internal
#' @noRd
cat0 <- function(prefix = "", x){
  if(!is.null(x)){
    cat(prefix, x, "\n")
  }
}

#' Javascript time to R time
#'
#' @keywords internal
#' @noRd
js_to_posix <- function(x){
  as.POSIXct(as.numeric(x) / 1000, origin = "1970-01-01")
}

#' taken from utils:::format.object_size
#'
#' @keywords internal
#' @noRd
format_object_size <- function (x, units = "b", ...){
  
  if(is.na(x)){
    return("File had no filesize")
  }
  
  units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb",
                              "Tb", "Pb", "B", "KB", "MB", "GB", "TB", "PB", "KiB",
                              "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))
  if (units == "auto")
    units <- if (x >= 1024^4)
      "Tb"
  else if (x >= 1024^3)
    "Gb"
  else if (x >= 1024^2)
    "Mb"
  else if (x >= 1024)
    "Kb"
  else "b"
  switch(units, 
         b = , B = paste(x, "bytes"), 
         Kb = , KB = paste(round(x/1024,1L), "Kb"), 
         Mb = , MB = paste(round(x/1024^2, 1L), "Mb"),
         Gb = , GB = paste(round(x/1024^3, 1L), "Gb"), 
         Tb = , TB = paste(round(x/1024^4, 1L), "Tb"), 
         Pb = , PB = paste(round(x/1024^5, 1L), "Pb"), 
         KiB = paste(round(x/1024, 1L), "KiB"),
         MiB = paste(round(x/1024^2, 1L), "MiB"), 
         GiB = paste(round(x/1024^3, 1L), "GiB"), 
         TiB = paste(round(x/1024^4, 1L), "TiB"),
         PiB = paste(round(x/1024^5, 1L), "PiB"), 
         EiB = paste(round(x/1024^6,1L), "EiB"), 
         ZiB = paste(round(x/1024^7, 1L), "ZiB"),
         YiB = paste(round(x/1024^8, 1L), "YiB"))
}

#' Timestamp to R date
#' @keywords internal
#' @noRd
timestamp_to_r <- function(t){
  as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S")
}


#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
#' @noRd
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
#' @noRd
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

#' Is this a try error?
#'
#' Utility to test errors
#'
#' @param test_me an object created with try()
#'
#' @return Boolean
#'
#' @keywords internal
#' @noRd
is.error <- function(test_me){
  inherits(test_me, "try-error")
}

#' Get the error message
#'
#' @param test_me an object that has failed is.error
#'
#' @return The error message
#'
#' @keywords internal
#' @noRd
error.message <- function(test_me){
  if(is.error(test_me)) attr(test_me, "condition")$message
}

#' Customer message log level
#'
#' @param ... The message(s)
#' @param level The severity
#'
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
#' @keywords internal
#' @noRd
#' @importFrom cli cli_h1 cli_div cli_alert_info cli_end
myMessage <- function(..., level = 2){
  
  compare_level <- getOption("googleAuthR.verbose")
  
  if(level >= compare_level){
    time <- paste(Sys.time(),">")
    mm <- paste(...)
    if(grepl("^#", mm)){
      cli::cli_h1(mm)
    } else {
      cli::cli_div(theme = list(span.time = list(color = "grey")))
      cli::cli_alert_info("{.time {time}} {mm}")
      cli::cli_end()
    }
    
  }
  
}

#' Substitute in a (nested) list
#'
#' @param template A template named list
#' @param replace_me A similar named list with different values to substitute
#'
#' @return The template with the values substituted.
#' @keywords internal
#' If replace_me has list names not in template, the value stays the same.
#' @noRd
substitute.list <- function(template, replace_me){

  ## remove possible NULL entries
  template <- rmNullObs(template)
  replace_me <- rmNullObs(replace_me)

  postwalk(template, function(x) replace.kv(x,replace_me))

}

#' Walk into a list
#'
#' If passed an object such as a nested list, will apply function
#'   on inner elements that are not lists.
#'
#' @param x what to check
#' @param func Function to apply if not a list
#' @keywords internal
#' @return the function acting on x or an inner element of x
#' @noRd
postwalk <- function(x,func){
  if(is.list(x)){
    func(lapply(x,postwalk,func))
  } else {
    func(x)
  }
}

#' Create a modified list
#'
#' @param template a (nested) list with elements to replace
#' @param replace a subset of template with same names but replacement values
#' @keywords internal
#' @return a list like template but with values replace from replace
#' @noRd
replace.kv <- function(template,replace) {
  if(!is.list(template)) return(template)

  i <- match(names(template),names(replace))
  w <- which(!is.na(i))

  replace(template,w,replace[i[w]])

}

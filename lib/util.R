#' Gets the value from a key-value pair table
#' @param key The value to search for
#' @param table The data fram eot consider for the seach
#' @param lookupCol The column index to lookup the key in (1 by default)
#' @param returnCol The column index of the value to be returned (2 by default)
#' @return Value from a lookup
util.lookup <- function(key, table, lookupCol = 1, returnCol = 2) {
  # I'm used to using VLOOKUP() in spreadsheets so I implemented my own kind of lookup
  return(table[match(key, table[, lookupCol]), returnCol])
}

#' Parses a comma separated string of numbers into a vector
#' "1,2,3,6" into c(1,2,3,6)
#' @param string String to be parsed
#' @return A vector of numbers
util.parseStringAsNumericVector <- function(string) {
  # lapply is basically arr.map() from javascript
  # returns c(1,2,3,6) from "1,2,3,6"
  unlist(
    lapply(
      strsplit(string, ","),
      as.numeric
    )
  )
}
util.lookup <- function(key, table, lookupCol = 1, returnCol = 2) {
  return(table[match(key, table[, lookupCol]), returnCol])
}

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
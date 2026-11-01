# BDA400 Assignment 5 - Crossunder
# Student: Parvana Babali
# Returns True/False strings as specified in the assignment pseudocode.

crossunder <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2)) {
    stop("arr1 and arr2 must be numeric vectors")
  }
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  if (length(arr1) == 0) return(character(0))

  crossunder_signals <- rep("False", length(arr1))
  crossunder_signals[1] <- "None"

  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      if (anyNA(c(arr1[i - 1], arr1[i], arr2[i - 1], arr2[i]))) {
        crossunder_signals[i] <- "False"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}

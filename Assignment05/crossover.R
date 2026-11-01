# BDA400 Assignment 5 - Crossover
# Student: Parvana Babali
# Returns Up, Down, or None for each position, matching the assignment pseudocode.

crossover <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2)) {
    stop("arr1 and arr2 must be numeric vectors")
  }
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  if (length(arr1) == 0) return(character(0))

  crossover_signals <- rep("None", length(arr1))

  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      if (anyNA(c(arr1[i - 1], arr1[i], arr2[i - 1], arr2[i]))) {
        crossover_signals[i] <- "None"
      } else if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}

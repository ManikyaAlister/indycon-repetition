rm(list = ls())
library(tidyverse)
library(here)
load(here("output/all-output.Rdata"))


# Which functional form fits the data best?  ------------------------------

extractLooic <- function(model_list, condition = NULL) {
  filtered_list <- if (!is.null(condition)) {
    model_list[sapply(model_list, function(x) x$cond == condition)]
  } else {
    model_list
  }
  
  results <- data.frame(
    #model = names(filtered_list),
    func = sapply(filtered_list, function(x) x$func),
    looic = sapply(filtered_list, function(x) x$looic),
    looic_se = sapply(filtered_list, function(x) x$looic_se)
  ) %>%
    mutate(
      rank = rank(looic)
    )
  
  results <- arrange(results, looic)
  
  results
}

extractLooic(all_output, condition = "independent")

extractLooic(all_output, condition = "dependent")




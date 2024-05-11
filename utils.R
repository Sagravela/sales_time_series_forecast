### UTILS

## DEPENDENCIES
# General
library(tidyverse)

# TS analysis
library(tsibble)
library(fabletools)
library(feasts)
library(fable)
library(skimr)

# Load and save data
library(arrow)
library(readxl)

# Multiprocess
library(furrr)
library(future)

# Miscelaneous
library(tictoc)
library(janitor)
library(crayon)
library(progressr)
library(glue)


## HELPER FUNCTIONS
# Helper function to plot an horizontal separator in console.
bar <- function() {
  cat("\n")
  cat(paste(rep("-", 70), collapse = ""))
  cat("\n")
}

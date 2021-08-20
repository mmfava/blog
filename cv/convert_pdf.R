#' ---
#' title: convert rmarkdown into pdf
#' author: Marília Melo Favalesso
#' date: 2021-03-24
#' ---

# packages
library(pagedown)
library(xaringan)
library(tidyverse)

# convert to pdf
setwd("C:/Users/mmfav/OneDrive/GitHub/site/cv")
purrr::map(dir(pattern = ".Rmd$"), chrome_print)

# end ---------------------------------------------------------------------

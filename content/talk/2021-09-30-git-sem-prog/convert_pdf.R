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
setwd("C:/Users/mmfav/OneDrive/GitHub/meu_site/content/talk/2021-09-30-git-sem-prog")

pagedown::chrome_print("slides.html", timeout = 120,  options = list(transferMode = "ReturnAsStream"))
# ---------------------------------end---------------------------------------- #


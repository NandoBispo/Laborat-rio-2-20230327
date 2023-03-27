# PACOTES ----
if (!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse, janitor, kableExtra, summarytools, 
               moments, ggthemes, patchwork, glue, ggpubr, 
               formattable, DT)

# https://curso-r.githud.io/zen-do-r/git-githud.html
gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_githud()
# _________________________________________________
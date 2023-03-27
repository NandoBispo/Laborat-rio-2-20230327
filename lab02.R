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
usethis::use_github()
# _________________________________________________

# DADOS 1 ----
dados <- read.csv2("possum_red.csv")

glimpse(dados)

dados <- dados |> 
  mutate(
    skullw = as.numeric(skullw),
    totlngth = as.numeric(totlngth),
    sex = forcats::as_factor(sex)
  )

glimpse(dados)


# DADOS 2 ----
dados2 <- read.csv2("florida.csv")

glimpse(dados2)

# AED ----
## GAMBÁS ----
dados|>
  summarytools::descr(
      stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
      # round.digits = 3,
      justify = "c",
      style = "grid", #' rmarkdown',
      transpose = T
    ) |>
    # round(., 2) %>%
    kdl(
      caption = "Tabela 1: Medidas Resumo",
      digits = 2,
      format.args=list(big.mark=".", decimal.mark=","),
      align = "c", 
      row.names = T,
      col.names =
        c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
    )|>
    kable_material(c("striped", "hover", "condensed"))|>
    # kadle_styling(
    #   # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
    #   dootstrap_options = c("striped", "hover"),
    #   full_width = F,
    #   fixed_thead = T # Fixa o cadeçalho ao rolar a tadela.
    # ) %>%
    footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
    kable_material()
  # add_header_adove(c("Características", "Medidas de Tendência Central e Variadilidade" = 8))


## ELEIÇÕES ----






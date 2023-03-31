# PACOTES ----
if (!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse, janitor, kableExtra, summarytools, 
               moments, ggthemes, patchwork, glue, ggpubr, 
               formattable, gridExtra)

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
  filter(sex == "f")|>select(sex)|>count()
  rename("Largura Crânio" = skullw, "Comprimento Total" = totlngth)|>
  summarytools::descr(
      stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
      # round.digits = 3,
      justify = "c",
      style = "grid", #' rmarkdown',
      transpose = T
    ) |>
    # round(., 2) %>%
    kbl(
      caption = "Tabela 1: Medidas Resumo para o sexo feminino.",
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
    # footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
    kable_material()
  # add_header_adove(c("Características", "Medidas de Tendência Central e Variadilidade" = 8))

dados|>
  filter(sex == "m")|> select(sex)|>count()
  rename("Largura Crânio" = skullw, "Comprimento Total" = totlngth)|>
  summarytools::descr(
      stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
      justify = "c", style = "grid", transpose = T
    ) |> tableGrob
    kbl(
      caption = "Tabela 2: Medidas Resumo para o sexo masculino",
      format.args=list(big.mark=".", decimal.mark=","),
      digits = 2, align = "c", 
      row.names = T,
      col.names =
        c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
    )|>
    kable_material(c("striped", "hover", "condensed"))|>
    kable_material()

### BoxPlot ----
b1 <- dados|>
      filter(sex == "m")|>#, skullw > 67)|> View()
  ggplot(aes(x = sex, y = skullw)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Medidas do sexo Masculino',
    x = "Sexo",
    y = "Largura do Crânio"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))+ 
    stat_summary(
      fun=mean, geom="point", shape=18, size=3)+
    annotate("text", x = "m", y = 69.5,
             label = "68,6",
             size=2, color="blue")+
  theme_bw()

b2 <- dados|>
      filter(sex == "f")|>#, skullw > 67)|> View()
  ggplot(aes(x = sex, y = skullw)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Medidas do sexo Feminino',
    x = "Sexo",
    y = "Largura do Crânio"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))+
    stat_summary(
      fun=mean, geom="point", shape=18, size=3)+
    annotate("text", x = "f", y = 68.5,
             label = "67,7",
             size=2, color="blue")+
    annotate("text", x = "f", y = 50.7,
             label = "51,5",
             size=2, color="blue")+
  theme_bw()

b3 <- dados|>
      filter(sex == "m")|>
  ggplot(aes(x = sex, y = totlngth)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    # title = 'Medidas do sexo Masculino',
    x = "Sexo",
    y = "Comprimento Total"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))+
  stat_summary(
    fun=mean, geom="point", shape=18, size=3)+
  theme_bw()

b4 <- dados|>
      filter(sex == "f")|>
  ggplot(aes(x = sex, y = totlngth)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    # title = 'Medidas do sexo Feminino',
    x = "Sexo",
    y = "Comprimento Total"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      dig.mark = ".",
      decimal.mark = ","))+
  stat_summary(
    fun=mean, geom="point", shape=18, size=3)+
  annotate("text", x = "f", y = 74,
           label = "75",
           size=2, color="blue")+
  theme_bw()

(b1+b2)/(b3+b4)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura 1: Comparação das medidas morfológicas por sexo",
    # sudtitle = "",
    # caption = "Fonte: Instituto Nacional de diabetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )

### Assimetria ----
tibble(
  c("Largura do Crânio", "Comprimento Total"),
  c(
    moments::skewness(subset(dados$skullw, dados[2]=="f")),
    moments::skewness(subset(dados$totlngth, dados[2]=="f")))) |> 
  kbl(
    caption = "Coeficientes de Assimetria das variáveis do sexo feminino",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c",
    col.names =
      c("Variável", "Coeficiente")
  ) |>
  kable_styling(
    # bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    bootstrap_options = c("striped", "hover"),
    full_width = F,
    # fixed_thead = T # Fixa o cabeçalho ao rolar a tabela.
  ) |>
  # footnote(general = "Fonte: Edre Coutinho") |>
  kable_material()

tibble(
  c("Largura do Crânio", "Comprimento Total"),
  c(
    moments::skewness(subset(dados$skullw, dados[2]=="m")),
    moments::skewness(subset(dados$totlngth, dados[2]=="m")))) |> 
  kbl(
    caption = "Coeficientes de Assimetria das variáveis do sexo masculino",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c",
    col.names =
      c("Variável", "Coeficiente")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = F,
  ) |>
  kable_material()

tibble(
  c("Largura do Crânio", "Comprimento Total"),
  c(
    moments::skewness(dados$skullw),
    moments::skewness(dados$totlngth))) |> 
  kbl(
    caption = "Coeficientes de Assimetria",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c",
    col.names =
      c("Variável", "Coeficiente")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = F,
  ) |>
  kable_material()

moments::skewness(subset(dados$skullw, dados[2]=="f"))
moments::skewness(subset(dados$skullw, dados[2]=="m"))
moments::skewness(dados$skullw)

### Curtose ----
tibble(
  c("Largura do Crânio", "Comprimento Total"),
  c(
    moments::kurtosis(dados$skullw),
    moments::kurtosis(dados$totlngth))) |> 
  kbl(
    caption = "Coeficientes de Curtose",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c",
    col.names =
      c("Variável", "Coeficiente")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = F,
  ) |>
  kable_material()


  
  
  
### Barras ----

dados|>
  # group_by(sex)|>
  count(sex)|>
  mutate(
    sex = forcats::fct_reorder(sex, n),
    sex = lvls_revalue(sex, c("Feminino", "Masculino")),
    pct = prop.table(n) * 100)|>
  ggplot(aes(x = sex, y = n, fill = sex))+
  geom_col() +
  geom_text(
    aes(
      label = glue::glue("{n} ({round(pct, 2)}%)")), vjust = 1.5)+
  # geom_label(aes(x = sex, y = n, label = n))+
  labs(
    # title = "Sexo",
    x = "Sexo", y = "Frequência"
  ) + theme_bw(base_size = 10) +
  theme(legend.position = "none")

### Setores ----

dados |>
  count(sex) |>
  mutate(
    sex = forcats::fct_reorder(sex, n),
    sex = lvls_revalue(sex, c("FEM", "MASC")), 
    pct = prop.table(n) * 100)|>
  ggplot() +
  aes(x = " ", y = pct) +
  geom_bar(stat="identity", fill = c("skyblue", "pink"),
           width = 0.65) +
  coord_polar("y", start=0) +
  labs(fill = "black") +
  theme_void() +
  geom_text(
    aes(
      label = glue::glue("{sex} ({round(pct, 2)}%)")),
    size = 3, colour = "black",
    position=position_stack(vjust=0.5))





### Histograma ----

h1 <- dados|>
  ggplot() +
  aes(x = skullw) +
  geom_histogram(
    # aes(y = after_stat(density)),
    binwidth = 1.5,
    fill = "lightblue",
    colour = "darkblue") +
  # geom_density(
  #   alpha = 0.2,
  #   fill = "blue",
  #   colour = "blue") +
  labs(
    # title = "Glicose",
    x = "Largura do crânio",
    y = "Densidade"
  )

h2 <- dados|>
  ggplot() +
  aes(x = totlngth) +
  geom_histogram(
    # aes(y = after_stat(density)),
    binwidth = 1.5,
    fill = "lightblue",
    colour = "darkblue") +
  # geom_density(
  #   alpha = 0.2,
  #   fill = "blue",
  #   colour = "blue") +
  labs(
    # title = "Glicose",
    x = "Comprimento total",
    y = "Densidade"
  )

(h1/h2)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura 1: Comparação das medidas morfológicas por sexo",
    # sudtitle = "",
    # caption = "Fonte: Instituto Nacional de diabetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )

### Densidade ----

d1 <- dados |>
  ggplot(aes(x = skullw)) +
  geom_density(
    # x = skullw, 
    fill = "lightblue",
    colour = "blue",
    alpha = 0.2) +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados$skullw),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  geom_vline(
    xintercept = quantile(dados$skullw, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    title = "",
    # subtitle = ,
    x = "Largura do crânio", # "Gastos",
    y = "Densidade"
  ) +
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  # scale_y_continuous(
  #   labels = scales::number_format(
  #     big.mark = ".",
  #     decimal.mark = ","
  #   )) +
  theme_bw()+
  theme(legend.position = "none")

d2 <- dados |>
  ggplot(aes(x = totlngth)) +
  geom_density(
    # x = skullw, 
    fill = "lightblue",
    colour = "blue",
    alpha = 0.2) +
  geom_vline(
    # show.legend = T,
    xintercept = mean(dados$totlngth),
    color = "red",
    linetype = "dashed" # "dotted"
  ) +
  geom_vline(
    xintercept = quantile(dados$totlngth, 0.5),
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    title = "",
    # subtitle = ,
    x = "Comprimento total", # "Gastos",
    y = "Densidade"
  ) +
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  # scale_y_continuous(
  #   labels = scales::number_format(
  #     big.mark = ".",
  #     decimal.mark = ","
  #   )) +
  theme_bw()+
  theme(legend.position = "none")

(d1/d2)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura 1: Comparação das medidas morfológicas por sexo",
    # sudtitle = "",
    # caption = "Fonte: Instituto Nacional de diabetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
    legend.position = "none"
  )



### Dispersão ----  
dados|>
  ggplot(aes(
    y = skullw, 
    x = totlngth, color = skullw)) +
  geom_point()+
  labs(
    title = 'Figura 5: Relação entre Comprimento Total e Largura do Crânio',
    x = 'Comprimento Total',
    y = 'Largura do Crânio',
    # caption = "Fonte: Instituto Nacional de diabetes e de Doenças Digestivas e Renais - EUA",
    # color = "N° de Gestações"
  )+
  # scale_x_continuous(
  #   labels = scales::number_format(
  #     big.mark = ".",
  #     decimal.mark = ","
  #   ))+
  # scale_y_continuous(
  #   labels = scales::number_format(
  #     big.mark = ".",
  #     decimal.mark = ","
  #   ))+
  theme_bw(base_size = 10)+
  theme(legend.position = "none")

# dados|>
  round(cor(dados$skullw, dados$totlngth),4)

  cor.test(dados$skullw, dados$totlngth)

dados|>
  filter(skullw < 63)|>
  ggplot(aes(
    y = skullw, 
    x = totlngth, color = skullw)) +
  geom_point()+
  labs(
    title = 'Figura 5: Relação entre Comprimento Total e Largura do Crânio',
    x = 'Comprimento Total',
    y = 'Largura do Crânio')+
  theme_bw(base_size = 10)+
  theme(legend.position = "none")

# dados|>

  round(cor(subset(dados$skullw, dados[3]<63), subset(dados$totlngth, dados[3]<63)),4)
  
cor.test(subset(dados$skullw, dados[3]<63), subset(dados$totlngth, dados[3]<63))
  
  cor.test(dados$skullw, dados$totlngth)
  
  
  
  
## ELEIÇÕES ----


#git-commit-message-4fc2e396f9.txt
#git push




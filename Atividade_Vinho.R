####Atividade de Ciencia De Dados####


library(tidyverse)

#Importando a base de dados

BDVinho <- read_csv("E:/Estatística/Ciência de Dados/Atividade Vinho/GitHub/Vamos Tomar um Vinho/BDVinho.csv")
view(BDVinho)

#Alterando o nome das colunas
dados<- janitor::clean_names( BDVinho)
View(dados)

#Investigação e contagem dos NA'S
dados |>
  is.na() |>
  colSums()

#Criação de uma variável categórica
dados1<- dados |>
  mutate(Classificacao=case_when(
    residual_sugar>25 ~ "Suave",
    residual_sugar<4 ~ "Seco",
    TRUE~ "Meio_Seco"
  ))|>
  view()

dados2<- dados1 |>
  mutate(Qualidade=case_when(
    quality>8 ~ "Excelente",
    quality<6.1 ~ "Abaixo da Média",
    TRUE~ "Médios"
  ))|>
  view()


#filtragem de linha
dados2 |>
  filter( Qualidade>8 & alcohol>9) |>
  View()

#análise descritiva
dados2 |>
  summarise_at(
    .vars = vars(alcohol, quality),
    .funs = list(media = mean, desvio = sd, mediana=median),
    na.rm = TRUE
  ) |> View()

#Gráfico
install.packages("esquisse",dependencies = TRUE )
library(esquisse)
esquisse::esquisser(dados2,viewer = "browser")

ggplot(dados) +
  aes(x = residual_sugar, y = alcohol) +
  geom_point(
    shape = "circle plus",
    size = 1.5,
    colour = "#8905A9"
  ) +
  labs(
    title = "Resíduo de Açúcar e Álcool após a Fermentação"
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold.italic",
                              hjust = 0.5)
  )


ggplot(dados2) +
  aes(x = residual_sugar, y = Qualidade, fill = Classificacao) +
  geom_violin(adjust = 1L, scale = "area") +
  scale_fill_hue(direction = 1) +
  labs(title = "Classificação dos vinhos ") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold.italic",
                              hjust = 0.5)
  )

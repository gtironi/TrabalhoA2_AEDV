library(ggplot2)
library(dplyr)
library(ggrepel)

theme_set(theme_classic())
theme_update(legend.position="top")
theme_update(plot.title = element_text(size = 12, face = "bold"))

df <- read.csv("nbaplayersdraft.csv", sep = ",")

dados <- df %>% rename("Ano" = "year",
                    "Rank" = "rank",
                    "Jogador" = "player",
                    "Anos_ativos" = "years_active",
                    "Jogos" = "games",
                    "Minutos_jogados" = "minutes_played",
                    "Pontos" = "points",
                    "Rebotes" = "total_rebounds",
                    "Assistência" = "assists",
                    "Média_de_Minutos_Jogados" = "average_minutes_played",
                    "Média_de_Assistências" = "average_minutes_played",
                    "Média_de_Pontos" = "points_per_game",
                    "Média_de_Rebotes" = "average_total_rebounds")

# ------------------------------------------------------------------------------
grafico_1 <- ggplot(dados, mapping = aes(group = Rank, x = Rank, y = Pontos)) +
  geom_boxplot() +
  coord_flip()

grafico_1


#BOXPLOT DA MÉDIA DE PONTOS POR JOGO DOS JOGADORES POR RANK COM LINHA DE TENDÊNCIA
grafico_pontos <- ggplot(dados, mapping= aes(x = Rank, y = Média_de_Pontos, group = Rank, label = Jogador)) +
  geom_boxplot() +
  geom_smooth(method = "loess", se=TRUE, aes(group=1)) +
  coord_flip() +
  geom_text_repel(size = 3)

grafico_pontos
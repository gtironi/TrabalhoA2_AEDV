library(ggplot2)
library(dplyr)
library(ggrepel)

theme_set(theme_classic())
theme_update(legend.position="top")
theme_update(plot.title = element_text(size = 12, face = "bold"))

df <- read.csv("nbaplayersdraft.csv", sep = ",")

df <- na.omit(df)

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
                    "Média_de_Assistências" = "average_assists",
                    "Média_de_Pontos" = "points_per_game",
                    "Média_de_Rebotes" = "average_total_rebounds")

# ------------------------------------------------------------------------------
#grafico_1 <- ggplot(dados, mapping = aes(group = Rank, x = Rank, y = Pontos)) +
#  geom_boxplot() +
#  coord_flip()

#grafico_1


#BOXPLOT DA MÉDIA DE PONTOS POR JOGO DOS JOGADORES POR RANK COM LINHA DE TENDÊNCIA
grafico_pontos <- ggplot(dados, mapping= aes(x = Rank, y = Média_de_Pontos, group = Rank, label = ifelse(Jogador %in% list("Stephen Curry", "Kobe Bryant", "Isaiah Thomas", "Nikola Jokić"), Jogador, ""))) +
  geom_boxplot(color = "gray50") +
  geom_smooth(method = "loess", se=FALSE, aes(group=1), size = 2, color = "#cf6143") +
  geom_text_repel(size = 3) +
  #theme_minimal() +
  scale_x_reverse(breaks = c(1, 20, 40, 60))

grafico_pontos
ggsave("grafico_pontos.png", plot = grafico_pontos)


#BOXPLOT DA MÉDIA DE MINUTOS JOGADOS DOS JOGADORES POR RANK COM LINHA DE TENDÊNCIA
grafico_minutos <- ggplot(dados, mapping= aes(x = Rank, y = Média_de_Minutos_Jogados, group = Rank, label = ifelse(Jogador %in% list("Anthony Bennett", "Isaiah Thomas"), Jogador, ""))) +
  geom_boxplot(color = "gray50") +
  geom_smooth(method = "loess", se=FALSE, aes(group=1), size = 2, color = "#cf6143") +
  geom_text_repel(size = 3, point.size = 4) +
  #theme_minimal() +
  scale_x_reverse(breaks = c(1, 20, 40, 60))

grafico_minutos
ggsave("grafico_minutos.png", plot = grafico_minutos)


#BOXPLOT DA MÉDIA DE ASSITÊNCIAS DOS JOGADORES POR RANK COM LINHA DE TENDÊNCIA
grafico_assistencias <- ggplot(dados, mapping= aes(x = Rank, y = Média_de_Assistências, group = Rank, label = ifelse(Jogador %in% list("Trae Young", "John Wall", "Nikola Jokić", "Chris Paul", "Isaiah Thomas"), Jogador, ""))) +
  geom_boxplot(color = "gray50") +
  geom_smooth(method = "loess", se=FALSE, aes(group=1), size = 2, color = "#cf6143") +
  geom_text_repel(size = 3) +
  #theme_minimal() +
  scale_x_reverse(breaks = c(1, 20, 40, 60))

grafico_assistencias
ggsave("grafico_assistencias.png", plot = grafico_assistencias)

#BOXPLOT DA MÉDIA DE REBOTES DOS JOGADORES POR RANK COM LINHA DE TENDÊNCIA
grafico_rebotes <- ggplot(dados, mapping= aes(x = Rank, y = Média_de_Rebotes, group = Rank, label = ifelse(Jogador %in% list("Andre Drummond", "Nikola Jokić"), Jogador, ""))) +
  geom_boxplot(color = "gray50") +
  geom_smooth(method = "loess", se=FALSE, aes(group=1), size = 2, color = "#cf6143") +
  geom_text_repel(size = 3) +
  #theme_minimal() +
  scale_x_reverse(breaks = c(1, 20, 40, 60))

grafico_rebotes
ggsave("grafico_rebotes.png", plot = grafico_rebotes)

#--------------------------------------------------------------------------------------

# TENTATIVA DE FAZER UM GRÁFICO MOSTRANDO A DIFERENÇA DE ESTATÍTICAS
# DOS FIRST PICKS COMPARADO COM OS OUTROS

df_comparacao_ranks <- read.csv("relação_entre_ranks.csv", sep = ",")

grafico_comparacao <- ggplot(df_comparacao_ranks, mapping = aes(group = Rank, x = Rank, y = Média, fill = Rank)) +
  geom_bar(stat='identity', position = 'dodge') +
  facet_grid(~Tipo_de_média)

grafico_comparacao
ggsave("grafico_comparacao.png", plot = grafico_comparacao)

#-------------------------------------------------------------------------------------
# ANÁLISE UNIDIMENSIONAL

# FREQUÊNCIA DOS TIMES
ggplot(dados, mapping = aes(x= team)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 45))

# tem muitos colleges pra fazer histograma de freqûencia
ggplot(dados, mapping = aes(x= college)) +
geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 45))

#
ggplot(dados, mapping = aes(y= Média_de_Pontos)) +
  geom_boxplot()
mean(dados$Média_de_Pontos, na.rm = TRUE)

ggplot(dados, mapping = aes(y= Média_de_Minutos_Jogados)) +
  geom_boxplot()
mean(dados$Média_de_Minutos_Jogados, na.rm = TRUE)

ggplot(dados, mapping = aes(y= Média_de_Rebotes)) +
  geom_boxplot()
mean(dados$Média_de_Rebotes, na.rm = TRUE)

ggplot(dados, mapping = aes(y= Média_de_Assistências)) +
  geom_boxplot()
mean(dados$Média_de_Assistências, na.rm = TRUE)

ggplot(dados, mapping = aes(x= Rank)) +
  geom_histogram(stat = "count")

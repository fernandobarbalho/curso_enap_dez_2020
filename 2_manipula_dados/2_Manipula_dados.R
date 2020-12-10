library(readr)

dados_metro_sudeste <- read_csv("2_manipula_dados/Data/Interface com RSiconfi.csv")

library(tidyverse)

#A bibilioteca tidyverse permite fazer diversos tipos de manipulação de dados, 
#tais como filtros, seleção de sub-conjunto de colunas, alteração de colunas, acréscimo de colunas, entre outras

#Filtrar dados

#o operador pipe %>% permite que se concatene diversas funções do tideverse. Indica para o R que a próxima instrução será feita a partir do conjunto de dados 
#já tratado anteriormente

dados_metro_sudeste %>%
  filter(SG_ENTE == 'RJ')

dados_metro_sudeste %>% 
  filter(SG_ENTE == "SP")  # Filtra para permanecer apenas os dados de São Paulo


dados_metro_sudeste %>% 
  filter(SG_ENTE == "SP") %>% # Filtra para permanecer apenas os dados de São Paulo
  filter(AN_EXERCICIO == 2019) %>%  #sobre o resultado anterior acrescenta um novo filtro. Dos municípios de SP ficarão apenas os dados de 2018
  filter(NR_PERIODO == 6)


filtro_1 <-
dados_metro_sudeste %>%
  filter(SG_ENTE == "SP",
         AN_EXERCICIO == 2019,
         NR_PERIODO == 6)

#usar a função mutate para acrescentar colunas
dados_metro_sudeste %>%
  mutate(gasto_por_habitante = VALUE / QT_HABITANTE)

#A função mutate pode ser usada para alterar  o valor de uma coluna

#seleciona apenas uma parte das colunas
temp<-
dados_metro_sudeste %>%
  mutate(gasto_por_habitante = VALUE / QT_HABITANTE) %>%
  select(NO_ENTE, VALUE, QT_HABITANTE, gasto_por_habitante)

# E agora live coding para group_by e estatísticas descritiva

temp2<-
dados_metro_sudeste %>%
  group_by(AN_EXERCICIO, SG_ENTE, funcao) %>%
  summarise(
    soma_valor = sum(VALUE)
  )

dados_infografico<-
dados_metro_sudeste %>%
  group_by(SG_ENTE, funcao) %>%
  summarise(
    soma_valor = sum(VALUE), #soma os valores
    media_valor = mean(VALUE), #calcula a média dos valores
    mediana_valor = median(VALUE), #calcula a mediana dos valores
    minimo_valor =min(VALUE), #mostra o valor mínimo
    máximo_valor = max(VALUE)#mostra o valor máximo
  )

write_csv2(dados_infografico, file = "dados_infograficos_metro.csv")



temp_3<-
dados_metro_sudeste %>%
  mutate(valor_habitante = VALUE/QT_HABITANTE) %>%
  group_by(SG_ENTE, funcao) %>%
  summarise(
    media_gasto_habitante = mean(valor_habitante),
    mediana_gasto_habitante = median(valor_habitante),
    max_gasto_habitante = max(valor_habitante),
    min_gasto_habitante = min(valor_habitante)
  )


summary(tir_municipio)


#itr_municipio<-
  
itr_municipio<-  
tir_municipio %>%
  mutate(media = rowMeans(.[,-1:-2], na.rm = TRUE)) %>%
  pivot_longer(!c(1:2,23),names_to = "ano", values_to = "valor" ) %>%
  mutate(valor = ifelse(is.na(valor), media,valor )) %>%
  separate(UF, sep = "-",into = c("UF", "Sigla_UF"))

itr_municipio <-
itr_municipio %>%
  filter(!is.na(Sigla_UF)) 

itr_municipio %>%
  filter(is.na(Sigla_UF)) 



#Exclui as linhas que não se referem a municípios

tir_municipio <- tir_municipio [-c(5526:5528),]


#Substituindo os valores ausentes pela média do município
for (i in 1:NROW(tir_municipio)) {
  
  tir_municipio[i,is.na(tir_municipio[i,])] <- mean(t(tir_municipio[i,3:21]), na.rm = TRUE)
  
}


#Subsitituindo os valores negativo pela média do município
for (i in 1:NROW(tir_municipio)) {
  
  tir_municipio[i,tir_municipio[i,]<0] <- 0
  tir_municipio[i,tir_municipio[i,]==0] <- mean(t(tir_municipio[i,3:21]), na.rm = TRUE)
  
}


library(tidyverse)

#Código para alterar o formato da base de dados e resolver o problema de dislexia 
#do programador

itr_municipio<- #problema de dislexia: criou a base corrigida com o novo nome
  tir_municipio %>% 
  gather(key = "ano", value = "valor_arrecadado", -(1:2)) %>%  #transpõe as colunas de ano para linha
  separate(UF, into= c("UF", "UF_sigla"), sep = "-") %>% #separa a sigla
  separate(municipio, into = c("municipio", "UF_municipio"), sep = " - ") %>% #separa a informação de estado
  select(-4) #seleciona todas as colunas, menos a quarta (UF_municipio) para evitar redundância





#Os dados não deixam claro se os valores são históricos ou atualizados por algum índice
#de inflação. O mais correto é tentar contato com o produtor do dado para buscar essa 
#resposta. Para o caso específico vamos usar o entedimento que se trata de dados
#atualizados

#Resolver o problema do ano 2000 que não se refere a dados de 12 meses.
#solução: assumir que há linearidade dos valores e dividir o valor de 2000 
#         para cada município por 5 (referente ao período entre agosto e dezembro)
#         e em seguida multiplicar por 12

itr_municipio<-
  itr_municipio %>% 
  mutate(valor_arrecadado= ifelse(ano==2000, valor_arrecadado/5*12, valor_arrecadado))
#a função mutate acima, cria novas variáveis ou altera valor de variáveis já existente 
#no caso, alterou a variavel valor arrecado para 5/12 do seu valor original para todas 
#as ocorrências do ano 2000


tir_municipio %>%
  
  
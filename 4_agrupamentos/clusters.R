library(cluster)
library(purrr)
set.seed(1)

#gera um formato não tidy para avaliar a evolução anual como parâmetro do cluster
itr_spread<-
  itr_municipio %>% 
  filter(ano>2000) %>%
  spread(ano, valor) #pivot_wider

timestamp()
#busca identificar qual o número ideal de grupos
sil_width <- map_dbl(2:4,  function(k){
  model<- pam(x = itr_spread[,4:21], k = k)
  model$silinfo$avg.width
})
timestamp()

#O número ideal de grupos é 2
model<- pam(x = itr_spread[,4:21], k = 2)

#valor da width média
model$silinfo$avg.width

#cria uma tabela 
itr_cluster<-
  itr_spread%>%
  mutate(cluster = model$clustering) 


#Faz o gráfico com escala logarítimica. É interessante em seguida ver o efeito do gráfico com escala linear


itr_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:4,25)) %>%
  ggplot(aes(x= ano, y= valor_arrecadado )) +
  geom_line(aes(group = municipio,  color= factor(cluster))) +
  scale_y_log10() 

ggsave(filename = "my_graph.png", plot = this_graph)
  



#Informa o número de elementos por agrupamento formado

itr_cluster %>%  
  group_by(cluster) %>%
  summarise(
    quantidade= n()
  ) %>%
  ggplot()  +
  geom_col(aes(x= quantidade, y=factor(cluster),  fill= factor(cluster))) 

#Informa o valor arrecadado por agrupamento formado

itr_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:5,25)) %>%  
  group_by(cluster) %>%
  summarise(
    valor_total= sum(valor_arrecadado)
  ) %>%
  ggplot()  +
  geom_col(aes(x=factor(cluster), y= valor_total, fill= factor(cluster)))  +
  coord_flip()

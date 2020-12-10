#Visualização de dados pode apoiar na sumarização da informação e até mesmo ajudar a verificar
#inconsistência de dados. Uma das formas de se verificar isso é através de gráficos que mostrem
#a distribuição dos dados em torno de uma variável.

#Num primeiro exemplo vamos visualizar como se distribui os dados ao longo dos anos.
#Primeiro vamos ver essa distribuição para os dados de itr


#Com escala linear
library(ggplot2)
#A bilbioteca ggplot2 trabalha com uma gramática de gráficos para gerar as figuras.
#A explicação dessa gramática virá pelo exemplo
itr_municipio %>% #indicação da fonte dos dados
  ggplot( #ggplot é a função que desenha o gráfico.  
    aes( #Na função aes indicamos os elementos que compõem um gráfico
      x=ano, #elemento x, normalmente eixo horizontal
      y=valor) #elemento y, normalmente eixo veritical
  )+
  geom_boxplot() #aqui indicamos a figura geométrica que fará a representação do gráfico.
#no caso, trata-se de um boxplot


#Com escala logartimica
#A bilbioteca ggplot2 trabalha com uma gramática de gráficos para gerar as figuras.
#A explicação dessa gramática virá pelo exemplo
itr_municipio %>% #indicação da fonte dos dados
  ggplot( #ggplot é a função que desenha o gráfico.  
    aes( #Na função aes indicamos os elementos que compõem um gráfico
      x=ano, #elemento x, normalmente eixo horizontal
      y=valor) #elemento y, normalmente eixo veritical
  )+
  scale_y_log10()+
  geom_boxplot() #aqui indicamos a figura geométrica que fará a representação do gráfico.
#no caso, trata-se de um boxplot


itr_municipio %>%
  filter(ano <= 2012) %>%
  slice_max (order_by = valor, n= 20) %>%
  mutate(municipio = reorder(municipio, valor ))%>%
  ggplot() +
  geom_col(aes(x=valor, y= municipio, fill = UF)) +
  facet_wrap(ano~.)
  



#Agora usando como referência receitas administradas pela RFB
rec_adm_rfb_mun %>% #indicação da fonte dos dados
  ggplot( #ggplot é a função que desenha o gráfico.  
    aes( #Na função aes indicamos os elementos que compõem um gráfico
      x=ano, #elemento x, normalmente eixo horizontal
      y=valor_arrecadado_administradas) #elemento y, normalmente eixo veritical
  )+
  geom_boxplot() #aqui indicamos a figura geométrica que fará a representação do gráfico.
#no caso, trata-se de um boxplot


#Uma outra análise que pode ser feita é pela comparação dos estados

itr_municipio %>% #indicação da fonte dos dados
  ggplot( #ggplot é a função que desenha o gráfico.  
    aes( #Na função aes indicamos os elementos que compõem um gráfico
      x=UF_sigla, #elemento x, normalmente eixo horizontal
      y=valor_arrecadado) #elemento y, normalmente eixo veritical
  )+
  geom_boxplot() #aqui indicamos a figura geométrica que fará a representação do gráfico.
#no caso, trata-se de um boxplot


#Agora usando como referência receitas administradas pela RFB
rec_adm_rfb_mun %>% #indicação da fonte dos dados
  filter(UF_sigla != "-",
         valor_arrecadado_administradas < median(valor_arrecadado_administradas)&
           valor_arrecadado_administradas >0 ) %>%
  ggplot( #ggplot é a função que desenha o gráfico.  
    aes( #Na função aes indicamos os elementos que compõem um gráfico
      x=UF_sigla, #elemento x, normalmente eixo horizontal
      y=valor_arrecadado_administradas) #elemento y, normalmente eixo veritical
  )+
  geom_violin() #aqui indicamos a figura geométrica que fará a representação do gráfico.
#no caso, trata-se de um boxplot

UFs_adm<-rec_adm_rfb_mun %>%
  distinct(UF_sigla)


rec_adm_rfb_mun %>%
  distinct(ano)


itr_municipio %>%
  mutate(tipo_receita = "ITR") %>%
  filter(ano>=2005) %>%
  select(6,5) %>%
  bind_rows(rec_adm_rfb_mun %>%
              filter(UF_sigla != "-") %>%
              mutate(tipo_receita= "RFB_admin",
                     valor_arrecadado =  valor_arrecadado_administradas) %>%
              select(5,6)) %>%
  ggplot(aes(x=tipo_receita, y= valor_arrecadado)) +
  geom_col() +
  coord_flip()
library(tidyverse)
library(tidytext)
library(wordcloud)


#Busca as 18000 últimas ocorrências de tweetes que aparecem a palava Maradona
maradona_tweets<-
  rtweet::search_tweets(q="maradona", n=18000,include_rts = FALSE)



###### EStatística de países e de idiomas

maradona_tweets %>%
  filter(!is.na(country)) %>%
  mutate(pais = country) %>%
  group_by(pais) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  mutate(pais= reorder(pais, quantidade)) %>%
  ggplot() +
  geom_col(aes(y=pais, x= quantidade),fill = "#74ACDF") + 
  theme_light() 



maradona_tweets%>%
  filter(lang != "und" )%>%
  mutate(idioma = lang) %>%
  group_by(idioma) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  mutate(idioma= reorder(idioma, quantidade)) %>%
  ggplot() +
  geom_col(aes(y=idioma, x= quantidade),fill = "#74ACDF") + 
  theme_light() 



################ tuítes em espanhol
df_maradona_es <- 
  maradona_tweets %>%
  filter(lang=='es') %>%
  select(status_id, text)

#Acrescenta aos stop_words uma série de palavras que não agregam valor na análise
stop_words_grupo<- c(stopwords::stopwords("es"), c("https","t.co","maradona", "diego", "armando", "si","q","2"))

analise_mensagem <- tibble(text=df_maradona_es$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(tibble(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 40, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


analise_twitter_secoes <- tibble(texto= df_maradona_es$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_grupo)

word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

set.seed(2016)

word_cors %>%
  filter(correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


################ tuítes em italiano
df_maradona_it <- 
  maradona_tweets %>%
  filter(lang=='it') %>%
  select(status_id, text)


stop_words_grupo<- c(stopwords::stopwords("it"), c("https","t.co","maradona", "diego", "armando","encuesta5futbol", c(1:5)))

analise_mensagem <- tibble(text=df_maradona_it$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(tibble(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 40, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


################ tuítes em inglês
df_maradona_en <- 
  maradona_tweets %>%
  filter(lang=='en') %>%
  select(status_id, text)


stop_words_grupo<- c(stopwords::stopwords("en"), c("https","t.co","maradona", "diego", "armando","encuesta5futbol", c(1:5)))

analise_mensagem <- tibble(text=df_maradona_en$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(tibble(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 40, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


################ tuítes em francês
df_maradona_fr <- 
  maradona_tweets %>%
  filter(lang=='fr') %>%
  select(status_id, text)


stop_words_grupo<- c(stopwords::stopwords("fr"), c("https","t.co","maradona", "diego", "armando","encuesta5futbol", "a","c'est","c`est", "c'est","c'est", "ça","j'ai","si","qu'il", "DidierRoustan", c(1:5)))

analise_mensagem <- tibble(text=df_maradona_fr$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(tibble(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 40, colors=brewer.pal(6,"Dark2"),random.order=FALSE))



################ tuítes em português
df_maradona_pt <- 
  maradona_tweets %>%
  filter(lang=='pt') %>%
  select(status_id, text)


stop_words_grupo<- c(stopwords::stopwords("pt"), c("https","t.co","maradona", "diego", "armando","encuesta5futbol", "a","c'est","c`est", "c'est","c'est", "ça","j'ai","si","qu'il", "miltonneves", "é","q", c(1:5)))

analise_mensagem <- tibble(text=df_maradona_pt$text) %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count(palavra, sort = TRUE) %>%
  ungroup()


analise_mensagem %>%
  anti_join(tibble(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 40, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

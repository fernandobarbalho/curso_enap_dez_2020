
library(rtweet)

#Pega os últimos 3200 posts da RFB
df_tl_rfb <- rtweet::get_timeline(user = "@ReceitaFederal", n = 3200)

#OU consome dados já previamente baixados
df_tl_rfb <- readRDS("/cloud/project/text_mining/data/df_tl_rfb.rds")

df_tl_rfb$ano_mes <- substr(df_tl_rfb$created_at,1,7)
df_tl_rfb$ano <- substr(df_tl_rfb$created_at,1,4)

save(list="df_tl_rfb", file = "df_tl_rfb.RData")
library(tidytext)

analise_mensagem <- df_tl_rfb %>%
  unnest_tokens(palavra,text,to_lower = TRUE) %>%
  count( ano_mes , palavra, sort = TRUE) %>%
  ungroup()

total_palavras <- analise_mensagem %>%
  group_by(ano_mes) %>%
  summarize(total=sum(n))

analise_mensagem <- inner_join(analise_mensagem, total_palavras)

analise_mensagem <- analise_mensagem %>%
  bind_tf_idf(palavra, ano_mes, n) %>%
  filter(tf_idf>=0.01)


stop_words_grupo <- unique( stopwords::stopwords("pt"))
stop_words_grupo <- c(stop_words_grupo,"8furu9tkzx","mxjapmblko", c('mídia','arquivo','oculto','https','http','q','tão','aí','10','1','tá',c('2013':'2020')))

analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  filter(n >= 45) %>%
  ungroup() %>%
  mutate(palavra = reorder(palavra, n)) %>%
  ggplot(aes(x=palavra, y=n)) +
  geom_segment( aes(x=palavra, xend=palavra, y=0, yend=n ) ) +
  geom_point(color = "orange" ) +
  theme_light(base_size = 12, base_family = "") +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    axis.ticks.length = unit(.99, "cm"),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") 

library(wordcloud)
analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 15, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

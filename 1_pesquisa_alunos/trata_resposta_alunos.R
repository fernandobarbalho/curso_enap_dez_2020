library(tidyverse)

unique(respostas_alunos$`Possui experiência com análise de dados?`)

respostas_alunos %>%
  filter(`Possui experiência com análise de dados?`== "Não") %>%
  select(`Quais desses softwares você já utilizou?`)
  

respostas_alunos %>%
  group_by(`Em que área você trabalha?`) %>%
  summarise(
    quantidade = n()
  ) %>%
  mutate(area = reorder(`Em que área você trabalha?`,quantidade)) %>%
  ggplot() +
  geom_col(aes(y=area, x= quantidade))

unlist(str_split("Excel;PowerBI;Qlik;R;Java;Calc", pattern = ";"))

fab<-
respostas_alunos %>%
  separate_rows (`Quais desses softwares você já utilizou?`, sep=  ";")


write

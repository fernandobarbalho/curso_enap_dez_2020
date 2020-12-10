#script para importação e tratamento de dados de ITR por município
#Essa linha e a linha de cima são comentários. Todo comentário começa com # 
#(cont) e não são reconhecidos pelo R

#O arquivo que vamos trabalhar está disponível no link indicado abaixo
#http://receita.economia.gov.br/dados/receitadata/arrecadacao/copy_of_arrecadacao-das-receitas-administradas-pela-rfb-por-municipio/arrecadacao-do-itr-por-municipio/arrecadacao-do-itr-por-municipio-e-uf-2000-a-2018.xlsx

#O comando abaixo atribui o endereço do arquivo para a variável endereco
endereco<-"http://receita.economia.gov.br/dados/receitadata/arrecadacao/copy_of_arrecadacao-das-receitas-administradas-pela-rfb-por-municipio/arrecadacao-do-itr-por-municipio/arrecadacao-do-itr-por-municipio-e-uf-2000-a-2019.xlsx"

#A linha abaixo faz o download do arquivo para o diretório corrente 


download.file(url = endereco, #url do arquivo de dado aberto vai ser baixado
              destfile = "tir_municipio.xlsx", #nome do arquivo destino
              mode = "wb") #indica ao R que trata-se de arquivo binário


#pode ser que seja interessante mudar o diretorio para uma pasta que centralize os dados

#Biblioteca para manipulação de planilha xlsx
library(readxl)
#comando para ler arquivo excel

tir_municipio <- read_excel("tir_municipio.xlsx", 
                            skip = 7)#Desconsidera as 7 primeiras linhas

tir_municipio <- read_excel("tir_municipio.xlsx",#nome do arquivo a ser lido 
                            skip = 8)#Desconsidera as 8 primeiras linhas

#nomes das variaveis
names(tir_municipio)

names(tir_municipio)[3]

filhos_d_marta<- c("Alexandre", "Fernando","Adriano")

estados_ne <-c("MA","PI","CE","RN","PB","PE","AL","SE","BA")




estados_ne[c(1,9)]

filhos_d_marta[2:3]



#alterando o nome da primeira coluna
names(tir_municipio)[1]<-"UF"
#alterando o nome da segunda coluna
names(tir_municipio)[2]<-"municipio"

names(tir_municipio)

tir_municipio$UF


unique(tir_municipio$UF)

saveRDS(dados_metro_sudeste, file="dados_metro_sudeste.rds")

a_ano <- 1996

paste0("http://repositorio.dados.gov.br/segrt/pensionistas/", a_ano, ".zip")


arquivo_lai_FGTS_SP_202009 <- read_delim("data_fgts/arquivo_lai_FGTS_SP_202009.csv", 
                                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                                         trim_ws = TRUE)

summary(arquivo_lai_FGTS_SP_202009)

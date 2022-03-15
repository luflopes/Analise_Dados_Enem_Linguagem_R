# Análise de Dados do ENEM 2017, 2018 e 2019

# Esta análise tem por objetivo fornecer insigths que ajudem a direcionar
# a tomada de decisão de uma instituição filantrópica que deseja investir 
# USD 10 por aluno/mês, durante 12 meses, para as turmas do ensino médio 
# de escolas em municípios carentes. Esta instituição deverá investir em 
# no máximo 100 escolas.

# Definindo o diretório de trabalho

setwd('C:/FCD/BigDataRAzure/Projetos')

# importando as bibliotecas para análise de dados

library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(sf)
library(tidyr, include.only = 'replace_na')
library(reshape2, include.only = 'melt')
library(stringr, include.only = 'str_replace')
library(rjson, include.only = 'fromJSON')

# Pergunta 2 - Quais dados (arquivos) você utilizou, onde e como os coletou?

# Os arquivos com os microdados do ENEM possuem cerca de 3 a 4 GB de tamanho, o que torna sua manipulação
# complexa em termos de tempo de processamento. Para contornar esta limitação, foram carregadas somente as
# colunas com as quais iremos trabalhar, reduzindo, assim, o tempo de processamento.

# Os dados a serem utilizados foram obtidos do portal do Inep:
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem

# importando o arquivo MICRODADOS_ENEM_2017.csv

dados_enem_2017 <- fread(file='C:/Users/lucaa/Documents/DataScienceProjects/DesafioAnaliseEnem/DADOS/MICRODADOS_ENEM_2017.csv',
                    sep=';',
                    select=c('NU_INSCRICAO', 'NU_ANO','NO_MUNICIPIO_RESIDENCIA',
                          'NO_MUNICIPIO_NASCIMENTO', 'SG_UF_NASCIMENTO',
                          'NO_MUNICIPIO_ESC', 'CO_ESCOLA','SG_UF_ESC',
                          'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC',
                          'NU_NOTA_MT','NU_NOTA_REDACAO', 'Q006'))
                    

# importando o arquivo MICRODADOS_ENEM_2018.csv

dados_enem_2018 <- fread(file='C:/Users/lucaa/Documents/DataScienceProjects/DesafioAnaliseEnem/DADOS/MICRODADOS_ENEM_2018.csv',
                         sep=';',
                         select=c('NU_INSCRICAO', 'NU_ANO','NO_MUNICIPIO_RESIDENCIA',
                                  'NO_MUNICIPIO_NASCIMENTO', 'SG_UF_NASCIMENTO',
                                  'NO_MUNICIPIO_ESC', 'CO_ESCOLA','SG_UF_ESC',
                                  'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC',
                                  'NU_NOTA_MT','NU_NOTA_REDACAO', 'Q006'))


# importando o arquivo MICRODADOS_ENEM_2019.csv

dados_enem_2019 <- fread(file='C:/Users/lucaa/Documents/DataScienceProjects/DesafioAnaliseEnem/DADOS/MICRODADOS_ENEM_2019.csv',
                         sep=';',
                         select=c('NU_INSCRICAO', 'NU_ANO','NO_MUNICIPIO_RESIDENCIA',
                                  'NO_MUNICIPIO_NASCIMENTO', 'SG_UF_NASCIMENTO',
                                  'NO_MUNICIPIO_ESC', 'CO_ESCOLA','SG_UF_ESC',
                                  'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_LC',
                                  'NU_NOTA_MT','NU_NOTA_REDACAO', 'Q006')
)

# Concatenando as tabelas das edições 2017, 2018 e 2019 em apenas uma tabela.

dados_enem_2017_to_2019 <- rbind(dados_enem_2017, dados_enem_2018, dados_enem_2019)

# Removendo os dataframes carregados da memória para reduzir a utilização de memória RAM

rm(dados_enem_2018, dados_enem_2018, dados_enem_2019)

# Salvando a tabela gerada pela união dos dados de 2017, 2018 e 2019 em formato csv.

write.csv(dados_enem_2017_to_2019,
          file='C:/Users/lucaa/Documents/DataScienceProjects/DesafioAnaliseEnem/DADOS/dados_enem_2017_to_2019.csv',
          sep=';')

# Importando o arquivo gerado com as edições do ENEM de 2017 até 2019.

dados_enem <- fread(file='C:/Users/lucaa/Documents/DataScienceProjects/DesafioAnaliseEnem/DADOS/dados_enem_2017_to_2019.csv')

#Pergunta 3 - Que tipo de tratamento você aplicou na base de dados?

#Resposta 3 - As técnicas que serão utilizadas no decorrer da análise incluem:
  
  # Remodelagem de dados;
  
  # Tratamento de valores missing;
  
  # Análise estatística descritiva;
  
  # Visualição de dados
  
  # Storytelling

# Pergunta 4 - Qual a proporção de missing nas bases e o que você fará com esses casos?
# Na sua opinião, quais os mais relevantes?

# Verificando o shape dos dados

dim(dados_enem)

# Verificando os dados

str(dados_enem)

# Adequando os tipos de dados

dados_enem$NU_ANO <- as.factor(dados_enem$NU_ANO)
dados_enem$NU_INSCRICAO <- as.character(dados_enem$NU_INSCRICAO)

# Visualizando o conjjunto de dados

View(head(dados_enem))

# Deletando a coluna de índice gerada durante a junção dos dados

dados_enem <- dados_enem %>%
              mutate(V1 = NULL)


# Verificando a quantidade de valores NA e sua proporção no total de registros

na_data <- sapply(dados_enem, function(x) sum(is.na(x)))
na_data <- data.frame(na_data)
na_data

# Filtrando as colunas que apresentam valores missing

na_data <- na_data %>%
            filter(na_data > 0) %>%
            mutate(perc_na = round((na_data / sum(na_data))*100, 2))
  
# Calculando a porcentagem de valores missing 

na_data$colunas <- row.names(na_data)
row.names(na_data) <- c(1:length(na_data$colunas))
na_data

# Gerando gráfico da porcentagem de valores NA por coluna

ggplot(na_data, aes(x = perc_na, y = colunas,))+
  geom_col(aes(xmin = 0, xmax=100 , fill = colunas),
           width = 0.9) + 
  geom_label(
    label=paste(na_data$perc_na, '%'), 
    nudge_x = -1.3, nudge_y = 0) +
labs(
  title = '\n Porcentagem de valores missing por coluna \n',
  y = 'Colunas', x = 'Porcentagem de valores missing') + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                     panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.line = element_line(colour = "black"))

# Removendo os dados missing da coluna "CO_ESCOLA"

dados_enem <- dados_enem %>%
            filter(CO_ESCOLA != is.na(CO_ESCOLA))



# Substituindo os valores missing da coluna "NO_MUNICIPIO_ESC" 
# pelos valores da coluna "NO_MUNICIPIO_NASCIMENTO"


vetor_NO <- list(dados_enem$NO_MUNICIPIO_ESC,dados_enem$NO_MUNICIPIO_NASCIMENTO)
vetor_NO <- coalesce(!!!vetor_NO)
sum(is.na(vetor_NO))
dados_enem <- dados_enem %>%
  mutate(NO_MUNICIPIO_ESC = vetor_NO, NO_MUNICIPIO_NASCIMENTO = NULL)

rm(vetor_NO)

# Substituindo os valores missing da coluna "SG_UF_ESC"
# pelos valores da coluna "SG_UF_NASCIMENTO" uma vez que não será 
# possível determinar a UF se ele não estiver presente em nenhuma
# das duas colunas utilizadas

vetor_SG <- list(dados_enem$SG_UF_ESC,dados_enem$SG_UF_NASCIMENTO)
vetor_SG <- coalesce(!!!vetor_SG)
sum(is.na(vetor_SG))
dados_enem <- dados_enem %>%
  mutate(SG_UF_ESC = vetor_SG, SG_UF_NASCIMENTO = NULL)

rm(vetor_SG)

# Substituindo os valores missing das colunas de nota por 0 (Zero)
# Aqui entendemos que os valores NA representam participantes que 
# não foram realizar a prova.

dados_enem <- dados_enem %>%
    mutate(NU_NOTA_CN = replace_na(NU_NOTA_CN, 0),
           NU_NOTA_CH = replace_na(NU_NOTA_CH, 0),
           NU_NOTA_LC = replace_na(NU_NOTA_LC, 0),
           NU_NOTA_MT = replace_na(NU_NOTA_MT, 0),
           NU_NOTA_REDACAO = replace_na(NU_NOTA_REDACAO, 0))

# Verificando a quantidade de valores missing após o tratamento dos dados.

na_data2 <- sapply(dados_enem, function(x) sum(is.na(x)))
na_data2 <- data.frame(na_data2)
na_data2


# Pergunta 5 - Qual o número de inscritos por ano?
# Existe alguma tendência a ser observada nos dados?

qtd_insc_ano <-  dados_enem %>%
  select(NU_ANO, NU_INSCRICAO) %>%
  group_by(NU_ANO) %>%
  summarise(contagem = n())

colnames(qtd_insc_ano) <- c('Ano', 'Quantidade')


ggplot(qtd_insc_ano, aes(x = as.factor(Ano), y=Quantidade)) +
  geom_col(aes(ymin=0, ymax=max(qtd_insc_ano$Quantidade), fill=as.factor(Ano))) + 
  geom_label( aes(y=qtd_insc_ano$Quantidade),
    label= qtd_insc_ano$Quantidade, 
    nudge_x = 0, nudge_y = 1,
    check_overlap = T) +
  labs(
    title = '\n Quantidade de inscrições no ENEM por ano \n',
    y = 'Quantidade', x = 'Ano') + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"))


# Pergunta 5 - Quais as notas médias por ano de realização da prova?

# Remodelagem do conjunto de dados para possibilitar a análise de forma gráfica


View(head(dados_enem))

dados_enem <- melt(dados_enem,
                    id.vars = c("NU_INSCRICAO", "NU_ANO",
                                "NO_MUNICIPIO_RESIDENCIA", "NO_MUNICIPIO_ESC",
                                "CO_ESCOLA", "SG_UF_ESC",
                                "Q006"),
                    variable.name = "AREA_CON", 
                    value.name = "NOTA")

dim(dados_enem)
View(head(dados_enem))

# Remodelando os dados da coluna AREA_CONHECIMENTO

dados_enem$AREA_CON <- str_replace(dados_enem$AREA_CON, 'NU_NOTA_CN', 'Ciencias da Natureza' )
dados_enem$AREA_CON <- str_replace(dados_enem$AREA_CON, 'NU_NOTA_CH', 'Ciencias Humanas' )
dados_enem$AREA_CON <- str_replace(dados_enem$AREA_CON, 'NU_NOTA_LC', 'Linguagens e Codigos' )
dados_enem$AREA_CON <- str_replace(dados_enem$AREA_CON, 'NU_NOTA_MT', 'Matematica' )
dados_enem$AREA_CON <- str_replace(dados_enem$AREA_CON, 'NU_NOTA_REDACAO', 'Redação' )

# Criando o gráfico para visualização da média por ano e por Área do conhecimento.

notas_ano_area <- dados_enem %>%
                  select(NU_ANO, AREA_CON, NOTA) %>%
                  group_by(NU_ANO, AREA_CON) %>%
                  summarise(media_ano_area = mean(NOTA))


View(head(notas_ano_area, n = 10L))


ggplot(notas_ano_area, aes(x = NU_ANO, y=media_ano_area, fill=AREA_CON)) + # (1)
  
  geom_col(position = 'dodge') + # (2)
  
  geom_text(aes(label = round(media_ano_area, 1)),
            position = position_dodge(.9),  vjust = -.5, size=4) + # (3)
  
  labs(title = '\n Média de notas por ano e por área do conhecimento \n',
            y = 'Nota Média', x = 'Ano') + # (4)
  
  theme(plot.title = element_text(hjust = 0.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black")) # (6)


# Pergunta 6 - Qual a renda média dos participantes por estado?


# Dados de renda obtidos do dicinário de dados do ENEM
# Estes dados indicam a faixa de renda dos participantes do ENEM

Q006 <- c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q')

RENDA <- c(0, 937.00,1405.50,1874.00,2342.50,2811.00,3748.00,4685.00,5622.00,
           6559.00,7496.00,8433.00,9370.00,11244.00,14055.00,18740.00,18740.00)

temp <- tibble(Q006, RENDA)

dados_enem <- left_join (dados_enem , temp , by = c ( "Q006" = "Q006"))


# Calculando a média de notas por estado

media_estado <- dados_enem %>%
  select(SG_UF_ESC, NOTA, RENDA) %>%
  filter(SG_UF_ESC != "" & RENDA != is.na(NA) ) %>%
  group_by(SG_UF_ESC) %>%
  summarise(nota_media = mean(NOTA), renda_media = mean(RENDA)) %>%
  arrange(SG_UF_ESC)

media_estado

# Você pode entrar no link acima e baixar os mapas de interesse.
# Aqui, iremos baixar o arquivo br_unidades_da_federacao.zip.
# ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2017/Brasil/BR

# Vamos usar a função st_read() do pacote sf (simple features)

file <- 'C:/FCD/BigDataRAzure/Projetos/DesafioEnem/BRUFE250GC_SIR.shp'

br_uf <- st_read(file, stringsAsFactors = FALSE)

br_uf <- br_uf %>% arrange(NM_ESTADO)

# Vamos importar um conjunto de de pares de valores (key:value) do arquivo disponibilizado
# no kaggle com as siglas e nomes dos estados para fazer o link entre os nomes, notas e siglas dos estados

estados_brasil <- fromJSON(file = "C:/Users/lucaa/Documents/DataScienceProjects/DesafioAnaliseEnem/DADOS/brazil_geo.json")


# Obtendo o os pares (SIGLA : NOME DO ESTADO)

lista_estados <- list()

for (feature in estados_brasil$features){
  lista_estados[[feature$id]] = feature$properties$name
}

# Criando uma lista apenas com os nomes dos estados na ordem em que as siglas aparecem na tabela media_estado

lista_dos_estados <- c()

for (estado in media_estado$SG_UF_ESC){
  lista_dos_estados <-  append(x = lista_dos_estados, values = lista_estados[[estado]])
}


# Criando uma coluna com os nomes dos estados obtidos acima para a tabela media_estado

media_estado$NM_ESTADO <- toupper(lista_dos_estados)

media_estado %>% print(n = Inf)

# Fazendo um merge com as tabelas br_uf que contém as geometrias por estados
# e a tabela media_estado que possui as notas médias por estado

media_estado <- left_join (br_uf , media_estado , by = c ( "NM_ESTADO" = "NM_ESTADO"))


# Tendo todas as informações na tabela media_estado, vamos remover da mémória
# as variáveis que não usaremos mais

rm(lista_estados, lista_dos_estados, br_uf, estados_brasil, temp)

# Agora vamos construir um gráfico de polígonos (mapa) no ggplot2 com a
# renda média dos participantes estado

ggplot(data = media_estado, aes(fill = renda_media)) +
  geom_sf() +
  labs(
    title = "Renda Média dos Participantes do ENEM por Estado do Brasil ",
    subtitle = "Referente aos anos de 2017, 2018 e 2019",
    fill = "renda_media"
  ) +
  # A partir de agora são só ajustes do gráfico
  theme(
    legend.position = "right",
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )  +
  scale_fill_viridis_c() +
  geom_sf_text(aes(label = paste(SG_UF_ESC,'\n' ,round(renda_media, 0))),
               label.padding = unit(0.5, "mm"), size = 3, colour = "#E0FFFF")


# Pergunta 8 - Qual a nota média por estado? 

# Agora vamos construir um gráfico de polígonos (mapa) no ggplot2 com a nota média por estado

ggplot(data = media_estado, aes(fill = nota_media)) +
  geom_sf() +
  labs(
    title = "Nota Média no ENEM por Estado do Brasil ",
    subtitle = "Referente aos anos de 2017, 2018 e 2019",
    fill = "nota_media"
  ) +
  # A partir de agora são só ajustes do gráfico
  theme(
    legend.position = "right",
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )  +
  scale_fill_viridis_c() +
  geom_sf_text(aes(label = paste(SG_UF_ESC,'\n' ,round(nota_media, 0))),
               label.padding = unit(0.5, "mm"), size = 3, colour = "#E0FFFF")

# Salvando o gráfico de mapa

ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE, ...)


# Existe alguma relação entre a renda média e a nota média por estado?

  

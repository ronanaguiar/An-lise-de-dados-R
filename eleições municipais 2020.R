install.packages("tidyverse")
library(dplyr)
library(tidyverse)
#Carregando os dados
votacao_sessao_2020_ES <- read_csv2("https://media.githubusercontent.com/media/ronanaguiar/An-lise-de-dados-R/main/votacao_secao_2020_ES.csv", locale=locale(encoding = "latin1" ))
sessoes_ES <- read_csv("https://github.com/ronanaguiar/An-lise-de-dados-R/raw/main/rpt_secoes_csv-202209011300.csv")
eleitor_ES <- read_csv2("https://github.com/ronanaguiar/An-lise-de-dados-R/raw/main/perfil_eleitor_secao_ATUAL_ES.csv", locale=locale(encoding = "latin1" ))

#Descrição dos dados de votação por sessão no ES
head(votacao_sessao_2020_ES)
dim(votacao_sessao_2020_ES)
summary(votacao_sessao_2020_ES)
glimpse(votacao_sessao_2020_ES)

hist(votacao_sessao_2020_ES$QT_VOTOS) 

summary(votacao_sessao_2020_ES$QT_VOTOS)

boxplot(votacao_sessao_2020_ES$QT_VOTOS)

sd(votacao_sessao_2020_ES$QT_VOTOS)

colSums(is.na(votacao_sessao_2020_ES))

str(votacao_sessao_2020_ES)

prop.table(table(is.na(votacao_sessao_2020_ES)))

votacao_sessao_2020_ES$DT_GERACAO <- as.Date(votacao_sessao_2020_ES$DT_GERACAO)
glimpse(votacao_sessao_2020_ES)

#limpando os dados de votação por sessão
votacao <- votacao_sessao_2020_ES %>%
            select(ANO_ELEICAO, DS_ELEICAO,
                   NM_MUNICIPIO, NR_SECAO,
                   NM_VOTAVEL, NR_VOTAVEL,
                   QT_VOTOS, NR_LOCAL_VOTACAO) %>%
            filter(NM_MUNICIPIO == "VITÓRIA", 
                   NM_VOTAVEL == "Partido Socialismo e Liberdade" |
                   NM_VOTAVEL == "CAMILA COSTA VALADÃO" )%>%
            arrange(desc(QT_VOTOS))
head(votacao)
tail(votacao)
glimpse(votacao)
unique(votacao$NM_VOTAVEL) 
unique(votacao$NR_SECAO)

#Quantidade de votos em legenda e na candidata
qtVotos <- votacao %>% group_by(NM_VOTAVEL) %>%
          summarise(qtVotosCandxLeg = sum(QT_VOTOS))
qtVotos
hist(votacao$QT_VOTOS)

#resumo dados de votação por sessão e quantidade de votos

resumoVotacao <- votacao %>%
          select(NR_SECAO,QT_VOTOS)
resumoVotacao

#sessões eleitorais
glimpse(sessoes_ES)
head(sessoes_ES)
sessoes <- sessoes_ES %>%
          select(nom_localidade,num_local, nom_local,
                 des_endereco, nom_bairro, num_secao,
                 qtd_aptos) %>%
          filter(nom_localidade == "VITÓRIA")
glimpse(sessoes)
resumoSessoes <- sessoes %>%
                select(num_local, nom_local,des_endereco,
                       nom_bairro, num_secao, qtd_aptos)
resumoSessoes
glimpse(resumoSessoes)
unique(resumoSessoes$num_secao)
str(resumoSessoes)
str(resumoVotacao)

#quantidade de votos por local de votação
localVotaçao <- resumoSessoes %>% 
  inner_join(resumoVotacao, by = c('num_secao' = 'NR_SECAO')) %>%
  arrange(desc(QT_VOTOS))
localVotaçao

#votos por bairro
votosBairro <- localVotaçao %>%
                group_by(nom_bairro)%>%
                summarise(QT_VOTOS = sum(QT_VOTOS), QT_APTOS = sum(qtd_aptos))%>%
                mutate(PERC_VOTOS = QT_VOTOS/QT_APTOS*100) %>%
                arrange(desc(PERC_VOTOS))
votosBairro
print(votosBairro, n = 48)
summary(votosBairro)
localVotaçao
view(votosBairro)

#Perfil do Eleitor
#Analisando os dados
str(eleitor_ES)
unique(eleitor_ES$ANO_ELEICAO)

#excluindo variável ANO_ELEICAO que só possui um valor 9999
eleitor_ES <- eleitor_ES %>% select(- ANO_ELEICAO) 

#limpando os dados, retirando variáveis desnecessárias e selecionando dados de Vitória
eleitor <- eleitor_ES %>%
          select(CD_MUNICIPIO, NM_MUNICIPIO,
                 NR_SECAO, CD_GENERO, DS_GENERO,
                 CD_ESTADO_CIVIL, DS_ESTADO_CIVIL,
                 CD_FAIXA_ETARIA, DS_FAIXA_ETARIA,
                 CD_GRAU_ESCOLARIDADE, DS_GRAU_ESCOLARIDADE,
                 QT_ELEITORES_PERFIL, QT_ELEITORES_DEFICIENCIA,
                 QT_ELEITORES_INC_NM_SOCIAL) %>%
          filter(NM_MUNICIPIO=="VITÓRIA")
head(eleitor)
glimpse(eleitor)

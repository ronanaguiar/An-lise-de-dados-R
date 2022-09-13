library(dplyr)
viagens <- read.csv("Rstudio/2019_Viagem.csv", 
                    sep = ";", 
                    dec = ",", 
                    fileEncoding = "latin1")
head(viagens)
View(viagens)
dim(viagens)
summary(viagens)

summary(viagens$Valor.passagens)

glimpse(viagens)

hist(viagens$Valor.passagens)

#Author Sabrina Barbosa
#01-05-2022
#Universidade Federal do Rio Grande do Norte
#Departamento de Demografia - CCET

library(pacman)
p_load(tidyverse,data.table, vroom,readr, readxl)
dadosidacre=read_excel("tabela1552 - Acre - Questão 1 - simplificado.xlsx")

numeradorwhip=function(tabela, coluna, posicoes){
  return(sum(tabela[posicoes,coluna]))
}

denominadorwhip=function(idades) {
  return(sum(idades)/5)
}

whippleindex=function(tabela, coluna, idades){
  listaposid=which(tabela$Idade %in% idades)
  numerador=numeradorwhip(tabela, coluna, listaposid)
  denominador=denominadorwhip(tabela[,coluna])
  resultwi=(numerador/denominador)*100
  return(resultwi)
}


listaidalv=c("25 anos",
             "30 anos",
             "35 anos",
             "40 anos",
             "45 anos",
             "50 anos",
             "55 anos",
             "60 anos",
             "65 anos",
             "70 anos",
             "75 anos",
             "80 anos",
             "85 anos")

whippleindex(dadosidacre, "Mulher",listaidalv)
whippleindex(dadosidacre, "Homem",listaidalv)
whippleindex(dadosidacre, "Total",listaidalv)
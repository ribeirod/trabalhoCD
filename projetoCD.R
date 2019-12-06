install.packages("visdat")
library(psych)
library(tidyr)
#library(ggplot2)
library(purrr)
library(gclus)
library(corrplot)
require(visdat)

# DADOS ORIGINAIS
# Obs.: O campo Date será ignorado.
x <- read.csv("/home/danielle/Desktop/Mathematics/ICMC/conteudo/8o-Semestre/Introdução à Ciência de Dados-SCC0275/trabalho/AED/eighthr.csv", dec=",", header=FALSE,stringsAsFactors=FALSE) 
colnames(x) <- c("Date", "WSR0", "WSR1", "WSR2", "WSR3", "WSR4", "WSR5", "WSR6", 
                 "WSR7", "WSR8", "WSR9", "WSR10", "WSR11", "WSR12", "WSR13", 
                 "WSR14", "WSR15", "WSR16", "WSR17", "WSR18", "WSR19", "WSR20", 
                 "WSR21", "WSR22", "WSR23", "WSR_PK", "WSR_AV", "T0", "T1", "T2", 
                 "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", 
                 "T13", "T14", "T15", "T16", "T17", "T18", "T19", "T20", "T21", 
                 "T22", "T23", "T_PK", "T_AV", "T85", "RH85", "U85", "V85", "HT85", 
                 "T70", "RH70", "U70", "V70", "HT70", "T50", "RH50", "U50", "V50", 
                 "HT50", "KI", "TT", "SLP", "SLP_", "Precp", "Classification")
x
nroColunas = ncol(x)
x <- x[,2:nroColunas]

nroLinhas = nrow(x)
nroColunas = ncol(x)

# AJUSTANDO OS DADOS: RETIRANDO '?' E NA
#substituindo valores faltantes pela média entre as observação não vazias anterior e a próxima
for(l in 1:nroLinhas){
  for(c in 1:nroColunas){
    if(x[l,c] == '?' || is.na(x[l,c])){
      lAuxM = l+1
      #enquanto não encontrar uma próxima observação sem '?' "continua a procurar à frente"
      while(x[lAuxM, c] == '?' || is.na(x[lAuxM,c])){
        lAuxM = lAuxM + 1
      }
      #enquanto não encontrar uma observação anterior sem '?' "continua a procurar atrás"
      lAuxm = l-1
      while(x[lAuxm, c] == '?' || is.na(x[lAuxm,c])){
        lAuxm = lAuxm - 1
      }
      #substitui '?' ou NA pela média (demos sorte! sabemos que na primeira 
      #observação não temos '?', logo, podemos obter lAuxm = lAuxm - 1)
      v <- c(x[lAuxm, c], x[lAuxM, c])
      options(digits = 2)
      v <- as.numeric(as.character(v))
      x[l, c] = mean(v)
    }
  }
}

# Convertendo tudo para numérico
x <- mapply(x, FUN=as.numeric)
x <- matrix(data=x, ncol=nroColunas, nrow=nroLinhas)
colnames(x) <- c("WSR0", "WSR1", "WSR2", "WSR3", "WSR4", "WSR5", "WSR6", 
                 "WSR7", "WSR8", "WSR9", "WSR10", "WSR11", "WSR12", "WSR13", 
                 "WSR14", "WSR15", "WSR16", "WSR17", "WSR18", "WSR19", "WSR20", 
                 "WSR21", "WSR22", "WSR23", "WSR_PK", "WSR_AV", "T0", "T1", "T2", 
                 "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", 
                 "T13", "T14", "T15", "T16", "T17", "T18", "T19", "T20", "T21", 
                 "T22", "T23", "T_PK", "T_AV", "T85", "RH85", "U85", "V85", "HT85", 
                 "T70", "RH70", "U70", "V70", "HT70", "T50", "RH50", "U50", "V50", 
                 "HT50", "KI", "TT", "SLP", "SLP_", "Precp", "Classification")

# ANÁLISE EXPLORATÓRIA
# Os dados estão estruturados no formato de tabela e os atributos são todos do tipo contínuo.

# Informações básicas sobre os atributos (talvez desnecessário)
summary(x[, 1:nroColunas-1])

#-> Plotando

# Covariâncias e correlações
#Como estamos analisando muitas variáveis, a covariância e a correlação podem 
#nos ajudar, de maneira rápido, a identificar variáveis redundantes, pois se duas 
#variáveis tem alta correlação pode ser que a informação de uma esteja presente 
#na outra, portanto, podemos remover uma das variáveis. Ainda assim, o 
#mais correto é fazer uma análise mais cuidadosa nesses casos e é o que faremos 
#realizando Decomposição Espectral via PCA (aplicar pca sobre as subamostras 
#explicadas/justificadas abaixo).

# Tudo junto
matCor <- cor(x[,1:nroColunas-1])
corrplot(matCor, method="color")

# 1º quadrante 
corrplot(matCor[1:26, 1:26], method="color")

# 2º quadrante 
corrplot(matCor[1:26, 27:53], method="color")

# 3º quadrante 
corrplot(matCor[1:26, 54:72], method="color")

# 4º quadrante 
corrplot(matCor[27:53, 1:26], method="color")

# 5º quadrante 
corrplot(matCor[27:53, 27:53], method="color")

# 6º quadrante 
corrplot(matCor[27:53, 54:72], method="color")

# 7º quadrante 
corrplot(matCor[54:72, 1:26], method="color")

# 8º quadrante 
corrplot(matCor[54:72, 27:53], method="color")

# 9º quadrante 
corrplot(matCor[54:72, 54:72], method="color")

# PRÉ-PROCESSAMENTO

# Balanceamento dos dados
# O desbalanceamento, em geral, causa problemas para a classificação. 
# Para resolver isso podemos utilizar abordagens de subamostragem ou 
# superamostragem. 

qtdClassZero = sum(x[,"Classification"] == 0)
qtdClassUm = sum(x[,"Classification"] == 1)
print(paste("Qtd de classificações '0':", qtdClassZero))
print(paste("Qtd de classificações '1':", qtdClassUm))

# Note que, de 2534 observações, somente 160 são da classe 1. Sendo assim, 
#a base de dados está MUITO desbalanceada. Precisaremos trabalhar com subamostrar
#balanceadas desse conjunto de dados.

# ATENÇÃO !!!
#1) O método de classificação que iremos utilizar necessita de atributos numéricos 
#no intervalo 0-1? Se sim, TEMOS QUE NORMALIZAR TODOS OS ATRIBUTOS. 
#2) O método utilizado pressupõe normalidade dos dados? Se sim, considerando
# que o log e a raíz quadrada aproximam os dados de uma normal, pode ser interessante 
#aplicar alguma dessas funções.
# Obs.: Nesse caso de usar método de classificação com resposta 0-1,
# se tivéssemos atributos não numéricos teríamos que removê-los, ou transformá-los 
#em numéricos, e fazer a normalização 0-1.



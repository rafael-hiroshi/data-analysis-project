library(plyr)
#install.packages('plyr')

df = read.csv('RDO_3_v2.csv')

### Geral
barplot(table(df$HORA), space = 0,
        ylab = "Quantidade", xlab="Horário",
        main = "Frequência de Crimes em São Paulo (RDO) por Hora",
        border="azure4", col="azure2")

barplot(table(df$PERIODO), space = 0,
        ylab = "Quantidade", xlab="Horário",
        main = "Frequência de Crimes em São Paulo (RDO) por Período",
        border="azure4", col="azure2")

dfSexo <- df[df$SEXO_PESSOA != 'NULL',]
tableSexo <- sort(table(dfSexo["SEXO_PESSOA"]), decreasing=TRUE)[1:3]
barplot(tableSexo, space = 0,
        ylab = "Quantidade", xlab="Horário", ylim=c(0, 300000),
        main = "Frequência de Crimes em São Paulo (RDO) por Gênero",
        border="azure4", col="azure2")

### Roubos
dfRoubo = df[df$RUBRICA == 'Roubo',]
barplot(table(dfRoubo$HORA), space = 0,
        ylab = "Quantidade", xlab="Horário",
        main = "Frequência de Roubos em São Paulo (RDO) por Hora",
        border="azure4", col="azure2")

barplot(table(dfRoubo$PERIODO), space = 0,
        ylab = "Quantidade", xlab="Horário",
        main = "Frequência de Roubos em São Paulo (RDO) por Período",
        border="azure4", col="azure2")

### Casos de estupro
dfEstupro = df[df$RUBRICA == 'Estupro',]
tableSexo <- sort(table(dfEstupro["SEXO_PESSOA"]), decreasing=TRUE)[1:3]
barplot(tableSexo, space = 0,
        ylab = "Quantidade", xlab="Gênero",
        main = "Frequência Casos de Estupros em São Paulo (RDO) por Sexo",
        border="azure4", col="azure2")

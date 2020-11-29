library(plyr)
#install.packages('plyr')

View(df)
View(select(df, HORA_OCORRENCIA_BO, hora))
df = read.csv('RDO_3_v2.csv')

### Geral
barplot(table(df$HORA), space = 0,
        ylab = "Quantidade de Crimes", xlab="Horário",
        main = "Frequência de Crimes em São Paulo (RDO) por Hora",
        border="azure4", col="azure2")

barplot(table(df$PERIODO), space = 0,
        ylab = "Quantidade de Crimes", xlab="Horário",
        main = "Frequência de Crimes em São Paulo (RDO) por Período",
        border="azure4", col="azure2")

dfSexo <- df[df$SEXO_PESSOA != 'NULL',]
sexo <- sort(table(dfSexo["SEXO_PESSOA"]), decreasing=TRUE)[1:3]
barplot(sexo, space = 0,
        ylab = "Quantidade de Crimes", xlab="Horário", ylim=c(0, 300000),
        main = "Frequência de Crimes em São Paulo (RDO) por Período",
        border="azure4", col="azure2")

### Roubos
dfRoubo = df[df$RUBRICA == 'Roubo',]
barplot(table(dfRoubo$HORA), space = 0,
        ylab = "Quantidade de Roubos", xlab="Horário",
        main = "Frequência de Roubos em São Paulo (RDO) por Hora",
        border="azure4", col="azure2")

barplot(table(dfRoubo$PERIODO), space = 0,
        ylab = "Quantidade de Roubos", xlab="Horário",
        main = "Frequência de Roubos em São Paulo (RDO) por Período",
        border="azure4", col="azure2")

### Casos de estupro
dfEstupro = df[df$RUBRICA == 'Estupro',]
sexo <- sort(table(dfEstupro["SEXO_PESSOA"]), decreasing=TRUE)[1:3]
barplot(sexo, space = 0,
        ylab = "Quantidade de Roubos", xlab="Horário",
        main = "Frequência de Roubos em São Paulo (RDO) por Hora",
        border="azure4", col="azure2")

## Reminder: Corrigir labels dos gráficos

  
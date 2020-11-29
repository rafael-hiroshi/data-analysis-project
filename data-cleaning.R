library(plyr)
#install.packages('plyr')

View(rdo_database)

rdo_database <- read.csv('RDO_3.csv')
rdo_database["ID_DELEGACIA"] = rdo_database[colnames(rdo_database)[1]]
rdo_database = rdo_database[!duplicated(rdo_database[c("NUM_BO", "ANO_BO", "ID_DELEGACIA")]), ]

rdo_database$RUBRICA <- as.character(rdo_database$RUBRICA)
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Furto qualificado (art. 155, §4o.)'] <- 'Furto qualificado'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Roubo (art. 157)'] <- 'Roubo'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Furto qualificado (art. 155, §4o.)'] <- 'Furto qualificado'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Lesão corporal (art. 129)'] <- 'Lesão corporal'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Lesão corporal (art. 129)'] <- 'Lesão corporal'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Furto (art. 155)'] <- 'Furto'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Lesão corporal (art 129 § 9º)'] <- 'Lesão corporal'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Roubo (art. 157)'] <- 'Roubo'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Furto (art. 155)'] <- 'Furto'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Homicídio simples (art. 121)'] <- 'Homicídio simples'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Lesão corporal culposa (art. 129. §6o.)'] <- 'Lesão corporal culposa'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Homicídio qualificado (art. 121, §2o.)'] <- 'Homicídio qualificado'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Lesão corporal culposa na direção de veículo automotor (Art. 303)'] <- 'Lesão corporal culposa na direção de veículo automotor'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Estupro'] <- 'Estupro'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Estupro (art.213)'] <- 'Estupro'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Estupro de vulneravel (art.217-A)'] <- 'Estupro de vulneravel'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Homicídio culposo na direção de veículo automotor (Art. 302)'] <- 'Homicídio culposo na direção de veículo automotor'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Lesão corporal de natureza GRAVE (art. 129, §1o.)'] <- 'Lesão corporal de natureza GRAVE'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Lesão corporal culposa na direção de veículo automotor (Art. 303)'] <- 'Lesão corporal culposa na direção de veículo automotor'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Estupro de vulneravel (art.217-A)'] <- 'Estupro de vulneravel'
rdo_database$RUBRICA[rdo_database$RUBRICA == "Lesão corporal  de natureza 'GRAVÍSSIMA' (art. 129, §2o.)"] <- "Lesão corporal  de natureza 'GRAVÍSSIMA'"
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Homicídio qualificado (art. 121, §2o.)'] <- 'Homicídio qualificado'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Homicídio culposo (art. 121, §3o.)'] <- 'Homicídio culposo'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Lesão corporal (art 129 § 9º)'] <- 'Lesão corporal'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Furto de coisa comum (art. 156)'] <- 'Furto'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Estupro (art.213)'] <- 'Estupro'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Lesão corporal culposa (art. 129. §6o.)'] <- 'Lesão corporal culposa'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'Lesão corporal seguida de morte (art. 129, §3o.)'] <- 'Lesão corporal seguida de morte'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Homicídio simples (art. 121)'] <- 'Homicídio simples'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Furto de coisa comum (art. 156)'] <- 'Furto'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Homicídio culposo na direção de veículo automotor (Art. 302)'] <- 'Homicídio culposo na direção de veículo automotor'
rdo_database$RUBRICA[rdo_database$RUBRICA == 'A.I.-Lesão corporal de natureza GRAVE (art. 129, §1o.)'] <- 'Lesão corporal de natureza GRAVE'
rdo_database$RUBRICA <- as.factor(rdo_database$RUBRICA)

cidades <- sort(table(rdo_database["CIDADE"]), decreasing=TRUE)
saoPaulo <- rownames(cidades)[1]
rdo_database <- rdo_database[rdo_database$CIDADE == saoPaulo,]

rdo_database$HORA <- strtoi(substr(rdo_database[['HORA_OCORRENCIA_BO']],0, 2), base=10L)

rdo_database$PERIODO[rdo_database$HORA >= 6 & rdo_database$HORA < 12] <-"06:00 ás 11:59"
rdo_database$PERIODO[rdo_database$HORA >= 12 & rdo_database$HORA < 18] <-"12:00 ás 17:59"
rdo_database$PERIODO[rdo_database$HORA >= 18 & rdo_database$HORA < 24] <-"18:00 ás 23:59"
rdo_database$PERIODO[rdo_database$HORA >= 0 & rdo_database$HORA < 6] <-"00:00 ás 05:59"
rdo_database$PERIODO[is.na(rdo_database$HORA)] <-"Indefinido"

rdo_database$PERIODO_DESCRICAO[rdo_database$HORA >= 6 & rdo_database$HORA < 12] <-"Matutino"
rdo_database$PERIODO_DESCRICAO[rdo_database$HORA >= 12 & rdo_database$HORA < 18] <-"Vespertino"
rdo_database$PERIODO_DESCRICAO[rdo_database$HORA >= 18 & rdo_database$HORA < 24] <-"Noturno"
rdo_database$PERIODO_DESCRICAO[rdo_database$HORA >= 0 & rdo_database$HORA < 6] <-"Madrugada"
rdo_database$PERIODO[is.na(rdo_database$HORA)] <-"Indefinido"

write.csv(rdo_database,"RDO_3_v2.csv", row.names = FALSE)



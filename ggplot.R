library(ggplot2)
library(plyr)
library(scales)

View(df)
df = read.csv('RDO_3_v2.csv')

text_theme <-   theme(plot.title = element_text(hjust = 0.5),
                      axis.title.x = element_text(size = 13),
                      axis.title.y = element_text(size = 13))

ggplot(df, aes(x = HORA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo por Hora (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = round(seq(min(df$HORA, na.rm = TRUE), max(df$HORA, na.rm = TRUE))))

ggplot(df, aes(x = HORA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo por Hora x Gênero (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = round(seq(min(df$HORA, na.rm = TRUE), max(df$HORA, na.rm = TRUE)))) +
  facet_wrap(~SEXO_PESSOA)

count(df$SEXO_PESSOA)

ggplot(df, aes(x = RUBRICA)) + 
  geom_bar() + coord_flip() + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo (RDO)") +
  labs(x = "Tipos de Crime", y = "Quantidade") +
  #scale_y_continuous(trans = 'log10') +
  text_theme

df_roubo = df[(df$RUBRICA == 'Roubo' | df$RUBRICA == 'Furto' |
                df$RUBRICA == 'Lesão corporal' | df$RUBRICA == 'Furto qualificado') 
              & (!is.na(df$SEXO_PESSOA)),]

ggplot(df_roubo, aes(x = PERIODO_DESCRICAO)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo por Período (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~RUBRICA)

ggplot(df_roubo, aes(x = RUBRICA)) + 
  geom_bar(na.rm = TRUE) +
  theme_linedraw() + 
  ggtitle("Frequência dos Principais Crimes em São Paulo por Gênero (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  facet_wrap(~SEXO_PESSOA)

df_sexo <- df[!is.na(df$SEXO_PESSOA) & !is.na(df$COR_CUTIS) , ]
ggplot(df_sexo, aes(x = COR_CUTIS)) + 
  geom_bar(na.rm = TRUE) +
  theme_linedraw() + 
  ggtitle("Frequência dos Principais Crimes em São Paulo por Gênero (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  facet_wrap(~SEXO_PESSOA)

ggplot(df_roubo, aes(x = HORA, fill=SEXO_PESSOA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo por Período (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~COR_CUTIS)

df_estupro <- df[(df$RUBRICA == 'Estupro' | df$RUBRICA == 'Estupro de vulneravel') & !is.na(df$SEXO_PESSOA) ,]
ggplot(df_estupro, aes(x = HORA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo por Período (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = round(seq(min(df$HORA, na.rm = TRUE), max(df$HORA, na.rm = TRUE)))) +
  facet_wrap(~SEXO_PESSOA)

ggplot(df_estupro, aes(x = FAIXA_ETARIA, fill=SEXO_PESSOA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo por Período (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


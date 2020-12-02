df_crimes <- read.csv('RDO_principais_crimes.csv')

df_crimes %>% count(RUBRICA, sort=TRUE)

text_theme <-   theme(plot.title = element_text(hjust = 0.5),
                      axis.title.x = element_text(size = 13),
                      axis.title.y = element_text(size = 13))

ggplot(df_crimes, aes(x = RUBRICA)) + 
  geom_bar() + coord_flip() + 
  theme_linedraw() + ggtitle("Quantidade de Crimes em São Paulo (RDO)") +
  labs(x = "Tipos de Crime", y = "Frequência") +
  #scale_y_continuous(trans = 'log10') +
  text_theme

ggplot(df_crimes, aes(x = HORA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Quantidade dos Principais Crimes em São Paulo por Horário (RDO)") +
  labs(x = "Horário", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~RUBRICA)

df_sexo_pessoa <- df_crimes[!is.na(df_crimes$SEXO_PESSOA) ,]
ggplot(df_sexo_pessoa, aes(x = SEXO_PESSOA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Quantidade dos Principais Crimes em São Paulo por Sexo (RDO)") +
  labs(x = "Sexo", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~RUBRICA)

df_cor <- df_crimes[!is.na(df_crimes$COR_CUTIS) ,]
ggplot(df_cor, aes(x = COR_CUTIS)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Quantidade dos Principais Crimes em São Paulo por Cor (RDO)") +
  labs(x = "Cor", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~RUBRICA)

df_faixa_etaria <- df_crimes[!is.na(df_crimes$COR_CUTIS) ,]
ggplot(df_faixa_etaria, aes(x = FAIXA_ETARIA)) + 
  geom_bar(na.rm = TRUE) + 
  theme_linedraw() + ggtitle("Quantidade dos Principais Crimes em São Paulo por Faixa Etária (RDO)") +
  labs(x = "Faixa Etária", y = "Quantidade") +
  text_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~RUBRICA)

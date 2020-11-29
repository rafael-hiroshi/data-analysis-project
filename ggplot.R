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

ggplot(df, aes(x = RUBRICA)) + 
  geom_bar() + coord_flip() + 
  theme_linedraw() + ggtitle("Frequência de Crimes em São Paulo (RDO)") +
  labs(x = "Tipos de Crime", y = "Quantidade") +
  text_theme

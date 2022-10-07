# Seminário: Análise de dados COVID-19 em Portugal
#autor: João F Pereira

# Demonstração prática de uma análise exploratória a dados Portugueses da COVID-19


#1 Obtenção dos dados ----
install.packages("readr")
library(readr)

# Ler os dados do ficheiro csv
data <- read.csv2("data_hospCOVID19.csv", encoding="Latin-1")
View(data)
str(data)

format(as.Date(data$Entrada), "%Y-%m")

# configurar as variáveis
library(dplyr)
library(zoo) #para as datas

data <- data %>%
  mutate(Entrada=as.yearmon(Entrada),
         Saida=as.yearmon(Saida),
         Regiao=factor(Regiao),
         Genero = factor(Genero),
         Grupo = factor(Grupo),
         Tipo = factor(Tipo),
         Resultado = factor(Resultado)
  )

str(data)



#2 Transformação ----
LoS <- as.data.frame(cbind(
  Var = "All",
  mean = mean(data$Tempo_perman),
  median = median(data$Tempo_perman),
  t(quantile(data$Tempo_perman,c(0,0.25,0.50,0.75,0.90,0.95,0.99,1))),
  max = max(data$Tempo_perman),
  IQR = IQR(data$Tempo_perman)))

LoS_nonICU <- as.data.frame(cbind(
  Var = "non-ICU",
  mean = mean(filter(data,data$Tipo == "non-ICU")$Tempo_perman),
  median = median(filter(data,data$Tipo == "non-ICU")$Tempo_perman),
  t(quantile(filter(data,data$Tipo == "non-ICU")$Tempo_perman,c(0,0.25,0.50,0.75,0.90,0.95,0.99,1))),
  max = max(filter(data,data$Tipo == "non-ICU")$Tempo_perman),
  IQR = IQR(filter(data,data$Tipo == "non-ICU")$Tempo_perman)))

LoS_ICU <- as.data.frame(cbind(
  Var = "ICU",
  mean = mean(filter(data,data$Tipo == "ICU")$Tempo_perman),
  median = median(filter(data,data$Tipo == "ICU")$Tempo_perman),
  t(quantile(filter(data,data$Tipo == "ICU")$Tempo_perman,c(0,0.25,0.50,0.75,0.90,0.95,0.99,1))),
  max = max(filter(data,data$Tipo == "ICU")$Tempo_perman),
  IQR = IQR(filter(data,data$Tipo == "ICU")$Tempo_perman)))

LoS_stat <- rbind(LoS,LoS_nonICU,LoS_ICU)




#3 Exploração ----
library(ggplot2)
# Histogramas dos tempos de permanencia
## non-ICU
data %>% 
  filter(Tipo == 'non-ICU') %>% 
  ggplot() +
  labs(x='Lenght of stay(days)',y='Patientes', title = 'Non-ICU') +
  xlim(0,100) +
  geom_histogram(aes(x=Tempo_perman),fill='#56B4E9')

#gravar
ggsave(
  'graficos/hist_timestay_nonICU.jpeg',
  plot = last_plot(),
  width = 10,
  height = 4,
  units = "in",
  dpi = 600
)

## non-ICU
data %>% 
  filter(Tipo == 'ICU') %>% 
  ggplot() +
  labs(x='Lenght of stay(days)',y='Patientes', title = 'ICU') +
  xlim(0,150) +
  geom_histogram(aes(x=Tempo_perman),fill='tomato1')

#gravar
ggsave(
  'graficos/hist_timestay_ICU.jpeg',
  plot = last_plot(),
  width = 10,
  height = 4,
  units = "in",
  dpi = 600
)

#Boxplots
dodge <- position_dodge(width = 1)

ggplot(data,aes(x=Tipo, y=Tempo_perman, color = Tipo)) +
  labs(x=NULL,y="Tempo de perman(dias)",title=NULL) +
  stat_boxplot(width = .2, alpha=0.6, size=0.8) +
  theme(legend.position = "none",text = element_text(size = 15)) +
  scale_colour_manual(values = c ("tomato1","#56B4E9")) 
  #scale_y_continuous(expand = c(0,0), limits = c(-3, 85)) 
#coord_flip() +
ggsave(
  'graficos/boxplot_tempPerm.jpeg',
  plot = last_plot(),
  width = 6,
  height = 8,
  units = "in",
  dpi = 600
)


#Box/Violin plot
ggplot(data,aes(x=Tipo, y=Tempo_perman)) +
  labs(x=NULL,y="Tempo perman(dias)",title=NULL) +
  geom_violin(aes(fill=Tipo,colour = Tipo), size = 1) +
  stat_boxplot(outlier.alpha = 0, coef = 0, color = "gray20", width = .2, alpha=0.6, size=0.8) +
  stat_boxplot(geom = "errorbar",           color = "gray20", width = .2, alpha=1, size=0.8) + 
  stat_summary(aes(x=Tipo,y=Tempo_perman),
               fun = "mean", geom = "point", shape = 23, size = 3,lwd=1, fill = "white",position = dodge) +
  theme_bw() + 
  theme(legend.position = "none",text = element_text(size = 15)) +
  scale_fill_manual(values = c ("#56B4E9", "tomato1")) +
  scale_colour_manual(values = c ("#56B4E9", "tomato1")) +
  scale_y_continuous(expand = c(0,0), limits = c(-3, 85)) 
#coord_flip() +
ggsave(
  'graficos/boxViolin_tempPerm.jpeg',
  plot = last_plot(),
  width = 6,
  height = 8,
  units = "in",
  dpi = 600
)


# Graficos de linha
data %>% 
  ggplot() +
  theme_bw() +
  labs(x=NULL,y='Entradas', title = NULL) +
  theme(legend.title = element_blank(),
        legend.position="top",
        plot.background = element_rect(color = "white"),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, vjust = 0.6, hjust=0.5),
  ) +
  geom_line(aes(x=Entrada,group=Tipo,colour=Tipo), stat="count", size = 0.5) +
  scale_y_continuous(breaks=seq(0, 20000, by=1000)) +
  scale_colour_manual(values = c ("tomato1","#56B4E9")) 
ggsave(
  'graficos/linegraph_entradas.jpeg',
  plot = last_plot(),
  width = 8,
  height = 4,
  units = "in",
  dpi = 600
)

#questao 4
install.packages("gridExtra")
install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(gridExtra)

penguins <- palmerpenguins::penguins

colnames(penguins) <- c("especies", "ilha", "comprimento_bico",
                        "profundidade_bico", "comprimento_nadadeira",
                        "massa_corporal", "sexo", "ano")

view(penguins)
#quais especies tem?
unique(penguins$especies)
#quantas ilhas tem?
unique(penguins$ilha)

#quantos pinguins tem em cada ilha e como está a distribuição deles
count(penguins, especies, ilha)
#aqui seria bom um gráfico


#penguins = penguins %>%
# filter(!is.na(comprimento_bico))
penguins_count %>%
  ggplot(aes(x=ilha, y=n, fill=ilha))+
  geom_bar(stat="identity")+
  geom_label(aes(label=n), fill="white")+
  scale_fill_manual(values = c("darkorange", "purple", "cyan4"))+
  labs(x= "Ilha", y= "Número de indivíduos")

# penguins_data <- na.omit(penguins)
# histogram
penguins %>%
  ggplot(aes(x=comprimento_nadadeira,
             fill = especies)) +
    geom_histogram(alpha= 0.5) +
    labs(title = "Comprimento da nadadeira em relação a espécie")


#outro estilo de histograma sem sobreposição

penguins %>%
  ggplot(aes(x=comprimento_nadadeira, fill=especies))+
    geom_histogram(position = "dodge")+
    labs(x="Comprimento da nadadeira (mm)", 
         y= "Frequência absoluta")

#bar plot

penguins_count <- penguins %>%
  dplyr::count(especies)
penguins_count


penguins_count %>%
  ggplot(aes(x=especies,y=n, fill=especies))+
    geom_bar(stat= "identity") +
    scale_fill_manual(values = c("#CC4E0E", "#851CD9", "#16CC82"))+
    geom_label(aes(label=n),fill="white")+
    labs(x="Espécies", y="Numero de indivíduos")

#a partir daqui podemos calcular a porcentagem de cada espécie no espaço amostral


#box plot

penguins %>%
  ggplot(aes(x=especies, y=comprimento_nadadeira, fill=especies))+
  geom_boxplot()

#podemos também fazer um gráfico de violino. Este tipo de gráfico permite visualizar 
#a densidade e a distribuição dos pontos.

penguins %>%
  ggplot(aes(x=especies,y=comprimento_nadadeira, fill=especies))+
  geom_violin(width=.5)+
  geom_jitter(alpha=.4, position= position_jitter(width = .15,seed=0))+
  scale_fill_manual(values = c("#CC4E0E", "#851CD9", "#16CC82"))+
  labs(title = "Gráfico de violino", x = "Espécies", y = "Comprimento da nadadeira")

#colocar os dois juntos no texto para visualização


# scatter plot, ou gráfico de dispersão com duas variáveis

penguins %>%
  ggplot(aes(x=comprimento_bico,y=comprimento_nadadeira,color=especies, shape=especies))+
  geom_point(size=2,alpha=.7)+
  geom_smooth(method="lm", se=FALSE)+
  #lm = linear
  scale_shape_manual(values = c(15, 17, 19)) +
  #shape 15 = quadrado
  #shape 17 = triangulo
  #shape 19 = circulo
  scale_color_manual(values = c("#CC4E0E", "#851CD9", "#16CC82"))+
  theme(legend.position = "top")+
  labs(x="Comprimento do bico", y="Comprimento da nadadeira", color="Espécies", shape= "Espécies")
  


#ai dps faz a analise para outras características 

summary(penguins)
#com o summary podemos ter noção de algumas variáveis da biblioteca
#ano de início é 2007 , ano de fim é 2009
#165 fêmeas e 168 machos - 11 sem informação

#dividindo por sexo e analisando as médias de cada variável
penguins_por_sexo = penguins %>%
  filter(!is.na(sexo)) %>%
  group_by(especies,sexo) %>%
  summarise(profundidade_bico = mean(profundidade_bico),
            comprimento_bico = mean(comprimento_bico),
            comprimento_nadadeira = mean(comprimento_nadadeira),
            massa_corporal = mean(massa_corporal))
penguins_por_sexo

#profundidade do bico por sexo
pbsex <- penguins_por_sexo %>%
  ggplot(aes(x=especies,y=profundidade_bico,fill=sexo))+
  geom_col(position="dodge")+
  theme_bw()+
  labs(x="Espécie", y= "Profundidade do bico(mm)")

#comprimento do bico por sexo
cbsex <- penguins_por_sexo %>%
  ggplot(aes(x=especies,y=comprimento_bico,fill=sexo))+
  geom_col(position="dodge")+
  theme_bw()+
  labs(x="Espécie", y= "Comprimento do bico(mm)")

#comprimento da nadadeira por sexo
cnsex <- penguins_por_sexo %>%
  ggplot(aes(x=especies,y=comprimento_nadadeira,fill=sexo))+
  geom_col(position="dodge")+
  theme_bw()+
  labs(x="Espécie", y= "Nadadeira(mm)")

#massa corporal por sexo
mcsex <- penguins_por_sexo %>%
  ggplot(aes(x=especies,y=massa_corporal,fill=sexo))+
  geom_col(position="dodge")+
  theme_bw()+
  labs(x="Espécie", y= "Massa corporal(g)")

grid.arrange(mcsex,cnsex,cbsex,pbsex, nrow=2)
#analisando agora por ano
#verificar se houve uma redução brusca de alimentos que reduziu a massa corporal das espécies
#ou se houve um aumento significativo de recursos que fez elas engordarem

penguins_year = penguins %>%
  filter(!is.na(ano)) %>%
  group_by(especies,ano) %>%
  summarise(massa_corporal = mean(massa_corporal))
penguins_year
#pq ta aparecendo na?
#não houve significativa variação de peso ao longo de 2 anos

#penguins_year %>%
 # ggplot(aes(x=ano, y=massa_corporal, color=especies))+
 # geom_line()+
 # geom_point()

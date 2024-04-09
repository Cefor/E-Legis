### Análise

### Dados

nep_2016_2020 <- read.csv("banco_NEP.csv", sep = ";", dec = ",", quote = '"')

names(nep_2016_2020)

### Pacotes

library(dplyr)
library(tidyverse)   
library(scales)      
library(broom)       
library(wooldridge)  
library(huxtable)
library(jtools)
library(sjPlot)
library(performance)
library(texreg)
library(hrbrthemes)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(ade4)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(vtable)




### Grupos

summary(nep_2016_2020$Razao_2016_Coligados)

#### Análise de Clusters

### Selecionar somente as variáveis utilizadas na análise

banco <- nep_2016_2020 %>% select(SG_UE, Razao_2016_Coligados, Ano)


names(banco)

nep_2016_2020_NA <- na.omit(banco)

### Implementação

k.cluster = kmeans(nep_2016_2020_NA[2], 2 , nstart=25)

k.cluster

k.cluster$size

k.cluster$betweenss


### Adicionar variável Cluster no Banco de Dados

nep_2016_2020_NA$Tratamento = k.cluster$cluster

summary(nep_2016_2020_NA$Tratamento)

nep_2016_2020_NA$Tratamento <- as.factor(nep_2016_2020_NA$Tratamento)

summary(nep_2016_2020_NA$Tratamento)

ggplot(data = nep_2016_2020_NA, aes(x = Razao_2016_Coligados)) +
  geom_histogram(binwidth = 0.1, color = "white", boundary = 0) +
  facet_wrap(~ Tratamento)

nep_2016_2020_NA$Tratamento <- ifelse(nep_2016_2020_NA$Tratamento == "2", 1, 0)

summary(nep_2016_2020_NA$Tratamento)

nep_2016_2020_NA$Tratamento <- as.factor(nep_2016_2020_NA$Tratamento)

summary(nep_2016_2020_NA$Tratamento)

ggplot(data = nep_2016_2020_NA, aes(x = Razao_2016_Coligados)) +
  geom_histogram(binwidth = 0.1, color = "white", boundary = 0) +
  facet_wrap(~ Tratamento)

summary(nep_2016_2020_NA$Razao_2016_Coligados)

t.test(Razao_2016_Coligados ~ Tratamento, data = nep_2016_2020_NA)

### Verificação gráfica

nep_2016_2020_NA %>%
  ggplot( aes(x=Tratamento, y=Razao_2016_Coligados, fill=Tratamento)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Tratamento (Clusters)") +
  xlab("")

names(nep_2016_2020_NA)


ggplot(nep_2016_2020_NA, aes(x=Tratamento, y=Razao_2016_Coligados, fill=Ano)) + 
  geom_boxplot()

#### Juntar com o banco original

names(nep_2016_2020_NA)

banco_completo <- merge(nep_2016_2020, nep_2016_2020_NA, by = c("SG_UE", "Ano"))

summary(banco_completo)

banco_completo$Ano_Eleição <- as.factor(banco_completo$Ano)

banco_completo %>%
  ggplot( aes(x=Tratamento, y=Razao_2016_Coligados.y, fill=Ano_Eleição)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Tratamento (Clusters)") +
  xlab("")

ggplot(banco_completo, aes(x=Tratamento, y=Razao_2016_Coligados.y, fill=Ano_Eleição)) + 
  geom_boxplot()

library(ggpubr)

p <- ggboxplot(banco_completo, x = "Tratamento", y = "Razao_2016_Coligados.y",
               color = "red", palette = "jco",
               add = "jitter")

p

#  Add p-value
p + stat_compare_means()
# Change method
p + stat_compare_means(method = "t.test")


### Estatísticas Descritivas


st(banco_completo)

e_geral <- banco_completo %>% select(NEP, Magnitude, IFDM, Populacao, Partidos_Vereador,
                              Partidos_Prefeito, Tratamento, Ano_Eleição) 

e_geral_NA <- na.omit(e_geral)


st(e_geral_NA)

e_2016 <- subset(banco_completo, c(banco_completo$Ano == 2016))
e_2020 <- subset(banco_completo, c(banco_completo$Ano == 2020))

e_2016_d <- e_2016 %>% select(NEP, Magnitude, IFDM, Populacao, Partidos_Vereador,
                              Partidos_Prefeito, Tratamento) 

st(e_2016_d)

e_2020_d <- e_2020 %>% select(NEP, Magnitude, IFDM, Populacao, Partidos_Vereador,
                              Partidos_Prefeito, Tratamento) 

st(e_2020_d)


####


#### Diferenças em Diferenças

### Consideração do Tempo

banco_NA <- na.omit(banco_completo)

summary(banco_NA)

ggplot(banco_NA, aes(x = factor(Tratamento), y = NEP)) +
  # geom_point(size = 0.5, alpha = 0.2) +
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(~ Sem_Coligacao)


summary(banco_NA)

plot_data <- banco_NA %>% 
  mutate(Tratamento = factor(Tratamento, labels = c("Controle", "Tratamento")),
         Sem_Coligacao = factor(Sem_Coligacao, labels = c("2016", "2020"))) %>% 
  group_by(Tratamento, Sem_Coligacao) %>% 
  summarize(media_NEP = mean(NEP),
            dp_NEP = sd(NEP) / sqrt(n()),
            upper = media_NEP + (-1.96 * dp_NEP),
            lower = media_NEP + (1.96 * dp_NEP)) 

ggplot(plot_data, aes(x = Tratamento, y = media_NEP)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  color = "darkgreen", size = 1) +
  facet_wrap(~ Sem_Coligacao)

ggplot(plot_data, aes(x = Sem_Coligacao, y = media_NEP, color = Tratamento)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) + 
  # The group = highearn here makes it so the lines go across categories
  geom_line(aes(group = Tratamento))



### Diferenças


nep_diff <- banco_NA %>% 
  group_by(Sem_Coligacao, Tratamento) %>% 
  summarize(media_NEP = mean(NEP) )

nep_diff

### Efeito

antes_tratamento <- nep_diff %>% 
  filter(Sem_Coligacao == 0, Tratamento == 1) %>% 
  pull(media_NEP)

antes_controle <- nep_diff %>% 
  filter(Sem_Coligacao == 0, Tratamento == 0) %>% 
  pull(media_NEP)

depois_tratamento <- nep_diff %>% 
  filter(Sem_Coligacao == 1, Tratamento == 1) %>% 
  pull(media_NEP)

depois_controle <- nep_diff %>% 
  filter(Sem_Coligacao == 1, Tratamento == 0) %>% 
  pull(media_NEP)

diff_tratamento_antes_depois <- depois_tratamento - antes_tratamento
diff_controle_antes_depois <- depois_controle - antes_controle
diff_diff <- diff_tratamento_antes_depois - diff_controle_antes_depois

diff_antes_tratamento_controle <- antes_tratamento - antes_controle
diff_depois_tratamento_controle <- depois_tratamento - depois_controle
outra_diff_diff <- diff_antes_tratamento_controle - diff_depois_tratamento_controle


ggplot(nep_diff, aes(x = as.factor(Sem_Coligacao), 
                     y = media_NEP, 
                     color = as.factor(Tratamento))) + 
  geom_point() +
  geom_line(aes(group = as.factor(Tratamento))) +
  
  # If you uncomment these lines you'll get some extra annotation lines and
  # labels. The annotate() function lets you put stuff on a ggplot that's not
  # part of a dataset. Normally with geom_line, geom_point, etc., you have to
  # plot data that is in columns. With annotate() you can specify your own x and
  # y values.
  annotate(geom = "segment", x = "0", xend = "1",
           y = antes_tratamento, yend = depois_tratamento - diff_diff,
           linetype = "dashed", color = "grey50") +
  annotate(geom = "segment", x = "1", xend = "1",
           y = depois_tratamento, yend = depois_tratamento - diff_diff,
           linetype = "dotted", color = "blue") +
  annotate(geom = "label", x = "1", y = depois_tratamento - (diff_diff / 2), 
           label = "Efeito Proibição", size = 3)

### Regressão

summary(banco_NA)

names(banco_NA)

banco_NA$Eleicao_2020 <- as.factor(banco_NA$Sem_Coligacao)

modelo_1 <- lm(NEP ~ Tratamento*Eleicao_2020,
               data = banco_NA)
tidy(modelo_1)

modelo_2 <- lm(NEP ~ Tratamento + Eleicao_2020 + Tratamento*Eleicao_2020
               + Magnitude + IFDM + Populacao + Partidos_Vereador + Partidos_Prefeito,
               data = banco_NA)

tidy(modelo_2)

check_collinearity(modelo_2)

library(dotwhisker)

dwplot(list(modelo_1, modelo_2), show_intercept = TRUE)

plot_summs(modelo_2)

####

# Carregando os pacotes necessários
library(lmtest)
library(sandwich)
library(broom)

# Calculando erros padrão robustos e testes de coeficientes para o modelo 1
test_1 <- coeftest(modelo_1, vcov = vcovHC(modelo_1, type = "HC1"))

# Calculando erros padrão robustos e testes de coeficientes para o modelo 2
test_2 <- coeftest(modelo_2, vcov = vcovHC(modelo_2, type = "HC1"))

test_2

screenreg(list(test_1, test_2))


####


library(jtools)
library(interactions)


plot_model(modelo_1, type = "pred", terms = c("Eleicao_2020", "Tratamento"))

plot_model(modelo_2, type = "pred", terms = c("Eleicao_2020", "Tratamento"))

z <- plot_model(modelo_1, type = "int")

z

x <- plot_model(modelo_2, type = "int")

x

library(ggpubr)

ggarrange(z, x + rremove("x.text"), 
          labels = c("",""),
          ncol = 2, nrow = 1)

plot_model(modelo_2, type = "pred", terms = c("Tratamento", "Eleicao_2020"))

plot_model(modelo_1, type = "pred", terms = c("Tratamento", "Eleicao_2020"))

#### Exportação

htmlreg(list(modelo_1, modelo_2), 
        file = "Regressao_Completa_cluster.doc",
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE)




#### Mapa Partidos coligados


library(geobr)
library(ggplot2)
library(sf)


### Verificar dados/shapes para mapas

all_mun <- read_municipality(code_muni="all", year=2018)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

ggplot() +
  geom_sf(data=all_mun, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipios", size=8) +
  theme_minimal() +
  no_axis


#### Junção Bancos de Dados pela mesma variável

names(all_mun)



eleicoes_geral <- merge(all_mun, e_2016, 
                        by.x = "code_muni",
                        by.y = "id_municipio")

names(eleicoes_geral)

eleicoes_2020 <- merge(all_mun, e_2020, 
                       by.x = "code_muni",
                       by.y = "id_municipio")

names(eleicoes_2020)

### Mapa 

summary(eleicoes_geral)

### NEP

summary(eleicoes_geral$NEP)

####

summary(eleicoes_geral$Razao_2016_Coligados.x)

g<- ggplot() +
  geom_sf(data=eleicoes_geral, aes(fill=Razao_2016_Coligados.x), color= NA, size=.15)+
  labs(title="Proporção Partidos Coligados (0 a 1)",
       caption='', size=8)+
  scale_fill_distiller(palette = "RdGy", limits=c(0.0, 1.0),
                       name="")+
  theme_minimal()

g

summary(eleicoes_geral$Tratamento)

eleicoes_geral$tratamento_1 <- as.numeric(eleicoes_geral$Tratamento)

summary(eleicoes_geral$tratamento_1)

eleicoes_geral$tratamento_2 <- ifelse(eleicoes_geral$tratamento_1 == "2", 1, 0)

summary(eleicoes_geral$tratamento_2)

h<- ggplot() +
  geom_sf(data=eleicoes_geral, aes(fill=tratamento_2), color= NA, size=.15)+
  labs(title="Tratamento (0 e 1)",
       caption='', size=8)+
  scale_fill_distiller(palette = "RdGy", limits=c(0.0, 1.0),
                       name="")+
  theme_minimal()

h

library(ggpubr)

ggarrange(g, h + rremove("x.text"), 
          labels = c("",""),
          ncol = 2, nrow = 1)


####



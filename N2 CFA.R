#2 CFA

#OBS URGENTE - tem que rodar o 1_recodificacoes iniciais.R antes
#RODAR !

#exemplo cfa com todas, considerando NA (delete listwise)
all <- subset(df, select=c(P50_antidemoc_positivo,
                             P51_recod_numeric,
                             P52_recod_numeric,
                             P53_recod_numeric,
                             P54_recod_numeric,
                             P55_recod_numeric,
                             P56_recod_numeric_apoio_positivo,
                             P58_recod_numeric, DesengMoralAntiPetista,
                             DesengMoralAntiBolsonarista,
                             NaoDesengajado))

semP50 <- subset(df, select=c(P51_recod_numeric,
                           P52_recod_numeric,
                           P53_recod_numeric,
                           P54_recod_numeric,
                           P55_recod_numeric,
                           P56_recod_numeric_apoio_positivo,
                           P58_recod_numeric, DesengMoralAntiPetista,
                           DesengMoralAntiBolsonarista,
                           NaoDesengajado))

semP53 <- subset(df, select=c(P50_antidemoc_positivo,P51_recod_numeric,
                              P52_recod_numeric,
                              P54_recod_numeric,
                              P55_recod_numeric,
                              P56_recod_numeric_apoio_positivo,
                              P58_recod_numeric, DesengMoralAntiPetista,
                              DesengMoralAntiBolsonarista,
                              NaoDesengajado))

semP50eSemP56 <- subset(df, select=c(P51_recod_numeric,
                                 P52_recod_numeric,
                                 P53_recod_numeric,
                                 P54_recod_numeric,
                                 P55_recod_numeric,
                                 P58_recod_numeric, DesengMoralAntiPetista,
                                 DesengMoralAntiBolsonarista,
                                 NaoDesengajado))
semP50_P56_P58 <- subset(df, select=c(P51_recod_numeric,
                                         P52_recod_numeric,
                                         P53_recod_numeric,
                                         P54_recod_numeric,
                                         P55_recod_numeric, DesengMoralAntiPetista,
                                         DesengMoralAntiBolsonarista,
                                         NaoDesengajado))
semP50_P53_P56_P58  <- subset(df, select=c(P51_recod_numeric,
                                             P52_recod_numeric,
                                             P54_recod_numeric,
                                             P55_recod_numeric, DesengMoralAntiPetista,
                                             DesengMoralAntiBolsonarista,
                                             NaoDesengajado))

semP50eSemP53 <- subset(df, select=c(P51_recod_numeric,
                                     P52_recod_numeric,
                                     P56_recod_numeric_apoio_positivo,
                                     P54_recod_numeric,
                                     P55_recod_numeric,
                                     P58_recod_numeric, DesengMoralAntiPetista,
                                     DesengMoralAntiBolsonarista,
                                     NaoDesengajado))


library(lavaan)
library(semTools)
library(psych)
library(ltm)


scree(all[,1:8],pc=F,hline=T,add=F,main="P50,P51,P52,P53,P54,P55,P56,P58")

scree(semP50[,1:7],pc=F,hline=T,add=F,main="Sem P50")#1 PCA
scree(semP50eSemP56[,1:6],pc=F,hline=T,add=F,main="Sem P50 e Sem P56")#1 PCA
scree(semP50_P56_P58[,1:5],pc=F,hline=T,add=F,main="Sem P50, Sem P56 e Sem P58")#Deu Ruim
scree(semP50_P53_P56_P58[,1:4],pc=F,hline=T,add=F,main="Sem P50, Sem P56, Sem P58 e Sem P53")#Deu Ruim
scree(semP50eSemP53[,1:6],pc=F,hline=T,add=F,main="Sem P50 e Sem P53")#1 PCA

scree(semP53[,1:7],pc=F,hline=T,add=F,main="Sem P53")#1 PCA



# alpha de cronbach
all_na <- na.omit(all[,1:8])
ltm::cronbach.alpha(all_na, CI=F)
semP50_na <- na.omit(semP50[,1:7])
ltm::cronbach.alpha(semP50_na, CI=F)
semP50eSemP56_na <- na.omit(semP50eSemP56[,1:6])
ltm::cronbach.alpha(semP50eSemP56_na, CI=F)
semP50_P56_P58_na <- na.omit(semP50_P56_P58[,1:5])
ltm::cronbach.alpha(semP50_P56_P58_na, CI=F)
semP50_P53_P56_P58_na <- na.omit(semP50_P53_P56_P58[,1:4])
ltm::cronbach.alpha(semP50_P53_P56_P58_na, CI=F)
semP50eSemP53_na <- na.omit(semP50eSemP53[,1:6])
ltm::cronbach.alpha(semP50eSemP53_na, CI=F)

semP53_na<- na.omit(semP53[,1:7])
ltm::cronbach.alpha(semP53_na, CI=F)

#cfa
#all
# Especificação do modelo
model_all <- 'f =~P50_antidemoc_positivo + P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_all <- cfa(model_all, data = all)
summary(cfa_model_all, standardized = TRUE)

library(semTools)
semTools::fitmeasures(cfa_model_all, c("tli", "cfi", "rmsea", "srmr"))


#SEM P53
model_sem_p53 <- 'f =~ P50_antidemoc_positivo + P51_recod_numeric + P52_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_sem_p53 <- cfa(model_sem_p53, data = all)
summary(cfa_model_sem_p53, standardized = TRUE)
semTools::fitmeasures(cfa_model_sem_p53, c("tli", "cfi", "rmsea", "srmr"))


#SEM P50
model_sem_p50 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_sem_p50 <- cfa(model_sem_p50, data = all)
summary(cfa_model_sem_p50, standardized = TRUE)
semTools::fitmeasures(cfa_model_sem_p50, c("tli", "cfi", "rmsea", "srmr"))

#SEM P53 e SEM P50
model_sem_p50e53 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_sem_p50e53  <- cfa(model_sem_p50e53, data = all)
summary(cfa_model_sem_p50e53, standardized = TRUE)
semTools::fitmeasures(cfa_model_sem_p50e53, c("tli", "cfi", "rmsea", "srmr"))

#
#SEM P50 e Sem P56
model_sem_p50e56 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_sem_p50e56 <- cfa(model_sem_p50e56, data = all)
semTools::fitmeasures(cfa_model_sem_p50e56, c("tli", "cfi", "rmsea", "srmr"))

#
#
#SEM P50 e Sem P56 e semP58
model_sem_p50e56e58 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_sem_p50e56e58 <- cfa(model_sem_p50e56e58, data = all)
semTools::fitmeasures(cfa_model_sem_p50e56e58, c("tli", "cfi", "rmsea", "srmr"))

#
#SEM P50 e Sem P56 e semP58 e Sem P53
model_sem_p50e56e58e53 <- 'f =~ P51_recod_numeric + P52_recod_numeric + P54_recod_numeric + P55_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_sem_p50e56e58e53 <- cfa(model_sem_p50e56e58e53, data = all)
semTools::fitmeasures(cfa_model_sem_p50e56e58e53, c("tli", "cfi", "rmsea", "srmr"))


#histogramas dos 4 que se saíram melhor nos testes de cima
library(ggplot2)
library(gridExtra)
scores_all <- lavPredict(cfa_model_all)
scores_all -> all_na$scores
summary(scores_all)
hist(scores_all, breaks=50)
a<-ggplot(all_na, aes(scores)) +geom_histogram()+theme_minimal()+
  labs(title= "P50,P51,P52,P53,P54,P55,P56,P58",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(-0.12, 0.31)) # Definindo os limites do eixo x

a

scores_sem_p50 <- lavPredict(cfa_model_sem_p50)
hist(scores_sem_p50, breaks=50)
scores_sem_p50 -> semP50_na$scores
summary(scores_sem_p50)
b<-ggplot(semP50_na, aes(scores)) +geom_histogram()+
  theme_minimal()+
  labs(title= "Sem P50",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(-0.2, 0.31))

scores_sem_p50e53 <- lavPredict(cfa_model_sem_p50e53)
hist(scores_sem_p50e53, breaks=50)
scores_sem_p50e53 -> semP50eSemP53_na$scores
summary(scores_sem_p50e53)
c<-ggplot(semP50eSemP53_na, aes(scores)) +geom_histogram()+
  theme_minimal()+
  labs(title= "Sem P50 e Sem P53",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(-0.12, 0.31))

c


scores_sem_p50e53 <- lavPredict(cfa_model_sem_p50e53)
hist(scores_sem_p50e53, breaks=50)
scores_sem_p50e53 -> semP50eSemP53_na$scores
summary(scores_sem_p50e53)
c<-ggplot(semP50eSemP53_na, aes(scores)) +geom_histogram()+
  theme_minimal()+
  labs(title= "Sem P50 e Sem P53",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(-0.12, 0.31))

c


scores_sem_p53 <- lavPredict(cfa_model_sem_p53)
hist(scores_sem_p53, breaks=50)
scores_sem_p53 -> semP53_na$scores
summary(scores_sem_p53)
d<-ggplot(semP53_na, aes(scores)) +geom_histogram()+
  theme_minimal()+
  labs(title= "Sem P53",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(-0.12, 0.31))

d

#
a
b
c
d
grid.arrange(a,b)
grid.arrange(a,b,c)
grid.arrange(a,b,c,d)

#de 0 a 1
semP53_na$scores <- as.numeric(semP53_na$scores)
semP53_na$scores <- scales::rescale(semP53_na$scores, to = c(0, 1))
summary(semP53_na$scores)

semP50eSemP53_na$scores <- as.numeric(semP50eSemP53_na$scores)
semP50eSemP53_na$scores <- scales::rescale(semP50eSemP53_na$scores, to = c(0, 1))
summary(semP50eSemP53_na$scores)

semP50_na$scores <- as.numeric(semP50_na$scores)
semP50_na$scores <- scales::rescale(semP50_na$scores, to = c(0, 1))
summary(semP50_na$scores)

all_na$scores <- as.numeric(all_na$scores)
all_na$scores <- scales::rescale(all_na$scores, to = c(0, 1))
summary(all_na$scores)


a2<-ggplot(all_na, aes(scores)) +geom_histogram()+theme_minimal()+
  labs(title= "P50,P51,P52,P53,P54,P55,P56,P58",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(0, 1)) # Definindo os limites do eixo x

a2

b2<-ggplot(semP50_na, aes(scores)) +geom_histogram()+theme_minimal()+
  labs(title= "Sem P50",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(0, 1)) # Definindo os limites do eixo x

b2

c2<-ggplot(semP50eSemP53_na, aes(scores)) +geom_histogram()+theme_minimal()+
  labs(title= "Sem P50 e Sem P53",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(0, 1)) # Definindo os limites do eixo x

c2

d2<-ggplot(semP53_na, aes(scores)) +geom_histogram()+theme_minimal()+
  labs(title= "Sem P53",subtitle="Distribuição dos factor.scores Anti Democrático",
       x= "factor.scores Anti Democrático")+
  scale_x_continuous(limits = c(0, 1)) # Definindo os limites do eixo x

d2

grid.arrange(a2,b2,c2,d2)

summary(all_na$scores)
summary(semP50_na$scores)
summary(semP50eSemP53_na$scores)
summary(semP53_na$scores)


a21<-ggplot(all_na, aes(scores)) +geom_histogram(bins = 60)+theme_minimal()+
  labs(title= "",subtitle="Todas da Tabela 1",
       x= "",y="Contagem")+
  scale_x_continuous(limits = c(0, 1)) # Definindo os limites do eixo x


# Para a21
median_value_a21 <- median(all_na$scores, na.rm = TRUE)
mean_value_a21 <- mean(all_na$scores, na.rm = TRUE)

a21 <- a21 +
  geom_vline(xintercept = median_value_a21, color = "red", linetype = "dashed",size=2) +
  geom_vline(xintercept = mean_value_a21, color = "blue", linetype = "dashed",size=2) +
  labs(caption="OBS: a linha pontilhada vermelha indica a mediana
       e a linha pontilhada azul indica a média")+
  theme(plot.caption = element_text(size = 11))

a21


b21<-ggplot(semP50_na, aes(scores)) +geom_histogram(bins = 60)+theme_minimal()+
  labs(title= "",
       subtitle="Retirada da questão churchilliana",
       x= "",y="Contagem")+
  scale_x_continuous(limits = c(0, 1)) # Definindo os limites do eixo x


# Para b21
median_value_b21 <- median(semP50_na$scores, na.rm = TRUE)
mean_value_b21 <- mean(semP50_na$scores, na.rm = TRUE)

b21 <- b21 +
  geom_vline(xintercept = median_value_b21, color = "red", linetype = "dashed",size=2) +
  geom_vline(xintercept = mean_value_b21, color = "blue", linetype = "dashed",size=2)+
  labs(caption="OBS: a linha pontilhada vermelha indica a mediana
       e a linha pontilhada azul indica a média")+
  theme(plot.caption = element_text(size = 11))

b21


c21<-ggplot(semP50eSemP53_na, aes(scores)) +geom_histogram(bins = 60)+theme_minimal()+
  labs(title= "",subtitle="Retirada da questão churchilliana
       e da questão referente às manifestações",
       x= "",y="Contagem")+
  scale_x_continuous(limits = c(0, 1)) # Definindo os limites do eixo x
# Calcular a mediana e a média
median_value <- median(semP50eSemP53_na$scores, na.rm = TRUE)
mean_value <- mean(semP50eSemP53_na$scores, na.rm = TRUE)

# Adicionar linhas para a mediana (vermelha) e a média (azul)
c21 <- c21 +
  geom_vline(xintercept = median_value, color = "red", linetype = "dashed",size=2) +
  geom_vline(xintercept = mean_value, color = "blue", linetype = "dashed",size=2)+
  labs(caption="OBS: a linha pontilhada vermelha indica a mediana
       e a linha pontilhada azul indica a média")+
  theme(plot.caption = element_text(size = 11))

c21


a21
b21
c21





grid.arrange(a21,b21,c21, ncol=1,nrow=3)

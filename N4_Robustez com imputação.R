# Robustez - com imputação
# os tres modelos da tabela 1


# recod as de controle

table(df$P12, useNA = "always")#escolarid ok
df$Escolaridade <- df$P12
table(df$P6,useNA = "always")#ok
df$Mulher <- df$P6 == 2
summary(df$P8)
table(df$P8, useNA = "always")# transformar em faixa
# Recodificar a variável P8 em faixas etárias
df$faixa_etaria <- cut(df$P8, breaks = c(16, 25, 34, 59, 92), labels = c(1, 2, 3, 4), include.lowest = TRUE)
# Converter a nova variável faixa_etaria para numeric
df$faixa_etaria <- as.numeric(df$faixa_etaria)
table(df$faixa_etaria,useNA = "always" )



table(df$P60,useNA = "always")
df$Renda <- df$P60
table(df$P7,useNA = "always")
df$Raca_Branca <- df$P7 == 1

#!!!!!!!!!!! rodar só N1_recodificacoes iniciais 

all <- subset(df, select=c(Renda,Raca_Branca,faixa_etaria,Mulher,Escolaridade,P50_antidemoc_positivo,
                           P51_recod_numeric,
                           P52_recod_numeric,
                           P53_recod_numeric,
                           P54_recod_numeric,
                           P55_recod_numeric,
                           P56_recod_numeric_apoio_positivo,
                           P58_recod_numeric, DesengMoralAntiPetista,
                           DesengMoralAntiBolsonarista,
                           NaoDesengajado))

semP50 <- subset(df, select=c(Renda,Raca_Branca,faixa_etaria,Mulher,Escolaridade,P51_recod_numeric,
                              P52_recod_numeric,
                              P53_recod_numeric,
                              P54_recod_numeric,
                              P55_recod_numeric,
                              P56_recod_numeric_apoio_positivo,
                              P58_recod_numeric, DesengMoralAntiPetista,
                              DesengMoralAntiBolsonarista,
                              NaoDesengajado))
semP50eSemP53 <- subset(df, select=c(Renda,Raca_Branca,faixa_etaria,Mulher,Escolaridade,P51_recod_numeric,
                                     P52_recod_numeric,
                                     P56_recod_numeric_apoio_positivo,
                                     P54_recod_numeric,
                                     P55_recod_numeric,
                                     P58_recod_numeric, DesengMoralAntiPetista,
                                     DesengMoralAntiBolsonarista,
                                     NaoDesengajado))

#imputar
library(mice)
summary(all)
imp <- mice(all, seed=23109)# o nome da base e a seed sempre essa 23109
all <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(all)

summary(semP50)
imp <- mice(semP50, seed=23109)# o nome da base e a seed sempre essa 23109
semP50 <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(semP50)

summary(semP50eSemP53)
imp <- mice(semP50eSemP53, seed=23109)# o nome da base e a seed sempre essa 23109
semP50eSemP53 <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(semP50eSemP53)

# cfas

library(lavaan)
library(semTools)
#cfa
#all
# Especificação do modelo
model_all <- 'f =~P50_antidemoc_positivo + P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_all <- cfa(model_all, data = all)
summary(cfa_model_all, standardized = TRUE)
semTools::fitmeasures(cfa_model_all, c("tli", "cfi", "rmsea", "srmr"))


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

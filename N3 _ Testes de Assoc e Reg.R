#N3 vem do script N1 e N2 dois objetos df e aquela combinação do que preferir (atual: semP50)

# escolha = Sem P50

# Lista de objetos a manter
objetos_a_manter <- c("df","semP50_na")#manter o que for que usar

#OBSSSS: obs mantive somente com retirada de P50, se decidirmos diferente é só mudar o código

# Remove todos os objetos, exceto os objetos que você deseja manter
rm(list = setdiff(ls(), objetos_a_manter))




# variáveis de controle em df selecionar, ver se tem muito NA

#lógica:
#variáveis a serem testadas em bivariadas (indepedentes) e  regressão (indep + controles)

#Independente = Os 3 tipos de desengaj
#Controle = Idade (faixa:  16-25; 26-34; 35-59;  60+)
#Controle = Gênero
#Controle = Renda (faixas, 6 já existentes no banco)
#Controle = Escolaridade (faixas,6 já existentes no banco)
#Controle =  Raça (binário - Branco e Não Branco)

#nas regressão dois tipos de modelos
#dep (linear) antiDemoc
#dep (tercil superior de antidemoc, justiticado dado o histograma)

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


#obs nennuma na nessas de controle


# complete cases

str(semP50_na)#aqui nos interessa o score e as variáveis presentes para rodar o complete cases
nrow(semP50_na)
nrow(df)#base cheia
1500-1237 # 263 casos perdidos - OK

# df deve ter os de cima + essas selecionadas , antes de ir pro complete cases

df <- subset(df, select=c(Renda,Raca_Branca,faixa_etaria,Mulher,Escolaridade,
                          P51_recod_numeric,P52_recod_numeric, P53_recod_numeric,
                          P54_recod_numeric,P55_recod_numeric, P58_recod_numeric,
                          P56_recod_numeric_apoio_positivo,DesengMoralAntiPetista,
                          DesengMoralAntiBolsonarista,
                          NaoDesengajado))
summary(df)#deu certo


df -> dffull # para manter intacto algum completão
df <- na.omit(df)

df$antiDemoc <- semP50_na$scores
df <- subset(df, select=c(Renda,Raca_Branca,
                          faixa_etaria,Mulher,
                          Escolaridade,antiDemoc,
                          DesengMoralAntiPetista,
                          DesengMoralAntiBolsonarista,
                          NaoDesengajado))
rm(dffull)
rm(semP50_na)


#
# criando df$encossta para remover dm ambos


# Verificar se há algum caso em que duas ou mais variáveis sejam TRUE
df$encosta <- rowSums(df[, c("DesengMoralAntiPetista", "DesengMoralAntiBolsonarista", "NaoDesengajado")]) >= 2

# Exibir as linhas que se encaixam na condição
casos_encosta <- df[df$encosta, ]

str(df$encosta)
table(df$encosta)#26 remover

df <- subset(df, !encosta)

#1211 
1500-1211
#289 casos perdidos
(1211*100)/1500
# 19,3% de NA

#bivariada dep x indep


#gráfico de média de score não imputado por perfil
by(df$antiDemoc, df$DesengMoralAntiPetista, mean)#0.499
by(df$antiDemoc,df$DesengMoralAntiBolsonarista, mean )#0.167
by(df$antiDemoc,df$NaoDesengajado, mean )#.177
mean(df$antiDemoc)#0.279

Perfil <- c("D.M. Anti Petista",
            "D.M. AntiBolsonarista",
            "Não D.M.")
Media <-c(.499,.167,0.177)

dados <- data.frame(Perfil, Media)
mean(df$antiDemoc)
dados$Perfil <- factor(dados$Perfil, levels = c("D.M. Anti Petista", "D.M. AntiBolsonarista",
                                                "Não D.M."))

p <- ggplot(dados, aes(x = Perfil, y = Media, fill = Media > 0)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.279, color = "red", size = 0.8) +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Média de Escore AntiDemocrático por tipologia de Desengajamento Moral", 
       subtitle = "Banco das Clivagens: Brasil, 2023.",
       caption = "D.M. = Desengajado Moral\nA linha vermelha indica a média de toda a amostra") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.caption = element_text(size = 12),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

p + scale_y_continuous(labels = function(x) round(x, 3)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 1)) +
  geom_bar(stat = "identity",
           color = "orange", fill = ifelse(dados$Media > 0.279, "orange", "blue"), 
           position = position_dodge(width = 0.1),
           size=.3) + labs(y="Média Escore Anti Democrático")# so pra consulta




#Desvio padrão 1 acima e 1 abaixo# so pra consulta
# Obtém a média por grupo
means1 <- tapply(df$antiDemoc, df$DesengMoralAntiPetista, mean)

# Obtém o desvio padrão por grupo
sds <- tapply(df$antiDemoc, df$DesengMoralAntiPetista, sd)

# Obtém a média mais o desvio padrão acima por grupo
upper1 <- means1 + sds

# Obtém a média menos o desvio padrão abaixo por grupo
lower1 <- means1 - sds


#Desvio padrão 1 acima e 1 abaixo
# filtrando o data frame
df_filtrado <- subset(df$antiDemoc, df$DesengMoralAntiPetista == TRUE)

# calculando a média e o desvio padrão
media1 <- mean(df_filtrado)
sds <- sd(df_filtrado)
upper1 <- media1 + sds
lower1 <- media1 - sds

#Desvio padrão 1 acima e 1 abaixo
# filtrando o data frame
df_filtrado <- subset(df$antiDemoc, df$DesengMoralAntiBolsonarista == TRUE)

# calculando a média e o desvio padrão
media2 <- mean(df_filtrado)
sds <- sd(df_filtrado)
upper2 <- media2 + sds
lower2 <- media2 - sds


#Desvio padrão 1 acima e 1 abaixo # so pra consulta
# filtrando o data frame
df_filtrado <- subset(df$antiDemoc, df$NaoDesengajado == TRUE)

# calculando a média e o desvio padrão
media5 <- mean(df_filtrado)
sds <- sd(df_filtrado)
upper5 <- media5 + sds
lower5 <- media5 - sds


# dados
Perfil <- c("D.M. Anti Petista","D.M. Anti Petista","D.M. Anti Petista",
            "D.M. AntiBolsonarista","D.M. AntiBolsonarista","D.M. AntiBolsonarista",
            "Não D.M.","Não D.M.","Não D.M.")
val <- c(lower1,media1,upper1,
         lower2,media2,upper2,
         lower5,media5,upper5)
err <- c(media1-lower1, 0, upper1-media1,
         media2-lower2, 0, upper2-media2,
         media5-lower5, 0, upper5-media5)
dados <- data.frame(Perfil, val, err)

# gráfico # so pra consulta
ggplot(dados, aes(x = Perfil, y = val, fill = Perfil)) +
  geom_bar(stat = "identity", position = "dodge", size=5) +
  geom_errorbar(aes(ymin = val-err, ymax = val+err), position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "Distribuição de Escore AntiDemocrático por tipologia de Desengajamento Moral",
       subtitle = "Banco das Clivagens: Brasil, 2023.",
       caption = "Obs: D.M. = Desengajado Moral.
       Barra de erro: um desvio padrão") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+theme(axis.title.y = element_blank()) + coord_flip()


#
# da distribuição como um todo

library(tidyquant)
library(ggdist)

dfcfa_c <- df[,6:9]

str(dfcfa_c)
novos_dadosA <- gather(dfcfa_c, key = "nova_variavel", value = "valor", 2:4)
novos_dadosA <- novos_dadosA %>% 
  rename(nova_variavel = "nova_variavel")
novos_dados_filtradoA <- subset(novos_dadosA, valor == TRUE)
novos_dados_filtradoA$nova_variavel <- as.factor(novos_dados_filtradoA$nova_variavel)
levels(novos_dados_filtradoA$nova_variavel)

table(novos_dados_filtradoA$nova_variavel)#otimo
novos_dados_filtradoA$nova_variavel <- factor(novos_dados_filtradoA$nova_variavel,
                                              levels = c("DesengMoralAntiPetista",
                                                         "DesengMoralAntiBolsonarista",
                                                         "NaoDesengajado"),
                                              labels = c("D.M. AntiPetista",
                                                         "D.M. AntiBolsonarista",
                                                         "Não D.M"))
table(novos_dados_filtradoA$nova_variavel)#otimo
novos_dados_filtradoA %>%
  ggplot(aes(x = nova_variavel, y = antiDemoc, fill = nova_variavel)) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2, .width = 0, point_colour = NA) +
  geom_boxplot(width = -.2, outlier.colour = "red", outlier.shape = 1, alpha = 0.5, coef = 1.5) +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 21, fill = "white", colour = "black") +
  scale_fill_tq() +
  scale_colour_tq() +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  theme_tq() +
  labs(title = "Distribuição Escore Anti Democrático",
       subtitle = "Por perfil de desengajamento moral",
       x = "Perfil de desengajamento moral",
       y = "Escore Antidemocrático",
       fill = "",
       colour = "Outliers"
  ) +
  coord_flip() +
  theme(axis.text.y = element_blank()) + labs(caption="Fonte: BDC 2023.
                                              O círculo branco aponta a média")


#
#regressões


modelo1 <- lm(antiDemoc~DesengMoralAntiPetista+
               DesengMoralAntiBolsonarista+NaoDesengajado+
                Renda+faixa_etaria+Mulher+Escolaridade+
                Raca_Branca,data=df)
hist(df$antiDemoc, breaks=50)
df$tercil <- ntile(df$antiDemoc, 3)
by(df$antiDemoc,df$tercil, mean)#so pra ver
ggplot(df, aes(tercil, antiDemoc, group=tercil)) + geom_boxplot()#so pra ver

df$TercilSuperiorAntiDemoc <- df$tercil==3

modelo2 <- glm(TercilSuperiorAntiDemoc ~ DesengMoralAntiPetista+
                 DesengMoralAntiBolsonarista+NaoDesengajado+
                 Renda+faixa_etaria+Mulher+Escolaridade+
                 Raca_Branca, data = df, family=binomial(link=logit))
tab_model(modelo1, modelo2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


plot_models(modelo2)

library(marginaleffects)
plot_models(modelo2,
show.p = T, p.shape = TRUE, digits=4,
std.est=TRUE,p.threshold = c(0.05, 0.01, 0.001),
vline.color = "black",dot.size = 3, spacing=0.7,
ci.lvl=0.9, grid=F,colors = "Set2")


plot_cap(modelo2,condition="DesengMoralAntiBolsonarista",draw=F)
#mantendo tudo mais constante, um deseng moral antibolsonarista tem 10% de probabilidade de pertencer ao tercil superior
plot_cap(modelo2,condition="DesengMoralAntiPetista",draw=F)
#mantendo tudo mais constante, um  deseng moral antipwetista tem 66% de probabilidade

# verificando se tem algum encosta agora

# criando df$encossta2 


# Verificar se há algum caso em que duas ou mais variáveis sejam TRUE
df$encosta2 <- rowSums(df[, c("DesengMoralAntiPetista", "DesengMoralAntiBolsonarista", "NaoDesengajado")]) >= 2

# Exibir as linhas que se encaixam na condição
casos_encosta2 <- df[df$encosta2, ]
nrow(casos_encosta2)
#deu boa
#

#Categoria de Referência regs para os perfis
#aquele que não se encaixa em nenhum das três
#Diz Sim e Não para Petista/Bolsonarista no direito de discursar e
#Dá entre 4-6 no direito de concorrer


#criar df$perfil

df <- df %>%
  mutate(perfil = case_when(
    DesengMoralAntiPetista & !DesengMoralAntiBolsonarista & !NaoDesengajado ~ "DesengMoralAntiPetista",
    !DesengMoralAntiPetista & DesengMoralAntiBolsonarista & !NaoDesengajado ~ "DesengMoralAntiBolsonarista",
    !DesengMoralAntiPetista & !DesengMoralAntiBolsonarista & NaoDesengajado ~ "NaoDesengajado",
    !DesengMoralAntiPetista & !DesengMoralAntiBolsonarista & !NaoDesengajado ~ "cat_referencia"
  ))
table(df$perfil)
table(df$DesengMoralAntiPetista)


modelo22 <- glm(TercilSuperiorAntiDemoc ~ perfil+
                 Renda+faixa_etaria+Mulher+Escolaridade+
                 Raca_Branca, data = df, family=binomial(link=logit))
plot_cap(modelo22, condition=c("perfil"))

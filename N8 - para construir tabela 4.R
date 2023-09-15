#N8 - para construir tabela 4 

#rodar N1 e N2
# e
# nao rodar N3!


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
                          NaoDesengajado,PT_dir_discurso,Bolsonarista_dir_discurso,
                          dirPT,dirPLBolsonaro))
summary(df)#deu certo


df -> dffull # para manter intacto algum completão
df <- na.omit(df)

df$antiDemoc <- semP50_na$scores
df <- subset(df, select=c(Renda,Raca_Branca,
                          faixa_etaria,Mulher,
                          Escolaridade,antiDemoc,
                          DesengMoralAntiPetista,
                          DesengMoralAntiBolsonarista,
                          NaoDesengajado,PT_dir_discurso,Bolsonarista_dir_discurso,
                          dirPT,dirPLBolsonaro))
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
(289*100)/1500
# 19,26% de NA

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
table(df$perfil)#ok
table(df$DesengMoralAntiPetista)#ok
df$perfilf <- as.factor(df$perfil)


table(df$perfilf, df$PT_dir_discurso)
table(df$perfilf, df$Bolsonarista_dir_discurso)
# em suma o catreferencia é alguém que Não Respondeu OU Não Sabe Ou Responde Sim para uma questão e Não para outra, mas não se enquadra no segundo critério 

table(df$perfilf, df$dirPT)#dir pt concorrer
table(df$perfilf, df$dirPLBolsonaro)# dir PL Bolsonaro

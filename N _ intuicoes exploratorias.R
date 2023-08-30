#brainstorm
#Rodar antes '1_Para brainstorm com Eder.R' antes (ali também se vê a construção do Perfil)


# trabalhar aqui


#Os 3 perfis

# 1 df$DesengMoralAntiPetista
# 2 df$DesengMoralAntiBolsonarista
# 3 df$NaoDesengajado


#começar com P50 e indo.....
summary(df[,72:91])#boa!
#prop.table(table(df$DesengMoralAntiPetista,df$P),2)

#P50 ver os levels
prop.table(table(df$DesengMoralAntiPetista,df$P50_recod),1)#17%
prop.table(table(df$DesengMoralAntiBolsonarista,df$P50_recod),1)#8%
prop.table(table(df$NaoDesengajado,df$P50_recod),1)


prop.table(table(df$DesengMoralAntiPetista,df$P51_recod),1)
prop.table(table(df$DesengMoralAntiBolsonarista,df$P51_recod),1)
prop.table(table(df$NaoDesengajado,df$P51_recod),1)


prop.table(table(df$DesengMoralAntiPetista,df$P52_recod),1)
prop.table(table(df$DesengMoralAntiBolsonarista,df$P52_recod),1)
prop.table(table(df$NaoDesengajado,df$P52_recod),1)


prop.table(table(df$DesengMoralAntiPetista,df$P53_recod),1)
prop.table(table(df$DesengMoralAntiBolsonarista,df$P53_recod),1)
prop.table(table(df$NaoDesengajado,df$P53_recod),1)


prop.table(table(df$DesengMoralAntiPetista,df$P54_recod),1)
prop.table(table(df$DesengMoralAntiBolsonarista,df$P54_recod),1)
prop.table(table(df$NaoDesengajado,df$P54_recod),1)


prop.table(table(df$DesengMoralAntiPetista,df$P55_recod),1)
prop.table(table(df$DesengMoralAntiBolsonarista,df$P55_recod),1)
prop.table(table(df$NaoDesengajado,df$P55_recod),1)

#P56 levels - apoio a atos como o 8 de janeio
prop.table(table(df$DesengMoralAntiPetista,df$P56_recod_8jan),1)
prop.table(table(df$DesengMoralAntiBolsonarista,df$P56_recod_8jan),1)
prop.table(table(df$NaoDesengajado,df$P56_recod_8jan),1)

#P58  amostra 1 e 2 juntos interv milit/golpe militar

prop.table(table(df$DesengMoralAntiPetista,df$P58_recod),1)
prop.table(table(df$DesengMoralAntiBolsonarista,df$P58_recod),1)
prop.table(table(df$NaoDesengajado,df$P58_recod),1)




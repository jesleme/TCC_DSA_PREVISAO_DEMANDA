#
#         CARREGANDO PACOTES
#
pacotes <- c("odbc","DBI","sqldf","gsubfn","tidyverse","proto","RSQLite","feather",
             "RODBC","XLConnect","xlsx","writexl","readxl","rstatix","dplyr","RODBC"
             ,"openxlsx","dplyr", "xlsx", "readxl", "lubridate","ISwR"
             ,"forecast","urca","lmtest","seasonal","seasonalview","jsonlite", "lubridate"
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#
#         CARREGANDO E AJUSTANDO BASE
#
CAMINHO_VENDAS <- "G:/TCC/BASE_DADOS/VENDAS_15_23.XLSX"
VENDAS <- data.frame(read_xlsx(CAMINHO_VENDAS, sheet = "VENDA_01-2015_A_05-2023"))

VENDAS_AJUSTADA <- select(VENDAS, 3:14)


VENDAS_VETOR <- as.vector(t(VENDAS_AJUSTADA))
print(VENDAS_VETOR)

#
#         CARREGANDO CRIANDO SERIE
#
VENDAS_SERIE <- ts(VENDAS_VETOR, start = c(2015,1), end = c(2023,5), frequency = 12)
plot(VENDAS_SERIE)
print(VENDAS_SERIE)
summary(VENDAS_SERIE)
hist(VENDAS_SERIE)

ggseasonplot(window(VENDAS_SERIE, start = 2015, end = 2023))


#NORMALIDADE
qqnorm(VENDAS_SERIE)
qqline(VENDAS_SERIE)


# Ho = distribuição normal : p > 0.05
# Ha = distribuição não é normal : p <= 0.05
shapiro.test(VENDAS_SERIE)


#estacionariedade
# Ha = não é estacionária: teste estatístico >= valor crítico
# Ho = é estacionária:  teste estatístico < valor crítico
kpss <- ur.kpss(VENDAS_SERIE)
summary(kpss)

# Teste pp (Philips-Perron)
# Ho = é estacionária: p > 0.05
# Ha = não é estacionária: p <= 0.05
pp <- ur.pp(VENDAS_SERIE)
summary(pp)


# Teste df (Dickey Fuller)

# Ha = não é estacionária: teste estatístico > valor crítico
# Ho = é estacionária:  teste estatístico < valor crítico
df <- ur.df(VENDAS_SERIE)
summary(df)

summary(kpss)
summary(pp)
summary(df)

acf(VENDAS_SERIE)
pacf(VENDAS_SERIE)
tsdisplay(VENDAS_SERIE)


# Ho = n?o ? autocorrelacionado: p > 0.05
# Ha = ? autocorrelacionado: p <= 0.05
Box.test(VENDAS_SERIE, type = "Ljung-Box")


decomposicao <- decompose(VENDAS_SERIE)
plot(decomposicao)


### APLICAÇÃO SERIE SEM NENHUMA TRANSFORMAÇÃO

# Trace: apresenta no console a lista dos modelos.
# stepwise: seleção gradual(processo mais rápido, porém menos minucioso)
# approximation: seleção do melhor modelo por aproximação
#           (indicado para séries muito longas, diminui tempo computacional)
# Drift do modelo é um parâmetro que representa a tendência temporal num passeio aleatório.
# Interessante dobrar as ordens máximas: max.p = 10, max.q = 10, max.P = 4, max.Q = 4

#  CRIAÇÃO DA SERIES PARA TREINO E TESTE
VENDAS_SERIE_TREINO <- ts(VENDAS_SERIE[1:84], start = c(2015,1), end = c(2021,12), frequency = 12)
VENDAS_SERIE_TESTE <- ts(VENDAS_SERIE[85:101], start = c(2022,1), end = c(2023,5), frequency = 12)
print(VENDAS_SERIE_TREINO)
print(VENDAS_SERIE_TESTE)

# GERANDO MODELOS
MODELO_01 <- auto.arima(VENDAS_SERIE_TREINO, trace = T, stepwise = F, approximation = F, max.order = 5,
                        max.p = 5, max.q = 5, max.P = 2, max.Q = 2)
summary(MODELO_01)


MODELO_02 <- auto.arima(tsclean(VENDAS_SERIE), trace = T, stepwise = F, approximation = F, max.order = 5,
                        max.p = 5, max.q = 5, max.P = 2, max.Q = 2)
summary(MODELO_02)

summary(MODELO_01)
summary(MODELO_02)

MODELO_03 <- auto.arima(tsclean(VENDAS_SERIE_TREINO)^(1/3), trace = T, stepwise = F, approximation = F, max.order = 5,
                        max.p = 5, max.q = 5, max.P = 2, max.Q = 2)

summary(VENDAS_SERIE_EXP)
plot(VENDAS_SERIE_EXP)
lines(VENDAS_SERIE_EXP, col = 'red')

summary(MODELO_01)
summary(MODELO_02)
summary(MODELO_03)

# Análise dos resíduos (qualidade do modelo)
checkresiduals(MODELO_01)
checkresiduals(MODELO_02)
checkresiduals(MODELO_02)

plot(resid(MODELO_01))
plot(resid(MODELO_02))
plot(resid(MODELO_03))

qqnorm(resid(MODELO_01))
qqline(resid(MODELO_01))

qqnorm(resid(MODELO_02))
qqline(resid(MODELO_02))

qqnorm(resid(MODELO_03))
qqline(resid(MODELO_03))




# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(resid(MODELO_01))
shapiro.test(resid(MODELO_01))
shapiro.test(resid(MODELO_01))

acf(resid(MODELO_01))
pacf(resid(MODELO_01))

acf(resid(MODELO_02))
pacf(resid(MODELO_02))

acf(resid(MODELO_03))
pacf(resid(MODELO_03))

plot(VENDAS_SERIE)
lines(VENDAS_SERIE-MODELO_01$resid, col= "red")
lines(VENDAS_SERIE-MODELO_02$resid, col= "blue")
lines(VENDAS_SERIE-MODELO_03$resid, col= "green")
#legend("bottomleft",legend = c("VENDAS_SERIE", "MODELO_01"), col = c("black","red"), lty =1:2, cex = 0.8, box.lty = 0)


print(VENDAS_SERIE)


###
#   PREVISAO MODELO 1
###
?forecast()
VENDA_SERIE_PREVI_1 <- forecast(VENDAS_SERIE_TREINO, model = MODELO_01, h=17)
plot(VENDA_SERIE_PREVI_1)
lines(VENDAS_SERIE-MODELO_01$resid, col= "red")



PREVISAO_1 <- as.data.frame(VENDA_SERIE_PREVI_1)
View(PREVISAO_1)

plot(VENDAS_SERIE)
lines(VENDA_SERIE_PREVI_1$mean, col= "red")
lines(VENDAS_SERIE_TESTE, col= "blue")


accuracy(VENDA_SERIE_PREVI_1, VENDAS_SERIE_TESTE)
print(VENDA_SERIE_PREVI_1)



###
#   PREVISAO MODELO 2
###
VENDA_SERIE_PREVI_2 <- forecast(VENDAS_SERIE_TREINO, model = MODELO_02, h=17)
plot(VENDA_SERIE_PREVI_2)
lines(VENDAS_SERIE-MODELO_02$resid, col= "red")



PREVISAO_2 <- as.data.frame(VENDA_SERIE_PREVI_2)
View(PREVISAO_2)

plot(VENDAS_SERIE)
lines(VENDA_SERIE_PREVI_1$mean, col= "red")
lines(VENDA_SERIE_PREVI_2$mean, col= "blue")
lines(VENDA_SERIE_PREVI_3$mean, col= "green")

accuracy(VENDA_SERIE_PREVI_2, VENDAS_SERIE_TESTE)
print(VENDA_SERIE_PREVI_2)




accuracy(VENDA_SERIE_PREVI_2, VENDAS_SERIE_TESTE)
accuracy(VENDA_SERIE_PREVI_1, VENDAS_SERIE_TESTE)

###
#   PREVISAO MODELO 3
###
VENDA_SERIE_PREVI_3 <- forecast(VENDAS_SERIE_TREINO, model = MODELO_03, h=17)
plot(VENDA_SERIE_PREVI_3)
lines(VENDAS_SERIE-MODELO_03$resid, col= "red")



PREVISAO_3 <- as.data.frame(VENDA_SERIE_PREVI_3)
View(PREVISAO_3)

plot(VENDAS_SERIE)
lines(VENDA_SERIE_PREVI_1$mean, col= "red")
lines(VENDA_SERIE_PREVI_2$mean, col= "blue")
lines(VENDA_SERIE_PREVI_3$mean, col= "green")

accuracy(VENDA_SERIE_PREVI_3, VENDAS_SERIE_TESTE)



accuracy(VENDA_SERIE_PREVI_1, VENDAS_SERIE_TESTE)
accuracy(VENDA_SERIE_PREVI_2, VENDAS_SERIE_TESTE)
accuracy(VENDA_SERIE_PREVI_3, VENDAS_SERIE_TESTE)


# Análise dos resíduos (qualidade do modelo)
checkresiduals(MODELO_02)
checkresiduals(MODELO_03)

plot(resid(MODELO_01))
lines(resid(MODELO_02), col='red')
lines(resid(MODELO_03), col='green')

par(mfrow=c(2,2))
plot(resid(MODELO_01))
plot(resid(MODELO_02), col = "red")
plot(resid(MODELO_03), col = "blue")
plot(resid(MODELO_01))
lines(resid(MODELO_02), col='red')


qqnorm(resid(MODELO_02))
qqline(resid(MODELO_02))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(resid(MODELO_02))

acf(resid(MODELO_02))
pacf(resid(MODELO_02))

plot(VENDAS_SERIE)
lines(VENDAS_SERIE-MODELO_02$resid, col= "red")
#legend("bottomleft",legend = c("VENDAS_SERIE", "MODELO_01"), col = c("black","red"), lty =1:2, cex = 0.8, box.lty = 0)

plot(VENDAS_SERIE)
lines(VENDA_SERIE_PREVI_1$mean, col= "blue")
lines(VENDA_SERIE_PREVI_2$mean, col= "red")

summary(MODELO_01)
summary(MODELO_02)

boxplot(VENDAS_SERIE)



CAMINHO_20 <- "G:/TCC/BASE_DADOS/PREVISAO_1.XLSX"
write_xlsx(PREVISAO_1, CAMINHO_20)     
CAMINHO_20 <- "G:/TCC/BASE_DADOS/PREVISAO_2.XLSX"
write_xlsx(PREVISAO_2, CAMINHO_20)     
CAMINHO_20 <- "G:/TCC/BASE_DADOS/PREVISAO_3.XLSX"
write_xlsx(PREVISAO_3, CAMINHO_20)     




# Carrega a biblioteca necessária
library(dplyr)
library(sqldf)

# Defina o número de registros desejados
num_registros <- 10000

# Cria um vetor de datas fictícias
datas <- sample(seq(as.Date('2015-01-01'), as.Date('2023-05-31'), by="days"), num_registros, replace = TRUE)

# Cria um vetor de produtos fictícios
produtos <- sample(c
                   (   "ADULTO MASCULINO BERMUDA BASICO"
                     , "ADULTO MASCULINO BERMUDA COLECAO"
                     , "ADULTO MASCULINO CALCA BASICO"
                     , "ADULTO MASCULINO CALCA COLECAO"
                     , "ADULTO MASCULINO CAMISA BASICO"
                     , "ADULTO MASCULINO CAMISA COLECAO"
                     , "ADULTO MASCULINO CAMISA POLO BASICO"
                     , "ADULTO MASCULINO CAMISA POLO COLECAO"
                     , "ADULTO MASCULINO CAMISETA BASICO"
                     , "ADULTO MASCULINO CAMISETA COLECAO"
                     , "ADULTO MASCULINO CUECA BASICO"
                     , "ADULTO MASCULINO JAQUETA COLECAO"
                     , "ADULTO MASCULINO MEIA BASICO"
                     , "ADULTO MASCULINO MEIA COLECAO"
                     , "ADULTO MASCULINO REGATA COLECAO"
                     , "ADULTO MASCULINO SHORTS COLECAO"
                     , "ADULTO MASCULINO BLAZER COLECAO"
                     , "ADULTO MASCULINO BLUSA BASICO"
                     , "ADULTO MASCULINO BLUSA COLECAO"
                     , "ADULTO MASCULINO JAQUETA BASICO"
                     , "ADULTO MASCULINO MOLETOM BASICO"
                     , "ADULTO MASCULINO MOLETOM COLECAO"
                     , "ADULTO MASCULINO REGATA BASICO"
                     , "ADULTO MASCULINO SHORTS BASICO"
                   ), num_registros, replace = TRUE)

# Cria um vetor de quantidades fictícias
quantidades <- sample(1:30, num_registros, replace = TRUE)

# Crie um dataframe com os dados gerados
dados_vendas <- data.frame(Data = datas, Produto = produtos, Quantidade = quantidades)

#Atribui o campo Ano
dados_vendas$Ano <- lubridate::year(dados_vendas$Data)

#Resume ps valores por ano
dados_vendas  <- sqldf(" select Ano, sum(Quantidade) as [Quantidade] from dados_vendas group by Ano")



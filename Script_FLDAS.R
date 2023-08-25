
setwd('C:/Users/lucas/OneDrive/Área de Trabalho/Aula_5_Atividade_Hidro')

# Dados utilizados: 
# FLDAS = P, Et e Qs 
# P, Et e Qs são em kg/m²/s --> Multiplicar pelos segundos do mês. 
# Valor em mm = Valor raster * (60*60*24*(dias do mês))

library(sf)          
library(terra)      
library(tidyverse)

#CRS para ajustar os sistemas de coordedas 
CRS <- "+proj=longlat +datum=WGS84"

#Lista contendo todos os arquivos da pasta FLDAS para criar o raster
lista <- list.files(path = 'FLDAS', full.names = T, pattern = ".nc4$")

#Filtrando o shape file para o estado de interesse
shp <- vect('lml_unidade_federacao_a.shp') %>% #Define o arquivo 
       st_as_sf() %>%                          #Transforma em um formato que aceita o filtro
       filter(sigla == 'PR') %>%               #Filtro com a sigla do estado
       vect() %>%                              #Volta ao formato original 
       project(CRS)                            #Converte o CRS para fazer o clip

plot(shp)

#Converte a lista em raster e já realiza o clip e ajuste do layer
r = rast(lista) %>% mask(shp) %>% terra::trim()

plot(r[[1]])
lines(shp)

#Identificar os momes das variáveis para destacar aquelas que temos interesse
names(r) 

etp  = r %>% .['Evap_tavg']    #Evapotranspiração
roff = r %>% .['Qs_tavg']      #Escoamento Superficial 
rain = r %>% .['Rainf_f_tavg'] #Precipitação 

#Convertendo os dados para milimetros (mm)
etp  = etp*30*24*60*60  
roff = roff*30*24*60*60
rain = rain*30*24*60*60

#Metodo EH = P - (Roff+ET)
EH = rain - (roff+etp)
plot(EH[[490:496]], range = c(-300,300))

#Excluindo os valores negativos 
EH2 = app(EH, fun=function(x){ x[x < 0] <- NA; return(x)})
plot(EH2)

#Atribuindo as data como nomes
Nomes = seq(as.Date("1982/1/1"), as.Date("2023/6/1"), "month") 

names(EH) <- Nomes
names(EH2) <- Nomes
names(etp) <- Nomes
names(rain) <- Nomes
names(roff) <- Nomes

#animate(EH2[[1:10]])

################################################################################
#Plots direto do raster
par(mfrow=c(4,1))
EH2[[400:496]] %>% boxplot(ylim = c(0,300), col = "darkgreen") 

etp[[400:496]] %>% boxplot(ylim = c(0,200), col = "green")

roff[[400:496]] %>% boxplot(ylim = c(0,150), col = "orange")

rain[[400:496]] %>% boxplot(ylim = c(0,600), col = "blue")

################################################################################
#Raster para dataframe, plot simples e exportar dados mensais como csv

df.etp <- etp %>% as.data.frame() %>% 
  pivot_longer(cols = '1982-01-01':'2023-06-01', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

df.roff <- roff %>% as.data.frame() %>% 
  pivot_longer(cols = '1982-01-01':'2023-06-01', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

df.rain <- rain %>% as.data.frame() %>% 
  pivot_longer(cols = '1982-01-01':'2023-06-01', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

df.EH <- EH2 %>% as.data.frame(na.rm = T) %>% 
  pivot_longer(cols = '1982-01-01':'2023-06-01', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

df <- data.frame(df.etp$date, df.etp$values, df.rain$values, df.roff$values, df.EH$values) 
names(df) <- c("date", "ETP", "RAN", "ROF", "EHH")

par(mfrow=c(4,1))
plot(df$ETP, type = "l", col = "green")
plot(df$ROF, type = "l", col = "orange")
plot(df$RAN, type = "l", col = 'blue')
plot(df$EHH, type = "l", col = 'black')

write.csv(df, "FLDAS_MEAN_PR.csv")

################################################################################
# Convertendo dados mensais para dados anuais

d  <- time(etp)
y  <- as.numeric(format(d, "%Y"))
ym <- paste0(y)

etpy  <- tapp(etp, ym, sum)
roffy <- tapp(roff, ym, sum)
rainy <- tapp(rain, ym, sum)
EHy   <- tapp(EH, ym, sum)

################################################################################
#Plots anuais direto do raster

EHy %>% boxplot(col = "darkgreen") 

etpy %>% boxplot(col = "green")

roffy%>% boxplot(col = "orange")

rainy %>% boxplot(col = "blue")

################################################################################
#Raster para dataframe, plot simples e exportar dados mensais como csv

df.etpy <- etpy %>% as.data.frame() %>% 
  pivot_longer(cols = 'X1982':'X2023', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

df.roffy<- roffy %>% as.data.frame() %>% 
  pivot_longer(cols = 'X1982':'X2023', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

df.rainy <- rainy %>% as.data.frame() %>% 
  pivot_longer(cols = 'X1982':'X2023', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

df.EHy <- EHy %>% as.data.frame(na.rm = T) %>% 
  pivot_longer(cols = 'X1982':'X2023', 
               names_to = "date", values_to = 'values') %>% 
  group_by(date) %>% 
  summarise_at(vars(values), list(values = mean))

dfy <- data.frame(df.etpy$date, df.etpy$values, df.rainy$values, df.roffy$values, df.EHy$values) 
names(dfy) <- c("date", "ETP", "RAN", "ROF", "EHH")

par(mfrow=c(4,1))
plot(dfy$ETP, type = "l", col = "green")
plot(dfy$ROF, type = "l", col = "orange")
plot(dfy$RAN, type = "l", col = 'blue')
plot(dfy$EHH, type = "l", col = 'black')

write.csv(dfy, "FLDAS_MEAN_YEAR_PR.csv")

#############################################################################
#Selecionando dados, mapas e exportar .tif

names(EHy)

EH.2014 <- EHy %>% .['X2014']
EH.2022 <- EHy %>% .['X2022']

par(mfrow=c(1,2))
plot(EH.2014, range = c(0,1000), main = "2014")
lines(shp)
plot(EH.2022, range = c(0,1000), main = "2022")
lines(shp)

writeRaster(EH.2014, "EH_2014.tif", overwrite=TRUE)
writeRaster(EH.2022, "EH_2022.tif", overwrite=TRUE)


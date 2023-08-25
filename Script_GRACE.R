
setwd('C:/Users/lucas/OneDrive/Área de Trabalho/Aula_5_Atividade_Hidro')

library(sf)          
library(terra)      
library(tidyverse)
library(glue)

#Lista contendo todos os arquivos da pasta GRACE para criar o raster
lista <- list.files(path = 'GRACE/', full.names = T, pattern = ".nc4$")

# CRS para converter sistemas de coordenadas do shape file
CRS <- "+proj=longlat +datum=WGS84"

#Filtrando o shape file para o estado de interesse
shp <- vect('lml_unidade_federacao_a.shp') %>% #Define o arquivo 
       st_as_sf() %>%                          #Transforma em um formato que aceita o filtro
       filter(sigla == 'PR') %>%               #Filtro com a sigla do estado
       vect() %>%                              #Volta ao formato original 
       project(CRS)                            #Converte o CRS para fazer o clip

#Converte a lista em raster e já realiza o clip e ajuste do layer
r = rast(lista) %>% mask(shp) %>% terra::trim()

#Plot para verificar os dados
plot(r[[1]])
lines(shp)

#Converter dados diários em dados mensais
gws =  r                          #Duplica a informação
d <- time(gws)                    #Extair data dos dados originais
m <- as.numeric(format(d, "%m"))  #Extrai o mês
y <- as.numeric(format(d, "%Y"))  #Extrai o ano
ym <- paste0(y, m)                #Cria um indice com mês e ano
gws_m <- tapp(gws, ym, mean)      #Filtra o dado com o indice e calcula a média mensal
  
#Criando uma sequência mensal
st <- as.Date('2003/02/01')
en <- as.Date('2023/03/31')
Date2 <- seq(from = st, to = en, by= 'month')
names(gws_m) = Date2 #Nomeando as camada com as datas                

#Cria uma pasta e salva separadamente os arquivos mensais 
dir.create("GWS_month")                     
file_rf = glue('GWS_month/GWS_{Date2}.tif')
terra::writeRaster(gws_m, filename = file_rf , overwrite = TRUE)

#Pode ser salvo como um único arquivo também
terra::writeRaster(gws_m, 'GWS_month.tif' , overwrite = TRUE)

#Converter e exportar como csv
df <- gws_m %>% 
      as.data.frame(na.rm = T) %>% 
      pivot_longer(cols = '2003-02-01':'2023-03-01', 
                   names_to = "date", values_to = 'values') %>% 
      group_by(date) %>% 
      summarise_at(vars(values), list(values = mean))
df      


#criar um gráfico 
library(ggplot2)

df$date <- as.Date(df$date)

ggplot(df, aes(x=date, y=values)) +
  geom_point() +
  theme_classic()+
  labs(x = "Date",
       y = "Groundwater Storage (mm)",
       title = "Groundwater Storage",
       subtitle = "Paraná State, 1982-2023") +
  scale_x_date(date_labels = "%b %Y")


write.csv(df, "GWS_MEAN_PR.csv")

################################################################################
#Calculando a variação mensal 
GWS  <- gws_m
GWS2 <- gws_m[[-1]]
d_GWS <- GWS2 - GWS
d_GWS <- d_GWS[[-242]]
d_GWS <- c(d_GWS, d_GWS[[241]])
names(d_GWS) <- Date2

#Boxplot dos dados raster
boxplot(d_GWS)

#Retira os valores negativos
d_GWS[d_GWS < 0] = 0

#Plot mapa com as áreas de maior variação no período
GWS.m <- mean(d_GWS, na.rm = T)
plot(GWS.m)
lines(shp)

################################################################################
library(tmap)
library(av)

# Animate 
animation <- tm_shape(gws_m)+tm_raster()+
             tm_facets(nrow = 1, ncol = 1)
animation
tmap_animation(animation, filename = "Animation.mp4", 
               width=1200, height = 600, fps = 5, outer.margins = 0)

      
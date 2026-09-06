
library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(openxlsx)


a <- rast("raster_peld_2016_utm_10m.tif")#Importando raster
a <- ifel(a == 3, 1, NA)#Binarizando raster - habitat x nao-habitat

v<-as.polygons(a, na.rm = TRUE)#transformando raster em poligono
v_sf <- st_as_sf(v)#transformando o arquivo que era sf em st (a proxima funcao funciona apenas com o formato st)
v_lines <- st_cast(v_sf, "MULTILINESTRING")#transformando o vetor dos poligonos em linhas dos contornos da floresta
plot(v_lines)

#importanto pontos shp dos pitfall
b<-read_sf("C:/Users/rafae/OneDrive/Desktop/OneDrive/Documentos/Mestrado/R",
           layer="dados_unidos_paisagem_pitfall_wgs") %>% 
   dplyr::select(landscape, trap_ID, x_wgs, y_wgs) %>% #selecionando apenas colunas de interesse
   distinct() %>% #removendo duplicatas
  mutate(pitfall_id=paste0(trap_ID, "_", landscape, "_",x_wgs,"_", y_wgs)) %>% #dando um identificacao unica para cada pifall (paisagem_pitfall_localizacao)
  st_transform(., crs(a))#reprojetando para a mesmo projecao dos contorno da floresta (v_lines)

#plotando os dois (contornos da floresta e pontos)
ggplot() +
  geom_sf(data = v_lines) +
  geom_sf(data = b, color = "red") +
  theme_minimal()


b<-b %>% 
  mutate(dist=as.numeric(st_distance(b, v_lines)))#medindo a distancia do ponto para a linha mais proxima

#para transformar os valores de dentro da floresta negativos
inside <- st_within(b, v_sf, sparse = FALSE)#identificando quais pontos ficam dentro das linhas (i.e., dentro da floresta)
inside <- apply(inside, 1, any)#colocando a informacao de uma forma que a proxima funcao entenda

#alterando valores de dentro da floresta para valores negativos
b$dist[inside] <- -b$dist[inside]

#isso joga fora a geometria para podemos exportar o arquivo excel apenas
d<-b %>% 
  st_drop_geometry() %>% 
  dplyr::select(pitfall_id, dist)

e<-read_sf("C:/Users/rafae/OneDrive/Desktop/OneDrive/Documentos/Mestrado/R",
           layer="dados_totais") %>% 
  mutate(pitfall_id=paste0(trap_ID, "_", landscape, "_", x_wgs,"_", y_wgs)) %>% 
  st_transform(., crs(b)) %>% 
  st_drop_geometry()
  

f<-as.data.frame(merge(d,e, by='pitfall_id')) 

#exportanto excel
write.xlsx(f, "planilha_final.xlsx")



####================UNINDO DADOS BRUTOS COM DISTANCIA DE BORDA ====== ###########

print(c$pitfall_id[1])

c<- read.xlsx("dados_paisagem_e_formigas.xlsx")

c <- c %>%
  mutate(pitfall_id = paste0(trap_ID, "_", landscape, "_", 
                             "c(", x_wgs, ", ", y_wgs, ")"))

d<-merge(b,c,by = "pitfall_id")


#### NATALI LOPEZ DIAZ
####EXAMEN DE VISUALIZACION DE DATOS


# Carguemos modulos y datos 

library(janitor)
library(RColorBrewer)
library(data.table)
library(tidyverse)
library(grid)
library(gridExtra)
library(sf)
library(readxl)
library(ggrepel)
setwd("C:/Users/Natali/3D Objects/Visualizacion de datos/Examen")
Plantaciones <- read_excel("Plantaciones_100.xlsx")

#Variables 
colnames(Plantaciones)

# Valores faltantes por columnas 
colSums(is.na(Plantaciones))

#Plantacioes por Departamento

Plant_dep <-Plantaciones  %>% 
  group_by(DEPARTAMENTO) %>% 
# n() : cuenta el numero de observaciones en cada grupo
  summarise(Plant_dep = n()) %>% 
# Ordenemos en forma descendente este dataframe usando la columna NumEmpresas
  arrange(desc(Plant_dep))
  View(Plant_dep)

# Grafico de placiones por departamento
  
Plant_dep %>% 
mutate(DEPARTAMENTO = fct_reorder(DEPARTAMENTO, Plant_dep)) %>% 
ggplot(mapping = aes(x = DEPARTAMENTO, y = Plant_dep))+
geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
theme(axis.text.x = element_text(angle = 90))+
coord_flip()+
geom_text(aes(label = Plant_dep))


#Promedio de hectarias por Departamento 

PromHecpor_Dpto <- Plantaciones %>% 
group_by(DEPARTAMENTO) %>% 
summarise(PromedioHcta = mean(`SUPERFICIE PLANTACION`)) %>% 
arrange(desc(PromedioHcta))
View(PromHecpor_Dpto)


# Grafico Promedio de hectarias por Departamento
PromHecpor_Dpto %>% 
mutate(DEPARTAMENTO = fct_reorder(DEPARTAMENTO, PromedioHcta)) %>% 
ggplot(mapping = aes(x = DEPARTAMENTO , y = PromedioHcta))+
geom_bar(stat = "identity", fill = "green")+coord_flip()+
geom_text(aes(label = PromedioHcta))        


unique(Plantaciones$`TIPO PLANTACION`)
colnames()
str(Plantaciones)

 unique(Plantaciones$`TIPO PLANTACION`)

# Grafico 1 
graf1 <- Plantaciones %>% 
  # Transformemos la columna EDAD
  ggplot(aes(x = `SUPERFICIE PLANTACION`, fill = `TIPO PLANTACION`))+
  geom_histogram(stat = "count")+
  # Personalicemos un poquito 
  scale_fill_manual(values = c("#456F4B", "#D59D1D", "#B03060", "#CD2990"))+
  labs(y = "Cantidad de Hectareas",
       x = "Hectareas de plantaciones",
       title = "Histograma de hectareas de plantaciones",
       caption = "Fuente : Portal de Datos Abiertos")+
  `TIPO PLANTACION~.)

graf1 <- grid.arrange(graf1,
                      bottom =textGrob(
                        "Plantaciones",
                        x = 0.9,
                        hjust = 1,gp = gpar(fontface = "italic",
                                            fontsize = 14)
                      ))

##################################################

ggraf4 <-left_join(Plantaciones %>% 
            count(DEPARTAMENTO, name = "SUPERFICIE PLANTACION" ) %>% 
  ggplot()+
  geom_sf(aes(fill = DEPARTAMENTO), show.legend = T, colour = "white")+
  geom_label_repel(aes(label = DEPARTAMENTO ,geometry = geometry), 
                   size = 2,
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   label.size = 1,
                   max.overlaps = Inf) +
    
    
    



       
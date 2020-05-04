# Librerias para el analisis
library(sf)
library(ggplot2)
library(cluster)
library(geosphere)

# Ubicacion de lugar de trabajo
setwd("F:\\BACKUPPERSONAL\\LESZCZUK\\DiscoPosMorten\\Investigacion\\3-Terceros\\Vera Franco\\Beca Cin CAMM\\PROYECTO_BIOMASA_VERA")

# Lectura de datos
datos <- st_read("Centroides_posgar.shp")
plot(datos$geometry)

# Todos los metodos de la clase sf
methods(class = "sf")


# estructura de datos
str(datos)

# distancias entre puntos
distancia <- dist(st_coordinates(datos))
distancia <- distancia/1000

# Kmeans
modelo <- kmeans(distancia, 5)

#Asignar los cluster a los modeo 
datos$clusters <- modelo$cluster





BSS <- modelo$betweenss
TSS <- modelo$totss 

BSS/TSS*100
modelo$size
str(modelo)

centros <- as.data.frame(modelo$centers)
t(centros)
CoryCen <- cbind(cordenadas, t(centros))

#Guardar en la tabla los datos de cluster
cordenadas$clusters <- modelo$cluster

plot(cordenadas$st_coordinates.datos....1., cordenadas$st_coordinates.datos....2. , col = modelo$cluster)
points(modelo$centers, col = 1:5, pch = 8, cex = 2)     

#Agregar los datos al shapefile
datos$Grupos <- modelo$cluster
st_write(datos, "Clusters.shp", driver="ESRI Shapefile")

#encontrando el centro de los clusters

clus <- cluster::pam(st_coordinates(datos), 5)
clus_med <- data.frame(clus$medoids)
clus$clustering

exportar <- st_as_sf(clus_med, coords = c("X", "Y"), crs= 22177)
plot(exportar$geometry)
st_write(exportar, "Clusters_centros.shp", driver="ESRI Shapefile")


# Centroide de un grupo de puntos
grupo1 <- datos[datos$Grupos == 1,]
centerX <- st_centroid(grupo1)

# Generar un poligono 



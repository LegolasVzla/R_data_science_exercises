# Jose Prato 23185710 - Manuel Carrero 19821361

#Analisis de Componentes Principales
#################################################################
#Primero se procedera a remover terminos sparse dentro de la matrix ya que muchos valores son nulos
clxmatrix <- removeSparseTerms(tdm, sparse = 0.998) #0.998 para 878 terminos mientras mayor el porcentaje mas palabras

#De esta manera se pueden comparar ambas matrices y notar la diferencia en terminos esparcidos
tdm
clxmatrix
Datos <- as.matrix(clxmatrix)

#Cambiando filas por columnas para tener organizada la data
Datos <- t(Datos)
rownames(Datos)<-c(tw$screenName)

#Calculando componentes Principales
modelo <- PCA(Datos, scale.unit = TRUE, ncp = 3, graph = FALSE)

#Veamos el modelo…
modelo
#Veamos los componentes por variables
tail(modelo$var$coord,n=10)

#Veamos los componentes… por individuo
tail(modelo$ind$coord,n=10) 

#Veamos la representación de los datos por componente…
tail(modelo$ind$cos2,n=10)

#Graficamos el plano principal y el círculo de correlaciones por separado para las componentes principales 1 y 2
par(mfrow = c(1, 2)) # dividimos la pantalla para recibir dos gráficos
plot(modelo, axes = c(1, 2), choix = "ind", col.ind = "red", new.plot = TRUE)
plot(modelo, axes = c(1, 2), choix = "var", col.var = "blue", new.plot = TRUE) 

#Graficamos el plano principal y el círculo de correlaciones por separado para las componentes principales 1 y 3
par(mfrow = c(1, 2)) # dividimos la pantalla para recibir dos gráficos
plot(modelo, axes = c(1, 3), choix = "ind", col.ind = "red", new.plot = TRUE)
plot(modelo, axes = c(1, 3), choix = "var", col.var = "blue", new.plot = TRUE)

#Graficamos el plano principal y el círculo de correlaciones por separado para las componentes principales 2 y 3
par(mfrow = c(1, 2)) # dividimos la pantalla para recibir dos gráficos
plot(modelo, axes = c(2, 3), choix = "ind", col.ind = "red", new.plot = TRUE)
plot(modelo, axes = c(2, 3), choix = "var", col.var = "blue", new.plot = TRUE)

#Se consideran mal representados los cos2 < 60%
# Cálculo de representatividad con los componentes 1 y 2
cos2.ind <- (modelo$ind$cos2[, 1] + modelo$ind$cos2[, 2]) * 100
cos2.ind

# Gráfica los individuos que tengan cos2 >= 0.7 (70%)
par(mfrow = c(1, 1))
plot(modelo, axes = c(1, 2), choix = "ind", col.ind = "red", new.plot = TRUE, select = "cos2 0.7")

#Se consideran mal representados los cos2 < 60%
# Cálculo de representatividad con los componentes 1 y 2
cos2.var <- (modelo$var$cos2[, 1] + modelo$var$cos2[, 2]) * 100
cos2.var

# Grafica las variables que tengan cos2 >= 0.9 (90%)
plot(modelo, axes = c(1, 2), choix = "var", col.var = "blue", new.plot = TRUE, select = "cos2 0.9")


####################################################################################################

#Metodo de Clasificacion Jerarquica
#Obtenemos una muestra considerable de los individuos para clasificarlos acorde a las palabras presentes en sus tweets
HDatos <- head(Datos,n=13)

#Realizamos el modelo para la nueva muestra de individuos
modelo2 <- PCA(HDatos, scale.unit = TRUE, ncp = 3, graph = FALSE)

#Obtenemos la clasificacion
res.hcpc <- HCPC(modelo2, nb.clust = -1, consol = TRUE, min = 3, max = 3, graph = FALSE)
res.hcpc

#Tabla de datos con la asignación de las clases o cluster
 res.hcpc$data.clust
 
 #Graficando los cluster con el árbol clasificador
plot(res.hcpc, axes=c(1,2), choice="tree", rect=TRUE,
draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
centers.plot=FALSE)

#Graficando los cluster con el árbol clasificador en 3D
 plot(res.hcpc, axes=c(1,2), choice="3D.map", rect=TRUE,
draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
centers.plot=FALSE)

####################################################################################################


#Metodo Clusterización con K-medias

#Seleccionamos los componentes y almacenamos para poder ser procesados
Datoscomponentes <- modelo$ind$coord

#Selección de K mediante el "Codo de Jambu"
InerciaIC = rep(0, 30)
for (k in 1:30) {
    grupos = kmeans(Datoscomponentes, k)
    InerciaIC[k] = grupos$tot.withinss
}
plot(InerciaIC, col = "blue", type = "b")

#Asi podemos observar que un k valido en este caso es k=3
#Ejecutamos el metodo con la funcion kmeans del paquete base stats
grupos <- kmeans(Datoscomponentes, 3, iter.max = 100) #debe de ser sobre el PCA

#Revisando el modelo almacenado en grupos
grupos
grupos$cluster
grupos$centers
grupos$totss  # Inercia Total
grupos$withinss  # Inercia Intra-clases por grupo (una para cada grupo)
grupos$tot.withinss  # Inercia Intra-clases
grupos$betweenss  # inercia Inter-clases
# Verificación del Teorema de Fisher
grupos$totss == grupos$tot.withinss + grupos$betweenss
grupos$size  # Tamaño de las clases

#Graficamos los cluster generados y sus centros.
plot(Datoscomponentes, pch = 19)
points(grupos$centers, pch = 19, col = "blue", cex = 2)
points(Datoscomponentes, col = grupos$cluster + 1, pch = 19)


#Guardando la tabla de datos mas una columna con el cluster al que pertenece cada individuo
NDatos <- cbind(Datoscomponentes, Grupo = grupos$cluster)
head(NDatos,n=5)

####################################################################################################
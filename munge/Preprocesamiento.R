# Jose Prato 23185710 - Manuel Carrero 19821361
#Se realiza el cambio al directorio de trabajo
setwd("~/tarea1")

#Obtenemos la columna que nos conviene en este caso los Tweets
mydata <- tw$text

##### inicio limpieza de datos #####

#Quitamos los acentos para no despediciar data asumiendo que solo las vocales tienen 
mydata <- gsub("á","a",mydata)
mydata <- gsub("é","e",mydata)
mydata <- gsub("í","i",mydata)
mydata <- gsub("ó","o",mydata)
mydata <- gsub("ú","u",mydata)

mydata <- gsub("Á","A",mydata)
mydata <- gsub("É","E",mydata)
mydata <- gsub("Í","I",mydata)
mydata <- gsub("Ó","O",mydata)
mydata <- gsub("Ú","U",mydata)

# Convertimos cada tweet e formato ASCII
mydata = iconv(mydata, to="ASCII//TRANSLIT")

# remueve retweets
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", mydata)
# remove @otragente
txtclean = gsub("@\\w+", "", txtclean)

# Se realiza la construccion de un corpus para el tratado de la data
corpus <- Corpus(VectorSource(mydata), readerControl = list(language = "es"))

# Convertimos cada palabra a minúsculas
txtclean <- tm_map(corpus, content_transformer(tolower))

#Procedemos a eliminar URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
txtclean <- tm_map(txtclean, content_transformer(removeURL),lazy=TRUE)

# Procedemos a eliminar los numeros
 txtclean <- tm_map(txtclean, content_transformer(removeNumbers))

# Removemos los signos de puntuacion que no son necesarios
txtclean <- tm_map(txtclean, content_transformer(removePunctuation))


# Se eliminan las palabras comunes que no son de gran importancia tanto para el idioma ingles como para el español
txtclean <- tm_map(txtclean, content_transformer(removeWords),stopwords("spanish"))
txtclean <- tm_map(txtclean, content_transformer(removeWords),stopwords("english"))

#Ademas se eliminan las etiquetas comunes que no se encuentran en el conjunto por defecto de R
txtclean <- tm_map(txtclean, content_transformer(removeWords), c("6d","rt","a","b","c","d","e","f","g","h","i","j","k","l","m","n","ñ","o","p","q","r","s","t","u","v","w","x","y","z")) #Quitar las palabras locas

# Procedemos a eliminar espacios en blanco innecesarios
txtclean <- tm_map(txtclean, content_transformer(stripWhitespace))

txtclean <- tm_map(txtclean, stemDocument, language = "spanish")  #Lematizando

txtclean <- tm_map(txtclean, PlainTextDocument)

##### fin limpieza de datos #####

# Procedemos a crear la matriz de terminos
tdm <- TermDocumentMatrix(txtclean,control=list(wordLengths = c(1, Inf)))

# Luego se convierte a una matriz manipulable
mymatrix = as.matrix(tdm)

# Contamos la frecuencia de las palabras en froma decreciente
wf <- sort(rowSums(mymatrix),decreasing=TRUE)

#Procederemos a eliminar palabras con baja frecuencia ya que no seran consideradas para el posterior analisis
wf <- subset(wf, wf >= 10) 
wf2 <- subset(wf, wf >= 300)

# Por ultimo se crea un data frame con las palabras y sus frecuencias 
dm <- data.frame(word = names(wf), freq=wf)
dm2 <- data.frame(word = names(wf2), freq=wf2)

#Por ultimo procederemos a guardar la data para tener un respaldo de la misma
save (dm,dm2,tdm,file="data/datalimpia.RData")




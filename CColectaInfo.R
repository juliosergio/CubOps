######################
# CColectaInfo.R
#    Colección de la Información de Conchos
######################
source("CollectFilesF.R")

cuencaCoords <- readCsv("cordenas estaciones clicom.csv")
# cuencaCoords$Longitud <- as.numeric(cuencaCoords$Longitud)
dir <- "ResultadosConchos/3a_SeriesHomogenenizadas"

Fpattrn <- "H\\.csv$"

fls <- CollectFiles(Fpattrn, dir) # archivos c/información

# Completamos el directorio de la cuenca
cuenca <- "Conchos"

# Para los nombres de las estaciones:
xx <- strsplit(fls, ".*/|" %,% Fpattrn)

NomEsts <- sapply(xx,tail,1)

# Busquemos las coordenadas
claves <- as.integer(sapply(strsplit(NomEsts, "s"),"[",2))
ii <- match(claves, cuencaCoords$Clave)
Coords <- cuencaCoords[ii, c("Longitud", "Latitud")]
names(Coords) <- c("Lon", "Lat")
rownames(Coords) <- NomEsts

readTb <- read.csv
hdr <- F


cols <- c("year", "month", "day", "pp", "tmax", "tmin")

tablas <- lapply(fls, readTb, header=hdr, col.names=cols)

# A partir de aquí hará un arreglo con tres dimensiones (cubo), como sigue:
#   DIMENSIÓN 1: Fecha, construída a partir de las 3 primeras columnas
#   DIMENSIÓN 2: Nombres de las estaciones
#   DIMENSIÓN 3: Nombres de las variables
# De modo que: Cubote[i,j,k], donde i,j,k pueden ser enteros o strings
# me regresa el valor de la variable k, de la estación j en la fecha i

# Las fechas son todas uniformes para todas las tablas, construyamos el
# arreglo de fechas a partir de cualquiera de las tablas
Fechas <- do.call(function(...) paste(...,sep="-"), tablas[[1]][,1:3])
Fechas <- as.Date.character(Fechas)
nFechas <- length(Fechas)
nVars <- 3
nEsts <- length(NomEsts)

# Ahora podemos recortar las columnas de fecha de cada una de las tablas
ttablas <- lapply(tablas, function(e) as.matrix(e[,-(1:3)]))
Cubote <- array(do.call(c, ttablas), 
                c(nFechas,nVars,nEsts),
                list(as.character(Fechas), cols[-(1:3)], NomEsts))
# list(Fechas, cols[-(1:3)], NomEsts))

# Ahora arreglamos las dimensiones (trasponemos índices)
Cubote <- list(Coords=Coords, Cubote=aperm(Cubote, c(1,3,2)))

# Salvamos el objeto:
save(Cubote, file = cuenca %,% "_Cubote.RData")

creaDe_zip <- function(zippath) {
    dir <- getDir(zippath)
    
}
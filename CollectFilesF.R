# ======================
# JSS: CollectFilesF.R
# 
#     Permite encontrar un conjunto de archivos dispersos por
#     directorios, pero con cierta uniformidad en los nombres
#     y ponerlos su descripción en un vector de strings


if (!exists("LEIDO.MiniBiblioteca")) source("MiniBiblioteca.R")


CollectFiles <- function(descr="\\.txt",filtroDirs="", path=".") {
    dirs <- list.dirs(path=path)
    ii <- grepl(filtroDirs, dirs) # solo se consideran estos dirs.
    dirs <- dirs[ii]
    list.files(dirs, descr, full.names = T)
}

ExtractDir <- function(ss, filtroDirs) {
    aa <- strsplit(ss, filtroDirs)[[1]]
    aa[1] %,% filtroDirs
}

getDir <- function(path) {
    # Elimina el nombre del archivo del path 
    paste(head(strsplit(path, "/")[[1]], -1), collapse = "/") # Se quita el archivo
}

expand_zip <- function(zippath) {
    dir <- getDir(zippath)
    command <- paste0("cd ", dir, " ;unzip ", zippath)
    if (system(command, ignore.stdout = T)) return (NULL) # <- Error en ejecución
    return(dir)
}

creaDe_zip <- function(
    zippath,  # Archivo zip con los datos
    estspath, # Archivo con coordenadas de estaciones
    Fpattrn = paste0("(s", E_Int, ")H\\.csv"), # Patrón de archivos de datos
    cols = c("year", "month", "day", "pp", "tmax", "tmin") # Nombres de cols (opcional)
) {
    dir <- expand_zip(zippath) # directorio de expansión
    if (is.null(dir)) return (NULL) # Viene con Error
    oldWd <- getwd()
    setwd(dir)
    EstsCoords <- readCsv(estspath)
    # Archivos 
    fls <- CollectFiles(Exclusivo(Fpattrn)) # archivos c/información
    # Para los nombres de las estaciones:
    NomEsts <- ExtraeI(Fpattrn, fls, 2) # Encuentra los nombres de estaciones

    # Busquemos las coordenadas
    claves <- as.integer(sapply(strsplit(NomEsts, "s"),"[",2))
    # print(class(EstsCoords$Clave))
    # print(claves)
    # print("==========")
    # print(EstsCoords$Clave)
    ii <- match(claves, EstsCoords$Clave)
    Coords <- EstsCoords[ii, c("Longitud", "Latitud")]
    names(Coords) <- c("Lon", "Lat")
    rownames(Coords) <- NomEsts
    
    # Investigación de estructura de los archivos de datos
    fnam <- fls[1] # Todos los archivos tendrán la misma estructura
    frstLine <- readLines(fnam, 1)
    if (grepl(L_Num, frstLine)) { # Es una lista de números exclusivamte
        hdr <- F
    } else {
        hdr <- T
        # Los nombres de las columnas
        cols <- strsplit(frstLine, "[[:blank:]]*,[[:blank:]]*")[[1]]
    }

    tablas <- lapply(fls, read.csv, header=hdr, col.names=cols)
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
    nVars <- length(cols) - 3 # El núm. de columnas menos las 3 de la fecha
    nEsts <- length(NomEsts)
    # Ahora podemos recortar las columnas de fecha de cada una de las tablas
    ttablas <- lapply(tablas, function(e) as.matrix(e[,-(1:3)]))
    Cubote <- array(do.call(c, ttablas), 
                    c(nFechas,nVars,nEsts),
                    list(as.character(Fechas), cols[-(1:3)], NomEsts))
    # Ahora arreglamos las dimensiones (trasponemos índices)
    # y ponemos los dos objetos de interés Coords y Cubote
    Cubote <- list(Coords=Coords, Cubote=aperm(Cubote, c(1,3,2)))

    setwd(oldWd)
    return(Cubote)
}

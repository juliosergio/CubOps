# ==========================
# JSS: MiBiblioteca.R
# ==========================

# Variable para detectar que se ha leido Biblioteca:
LEIDO.MiniBiblioteca <- TRUE

library(dplyr)
library(tidyr)
# library(stringdist)
composite <- function(f,g) function(...) f(g(...))
# como operador:
`%cmp%` <- composite # Operador de composición de funciones

`%,%` <- function(x, y) paste0(x, y) # Operador de concatenación de cadenas

##> `%//%` <- function(x,y) as.integer(x/y) # División entera: ya existe %/%


# bdir <- "" # YA-NO>> "E:/RR/"
# source(bdir %,% "ManejaErrores.R")
# 
source("Sustituyes.R")
# source(bdir %,% "Geodetic.distance.R")
# source(bdir %,% "Intercala.R")
# source(bdir %,% "CoordinatedDrw.R")

lagpad <- function(x, k=1) {
    if (k>0) return (c(rep(NA, k), x)[1 : length(x)] )
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
}

mustGet <- function (prompt, default="", inclSet=NULL, filtra = function(x)x, stay=T) {
    # Obtiene un dato en línea, posiblemente obligando a que
    # se encuentre en un conjunto de strings dado:
    # USO: rr <- mustGet("De una respuesta [S/N] >", "n", c("s","S","n","N"))
    # o cheque contra una expresión regular:
    # USO: rr <- mustGet("De una fecha [YYYY-DD-MM] >", "", E_Fecha)
    #  flitra(): Es una función para filtrar la entrada
    #  stay:     Indica si debe permanecer en el ciclo hasta obtener alguna respuesta
    repeat {
        resp <- if ((resp<-filtra(readline(prompt)))=="") default else resp
        if (!stay & resp=="") return(resp) # Sale del ciclo
        if (resp != "") {
            if (is.null(inclSet)) return(resp)
            # Lo que viene en inclSet es o un conjunto de strings
            # -- sólo explicable si length(inclSet) >= 2
            # o una expresión regular que conformaría el tipo de
            # respuesta aceptable:
            # CASO 1: Conjunto de strings:
            if (length(inclSet) > 1) {
                if (resp %in% inclSet) return(resp)
            } else if (grepl(inclSet, resp)) #<- CASO 2: Expresión regular:
                return(resp) 
            # Todos los otros casos siguen en el ciclo
        }
    }
}

group.mean <- function(x, ini=1, nels=length(x)-ini+1, size=12) {
    # Hacer la media por grupos donde el tamaño de cada grupo
    # es 'size'. La media se inicia a partir índice inicia 'íni'
    # 'nels' es el número total de elementos del vector 'x' a 
    # cosiderar a partir de 'ini'
    # Se puede usar para calcular promedios de acumulados anuales
    # ------------
    
    # número total de elementos a considerar
    m <- floor(nels/size) # Número de grupos completos
    fin <- ini + m*size -1
    sum(x[ini:fin])/m
}

trim.mean <- function(x, ini=1, nels=length(x)-ini+1, ...) {
    # Hacer la media recortada
    # La media se inicia a partir índice inicia 'íni'
    # 'nels' es el número total de elementos del vector 'x' a 
    # cosiderar a partir de 'ini'
    # ------------
    
    # número total de elementos a considerar
    fin <- ini + nels -1
    mean(x[ini:fin], ...)
}


# Funciones para la moda

id.mode <- function(tt) as.numeric(names(tt)[which.max(tt)])  # identifica la moda en una tabla de fecuencias
# La moda de una serie de datos es la composición de la función table(), que calcula las frecuencias
# en la serie, con id.mode()
stat.mode <- id.mode %cmp% table # stat.mode(x) donde x es la serie de datos
get.dif <- function(x) stat.mode(x-dplyr::lag(x))


# Sintaxis de ifelse multiple -------------------------------------------------------
i_ <- function(if_stat, then) {
    if_stat <- lazyeval::expr_text(if_stat)
    then    <- lazyeval::expr_text(then)
    sprintf("ifelse(%s, %s, ", if_stat, then)
}

e_ <- function(else_ret) {
    else_ret <- lazyeval::expr_text(else_ret)
    else_ret
}

if.else_ <- function(...) {
    args <- list(...)
    
    for (i in 1:(length(args) - 1) ) {
        if (substr(args[[i]], 1, 6) != "ifelse") {
            stop("All but the last argument, need to be if.then_ functions.", call. = FALSE)
        }
    }
    if (substr(args[[length(args)]], 1, 6) == "ifelse"){
        stop("Last argument needs to be an else_ function.", call. = FALSE)
    }
    args$final <- paste(rep(')', length(args) - 1), collapse = '')
    eval_string <- do.call('paste', args)
    eval(parse(text = eval_string))
}

test.ie <- function() {
    dd <- data.frame(a=c(1,2,1,3), b=1:4)
    if.else_(
        i_(dd$a==1, dd$b),
        i_(dd$a==2, dd$b*100),
        e_(-dd$b)
    )
}

# END Sintaxis de ifelse multiple -------------------------------------------------------

dist2 <- function(pts, p0, pw=1) {
    # Calcula la distancia del punto p0 a todos los puntos en el conjunto pts
    r <- (pts[,1]-p0[,1])^2 + (pts[,2]-p0[,2])^2
    if (pw==2) r else r^(pw/2)
}

mdist2 <- function(pts, mp0, pw=1) {
    # multiple de lo anterior: es decir entrega una matriz en que
    # cada una de las columnas contiene las distancias de cada uno de
    # los puntos de pts al punto correspondiente de mp0
    sapply(as.data.frame(t(mp0)), function(p) dist2(pts, t(p), pw))
}

# Está vacío un vector?
is.empty.v <- function(v) !as.logical(length(v))

# Comparación que incluye NAs
compareNA <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}
# como operador
`%=%`<- compareNA

# Extrae primer elemento de una lista
bareId <- function (L) L[[1]]

## bareIId <- function(f, L, ...) f(bareId(L), ...)


# Productores de funciones
# = con primer argumento =
ffun <- function(f, ...) function(x) f(x, ...)
# p.ej. para hacer una función 'convert' (ver definición abajo) pero que multiplique por
# 500 en vez de 1/2.54 sería con:
#  conv50 <- ffun(convert, 50)

# = con argumento nombrado =
ffun0 <- function(f, name, ...) function(x) f(assign(name,x), ...)
# p.ej. ffun0(gsub, "x", pattern=",", replacement=""), crea una función
# que elimina todas las comas de su único argumento.

readCsv <- function(...) read.csv(..., stringsAsFactors = F)
writeCsv <- function(...) write.csv(..., row.names = F, na="")

readTable <- function(...) read.table(..., stringsAsFactors = F, header = T)


chkSintx <- as.logical %cmp% length %cmp% grep
forceNum <- function (x, na.as.0=F) tryCatch.W.E({
    x <- as.numeric(x)
    if (na.as.0) x[is.na(x)] <-0
    x
})$value

tstchr <- function(x, set="") is.na(x) | x %in% set

extrae <- function (Expr, ss) regmatches(ss, regexec(Expr, ss))

ExtraeI <- function (Expr, ss, i=1) sapply(extrae(Expr,ss), '[', i)

# Hace match de un conjunto de "expresiones regulares" contra una tabla de
# strings (un catálogo), el resultado es (a) un arreglo de lógicos indicando
# los elementos que se encontraron en la tabla, o (b) los índices a los elementos
# de la tabla correspondientes a cada una de las expresiones dadas en expSet
rexpsMatchL <- function(expSet,Tbl) apply(sapply(expSet, grepl, Tbl),1,any)
rexpsMatch <- function(expSet,Tbl) {
    # aqui la tabla de salida es del tamaño de expSet
    x <- sapply(expSet, grep, Tbl)
    x[sapply(x, function(e) !length(e))] <- NA
    unlist(x)
}

cambiaMultiple <- function(patrns, reemplazos, x) {
    n <- length(x)
    mask <- rep(TRUE, n)
    for (i in 1:length(patrns)) {
        inds <- mask
        inds[mask] <- grepl(patrns[i], x[mask])
        x[inds] <- reemplazos[i]
        mask <- mask & !inds
    }
    x
}

# corta registros de una tabla de acuerdo
# a una condición dada:
cuTable <- function (Tb, cond) Tb[cond,]

ConvierteNumeric <- forceNum %cmp% bareId

# Para convertir strings que representan números pero que en esa expresión
# contienen comas, se usan las siguientes funciones
ConvNumConComas00 <- forceNum %cmp% ffun0(gsub, "x", pattern=",", replacement="")

ConvNumConComas <- ConvNumConComas00 %cmp% bareId

# Para cambiar un caracter por otro
cambia <- function(x, patrn=",", repl=".", ...) gsub(patrn, repl, x, ...)
numericConComaDec <- forceNum %cmp% cambia %cmp% bareId


# Expresiones gramaticales
E_SI <- "^[[:blank:]]*[Ss][IiÍí][[:blank:]]*$" # Sí
E_SIoX <- E_SI %,% "|^[[:blank:]]*[Xx][[:blank:]]*$"
E_OTRO <- " ?OTRO ?" # OTRO
E_OBJ <- "[[:alpha:]]+[[:digit:]]*" # Objeto: letras + num
E_DefOBJ <- paste0(E_OBJ, ":") # Definición de Objeto
P_Gpo <- "^(.+)\\[\\[([[:digit:]]+)\\]\\],([[:alpha:]]+)"
E_Gpo <- paste0(P_Gpo, "\\.?$")
E_GpoFin <- paste0(P_Gpo, "\\.$")
E_Fin <- "\\.$"
E_Par <- "^\\((.+)\\)$"
Es_Continuacion <- "^\\.\\."
Es_Inline <- "^::"
Es_Indexado <- ".+\\[(.+)\\]"
E_1stAplfa <- "[^[:alpha:]]*([[:alpha:]]+)[^[:alpha:]]*"
#>> E_EdoInRPD <- "[^[:alpha:]]+([[:alpha:]]+)[^[:alpha:]]*"
E_1stMultAlfa <- "[^[:alpha:]]*([[:alpha:]][[:alpha:]]+)[^[:alpha:]]*"

E_1stToken <- "(^[[:alpha:]]?[^[:alpha:]]+)[[:alpha:]]"
# E_comaYOblancos <- "[[:blank:]]+,?[[:blank:]]*|[[:blank:]]*,?[[:blank:]]+|,"
E_comaYblancos <- "[[:blank:]]*,[[:blank:]]*"
E_comaGYblancos <- "[[:blank:]]*[-,][[:blank:]]*" # Coma o guión con blancos
E_comaYOblancos <- E_comaYblancos  %,% "|[[:blank:]]+" 
E_comaYbl_O_Y <- E_comaYblancos %,% "|[[:blank:]]+[YyEe][[:blank:]]+"
E_comaYbl_O_Yg <- E_comaYblancos %,% "|[[:blank:]]+[-YyEe][[:blank:]]+" # con guión
# E_grds <- "[[:blank:]]*[°º'\\\"][[:blank:]]*|[[:blank:]]+"
E_grds <- "[^.[:digit:]]+"


# Número de anexo denro de un string
E_anxIn <- "[^[:digit:]]*[[:digit:]]+[,.]([[:digit:]]+)[^[:digit:]]*"
E_Fecha <- "^([[:digit:]]+-){2}[[:digit:]]+$"

# Para que un patrón corresponda incluya exclusivamente todo el
# string de prueba:
Exclusivo <- function(s) paste0("^", s, "$")

# Exclusivamente un entero:
E_Int <- "[[:digit:]]+"
E_excInt <- Exclusivo(E_Int)

# PARA Lista de Números
# Definición simple de un número
E_Num <- "[[:blank:]]*-?[[:digit:]]+\\.?[[:digit:]]*[[:blank:]]*"

# Lista de "algo" separada por comas

E_list <- function(algo) paste0("(", algo, ",)*", algo)

L_Num <- Exclusivo(E_list(E_Num)) # La lista

sepElts <- ffun(strsplit, E_comaYbl_O_Yg)

# ------------------------------------
# Método de Horner para polinomios:
horn <- function(coefs, x=1/60) { # se usará p/convertir a grad. decimales
    ss <- 0 # La suma inicial
    for (i in length(coefs):1) 
        ss <- coefs[i] + ss*x
    ss
}

# Si de antemano se dan los coeficientes en orden inverso; esto es,
# el coeficiente para la x con mayor potencia primero y así para llegar 
# al coeficiente "independiente" al final, se puede usar esta versión,
# que resulta la más eficiente
rhorn <- function(coefs, x=1/60) { # se usará p/convertir a grad. decimales
    ss <- 0 # La suma inicial
    for (e in coefs) 
        ss <- e + ss*x
    ss
}


# Otra forma menos eficiente
fff <- function(a,b,x) x*a+b
horn1 <- function(coefs, x=1/60) {
    Rc <- coefs[length(coefs):1]
    Reduce(function(a,b) fff(a,b,x), Rc, 0)
}


# Cadenas representando grados con ° ' "
sepGrds <- ffun(strsplit, E_grds)
# Como numérico:
ToDecGrds00 <- function(s) {
    ss <- sepGrds(s)
    ss[sapply(ss, is.empty.v)] <- NA
    apply(
        do.call(rbind, ss),
        1,
        horn %cmp% as.numeric
    )
}

# Para usar con subtabla:
ToDecGrds <- ToDecGrds00 %cmp% bareId
# Con el negativo:
ToMinusDecGrds <- function(x) -ToDecGrds(x)


Extrae1stAlpha <- function (X) sapply(extrae(E_1stAplfa,X), '[', 2)
ExtraeEdo <- function (X) sapply(extrae(E_1stMultAlfa,X), '[', 2)
ExtraeRegAdm <- function (X) sapply(extrae(E_1stToken,X), '[', 2)
ExtraeRegHid <- function (X) sapply(extrae("/(..)",X), '[', 2)

ExtraeNAnx <- function (X) sapply(extrae(E_anxIn,X), '[', 2)




# ==============================
# FUNCIONES AUXILIARES:


# Evaluación de strings:
evalstr <- function(s, ...) eval(parse(text=s), ...)

# una variable como string
nameChar <- function(v1) {
    deparse(substitute(v1))
}


# Lectura y conversion a dplyr

toDplyr <- tbl_df %cmp% read.csv
toDplyrNf <- tbl_df %cmp% readCsv # sin considerar como Factores los strings


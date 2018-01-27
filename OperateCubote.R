######################
# OperateCubote.R
#    Hace operaciones en una 
#    estructura tipo Cubote
#
#    La idea es producir un data frame
#         x, y, var1 var2 ... varn
#
#    donde x, y, pueden ser la longitud y latitud
#    y var el resultado de operar sobre alguna variable 
#    los índices 1,2,..,n de var pueden corresponder a 
#    una serie de tiempo
######################
if (!exists("LEIDO.MiniBiblioteca")) source("MiniBiblioteca.R")

S.aggregate <- function (tt, mask, op, label, blank="*") {
    # mask: es un arreglo de 3 o menos lógicos que indica como se
    #       recomponen las fechas, correspondiente al 
    #       year, month, day
    #       p.ej., c(T,T,T) indica que permanecen todos los campos,
    #       es decir, no se "blanquea" ninguno de Y,M,D
    #   op: es la operación que se aplica.
    # blank: es un caracter que se usa como "relleno" para igualar
    #        expresiones de fecha.
    # label: etiqueta de la operación
    # ======================
    
    # Si la máscara son todos T, entonces el procesamiento es de la tabla en
    # general
    if (all(mask)) {
        rr <- apply(tt, 2, op, na.rm = T)
        rr <- if (is.vector(rr)) t(rr) else rr # Si es vector se traspone
        newcol <- 
            paste0(
                label, ".", 
                if (is.null(rownames(rr))) 1:nrow(rr) else rownames(rr)
            )
        return(cbind(op=newcol, as.data.frame(rr)))
    }
    
    # descomponemos la fecha que viene codificada en los
    # nombres de los renglones:
    
    descomp <- do.call(rbind, strsplit(rownames(tt),split = "-"))
    # Se "blanquean" las columnas con F, en la máscara
    descomp[,!mask] <- blank
    #>>>> y se recompone de acuerdo a la máscara
    #>>>> recomp <- apply((descomp[,mask,drop=F]),1,paste,collapse="-")
    
    # y se recompone:
    recomp <- apply(descomp,1,paste,collapse="-")
    rr <- aggregate(tt,list(agr=recomp),op, na.rm = T)
    # ss <- split(as.data.frame(tt),f = list(agr=recomp))
    # rr <- lapply(ss, function(elt) sapply(elt, op))
    colnames(rr)[1] <- label
    return(rr)
}

arreglaDspl <- function (seccion, Coords, traspose=T) {
    # Arregla para desplegado una tabla
    n <- ncol(seccion) %/% nrow(Coords)
    ee <- cbind(sapply(Coords, rep, each=n),t(seccion)) # data.frame
    rownames(ee) <- sub("\\.", ":", colnames(seccion)) # Para evitar confusiones
    as.data.frame(if (traspose) t(ee) else ee)
}

opera <- function() {
    # Se abre el cubote
    prefix <- mustGet("Prefijo del Cubote:", "Conchos") # p.ej. Conchos
    fnam <- prefix %,% "_Cubote.RData" 
    load(fnam) # Lista con dos objetos Coords y Cubote
    
    vv <- mustGet("Variable [pp, tmax, tmin]:", inclSet = c("pp", "tmax", "tmin"))
    # seccion <- rbind(t(Cubote$Coords), Cubote$Cubote[,,vv]) # Inicializa la sección
    seccion <- Cubote$Cubote[,,vv]
    repeat { 
        switch(
            mustGet("O)peración, " %,% 
                    "\nS)alir y guardar, \nA)bortar \n ===>", filtra=toupper), 
            O={
                mascara <- evalstr(mustGet("Máscara de agrupamto, p.ej. c(T,T,F)=>"))
                operacion <- evalstr(mustGet("Operación a aplicar, p.ej. sum =>"))
                etq <- mustGet("Nombre su operación=>")
                t0 <- S.aggregate(seccion, mascara, operacion, etq) # Se eliminan lon y lat
                # t0 trae una columna de más al principio que corresponde a la operación
                #    realizada. Haremos que esta sean los nombres de los renglones en
                #    seccion:
                seccion <- as.matrix(t0[,-1]) # Para nuevas operaciones, en caso requerido
                rownames(seccion) <- t0[,1]
                # La estructura de salida:
                ee <- arreglaDspl(seccion, Cubote$Coords, F)
                #   ajustamos dimensiones
                # n <- ncol(seccion) %/% nrow(Cubote$Coords)
                # ee <- cbind(sapply(Cubote$Coords, rep, each=n),t(seccion)) # data.frame
                # rownames(ee) <- colnames(seccion)
                View(ee)
            },
            S={
                saveRDS(ee, prefix %,% "_" %,% vv %,% "_" %,% etq %,% ".rds")
                break
            },
            A={
                break
            },
            print("No se hizo NADA!")
        )
    }
}

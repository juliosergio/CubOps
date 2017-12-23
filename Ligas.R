##########################
# Ligas.R
# 
# Usa JSON y XML
##########################
# library(sofa)
library(jsonlite)

library(xml2)
# YANO>> library(tcltk2)
source("MiniBiblioteca.R")


doFlat <- function(elt) {
    # Aplana un elemento
    L <- as_list(read_html(elt))
    toJSON(c(list(descr=L$body$a[[1]]), attributes(L$body$a)), auto_unbox = T)
}


creaDe_HTML <- function (html.file) {
    # Dado el nombre de un archivo que contiene la descripción HTML
    # de tus ligas (tipo Delicious), lo pasa a una estructura de R
    # teniendo como intermediaria una estructura de JSON
    # html.file: nombre del archivo
    
    oo <- readLines(html.file) # P.ej. "Mi_delicious.html"
    # De aquí interesan solo los que empiezan con <DT> o <DD>
    
    ii <- grep("<DD>|<DT>", oo) 
    oo <- oo[ii]
    
    bb <- NULL
    prv <- 0
    for (i in 1:length(oo)) {
        elt <- oo[i]
        if (grepl("<DT>",elt)) {
            elt <- sub("PRIVATE", " NOTE=\"\" PRIVATE", elt) # Va con NOTE vacía
            bb <- c(bb, sub("<DT>", "", elt)) # Agrega al final
            prv <- prv+1 # el último elemento de bb
        } else { # Es <DD>, o sea una nota que se debe agregar al último
            bb[prv] <- sub("NOTE=\"\"", 
                           paste0(
                               "NOTE=\"",
                               sub("<DD>", "", elt),
                               "\""
                           ),
                           bb[prv]
            )
        }
    }
    
    # Solo para comparar
    #>> jj <- grepl("<DT>", oo)
    #>> aa <- data.frame(
    #>>     oo = oo,
    #>>     bb = character(length(oo)),
    #>>     stringsAsFactors = F
    #>> )
    #>> 
    #>> aa$bb[jj] <- bb
    
    # bb contiene la pura información que necesitamos
    
    xx <- paste0("[", paste(as.character(lapply(bb, doFlat)), collapse=","), "]")
    MisLinks <- fromJSON(xx)[,1:6] # Conversión a data.frame
    
    MisLinks$add_date <- as.POSIXct(as.numeric(MisLinks$add_date), origin="1970-01-01")
    # MisLinks$tags <- StandardText(MisLinks$tags)
    MisLinks$tags <- MisLinks$tags
    
    return(MisLinks)
}

guardaLinks <- function (ss, fn) {
    # Guardemos el Objeto en disco
    # save(MisLinks, file = "MisLinks.RData")
    saveRDS(ss, file=fn)
}

E_SepComma <- "[[:blank:]]*,[[:blank:]]*" # Separador coma

setTest <- function(inSet, TstSet, inMask=T, filterff=StandardText) {
    # Los conjuntos vienen como Strings con sus elementos separados
    # por comas
    # filterff: función de filtrado (estandarizado) de los textos
    #           correspondientes a los tags.
    # ----------------------------
    # YANO>> inMask <- if (is.null(inMask)) rep(T,length(inSet)) else inMask
    inSet <- strsplit(filterff(inSet), E_SepComma)[[1]]
    TstSet <- strsplit(filterff(TstSet), E_SepComma)[[1]]
    patron <- is.element(inSet, TstSet)
    return(all(patron==inMask))
}


reTest <- function(SubTabla, expr, ...) {
    # OBJETIVO: Aplica a c/u de los elementos de la subtabla la expersión regular 'expr'
    #           y sumariza el resultado por renglones (con el operador 'or')
    # Una matriz uniforme
    # el argumento "..." se pasa a grepl, tal cual
    mm <- sapply(SubTabla, as.character)
    apply(mm, 1, function(e) Reduce(function(x,y) (x | grepl(expr,as.character(y), ...)), e, init = F))
}

crea1regDF <- function(descr, href, nota, tags) {
    return (
        data_frame(
            descr    = descr,
            href     = href,
            add_date = Sys.time(),
            note     = nota,
            private  = "0",
            tags     = tags
        )
    )
}

# =============== Visualizador de tablas ===========================
#>>> EL SIGUIENTE CÓDIGO HA SIDO REMOVIDO PORQUE AL PARECER 
#>>> LOS PAQUETES DE tk y tcl YA NO ESTÁN RECIBIENDO SOPORTE
#>>> EN R.
#>>> SIN EMBARGO, LO DEJO COMO ANOTACIÓN, DEBIDO AL USO QUE
#>>> HACE DE LA SOBRECARGA DE OPERACIONES EN EL MODELO OO (S4)
#>>> AL QUE SE HACE REFERENCIA.
#>>> ===============================================================
#>>> tclArrayVar <- function(x = NULL) {
#>>>     # Check argument
#>>>     if (!is.null(x) && !is.vector(x) && length(dim(x))!= 2)
#>>>         stop("Array must be one-dimensional or two-dimensional, or NULL.")
#>>>     
#>>>     # library(tcltk2)
#>>>     
#>>>     # Create the Tcl variable and the R Tcl object
#>>>     n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount + 1
#>>>     name <- paste0("::RTcl", n)
#>>>     l <- list(env = new.env(), nrow = 0, ncol = 0, ndim = 0)
#>>>     assign(name, NULL, envir = l$env)
#>>>     reg.finalizer(l$env, function(env) tcl("unset", ls(env)))
#>>>     class(l) <- "tclArrayVar"
#>>>     
#>>>     # A NULL array
#>>>     if (is.null(x)) {
#>>>         .Tcl(paste0("set ", name, "(0,0) \"\""))
#>>>         l$nrow <- 0
#>>>         l$ncol <- 0
#>>>         l$ndim <- 2
#>>>         return(l)
#>>>     }
#>>>     
#>>>     # A vector, matrix, or data frame
#>>>     if (is.vector(x)) {
#>>>         ndim <- 1
#>>>         x <- as.data.frame(x)
#>>>     } else ndim <- 2
#>>>     
#>>>     # Populate the Tcl array
#>>>     for (i in (1:nrow(x)))
#>>>         for (j in (1:ncol(x)))
#>>>             .Tcl(paste0("set ", name, "(", i, ",", j,") {", x[i, j], "}"))
#>>>     
#>>>     # Process dim names
#>>>     if (nrow(x)) {
#>>>         if (is.null(rownames(x)))
#>>>             rownames(x) <- rep("", nrow(x))
#>>>         for (i in 1:nrow(x))
#>>>             .Tcl(paste0("set ", name, "(", i, ",", 0, ") \"", 
#>>>                         rownames(x)[i], "\""))
#>>>     } 
#>>>     
#>>>     if (ncol(x)) {
#>>>         if (is.null(colnames(x)))
#>>>             colnames(x) <- rep("", ncol(x))
#>>>         for (j in 1:ncol(x))
#>>>             .Tcl(paste0("set ", name, "(", 0, ",", j, ") \"", 
#>>>                         colnames(x)[j], "\""))
#>>>     }
#>>>     
#>>>     l$nrow <- nrow(x)
#>>>     l$ncol <- ncol(x)
#>>>     l$ndim <- ndim
#>>>     l
#>>> }
#>>> 
#>>>> EJEMPLO DE LA SOBRECARGA DE OPERACIONES:
#
#>>> # edit() generic function is defined in the utils package
#>>> edit.tclArrayVar <- function(name, height = 20, width = 10) {
#>>>     # library(tcltk2)
#>>>     
#>>>     win <- tktoplevel()
#>>>     
#>>>     tclArrayName <- ls(name$env)
#>>>     tkwm.title(win, tclArrayName)
#>>>     
#>>>     table <- tk2table(win,
#>>>                       rows = name$nrow + 1, cols = name$ncol + 1,
#>>>                       titlerows = 1, titlecols = 1,
#>>>                       maxwidth = 1000, maxheight = 1000,
#>>>                       drawmode = "fast",
#>>>                       height = height + 1, width = width + 1,
#>>>                       xscrollcommand = function(...) tkset(xscr, ...),
#>>>                       yscrollcommand = function(...) tkset(yscr,...))
#>>>     xscr <-tk2scrollbar(win, orient = "horizontal",
#>>>                         command = function(...) tkxview(table, ...))
#>>>     yscr <- tk2scrollbar(win, orient = "vertical",
#>>>                          command = function(...) tkyview(table, ...))
#>>>     
#>>>     tkgrid(table, yscr)
#>>>     tkgrid.configure(yscr, sticky = "nsw")
#>>>     tkgrid(xscr, sticky = "new")
#>>>     tkgrid.rowconfigure(win, 0, weight = 1)
#>>>     tkgrid.columnconfigure(win, 0, weight = 1)
#>>>     tkconfigure(table, variable = tclArrayName,
#>>>                 background = "white", selectmode = "extended")
#>>>     tkconfigure(table, selectmode = "extended",
#>>>                 rowseparator = "\"\n\"", colseparator = "\"\t\"")
#>>>     tkconfigure(table, resizeborders = "both")
#>>>     print("fin")
#>>>     
#>>> }
#>>> 
#>>> DEFINICION DE OPERACIONES DE EDICION
#>>> `[.tclArrayVar` <- function(object, i, j = NULL) {
#>>>     # library(tcltk2)
#>>>     
#>>>     if (is.null(j) && object$ndim != 1)
#>>>         stop("Object is not a one-dimensional tclArrayVar")
#>>>     if (!is.null(j) && object$ndim != 2)
#>>>         stop("Object is not a two-dimensional tclArrayVar")
#>>>     
#>>>     if (object$ndim == 1) j <- 1
#>>>     tclArrayName <- ls(object$env)
#>>>     tclvalue(paste0(tclArrayName, "(", i, ",", j, ")"))
#>>> }
#>>> 
#>>> `[<-.tclArrayVar` <- function(object, i, j = NULL, value) {
#>>>     # library(tcltk2)
#>>>     
#>>>     if (is.null(j) && object$ndim != 1)
#>>>         stop("Object is not a one-dimensional tclArrayVar")
#>>>     if (!is.null(j) && object$ndim != 2)
#>>>         stop("Object is not a two-dimensional tclArrayVar")
#>>>     
#>>>     if (object$ndim == 1) j <- 1
#>>>     tclArrayName <- ls(object$env)
#>>>     .Tcl(paste0("set ", tclArrayName, "(", i, ",", j, ") ", value))
#>>>     if (i > object$nrow) object$nrow <- i
#>>>     object
#>>> }


# ######################## == test == ###############################
test <- function() {
    switch(
        mustGet(    "C)rea archivo, \nA)grega registros, \n" %,% 
                    "B)usca en estructura, \nP)rueba tags, \n" %,%
                    "M)odifica reg, \nE)limina reg \n ===>", filtra=toupper), 
        
        C={
            narch <- readline("Nombre archivo HTML:")
            MisLinks <- creaDe_HTML(narch)
            guardaLinks(MisLinks, "MisLinks.rds")
        },
        E={
            MisLinks <- readRDS("MisLinks.rds")
            repeat {
                i <- mustGet("Indices de regs. a eliminar:", stay = F)
                if (i == "") break
                i <- evalstr(i)
                MisLinks <- MisLinks[-i,]
            }
            rownames(MisLinks) <- 1:nrow(MisLinks)
            guardaLinks(MisLinks, "MisLinks.rds")
        },
        M={
            MisLinks <- readRDS("MisLinks.rds")
            repeat {
                i <- mustGet("Indice de reg. a modificar:", stay = F)
                if (i == "") break
                i <- evalstr(i)
                dd <- MisLinks[i,]
                hh <- mustGet("[" %,% dd$href %,% "]  " %,% "Liga (href):", stay = F)
                hh <- if (hh == "" | grepl("^http",hh)) hh else "http://" %,% hh
                tt <- mustGet("[" %,% dd$descr %,% "]  " %,% "Titulo:", stay = F)
                tg <- mustGet("[" %,% dd$tags %,% "]  " %,% "Sus tags separados por \",\" :", stay = F)
                nn <- mustGet("[" %,% dd$note %,% "]  " %,% "Sus notas:", filtra = cambiaNLs, stay = F) # Incluye <NEWLINE>s

                dd <- data_frame(
                    descr    = (if (tt=="") dd$descr else tt),
                    href     = (if (hh=="") dd$href else hh),
                    add_date = Sys.time(),
                    note     = (if (nn=="") dd$note else nn),
                    private  = "0",
                    tags     = (if (tg=="") dd$tags else tg)
                )
                MisLinks[i,] <- dd
            }
            guardaLinks(MisLinks, "MisLinks.rds")
        },
        A={
            MisLinks <- if (file.exists("MisLinks.rds")) readRDS("MisLinks.rds") else NULL
            repeat {
                hh <- mustGet("Liga (href):", stay = F)
                if (hh == "") break
                hh <- if (grepl("^http",hh)) hh else "http://" %,% hh
                tt <- mustGet("Titulo:", stay = F)
                tg <- mustGet("Sus tags separados por \",\" :", stay = F)
                nn <- mustGet("Sus notas:", filtra = cambiaNLs, stay = F) # Incluye <NEWLINE>s
                dd <- data_frame(
                    descr    = tt,
                    href     = hh,
                    add_date = Sys.time(),
                    note     = nn,
                    private  = "0",
                    tags     = tg
                )
                if (is.null(MisLinks)) {
                    MisLinks <- dd
                } else 
                    MisLinks[nrow(MisLinks)+1,] <- dd
            }
            guardaLinks(MisLinks, "MisLinks.rds")
        },
        B={
            MisLinks <- readRDS("MisLinks.rds")
            repeat {
                cols <- readline("Columnas FMT:c(i,...) o '*'=todas :")
                if (cols == "") break
                cols <- if(cols=="*") 1:ncol(MisLinks) else evalstr(cols)
                re <- readline("RegExpr:")
                # Con subconjunto de columnas
                ii <- reTest(MisLinks[,cols, drop=F], re)
                subLinks <- MisLinks[ii,]
                # MisSubL <- tclArrayVar(subLinks)
                # edit(MisSubL)
                View(subLinks)
            }
        },
        P={
            MisLinks <- readRDS("MisLinks.rds") #, envir = .GlobalEnv)
            repeat {
                unSet <- readline("Etiquetas, separadas por \",\"=>")
                if (unSet == "") break
                msk <- readline("Mascara (opcional)->")
                if (msk != "")
                    msk <- evalstr(msk) # Un vector de lógicos
                else 
                    msk <- T
                ii <- sapply(MisLinks$tags, function(tags) setTest(unSet, tags, msk)) # Los índices que hacen match
                subLinks <- MisLinks[ii,]
                # MisSubL <- tclArrayVar(subLinks)
                # edit(MisSubL)
                View(subLinks)
            }
        }, 
        print("No se hizo NADA!")
    )
}





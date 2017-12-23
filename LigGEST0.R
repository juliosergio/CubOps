# ========================
# LigGEST.R
# Interfaz para manejar Links
# 
# ========================
library(tcltk2)
if (!exists("LEIDO.MiBiblioteca")) source("../RR/MiBiblioteca.R", chdir = T)
if (!exists("LEIDO.Intercala")) source("../RR/Intercala.R", chdir = T)
source("Ligas.R")
# debugSource("Ligas.R")


#<-- DEFINICIONES
indir <- "./" # (***) Directorio inicial para búsqueda de archivos de datos


#<--END-DEFINICIONES


# Variables de aspecto:
fontHeading <- tkfont.create(family="times",size=40,weight="bold",slant="italic")
fontHeading1<-tkfont.create(family="times",size=20,weight="bold",slant="italic")
fontHeading2<-tkfont.create(family="times",size=14,weight="bold")
fontTextLabel <- tkfont.create(family="times",size=12)
fontFixedWidth <- tkfont.create(family="courier",size=12)

# funciones de utilería para avanzar en renglones y columnas
right <- function(v) {
    return( as.integer(v+c(0,1)) )
}

down <- function(v) {
    return( as.integer(c(v[1]+1,0)) )
}

# Función transformadora:
trnsFunct <- function(f,...) {
    return(function() {f(...)})
}


DespliegaTxt <- function(title,txt) {
    # Despliega el texto contenido en el arreglo de strings "txt"
    onOK <- function()
    {
        tkgrab.release(dlg)
        tkdestroy(dlg)
        if(exists("tt")) tkfocus(tt)
    }
    dlg <- tktoplevel()
    tkwm.title(dlg,title)
    c <- tk2frame(dlg, relief="sunken", borderwidth=10, padding=c(6,6,6,6))
    scr <- tk2scrollbar(c,  orientation="vertical", command=function(...)tkyview(Mtxt,...))
    Mtxt <- tk2text(c,bg="white", yscrollcommand=function(...)tkset(scr,...))
    for (line in txt) {
        tkinsert(Mtxt,"end",paste(line,"\n"))
        # print(paste("Inserted:", line))
    }
    tkgrid(Mtxt,scr)
    tkgrid.configure(scr,sticky="ns")
    tkconfigure(Mtxt, state="disabled")
    # tkgrid(tklabel(c, text="       "))
    OK.but     <-tk2button(c,text="    OK    ",command=onOK)
    tkgrid(OK.but, sticky="nsew", padx=250)
    # tkgrid(tklabel(c, text="       "))  
    tkgrid(c, column=0, row=0, sticky="nsew")
    tkfocus(Mtxt)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkdestroy(dlg);if(exists("tt")) tkfocus(tt)})
    tkfocus(dlg)
    tkwait.window(dlg)
}


DespliegaLicencia <- function() {
    DespliegaTxt("Licencia de uso de Compara", readLines("licencia.txt"))
}


LaunchFileSel <- function(ar) {
    tipos <- "{ {Archivos RDS} {.rds} } { {Todos Archivos} * }"
    tclvalue(ar) <- tclvalue(tkgetOpenFile(
        initialdir=indir, 
        filetypes=tipos, 
        title="Elija archivo"))
}

LaunchFileSaveSel <- function() {
    # Funcion que dispara un seleccionador de archivos
    # tclvalue(FileName) <- ""
    tipos <- "{{Archivo RDS} {.rds}} {{Todos los archivos} *}"
    s <- tclvalue(tkgetSaveFile(initialdir=indir, filetypes=tipos, title="Elija archivo para guardar"))    
    s <- if(grepl("rds$")) s else s %,% ".rds"
    # Solo se cambia el valor si se ha seleccionado algo:
    s
}


# Función para terminar
done<-function(){
    tkgrab.release(tt)
    tkdestroy(tt)
    # rm(tt)
}

# ===
ExOp <- function() {
    switch(
        as.integer(tclvalue(tcl(.Tk.ID(OpCB), "current"))) + 1,
        # "1. Tags directed search",
        {
            
        },
        # "2. Regular expr search",
        {
            
        },
        # "3. Add registers",
        {
            
        },
        # "4. Edit registers",
        {
            
        },
        # "5. Delete registers",
        {
            
        },
        # "6. Create links file from HTML (Delicious type)",
        {
            
        },
        # "7. View links file"
        {
            f <- getfileN(arch, "Please, select your links file")
            if (is.null(f)) return()
            MisLinks <- readRDS(f) 
            MisSubL <- tclArrayVar(MisLinks)
            edit(MisSubL, height=20, width=ncol(MisLinks))
        }
    )
}

getfileN <- function(ar, msg="Please, select your file") {
    # Esta función obtiene el nombre del archivo sobre el que se desea operar
    f <- tclvalue(ar)
    if (f == "") {
        tkmessageBox(title="ATTENTION!",message=msg, icon="warning",type="ok")
        return (NULL)
    }
    return (f)
}

getdir <- function(path) paste(head(strsplit(path, "/+")[[1]],-1), collapse = "/") %,% "/"

ExOpMALA <- function() {
    f1 <- getfileN(arch, "Please, select your links file")
    #>> if (is.null(f1)) return()
    indir <- getdir(f1) # actualiza directorio
    i <- as.integer(tclvalue(tcl(.Tk.ID(OpCB), "current"))) + 1
    tt <- EjecutaOp(f1, i)
    msg <-"(Op:" %,% i %,% ") OP. Exitosa!"
    tkmessageBox(title="Operación Exitosa!",message=msg,icon="info",type="ok")
    #>> repeat {
    #>>     fnam <- LaunchFileSaveSel()
    #>>     if (fnam != '') break
    #>>     tkmessageBox(title="ATENCI?N!",message="Indique ARCHIVO DE SALIDA", icon="warning",type="ok")
    #>> }
    #>> writeCsv(tt, file=fnam)
}

PrgmTitle <- "    LigGEST v 1.0    "

# ################# === MAIN PROGRAM ==== ##########################
# 
# posición inicial:
pos <- c(0,0)
# tcl:> El toplevel corresponde a la raíz de ventanas methods::"."
tt <- tktoplevel()
tkwm.title(tt,PrgmTitle)
# En el siguiente frame meteremos todo:
# tcl:> ttk::frame .c -padding "3 3 12 12"
c <- tk2frame(tt, relief="sunken", borderwidth=10, padding=c(6,6,6,6))
ce <- tk2frame(c)
pos1 <- c(0,0)
#------------
imgIMTA <- tkimage.create("photo", file="logoImta.gif") # <-- Sólo acepta gifs
logoIMTA <- tk2label(ce, image=imgIMTA)
tkgrid(logoIMTA, row=pos1[1], column=pos1[2], sticky="w", padx=5, pady=5)
#------------
Titulo <- tklabel(ce, text=PrgmTitle,font=fontHeading1, foreground="blue")
pos1 <- right(pos1)
tkgrid(Titulo, row=pos1[1], column=pos1[2], sticky="nsew", padx=5, pady=5)
#------------
LicenciaBut <- tk2button(ce,text="Licence",command=DespliegaLicencia)
pos1 <- right(pos1)
tkgrid(LicenciaBut, row=pos1[1], column=pos1[2], sticky="e", padx=5, pady=5)
tkgrid(ce, row=pos[1], column=pos[2], columnspan=2)
#------------
div8 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div8, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
#--------------
pos <- down(pos)
Prsnt <- tk2label(c, text="Gestor de Ligas en la Red (Web Links Manager)", font=fontHeading2, justify="center", foreground="#0a457d", background="gray")
tkgrid(Prsnt, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=1)
#------------
div1 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div1, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------ si se requirieran tipos de comparacion p.ej. 
#>>> vv1 <- tclVar("algo")
#>>> rb1 <- tk2radiobutton(c, text="ALGO", variable=vv1, value="algo") #, 
#>>>                       # command=function()tclvalue(vv1)<-"")
#>>> rb2 <- tk2radiobutton(c, text="OTRO", variable=vv1, value="otro") #, 
#>>>                       # command=function()tclvalue(vv1)<-"")
#>>> tkgrid(rb1, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#>>> pos <- right(pos)
#>>> tkgrid(rb2, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
div4 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div4, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#======== OPERACIONES POSIBLES =============
opExTxt <- tk2label(c, text=" OPERATION:", font=fontTextLabel)
pos <- down(pos)
tkgrid(opExTxt, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
#-------------
#-------------
operaciones <- c(
    "1. Tags directed search",
    "2. Regular expr search",
    "3. Add registers",
    "4. Edit registers",
    "5. Delete registers",
    "6. Create links file from HTML (Delicious type)",
    "7. View links file"
)

op <- tclVar(operaciones[7])
OpCB <- tk2combobox(c, state="readonly", textvariable=op, width=29)
pos <- right(pos)
tkgrid(OpCB, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
tkconfigure(OpCB, values=operaciones)
#------------
div52 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div52, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------

#======== ARCHIVO LINKS =============
infoFile <- tk2label(c, text="Links File (.rds):",font=fontTextLabel)
pos <- right(pos)
# tcl:> grid .c.infoFile -column 1 -row 0 -sticky nsew -padx 5
tkgrid(infoFile, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
arch <- tclVar("")
selFileBut <- tk2button(c,text="Select File",command=trnsFunct(LaunchFileSel, arch))
pos <- down(pos)
tkgrid(selFileBut, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
#-------------
FileNameEnt <- tk2entry(c, state="readonly", textvariable=arch)
pos <- right(pos)
tkgrid(FileNameEnt, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
div5 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div5, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------

# ========= BOTONES =========================
pos <- down(pos)
bckgrnd <- tk2label(c, background="#0a457d")
tkgrid(bckgrnd, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=1)
ExBut <- tk2button(c, text="Execute op", command=ExOp)
tkgrid(ExBut, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
#--------------
ExitBut <- tk2button(c, text="Exit", command=done)
pos <- right(pos)
tkgrid(ExitBut, row=pos[1], column=pos[2], sticky="w", padx=5, pady=5)
#------------
div3 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div3, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
#---------------
# tcl:> grid .c -column 0 -row 0 -sticky nsew
tkgrid(c, column=0, row=0, sticky="nsew")

# =========== FOCUS =======================
tkfocus(tt)









# ========================
# CubOps.R
# Interfaz para efectuar operaciones sobre
# campos escalares diversos
# ========================
library(shiny)
library(shinyjs)
library(DT)
source("MiniBiblioteca.R")
source("CollectFilesF.R")
source("OperateCubote.R")
source("Density.R")

# === Estructuras Globales ===
G_reset <- function(rsCubote=T) {
    if(rsCubote) {
        Cubote <<- NULL # Arreglo Global con datos
        Vnames <<- NULL
    }
    subTabla <<- NULL # Un fragmento de Cubote
    displTable <<- NULL # Tabla a desplegar
    qprobs <<- seq(0,1,length.out = 5) # Probabilidades p/quantile
    sqprobs <<- paste(qprobs, collapse = ",")
    iMask <<- c(T,T,T) # Máscara de Año, Mes, Día (Year, Month, Day)
    vtn <<- 1:3
    names(vtn) <<- c("Year", "Month", "Day")
}
G_reset()

#<-- DEFINICIONES
indir <- "./" # (***) Directorio inicial para búsqueda de archivos de datos

tabIni <- data.frame(
    name=c(
        "href",
        "tit",
        "tags",
        "nota"
    ),
    descr=c(
        "Liga (href):", 
        "Título:",
        "Tags separados por ',':",
        "Notas:"),
    value=rep("",4), 
    stringsAsFactors = F
)

tabEdt <- tabIni
tabEdt$value <- c("uno", "dos", "tres", "cuatro")
maxR = 800
cn <- c("descr","href","add_date","note","private","tags")
vtnams <- 1:length(cn)
names(vtnams) <- cn


groupTexts <- function(t0=tabIni, apnd="") {
    # Debe entregar el resultado como una lista
    lapply(
        1:nrow(t0), 
        function(i)
            with(t0[i,],
                textInput(name %,% apnd, descr, value)
                # print(paste(name, descr, value))
            )
    )
}

updateGroupTxts <- function(session, t0=tabEdt, apnd="E") {
    for  (i in 1:nrow(t0)) {
         updateTextInput(session, t0[i,"name"] %,% apnd, value=t0[i,"value"])
    }
}

ArmaJS_InSet <- function (elt, Set, eltIsJSVar = T) {
    # Implementa una expresión en JavaScript
    # para determinar si el elemento elt está
    # en el conjunto Set. 
    # ambos argumentos son strings y el resultado
    # también.
    encl <- if (eltIsJSVar) "" else "'"
    "'" %,% Set %,% "'.match(RegExp(" %,% encl %,%
        elt %,% encl %,%")) != null"
}

stylizedDT <- function(ddf, ...) {
    datatable(
        ddf, 
        options = list(scrollX=T, 
                       style='bootstrap',
                       pageLength = 10), 
        ...
    )
}

manipulaDisplT <- function (input, session, Table) {
    # hh <- hist(Table[-(1:2),as.integer(input$colNum)], plot=F)
    nn <- 1:ncol(Table)
    names(nn) <- names(Table)

    dspTbl <- renderDataTable(stylizedDT(Table))
    tipOp <- renderText("Table")

    updateSelectInput(
        session,
        "colNum",
        choices = nn,
        selected = 1
    )
    
    tb <- Table[-(1:2), 1]
    hh <- hist(tb, plot=F)
    # View(hh)
    # p <- density(tb) #, bw = "SJ")
    # ff0 <- dfunCreate(tb)
    # ff0 <- dSfunCreate(tb) # c/Splines
    
    val <- length(hh$counts)
    updateSliderInput(
        session,
        "binsNum",
        value = val, min = 1, max = 4*val
    )
    
    list(dspTbl=dspTbl, tipOp=tipOp)
}

# =====================================

ui <- fluidPage(
    useShinyjs(),  # Set up shinyjs
    tags$head(tags$style("#dspTbl {white-space: nowrap;}")),
    fluidRow(
        column(4,img(height=80, width=80*591/203, src="logoImtaM.png")),
        column(7,
               h2(em(strong("CubOps:"), "Operaciones sobre campos escalares")),
               offset = 0)
    ),
    hr(),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "op", "Acción",
                c("", 
                  "Colecta Datos",
                  "Lee Archivo",
                  "Guarda Archivo Datos",
                  "Elige Variable",
                  "Operacion"
                  )
            ),
            tags$hr(),
            conditionalPanel(
                condition = "input.op == 'Colecta Datos'",
                fileInput("fdat","Archivo ZIP", accept = "application/zip"),
                fileInput("fdat1", "Info Estaciones (Arch. csv)", accept = "text/csv")
            ),
            conditionalPanel(
                condition = "input.op == 'Guarda Archivo Datos'",
                textInput("fnam", "Nombre Archivo(.rds)", "")
            ),
            conditionalPanel(
                condition = "input.op == 'Lee Archivo'",
                fileInput("fdat0","Archivo rds",accept = c("application/rds", "application/RDS"))
            ),
            conditionalPanel(
                condition = "input.op == 'Elige Variable'",
                selectInput("vname", "Variable a revisar", Vnames)
            ),
            conditionalPanel(
                condition = "input.op == 'Operacion'",
                # selectInput("vname", "Variable a revisar", Vnames),
                selectInput(
                    "op0", "Operación",
                    c("mean",
                      "median",
                      "sum",
                      "min",
                      "max",
                      "range",
                      "quantile"
                    )
                ),
                conditionalPanel(
                    condition = "input.op0 == 'quantile'",
                    textInput("qprobs",
                              "Probabilidades separadas por ','", 
                              value = sqprobs)
                ),
                checkboxGroupInput("iMask", "Agrupamiento", 
                                   vtn, 
                                   vtn[iMask]),
                textInput("etq", "Nombre de su operación")
            ),
            conditionalPanel(
                condition = "input.op != 'Guarda Archivo Datos'",
                wellPanel(actionButton("go1", "Ejecuta")) 
            ),
            conditionalPanel(
                condition = "input.op == 'Guarda Archivo Datos'",
                wellPanel(downloadButton("downloadData", "Descarga Tabla"))
            )
        ),
        mainPanel(
            conditionalPanel(
                condition = "output.tipOp == 'Text'",
                h5(em(strong("CubOps:"), "Operación exitosa")),
                textOutput("txt", container = pre)
            ),
            conditionalPanel(
                condition = "output.tipOp == 'Table'",
                dataTableOutput("dspTbl"),
                wellPanel(downloadButton("downloadST", "Descarga Resultado")),
                fluidRow(
                    column(
                        4, 
                        selectInput(
                            "colNum",
                            "Columna de análisis",
                            ""
                        )
                    ), # column
                    column(
                        4,
                        selectInput(
                            "tGraf",
                            "Tipo de gráfico",
                            c("", "hist", "serie", "boxplot"),
                        )
                    ) # column
                ), # fluidRow
                conditionalPanel(
                    condition = "input.tGraf =='hist'",
                    sliderInput(
                        "binsNum",
                        "Número aprox. bins",
                        value = 25, min = 1, max = 100
                    )
                ),
                plotOutput("plt")
            )
        )
    )
)

server <- function(input, output, session) {
    output$tipOp <- renderText("None")
    outputOptions(output, "tipOp", suspendWhenHidden=F)

    observeEvent(input$go1, {
        # Vop(      # input$op)
        # renderText(
        print("Aquí toy")
        output$tipOp <- renderText("None")
        
        op <- switch(
            input$op, 
            "Colecta Datos"="C",
            #>>  "Guarda Archivo Datos"="G",
            "Lee Archivo"="L",
            "Elige Variable"="E",
            "Operacion"="O"
        )
        switch (
            op,
            C = { # Crea el Cubote a partir de una estructura de
                  # directorios en un archivo ZIP
                  #             Los datos --+        Las Estaciones --+
                  #                         |                         |
                  #                         V                         V
                Cubote <<- creaDe_zip(input$fdat$datapath, input$fdat1$datapath)
                Vnames <<- dimnames(Cubote$Cubote)[[3]]
                updateSelectInput(session, "vname", "Variable a revisar", Vnames)
                # print("CUBOTE:>>" %,% Cubote %,% "<<")
                # displTable <<- stylizedDT(Cubote$Coords)
                txt <- paste0(capture.output(str(Cubote)), "\n")
                output$txt <- renderText(txt)
                output$tipOp <- renderText("Text")
            },
            L = { # Crea la Tabla a partir de un archivo RDS
                Cubote <<- readRDS(input$fdat0$datapath)
                Vnames <<- dimnames(Cubote$Cubote)[[3]]
                updateSelectInput(session, "vname", "Variable a revisar", Vnames)
                txt <- paste0(capture.output(str(Cubote)), "\n")
                output$txt <- renderText(txt)
                output$tipOp <- renderText("Text")
            },
            E = { # Elige la variable a tratar
                G_reset(F)
                subTabla <<- Cubote$Cubote[,,input$vname]
                displTable <<- arreglaDspl(subTabla, Cubote$Coords)
                # Actualizaciones:
                updateTextInput(session, "qprobs", value = sqprobs)
                updateCheckboxGroupInput(session, "iMask", selected = vtn[iMask])
                updateTextInput(session, "etq", value = "")
                vv <- manipulaDisplT(input, session, displTable)
                output$dspTbl <- vv$dspTbl
                output$tipOp <- vv$tipOp
            },
            O = {
                fOp <- get(input$op0) # Cambiamos el texto a función
                # Pero en el caso "quantile" hacemos tratamiento especial
                if (input$op0=="quantile") { 
                    # checamos la sintaxis de input$qprobs por caracteres inválidos
                    if (chkSintx("[^[:digit:],.]", input$qprobs)) {
                        showModal(modalDialog(
                            title = "ERROR de entrada",
                            "Secuencia inválida: intente de nuevo!"
                        ))
                        fOp <- NULL
                    } else {
                        qprobs <<- as.numeric(evalstr("c(" %,% input$qprobs %,% ")"))
                        fOp <- function(x) quantile(x, qprobs)
                    }
                }
                msk <- rep(F, length(vtn))
                msk[as.integer(input$iMask)] <- T
                iMask <<- msk
                
                # LA OPERACIÓN:
                t0 <- S.aggregate(subTabla, msk, fOp, input$etq) # Se eliminan lon y lat
                # t0 trae una columna de más al principio que corresponde a la operación
                #    realizada. Haremos que esta sean los nombres de los renglones en
                #    seccion:
                subTabla <<- as.matrix(t0[,-1]) # Para nuevas operaciones, en caso requerido
                rownames(subTabla) <<- t0[,1]
                # La estructura de salida:
                displTable <<- arreglaDspl(subTabla, Cubote$Coords)
                # output$dspTbl <- renderDataTable(stylizedDT(displTable))
                # output$tipOp <- renderText("Table")
                vv <- manipulaDisplT(input, session, displTable)
                output$dspTbl <- vv$dspTbl
                output$tipOp <- vv$tipOp
            }
        )
    })
    
    observe({
        # La entrada el número de columna, cada vez que cambie
        n <- as.integer(input$colNum)
        if (!is.na(n)) {
            tb <- displTable[-(1:2), n]
            hh <- hist(tb, plot=F)

            val <- length(hh$counts)
            updateSliderInput(
                session,
                "binsNum",
                value = val, min = 1, max = 4*val
            )
        } # if
    }) # observe
    
    observe({
        # La entrada el número de columna, cada vez que cambie
        n <- as.integer(input$binsNum)
        isolate( 
            if (input$colNum != "") { 
                tb <- displTable[-(1:2), as.integer(input$colNum)]
                hh <- hist(tb, breaks = n, plot=F)
                # View(hh)
                p <- density(tb) # , bw = "SJ")
                # ff0 <- dfunCreate(tb)
                # ff0 <- dSfunCreate(tb) # c/Splines
                output$plt <- renderPlot({
                    plot(hh, main = "histograma columna", 
                         freq = F, col="gray", xlim = range(p$x), ylim = range(p$y))
                    lines(p, col="blue", lwd=2)
                    # curve(ff0, col="blue", lwd=2, add=T)
                }) # output$plt
            } # if
        ) # isolate 
    }) # observe
    
    output$downloadData <- downloadHandler(
        filename = function () {
            print("fnam>>" %,% input$fnam)
            nn <- if (grepl("\\.rds$", input$fnam)) input$fnam else input$fnam %,% ".rds"
            #output$txt <- renderText("El objeto se guardó en:" %,% nn)
            #output$tipOp <- renderText("Text")
            return(nn)
        },
        content = function(file) saveRDS(Cubote, file)
    )
    output$downloadST <- downloadHandler(
        filename = function () {
            "Res-" %,% sub(" ", "_", Sys.time()) %,% ".csv"
        },
        content = function(file) write.csv(displTable, file)
    )

    # Lo siguiente es para poder observar cuando se pucha uno de
    # los download buttons
    observe({
        if(is.null(input$rnd)) {
            runjs("
                var click = 0;
                Shiny.onInputChange('rnd', click);
                var dwnldBtn = document.getElementById('downloadData');
                dwnldBtn.onclick = function() {click += 1; Shiny.onInputChange('rnd', click)};
            ")      
        } else if (input$rnd > 0) 
            isolate({
                # Aquí se ha puchado el botón
                nn <- if (grepl("\\.rds$", input$fnam)) input$fnam else input$fnam %,% ".rds"
                output$txt <- renderText("El objeto se guardó en: " %,% nn)
                output$tipOp <- renderText("Text")
                # reseteamos el contador:
                runjs("
                    var click = 0;
                    Shiny.onInputChange('rnd', click);
                ")      
            })
    })
}

shinyApp(ui = ui, server = server)


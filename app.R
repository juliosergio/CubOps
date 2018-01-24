# ========================
# CubOps.R
# Interfaz para efectuar operaciones sobre
# campos escalares diversos
# ========================
library(shiny)
library(shinyjs)
library(DT)
dbg <- F
sss <- if(dbg) debugSource else source

sss("MiniBiblioteca.R")
sss("CollectFilesF.R")
sss("OperateCubote.R")
sss("Density.R")

options(shiny.maxRequestSize= 30*1024^2)

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

# GLOBAL:
#   directorio temporal
ttdir <- tempdir()
#   plot temporal
ttplt <- ttdir %,% "/tmpplt.png"


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
    
    updateSelectInput(session, "tGraf", selected = "")
    
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
                      "var",
                      "sd",
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
            tabsetPanel(
                tabPanel(
                    "Salidas",
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
                        conditionalPanel(
                            condition = "output.plt != null",
                            plotOutput("plt"),
                            wellPanel(downloadButton("downloadPlot", "Descarga Gráfico"))
                        )
                    )
                ), # Salidas
                tabPanel(
                    "Manual", 
                    # Para incluir el manual producido con Rmd, se debe editar
                    # el HTML, y quitar o comentar los tags <html> y </ html>
                    # de lo contrario se descompone todo el documento de Shiny
                    # (todos los conditionalPanel aparecen, sin hacer caso
                    # de su condición)
                    # includeHTML("Manual.html") # 
                    uiOutput("markdown")
                ) # Manual
            ) # tabsetPanel
        ) # mainPanel
    )
)

server <- function(input, output, session) {
    output$tipOp <- renderText("None")
    outputOptions(output, "tipOp", suspendWhenHidden=F)
    
    # El manual de usuario:
    output$markdown <- renderUI({
        src <- normalizePath('Manual.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(ttdir)
        on.exit(setwd(owd))
        knitr::opts_knit$set(root.dir = owd)
        
        tagList(
            HTML(knitr::knit2html(text = readLines(src), fragment.only = TRUE)))
    })

    observeEvent(input$go1, {
        # print("Aquí toy")
        output$tipOp <- renderText("None")
        
        switch (
            input$op,
            "Colecta Datos" = { # Crea el Cubote a partir de una estructura de
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
            }, # --Colecta Datos
            "Lee Archivo" = { # Crea la Tabla a partir de un archivo RDS
                Cubote <<- readRDS(input$fdat0$datapath)
                Vnames <<- dimnames(Cubote$Cubote)[[3]]
                updateSelectInput(session, "vname", "Variable a revisar", Vnames)
                txt <- paste0(capture.output(str(Cubote)), "\n")
                output$txt <- renderText(txt)
                output$tipOp <- renderText("Text")
            }, # --Lee Archivo
            "Elige Variable" = { # Elige la variable a tratar
                G_reset(F)
                subTabla <<- Cubote$Cubote[,,input$vname]
                displTable <<- arreglaDspl(subTabla, Cubote$Coords)
                # Actualizaciones:
                updateTextInput(session, "qprobs", value = sqprobs)
                updateCheckboxGroupInput(session, "iMask", selected = vtn[iMask])
                updateTextInput(session, "etq", value = "")
                # YA EN manipulaDsiplT >>> updateSelectInput(session, "tGraf", selected = "")

                vv <- manipulaDisplT(input, session, displTable)
                output$dspTbl <- vv$dspTbl
                output$tipOp <- vv$tipOp
            }, # --Elige variable
            "Operacion" = {
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
                        fOp <- function(x, ...) quantile(x, qprobs, ...)
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
                vv <- manipulaDisplT(input, session, displTable)
                output$dspTbl <- vv$dspTbl
                output$tipOp <- vv$tipOp
            } # --Operacion
        ) # --switch
    })
    
    observe({
        # La entrada el número de columna, cada vez que cambie
        n <- as.integer(input$colNum)
        isolate (
            if (!is.na(n) && input$tGraf == "hist") {
                tb <- displTable[-(1:2), n]
                hh <- hist(tb, plot=F)
    
                val <- length(hh$counts)
                updateSliderInput(
                    session,
                    "binsNum",
                    value = val, min = 1, max = 4*val
                )
            } # if
        ) # isolate
    }) # observe
    
    observe({
        # La entrada del tipo de Gráfico c/vez que cambie
        tGraf <- input$tGraf
        n <- as.integer(input$binsNum)
        n0 <- as.integer(input$colNum)
        isolate({
            if (tGraf != "" && !is.na(n0)) {
                tb <- displTable[-(1:2), n0]
                ntb <- rownames(displTable)[-(1:2)]
                switch (tGraf,
                    hist = {
                        hh <- hist(tb, breaks = n, plot=F)
                        # View(hh)
                        p <- density(tb) # , bw = "SJ")
                        # ff0 <- dfunCreate(tb)
                        # ff0 <- dSfunCreate(tb) # c/Splines
                        output$plt <- renderPlot({
                            plot(hh, main = "histograma columna", 
                                 freq = F, col="gray", xlim = range(p$x), ylim = range(p$y))
                            lines(p, col="blue", lwd=2)
                            dev.copy(png, ttplt)
                            dev.off()
                            # curve(ff0, col="blue", lwd=2, add=T)
                        }) # output$plt
                    }, # hist
                    serie = {
                        m <- length(tb)
                        # el número máximo de etiquetas a desplegar será 20
                        lbls <- if (m <= 20) ntb else maskChr(ntb, c(1, ceiling(m/20)-1))
                        output$plt <- renderPlot({
                            barplot(tb, names.arg = lbls, las=2)
                            dev.copy(png, ttplt)
                            dev.off()
                        })
                    }, # serie
                    boxplot = {
                        output$plt <- renderPlot({
                            boxplot(tb, col = "#75AADB", pch=19)
                            dev.copy(png, ttplt)
                            dev.off()
                        })
                    }
                )
            } else {
                output$plt <- renderPlot({NULL}) # Para borrar todo plot anterior
            } # --if
        }) # --isolate 
    }) # --observe
    
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
    output$downloadPlot <- downloadHandler(
        filename = function () {
            "Plot-" %,% sub(" ", "_", Sys.time()) %,% ".png"
        },
        content = function(file) file.copy(ttplt, file)
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
    
    # outputOptions(output, "tipOp", suspendWhenHidden=F)
}

shinyApp(ui = ui, server = server)


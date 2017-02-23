# ---- dependencies ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(c("dplyr", "shiny", "shinyBS", "pander", "tibble", "shinythemes", 
                 "devtools", "DT", "RCurl"), 
               character.only = T)

eval(parse(
  text = getURL(
    "https://raw.githubusercontent.com/annakrystalli/rmacroRDM/master/R/functions.R", 
    ssl.verifypeer = FALSE)
  )
  )

# ---- setup ----
if(exists("file_setup_path")){}else{
  file_setup_path <- "file_setup.R"
  source(file_setup_path)}
tab_cex <- "font-size:80%; word-wrap: break-word; height:40px"

# get data
ds <- init_db(script.folder, data.folder, spp.list_src = NULL, return.list = T)
sr <- load_sys.ref(view = F, return.list = T)
data_log <- sr$data_log
metadata <- sr$metadata
vnames <- sr$vnames
fileEncodings <- sr$fileEncodings

# input options
csv.files <- setNames(data_log$dcode, data_log$csv_file.name)
enc_files <- c(setNames(paste0(ds$input.folder,"pre/", get_file.paths()),
                        names(get_file.paths())),
               setNames(c(ds$metadata.path, ds$data_log.path, ds$vnames.path),
                        c("metadata", "data_log", "vnames")))
columns <- names(data_log)[!names(data_log) %in% c("dcode", "csv_file.name")]
names(columns) <- columns
numCols <- as.integer(length(columns))
meta.var_columns <- columns[columns %in% paste(ds$fcodes, "file.name", sep = "_")]
names(meta.var_columns) <- gsub("_file.name", "", names(meta.var_columns))
vname.cols <- names(vnames)[!names(vnames) %in% c("code", "meta")]


# ---- ui ----
ui = fluidPage(theme = shinytheme("cerulean"),
               mainPanel(
                 tabsetPanel(
                   
                   # ---- data_log ----
                   tabPanel("Data log",
                            h1("Configure data_log"),
                            bsButton("BUTupdateD", "Update data_log"),
                            br(),
                            bsButton("BUTnew", "Edit", style="primary", 
                                     icon = icon("edit", lib = "glyphicon")),
                            bsModal("modalnew", "Edit dataset entry", "BUTnew", size = "large",
                                    selectInput("csv.file", label = "Select csv.file to edit", choices = csv.files, selected = csv.files[1], multiple = FALSE,
                                                selectize = TRUE, width = NULL, size = NULL),
                                    uiOutput("sliders"),
                                    bsButton("BUTsave", "Save changes", style="success", 
                                             icon = icon("check", lib = "glyphicon"))),
                            bsButton("BUTreset", "Reset", 
                                     icon = icon("repeat", lib = "glyphicon")),
                            bsButton("BUTsaveD", "Save data_log changes", style="success", 
                                     icon = icon("check", lib = "glyphicon")),
                            br(),
                            textOutput("text"),
                            h3("current data_log"),
                            div(DT::dataTableOutput('table'), style = tab_cex)),
                   
                   # ---- fileEncoding ----
                   tabPanel("fileEncoding", 
                            h1("Check fileEncodings"),
                            fluidRow(
                              column(4,
                                     bsButton("enc_reset", "Reset encoding", 
                                              block = T, 
                                              icon = icon("repeat", lib = "glyphicon")),
                                     br(),
                                     bsButton("enc_update", "Update fileEncodings",
                                              style="primary", block = T, 
                                              icon = icon("ok-circle", lib = "glyphicon")),
                                     br(),
                                     bsButton("enc_save", "Save changes",
                                              style="success", block = T, 
                                              icon = icon("check", lib = "glyphicon")),
                                     selectInput("csv_enc", label = "Select csv.file to check", choices = enc_files, selected = enc_files[1], multiple = FALSE,
                                                 selectize = TRUE, width = NULL, size = NULL),
                                     uiOutput("enc_select")),
                              column(6, 
                                     h5(textOutput("enc_msg")),
                                     tableOutput('fileEncodings')),
                              column(2,
                                     radioButtons("view", label = h5("view options"),
                                                  choices = list("paginate" = 1, "scroll" = 2),
                                                  selected = 1),
                                     h6("Scroll can be slow. Not recommended for large datasets")),
                              tags$hr()),
                            br(),
                            div(DT::dataTableOutput('enc_table'), style = tab_cex)),
                   
                   # ---- vnames ----
                   tabPanel("vnames",
                            h1("Configure vnames"),
                            bsButton("BUTupdateV", "Update vnames"),
                            h3("current vnames"),
                            br(),
                            bsButton("BUTnew2", "Edit", style="primary", 
                                     icon = icon("edit", lib = "glyphicon")),
                            bsModal("modalnew2", "Edit vnames entry", "BUTnew2", size = "large",
                                    selectInput("csv.file2", label = "Select vnames column to edit", choices = vname.cols, selected = vname.cols[1], 
                                                multiple = FALSE,
                                                selectize = TRUE, width = NULL, size = NULL),
                                    HTML('<hr style="color: purple;">'),
                                    p("selecting an fcode creates an input box for each column name available in the corresponding file"),
                                    textOutput("error"),
                                    tableOutput("dup.df"),
                                    bsButton("BUTsave2", "Save changes", style="success", 
                                             icon = icon("ok-circle", lib = "glyphicon")),
                                    uiOutput("sliders2"),
                                    uiOutput("sliders3"),
                                    bsButton("BUTsave2", "Save changes", style="success", 
                                             icon = icon("ok-circle", lib = "glyphicon"))),
                            bsButton("BUTreset2", "Reset", 
                                     icon = icon("repeat", lib = "glyphicon")),
                            bsButton("BUTsaveD2", "Save vnames", style="success", 
                                     icon = icon("check", lib = "glyphicon")),
                            br(),
                            textOutput("text2"),
                            div(DT::dataTableOutput('vnames'), style = "font-size:90%")))))
# ---- server ----
server = function(input, output, session) {
  
  # ---- v:reactives ----
  v <- reactiveValues(data_log = data_log, msg = "", vnames = vnames, msg2 = "", error = "",
                      format = "wide", fileEncodings = fileEncodings, enc_msg = "", 
                      enc_table = NULL, input.values = NULL, options = list(bPaginate = T),
                      input.vars = NULL, inputs = NULL, enc_table = NULL,
                      dup.df = NULL, dat = NULL,
                      vars = NULL, l.vars = NULL, tmp_vnames = NULL)
  
  observeEvent(input$BUTupdateD, {
    source("/Users/Anna/Documents/workflows/rmacroRDM/R/functions.R", local = T)
    if(!identical(v$data_log, update_data_log())){
      v$data_log <- update_data_log()
      v$msg <- "data_log updated from FS. Unsaved changes in browser data_log"
    }
  })
  
  var.exists <- reactive({is.na(vnames[,input$csv.file2][vnames$code == "var"])})
  
  # ---- data-log ---- 
  output$table <- DT::renderDataTable({data_log})
  
  
  output$sliders <- renderUI({
    lapply(1:numCols, function(i) {
      column <- columns[i]
      names(column) <- names(meta.var_columns)[meta.var_columns == column]
      if(column %in% meta.var_columns){
        files <- list.files(paste(input.folder, "pre/",
                                  names(meta.var_columns)[meta.var_columns == column],
                                  "/", sep = "")) %>%
          grep(pattern = "Icon\r", inv=T, value=T)
        
        files <- files[!files %in% data_log[,column]]
        current <- data_log[data_log$dcode == input$csv.file, column]
        
        empty <- NA
        files <- c(current, empty, files)
        
        selectInput(paste("col", i, sep = ""), label = column, choices = files, selected = current, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL)
      }else{
        if(column == "format"){
          selectInput(paste("col", i, sep = ""), label = column, choices = c("wide", "long"), selected = NULL, multiple = FALSE,
                      selectize = TRUE, width = NULL, size = NULL)
        }
        current <- data_log[data_log$dcode == input$csv.file, column]
        textInput(paste0("col", i), label = column, value = current)
      }
    })
  })
  
  observeEvent(input$BUTsave, {
    if(is.null(input$BUTsave)){data_log}else{
      tmp_data_log <- v$data_log
      for(i in 1:numCols){
        cat("Showing",paste0("col", i), input[[paste0("col", i)]], "\n")
        column <- columns[i]
        tmp_data_log[tmp_data_log$dcode == input$csv.file, column] <- input[[paste0("col", i)]]}
      
      v$data_log <- tmp_data_log
      v$data_log
      v$msg <- "unsaved changes in browser data_log"
    }
  })
  
  output$table <- DT::renderDataTable({
    v$data_log
  }) 
  
  observeEvent(input$BUTreset,{
    v$data_log <- read.csv(file = paste(input.folder, "metadata/data_log.csv", sep = ""),
                           stringsAsFactors = F, fileEncoding = v$fileEncodings["data_log", "encoding"], 
                           na.strings = ds$na.strings, strip.white = T, 
                           blank.lines.skip = T)
    v$msg <- "browser data_log reset to file"
  })
  
  observeEvent(input$BUTsaveD,{
    v$msg <- "file data_log synced to browser"
    write.csv(v$data_log, file = paste(input.folder, "metadata/data_log.csv", sep = ""),
              row.names = F, fileEncoding = v$fileEncodings["data_log", "encoding"])
  })
  
  output$text <- renderText({ 
    v$msg
  })
  
  # ---- fileEncodings ----
  output$enc_select <- renderUI({
    selected <- v$fileEncodings[names(enc_files)[enc_files == input$csv_enc], "encoding"]
    selectInput("encodingIN", label = "select input file encoding", 
                choices = iconvlist(), 
                selected = selected, 
                multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
  })
  
  
  output$fileEncodings <- renderTable({v$fileEncodings})
  
  observeEvent(input$encodingIN, {
    if(is.null(input$csv_enc)){csv_enc <- enc_files[1]}else{csv_enc <- input$csv_enc}
    selected <- v$fileEncodings[names(enc_files)[enc_files == csv_enc], "encoding"]
    v$enc_table <- read.csv(csv_enc, header = T, strip.white = T, stringsAsFactors = F,
                            na.strings = ds$na.strings, blank.lines.skip = TRUE,
                            fileEncoding = input$encodingIN, check.names = F)
    if(input$encodingIN != selected){
      v$enc_msg <- "encoding changed in browser. Unsaved changes."
    }
  })
  
  # ---- buttons ----
  observeEvent(input$enc_update, {
    if(is.null(input$enc_update)){}else{
      dcode <- names(enc_files)[enc_files == input$csv_enc]
      v$fileEncodings[dcode,  "encoding"] <- input$encodingIN
      v$fileEncodings[dcode,  "assessment"] <- "user" 
      v$enc_msg <- "vnames unsynced from disk. Unsaved changes in browser"
    }
  })
  observeEvent(input$enc_save, {
    if(is.null(input$enc_save)){}else{
      fileEncodings <- v$fileEncodings
      save(fileEncodings, file = ds$fileEncodings.path)
      v$enc_msg <- "changes saved to disk."
    }
  })
  
  observeEvent(input$enc_reset, {
    if(is.null(input$enc_reset)){}else{
      load(ds$fileEncodings.path)
      v$fileEncodings <- fileEncodings
      v$enc_msg <- "fileEncodings reset to disk."
      output$enc_select <- renderUI({
        selected <- v$fileEncodings[names(enc_files)[enc_files == input$csv_enc], "encoding"]
        selectInput("encodingIN", label = "select input file encoding", 
                    choices = iconvlist(), 
                    selected = selected, 
                    multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
      })
    }
  })
  
  observeEvent(input$view, {
    if(input$view == 1){v$options <- list(bPaginate = T)}
    if(input$view == 2){v$options <- list(sScrollX = "800px", 
                                          sScrollY = "400px", 
                                          bPaginate = FALSE)}
    })
  
  # ---- outputs ----
  output$enc_msg <- renderText(v$enc_msg)
  #output$enc_table <- DT::renderDataTable({if(is.null(v$enc_table)){}else{v$enc_table}})
  output$enc_table <- DT::renderDataTable(datatable({if(is.null(v$enc_table)){}else{v$enc_table}},
                                                    options = v$options,
                                                    filter = "top"))
  
  
  # ---- vnames ----
  observeEvent(input$BUTupdateV, {
    #source("/Users/Anna/Documents/workflows/rmacroRDM/R/functions.R", local = T)
    print(identical(v$vnames, update_vnames()))
    if(!identical(v$vnames, update_vnames())){
      v$vnames == update_vnames()
      v$msg2 <- "vnames unsynced from disk. Unsaved changes in browser"
      v$vnames <- update_vnames()
      
      #print(v$vnames)
    }
  })
  
  
  output$vnames <- DT::renderDataTable(vnames, filter = "top",
                                       options = list(sScrollX = "800px", 
                                                      sScrollY = "400px", 
                                                      bPaginate = FALSE))
  
  output$sliders2 <- renderUI({
    data_log <- v$data_log
    vnames <- v$vnames
    
    if(is.na(data_log$format[data_log$dcode == paste0("D", substring(input$csv.file2,2))])){
    }else{
      v$format <- data_log$format[data_log$dcode == paste0("D", substring(input$csv.file2,2))]
    }
    
    fcode <- ds$fcodes[names(ds$fcodes) == gsub('[0-9]+', '',input$csv.file2)]
    path <- paste0(ds$input.folder, "pre/", fcode, "/",
                   data_log[data_log$dcode == paste0("D", substring(input$csv.file2,2)), 
                            paste0(fcode, "_file.name")])
    load(ds$fileEncodings.path)
    selected <- fileEncodings[names(enc_files) == input$csv.file2, "encoding"]
    dat <- read.csv(path, 
                    header = T, strip.white = T, stringsAsFactors = F,
                    na.strings = ds$na.strings, blank.lines.skip = TRUE,
                    fileEncoding = selected, check.names = F)
    
    dat <-  dat[,!sapply(dat, function(x)all(is.na(x)))]
    vars <- names(dat)
    vars <- setNames(make.names(vars), vars)
    v$vars <- vars
    v$dat <- dat
    
    numCols2 <- as.integer(length(vars))
    
    if(v$format == "long"){
      choices2 <- c(NA, ds$master.vars, ds$taxo.vars)
    }
    if(v$format == "wide"){
      choices2 <- c(NA, vnames$code)
    }
    
    lapply(1:numCols2, function(i) {
      var <- vars[i]
      current <- if(!var %in% vnames[,input$csv.file2]){NA}else{
        vnames$code[vnames[,input$csv.file2] == names(var)]}
      
      selectInput(paste("i_", var, sep = ""), label = names(var), choices = choices2, 
                  selected = current, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
    })
    
    
  })
  
  # ---- edit-box ----
  output$sliders3 <- renderUI({
    
    vnames <- v$vnames
    data_log <- v$data_log
    dat <- v$dat
    
    
    if(is.na(v$format)){return()}
    if(v$format == "wide"){
      v$l.vars <- NULL
      return()}
    var.name <- vnames[,input$csv.file2][vnames$code == "var"]
    if(is.na(var.name)){return()}else{
      
      choices3 <- c(NA, vnames$code[!vnames$code %in% c(ds$master.vars, ds$taxo.vars)])
      
      l.vars <- na.omit(unique(dat[, var.name]))
      l.vars <-  setNames(make.names(l.vars), l.vars)
      
      v$l.vars <- l.vars
      
      lapply(1:length(l.vars), FUN = function(i){
        l.var <- l.vars[i]
        current <- if(!names(l.var) %in% vnames[,input$csv.file2]){NA}else{
          vnames$code[vnames[,input$csv.file2] == names(l.var)]
        }
        selectInput(paste("i_", l.var, sep = ""), label = names(l.var), choices = choices3, 
                    selected = current, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL)
      })
    }
  })
  
  # ---- reactive-func ----
  get_inputs <- reactive({
    current.inputs <- paste0("i_", c(v$vars, v$l.vars))
    all.inputs <- names(input)[grep("i_",names(input))]
    all.inputs[all.inputs %in% current.inputs]
  })
  
  get_input.vars <- reactive({
    all.vars <- c(v$vars, v$l.vars)
    inputs <- v$inputs
    all.vars[match(gsub("i_", "",inputs), all.vars)]
  })
  
  get_input.values <- reactive({
    inputs <- v$inputs
    input.vars <- v$input.vars
    input.values <- NULL
    
    for(j in 1:length(input.vars)){
      if(is.null(input[[inputs[j]]])){}else{
        if(any(is.na(input[[inputs[j]]]), input[[inputs[j]]] == "NA")){
          add <- NA 
          names(add) <- names(input.vars[j])
          input.values <- c(input.values, add)
        }else{
          add <- input[[inputs[j]]]
          names(add) <- names(input.vars[j])
          input.values <- c(input.values, add)
        }
      }
    }
    input.values
  })
  
  get_tmp_vnames <- reactive({
    inputs <- v$inputs
    input.vars <- v$input.vars
    input.values <- v$input.values
    tmp_vnames <- v$vnames
    
    for(j in 1:length(input.vars)){
      if(is.null(input.values[j])){
      }
      if(is.na(input.values[j])){
        tmp_vnames[, input$csv.file2][tmp_vnames[, input$csv.file2] == names(input.vars[j])] <- NA
      }else{
        tmp_vnames[tmp_vnames$code == input.values[j], 
                   input$csv.file2] <- names(input.vars[j])
      }
    }
    tmp_vnames[tmp_vnames == "NA"] <- NA
    tmp_vnames
  })
  
  any_dups <- reactive({any(duplicated(na.omit(v$input.values)))})
  
  get_dups <- reactive({
    if(any_dups()){
      dups <- na.omit(v$input.values[duplicated(v$input.values)])
      dup.df <- data_frame(code = v$input.values[v$input.values %in% dups],
                           file = names(v$input.values)[v$input.values %in% dups])
      dup.df
    }else{NULL}
  })
  
  # ---- observe ----
  observeEvent(input$BUTsave2, {
    if(is.null(input$BUTsave2)){}else{
      v$inputs <- get_inputs()
      v$input.vars <- get_input.vars()
      v$input.values <- get_input.values()
      
      v$tmp_vnames <- get_tmp_vnames()
      if(any_dups()){
        v$error <- "ERROR: duplicate file vnames attempted to be assigned to single var code"
      }else{
        v$error <- "" }
    }
  })
  
  output$dup.df <- renderTable({
    get_dups()
  })
  
  output$error <-  renderText(v$error)
  
  output$text2 <- renderText({ 
    v$msg2
  })
  
  observeEvent(input$BUTsave2,{
    if(!any_dups()){
      v$vnames <- v$tmp_vnames
      v$msg2 <- "unsaved changes in browser vnames"}
    v$inputs <- get_inputs()
    v$input.vars <- get_input.vars()
    v$input.values <- get_input.values()})
  
  output$vnames <- DT::renderDataTable({
    v$vnames}, filter = "top",
    options = list(sScrollX = "800px", sScrollY = "400px", bPaginate = FALSE)) 
  
  observeEvent(input$BUTreset2,{
    load(ds$fileEncodings.path)
    selected <- fileEncodings[names(enc_files) == input$csv.file2, "encoding"]
    v$vnames <- read.csv(file = ds$vnames.path,
                         stringsAsFactors = F, fileEncoding = selected, 
                         na.strings=ds$na.strings, strip.white = T, 
                         blank.lines.skip = T)
    v$msg2 <- "browser vnames reset to file"
  })
  
  observeEvent(input$BUTsaveD2,{
    v$msg2 <- "file vnames synced to browser"
    write.csv(v$vnames, file = ds$vnames.path,
              row.names = F, fileEncoding = v$fileEncodings["vnames", "encoding"])
  })
  
  
}


runApp(list(ui = ui, server = server))

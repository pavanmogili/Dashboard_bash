library(shiny)
library(shinydashboard)
#library(reticulate)
library(shinyWidgets)
library(shinybusy)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)




tib<- read.csv("test_demo.csv")
dr <- RPostgreSQL::PostgreSQL()
cn <- dbConnect(
  dr,
  dbname = "postgres",
  host = "pubmed-annotated.ccug0inc2wxx.us-east-1.rds.amazonaws.com",
  user = "postgres",
  password = "AffiniaPubmed",
  port='5432'
)

tib3 <- dbGetQuery(cn, paste0("SELECT * FROM upload_data"))
dbDisconnect(cn)



ui <- dashboardPage(
  dashboardHeader(title = "AAV SAR Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pubmed/Patent DB",tabName = "database", icon = icon("th")),
      menuItem("Upload File",tabName = "upload", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    
    tabItems(
      tabItem(tabName = "database",
              fluidRow(
                column(width = 4,
                box(
                  title = "Filter", width = 12, solidHeader = TRUE, status = "primary",
                  selectizeGroupUI(
                    id = "my-filters",
                    inline = FALSE,
                    params = list(
                      var_two = list(inputId = "Serotype", title = "Select Standard Serotype", placeholder = 'Serotype'),
                      var_one = list(inputId = "Position", title = "Select Location", placeholder = 'Position'),
                      
                     # var_three = list(inputId = "Serotype", title = "Select Serotype", placeholder = 'sereotype'),
                      var_three = list(inputId = "Sub_Function", title = "Select Receptor", placeholder = 'Sub_Function')
                    #  var_five = list(inputId = "Functions", title = "Select Functions", placeholder = 'functions'),
                   #   var_six = list(inputId = "Standard_action", title = "Select Standard Action", placeholder = 'action'),
                    #  var_seven = list(inputId = "Action", title = "Select Action", placeholder = 'action')
                    #  var_eight = list(inputId = "VR.REGION", title = "Select VR Region", placeholder = 'action'),
                    )
                  )
                
                
                ),
                box(
                  title = "Field Selection", width = 12, solidHeader = TRUE, status = "primary",
                  checkboxGroupInput("variable", "Field names",
                                     names(tib), selected = 
                                       c( "Serotype", "Position","Sequence","Sub_Function"))
                )),
                column(width = 8,
                box(
                  title = "Table", width = 12, solidHeader = TRUE, status = "warning",
                  DT::dataTableOutput("table")
                ),
                box(
                  title = "Info", width = 12, solidHeader = TRUE, status = "warning",
                  actionButton("do", "Flag Sentence"),
                  textOutput("selected_var"),
                  textOutput("selected_var2"),
                  textOutput("selected_var3"),
                  uiOutput("tab")
                 
                ),
                box(
                  title = "Analytics", width = 12, solidHeader = TRUE, status = "warning",
                  
                  plotOutput("plot2"),
                  plotOutput("plot3")
                )
                )
              )
      )
      ,

      tabItem(tabName = "upload",
              fluidRow(
                column(width = 4,
                       box(
                         title = "Upload File", width = 12, solidHeader = TRUE, status = "primary",
                         fileInput("file1", "Choose PDF File",
                                   multiple = TRUE,
                                   accept = c(".pdf"))),
                       box(
                         title = "Filter", width = 12, solidHeader = TRUE, status = "primary",
                         selectizeGroupUI(
                           id = "my-filters2",
                           inline = FALSE,
                           params = list(
                             var_two = list(inputId = "Serotype", title = "Select Standard Serotype", placeholder = 'Serotype'),
                             var_one = list(inputId = "Position", title = "Select Location", placeholder = 'Position'),
                             # var_three = list(inputId = "Serotype", title = "Select Serotype", placeholder = 'sereotype'),
                             var_three = list(inputId = "Sub_Function", title = "Select Receptor", placeholder = 'Sub_Function')
                             #  var_five = list(inputId = "Functions", title = "Select Functions", placeholder = 'functions'),
                             #   var_six = list(inputId = "Standard_action", title = "Select Standard Action", placeholder = 'action'),
                             #  var_seven = list(inputId = "Action", title = "Select Action", placeholder = 'action')
                             #  var_eight = list(inputId = "VR.REGION", title = "Select VR Region", placeholder = 'action'),
                           )
                         )
                       ),
                       box(
                         title = "Field Selection", width = 12, solidHeader = TRUE, status = "primary",
                         checkboxGroupInput("variable2", "Field names",
                                            names(tib3), selected =
                                              c( names(tib3)))
                       )),
                column(width = 8,
                       box(
                         title = "Table", width = 12, solidHeader = TRUE, status = "warning",
                         DT::dataTableOutput("table2")
                       ),
                       box(
                         title = "Previous Uploaded Data", width = 12, solidHeader = TRUE, status = "warning",
                         DT::dataTableOutput("table3")
                       )
                       )
              )
      )
    )
  )
)

server <- function(input, output,session) {
  
  res_mod2 <- callModule(
    module = selectizeGroupServer,
    id = "my-filters2",
    data = tib3,
    vars = c( "Serotype", "Position","Sub_Function"),
    inline = FALSE 
  )
  
  

  print(tib3)
  output$table3 <- DT::renderDataTable({
    
    DT::datatable(
      res_mod2(),
      style = "bootstrap",
      selection = 'multiple',
      extensions = c('Buttons'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     pageLength = 100,
                     #buttons = list('excel'),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     buttons = list(
                       "copy"
                       
                     )
                     # columnDefs = list(list(visible=FALSE, targets)
      ),
      rownames = FALSE,
    )
  })
  
  dbDisconnect(cn)
  
  
  
  
  
 
  options(shiny.maxRequestSize=30*1024^2)
  
  
  observeEvent(input$table_rows_selected, {
  
    print( str(input$table_rows_selected))
    selected <- input$table_rows_selected
    
    time_x = res_mod()[selected, "Action"]
    Sentence_x = res_mod()[selected, "Sentence"]
    location_x = res_mod()[selected, "Location"]
    ID_x = res_mod()[selected, "Id"]
    print(time_x)
    
    
    output$selected_var <- renderText({ 
      paste("Sentence:", Sentence_x)
    })
    output$selected_var2 <- renderText({ 
      paste("Location:", location_x)
    })
    output$selected_var3 <- renderText({ 
      paste("ID:", ID_x)
    })
    link <- paste("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC",toString(ID_x),sep = "" )
    print(ID_x)
    print(link)
    url <- a("Pubmed Central", href=link) 
    output$tab <- renderUI({
      tagList("URL link:", url)
    })
  })
  observe({

    if (is.null(input$file1)) return()
   # if (TRUE) return()
    show_modal_spinner()
    print(input$file1$datapath)
    file.copy(input$file1$datapath, "paper.pdf", overwrite = TRUE)
    print("uploaded")
    system('sh ./test_rpython.sh')
    tib2<- read.csv("paper.csv")
    
    remove_modal_spinner()
    output$table2 <- DT::renderDataTable({

      DT::datatable(
        tib2,
        style = "bootstrap",
        selection = 'multiple',
        extensions = c('Buttons'),
        options = list(scrollY = 650,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE,
                       pageLength = 100,
                       #buttons = list('excel'),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       buttons = list(
                         "copy",
                         list(
                           extend = "collection",
                           text = 'Push to Database',
                           action = DT::JS("function ( e, dt, node, config ) {
                                   Shiny.setInputValue('test', true, {priority: 'event'});
                                }")
                         )
                       )
                       # columnDefs = list(list(visible=FALSE, targets)
        ),
        rownames = FALSE,
      )
    })
    
    observeEvent(input$test, {
      
      dr <- RPostgreSQL::PostgreSQL()
      cn <- dbConnect(
        dr,
        dbname = "postgres",
        host = "pubmed-annotated.ccug0inc2wxx.us-east-1.rds.amazonaws.com",
        user = "postgres",
        password = "AffiniaPubmed",
        port='5432'
      )
      print(dbListTables(cn))
      selected_data <- tib2[input$table2_rows_selected,  ]
      dbWriteTable(cn, "upload_data", selected_data, append = TRUE, row.names = FALSE) #, overwrite = TRUE)
   
      
      tib3 <- dbGetQuery(cn, paste0("SELECT * FROM upload_data"))
      print(tib3)
      output$table3 <- DT::renderDataTable({
        res_mod <- callModule(
          module = selectizeGroupServer,
          id = "my-filters2",
          data = tib3,
          vars = c( "Serotype", "Position","Sub_Function"),
          inline = FALSE 
        )
        DT::datatable(
          res_mod2(),
          style = "bootstrap",
          selection = 'multiple',
          extensions = c('Buttons'),
          options = list(scrollY = 650,
                         scrollX = 500,
                         deferRender = TRUE,
                         scroller = TRUE,
                         # paging = TRUE,
                         pageLength = 100,
                         #buttons = list('excel'),
                         dom = 'lBfrtip',
                         fixedColumns = TRUE,
                         buttons = list(
                           "copy"
                         )
                         # columnDefs = list(list(visible=FALSE, targets)
          ),
          rownames = FALSE,
        )
      })
      
      dbDisconnect(cn)
    })
  })

  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = tib,
    vars = c( "Serotype", "Position","Sub_Function"),
    inline = FALSE 
  )
  
  
 
  observeEvent( 
    input$variable,{
    print(input$variable)
    
  output$table <- DT::renderDataTable({ 
    subset_table <- res_mod()
    
    
    
    x <- subset_table["Serotype"]
    lvls <- unique(unlist(x))
    freq <- sapply(x,
                   function(x) table(factor(x, levels = lvls,
                                            ordered = TRUE)))
    output$plot2<-renderPlot({
      ggplot(data=subset_table["Serotype"],aes(x=Serotype))+geom_bar(stat="count")},height = 400,width = 600)
    output$plot3<-renderPlot({
      ggplot(data=na.omit(subset_table["Sub_Function"]),aes(x=Sub_Function))+geom_bar(stat="count")},height = 400,width = 600)
    # output$plot4<-renderPlot({
    #   ggplot(data=subset_table["Standard_action"],aes(x=Standard_action))+geom_bar(stat="count")},height = 400,width = 600)
    print (freq)
    print(colnames(freq))
    
    byHand <- group_by(subset_table, Sentence) %>%
      summarise_all(funs(paste(unique(.), collapse = ", ")))
    byHand2 <- subset_table[, input$variable, drop = F]
    DT::datatable(
      byHand2,
     # print(nrow(subset_table)),
      style = "bootstrap",
      selection = 'single',
      extensions = c('Buttons'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     # buttons = list('excel'),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     # columnDefs = list(list(visible=FALSE, targets)
                     buttons = list(
                       "copy",
                       list(
                         extend = "collection",
                         text = 'Flag',
                         action = DT::JS("function ( e, dt, node, config ) {
                         }")
                       )
                     )
      ), 
    # DBI::dbWriteTable(con, "trial_db", trial3_data, append = TRUE)
    #  rownames = FALSE,
    )
  }) 
  })
}


shinyApp(ui, server)
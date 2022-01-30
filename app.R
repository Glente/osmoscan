#Packages
list.of.packages <- c("shiny","readr","plotly", "magrittr", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(readr)
library(plotly)
library(magrittr)
library(stringr)

# UI for app
ui<-(pageWithSidebar(
    # title
    headerPanel("Ektacytometry: Osmoscan"),
    
    #input
    sidebarPanel
    (
        # Input: Select a file ----
        
      #File 1
      
        fileInput("file1", "Choose File 1 (CSV)",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
      
             textInput("file1_legend", "Description file 1", "Patient"), 
      
        tags$hr(),
        tags$hr(), 
      
        #File 2
        
        fileInput("file2", "Choose File 2 (CSV)",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
      textInput("file2_legend", "Description file 2", "Control"),
      
      
      # Horizontal line ----
      tags$hr(),
      
      #File 3
      fileInput("file3", "Choose File 3 (CSV)",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      textInput("file3_legend", "Description file 3", "Control 2"),
      # Horizontal line ----
      tags$hr(),
      
      #Options
      
      h3("Options"),
      
      
      
      #Help lines and annotation
      checkboxInput("lines", "Lines", value = TRUE, width = NULL),
      checkboxInput("annotations", "Annotations", value = TRUE, width = NULL),
      
      #CSV file format. Default is EU
      radioButtons("format","CSV format", c("EU" = "EU", "US/UK" = "US"), selected = "EU", inline=T),
      
      # Choose image format for plotly ----
      radioButtons ("image", "Image save format", choices = c(png = "png", jpeg = "jpeg", svg = "svg"), selected = "png", inline=T),
      
      actionButton("browser", "Debug"),
      #tags$script("$('#browser').hide();")
      
          
    ),
    
    # Output ----
    
    mainPanel(
      tabsetPanel(  
        
        tabPanel("Plot",plotlyOutput("plot"),
        
        tags$hr(),     
        icon("hospital"), 
        HTML("<i>Andreas GlenthÃ¸j, Danish Center for Hemoglobinopathies<br></i>"),
        icon("envelope"), 
        tags$a(href="mailto:andreas.glenthoej@regionh.dk", "andreas.glenthoej@regionh.dk"),
        HTML("<br>"),
        icon("github"), 
        tags$a(href="https://github.com/Glente/osmoscan", "Github")
          ),
        
        
        
        tabPanel("Table: Info",
                 h2(textOutput("file1_name")),
                 tableOutput("file1_info"),
                 
                 h2(textOutput("file2_name")),
                 tableOutput("file2_info"),
                 
                 h2(textOutput("file3_name")),
                 tableOutput("file3_info"),
                 
                 ),
        
        
        tabPanel("Table: Values",tableOutput("table1"),tableOutput("table2"),tableOutput("table3"))
      )
    )
))


# Define server logic to read selected file ----
server <- function(input, output) {

  observeEvent(input$browser,{
    browser()
  })
  
  
#Basisudregninger ----    
  
  #Find line with "#"
  
  file1_line <- reactive({req(input$file1)
    read_lines(input$file1$datapath) %>% str_which("#")})
  file2_line <- reactive({req(input$file2)
    read_lines(input$file2$datapath) %>% str_which("#")})
  file3_line <- reactive({req(input$file3)
    read_lines(input$file3$datapath) %>% str_which("#")})
  
  #import O and EI ----
  
    #file1
  file1 <- reactive({
    req(input$file1)
    if(input$format=='EU') {file1 <- read_csv2(input$file1$datapath, skip = (file1_line()-1))    }
    else
    if(input$format=='US') {file1 <- read_csv(input$file1$datapath, skip = (file1_line()-1))    }
    
    file1 <- file1[!(file1$O. > 600),]
    file1 <- file1[!(file1$O. < 100),]
    
  })
  
    #file2
  file2 <- reactive({
    req(input$file2)
    if(input$format=='EU') {file2 <- read_csv2(input$file2$datapath, skip = (file2_line()-1))    }
    else
    if(input$format=='US') {file2 <- read_csv(input$file2$datapath, skip = (file2_line()-1))    }
    
    file2 <- file2[!(file2$O. > 600),]
    file2 <- file2[!(file2$O. < 100),]
    
    })
  
  #file3
  file3 <- reactive({
    req(input$file3)
    
    if(input$format=='EU') {file3 <- read_csv2(input$file3$datapath, skip = (file3_line()-1))    }
    else
    if(input$format=='US') {file3 <- read_csv(input$file3$datapath, skip = (file3_line()-1))    }
    
    file3 <- file3[!(file3$O. > 600),]
    file3 <- file3[!(file3$O. < 100),]
    
  })
  
  ##Generate file info ----
  file1_info <- reactive({
    req(input$file1)
    if(input$format=='EU') {read_csv2(input$file1$datapath, n_max=(file1_line()-1), col_names = c("parameter","value")) }
    else
    if(input$format=='US') {read_csv(input$file1$datapath, n_max=(file1_line()-1), col_names = c("parameter","value")) }
  })

  file2_info <- reactive({
    req(input$file2)
    if(input$format=='EU') {read_csv2(input$file2$datapath, n_max=(file2_line()-1), col_names = c("parameter","value")) }
    else
    if(input$format=='US') {read_csv(input$file2$datapath, n_max=(file2_line()-1), col_names = c("parameter","value")) }
    })
  
  file3_info <- reactive({
    req(input$file3)
    if(input$format=='EU') {read_csv2(input$file3$datapath, n_max=(file3_line()-1), col_names = c("parameter","value")) }
    else
    if(input$format=='US') {read_csv(input$file3$datapath, n_max=(file3_line()-1), col_names = c("parameter","value")) }
  })
  

 #Names of files
  file1_name <- output$file1_name <- renderText({ 
    req(input$file1)
    as.character(file1_info()$value[14]) })
  
  file2_name <- output$file2_name <- renderText({ 
    req(input$file2)
    as.character(file2_info()$value[14]) })
  
  file3_name <- output$file3_name <- renderText({ 
    req(input$file3)
    as.character(file3_info()$value[14]) })
  
  #Dates of files
  file1_date <- output$file1_date <- renderText({ 
    req(input$file1)
    as.character(file1_info()$value[13]) })
  
  file2_date <- output$file2_date <- renderText({ 
    req(input$file2)
    as.character(file2_info()$value[13]) })
  
  file3_date <- output$file3_date <- renderText({ 
    req(input$file3)
    as.character(file3_info()$value[13]) })

  ##Extract ektacytometry data ----
  file1_ekta <- reactive({
    req(input$file1)
    if(input$format=='EU') {file1_ekta <- read_csv2(input$file1$datapath, skip = (file1_line()-8), n_max=7,  col_names = c("parameter","value"))  }
    else
    if(input$format=='US') {file1_ekta <- read_csv(input$file1$datapath, skip = (file1_line()-8), n_max=7,  col_names = c("parameter","value"))  }
  })
  
  
  
#Output  
    output$table1 <- renderTable({
        
      req(input$file1)
      return(file1())
    })
    
    output$file1_info <- renderTable({
      
      req(input$file1)
      
      file1_info <- 
        read_csv2(input$file1$datapath,
                           
                           skip = (file1_line()-8),
                           n_max = 7,
                           col_names = FALSE
                           
      )
      rownames(file1_info) <- file1_info$X1
      file1_info <- as.data.frame(t(as.matrix(file1_info)))
      file1_info = file1_info[-1,]
      file1_info

    })
    
    output$table2 <- renderTable({
        req(input$file2)
        return(file2())
    })
    
    output$table3 <- renderTable({
      req(input$file3)
      return(file3())
    })
    
    output$file2_info <- renderTable({
      
      req(input$file2)
      
      file2_info <- read_csv2(input$file2$datapath,
                            
                                skip = (file2_line()-8),
                                n_max = 7,
                                col_names = FALSE
                                
      )
      rownames(file2_info) <- file2_info$X1
      file2_info <- as.data.frame(t(as.matrix(file2_info)))
      file2_info = file2_info[-1,]
      file2_info
      
    })

    output$table2 <- renderTable({
      req(input$file2)
      return(file2())
    })
    
    output$file3_info <- renderTable({
      
      req(input$file3)
      
      file3_info <- read_csv2(input$file3$datapath,
                              
                              skip = (file3_line()-8),
                              n_max = 7,
                              col_names = FALSE
                              
      )
      rownames(file3_info) <- file3_info$X1
      file3_info <- as.data.frame(t(as.matrix(file3_info)))
      file3_info = file3_info[-1,]
      file3_info
      
    })
    
    
        
    plot1 <- output$plot<-renderPlotly({
    
    req(input$file1)
      
    if (!is.null(input$file1)) { 
        file1 <- file1() 
        fig <- plot_ly(file1, x = ~file1$O., y = ~file1$EI, name = input$file1_legend, type = 'scatter', mode = 'lines', color = I('blue')) 
        
        
      } 
    
    if (!is.null(input$file2)) { 
        file2 <- file2() 
        fig <- fig %>% add_trace(file2, x = ~file2$O., y = ~file2$EI, name = input$file2_legend, type = 'scatter', mode = 'lines', color = I('red'))
    } 
      
    if (!is.null(input$file3)) { 
        file3 <- file3()
        fig <- fig %>% add_trace(file3, x = ~file3$O., y = ~file3$EI, name = input$file3_legend, type = 'scatter', mode = 'lines', color = I('darkgreen'))
      } 
    
    #Layout
      fig <- fig %>% layout(xaxis = list(title = "Osmolality (mOsm/kg)", range = c(100,550)), yaxis = list(title = "Elongation Index (EI)", range = c(0,0.650)), showlegend = TRUE)
      fig <- fig %>%  config(toImageButtonOptions = list(format = input$image, filename = file1_name(), scale = 4, width = 800, height = 600))
      
      #Insert help lines
      if(input$lines==TRUE) {
      
      fig <- fig %>% add_segments(x = 0, xend = file1_ekta()$value[2], y = file1_ekta()$value[1], yend = file1_ekta()$value[1], showlegend = FALSE, line = list(color = I('blue'), dash = 'dot', width = 1)) #EImin
      fig <- fig %>% add_segments(x = file1_ekta()$value[2], xend = file1_ekta()$value[2], y = 0, yend = file1_ekta()$value[1], showlegend = FALSE, line = list(color = I('blue'), dash = 'dot', width = 1)) #Omin
      fig <- fig %>% add_segments(x = 0, xend = file1_ekta()$value[4], y = file1_ekta()$value[3], yend = file1_ekta()$value[3], showlegend = FALSE, line = list(color = I('blue'), dash = 'dot', width = 1)) #EImax
      fig <- fig %>% add_segments(x = file1_ekta()$value[4], xend = file1_ekta()$value[4], y = 0, yend = file1_ekta()$value[3], showlegend = FALSE, line = list(color = I('blue'), dash = 'dot', width = 1)) #Omax
      fig <- fig %>% add_segments(x = 0, xend = file1_ekta()$value[6], y = file1_ekta()$value[5], yend = file1_ekta()$value[5], showlegend = FALSE, line = list(color = I('blue'), dash = 'dot', width = 1)) #EIhyper
      fig <- fig %>% add_segments(x = file1_ekta()$value[6], xend = file1_ekta()$value[6], y = 0, yend = file1_ekta()$value[5], showlegend = FALSE, line = list(color = I('blue'), dash = 'dot')) #Ohyper
      }
      
      if(input$annotations==TRUE) {
      #Insert annotations with data
      fig <- fig %>% add_annotations(x = file1_ekta()$value[2], y = file1_ekta()$value[1], text = paste("EImin =",file1_ekta()$value[1]), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 60, ay = 0)
      fig <- fig %>% add_annotations(x = file1_ekta()$value[2], y = file1_ekta()$value[1], text = paste("Omin =",file1_ekta()$value[2]), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 0, ay = 20)
      fig <- fig %>% add_annotations(x = file1_ekta()$value[4], y = file1_ekta()$value[3], text = paste("EImax =",file1_ekta()$value[3]), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 100, ay = 0)
      fig <- fig %>% add_annotations(x = file1_ekta()$value[4], y = file1_ekta()$value[3], text = paste("Omax =",file1_ekta()$value[4]), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 0, ay = 20)
      fig <- fig %>% add_annotations(x = file1_ekta()$value[6], y = file1_ekta()$value[5], text = paste("EIhyper =",file1_ekta()$value[5]), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 70, ay = 0)
      fig <- fig %>% add_annotations(x = file1_ekta()$value[6], y = file1_ekta()$value[5], text = paste("Omax =",file1_ekta()$value[6]), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 0, ay = 20)
      
      #Area
      fig <- fig %>% add_annotations(x = file1_ekta()$value[4], y = (file1_ekta()$value[3]*2/5), text = paste("Area =",file1_ekta()$value[7]), xref = "x", yref = "y", showarrow = FALSE, ax = 0, ay = 20)
      }
      
      fig
      
        
    })


}

# Create Shiny app ----
shinyApp(ui, server)

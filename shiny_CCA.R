library(shiny)
library(PMA)
library(ggplot2)
library(plotly)
library(DT)

ui <- fluidPage(
  h1("Sparse CCA"),
  tabsetPanel(
    tabPanel(
      title = "Readme",
      "Please follow the instructions below to perform your analysis",
      br(),
      br(),
      "In the 'Inputs' tabEach of them contains a table with rows as samples and columns and features. 
      Note that they must have an equal number of samples, but can have different numbers 
      of features.",
      "Note: A feature is a measureable individual property of a phenomenon being observed 
      such as genes or OTUs.",
      "If you have any question, please send an email to rkpoon22@tamu.edu"
    ),
    tabPanel(
      title ="Inputs",
      fileInput("file1","First data"),
      fileInput("file2","Second data"),
      textInput("name1","Name for the first data"),
      textInput("name2","Name for the second data"),
      numericInput("type1","Number of data class A",value=6,min=2),
      numericInput("type2","Number of data class B",value=6,min=2),
      actionButton("click_table1","Show Data 1"),
      actionButton("click_table2","Show Data 2"),
      actionButton("click_plot","Show Plots")
    ),
    tabPanel(
      title = "Data 1",
      DT::dataTableOutput("table1")
    ),
    tabPanel(
      title = "Data 2",
      DT::dataTableOutput("table2")
    ),
    tabPanel(
      title = "Plots",
      plotlyOutput("plot1",width = "auto", height = "auto"),
      br(),
      br(),
      plotlyOutput("plot2",width = "auto", height = "auto")
    )
  )
  
)

server <-function(input,output){
  input1 <- reactive({
    data <- read.csv(input$file1$datapath)
    data <- t(data[,-1])
    data <- as.matrix(apply(data,2,as.numeric))
    sd1 <- apply(data,2,sd)
    data <- data[,which(sd1 != 0)]
    data
  })
  
  observeEvent(input$click_table1,{
   output$table1 <- DT::renderDataTable({
     read.csv(input$file1$datapath)
   })
  })  
  
  input2 <- reactive({
    data <- read.csv(input$file2$datapath)
    data <- t(data[,-1])
    data <- as.matrix(apply(data,2,as.numeric))
    sd2 <- apply(data,2,sd)
    data <- data[,which(sd2 != 0)]
    data
  })  
  
  observeEvent(input$click_table2,{
  output$table2 <- DT::renderDataTable({
    read.csv(input$file2$datapath)
  })
  })
  
  observeEvent(input$click_plot,{
    ccaScores <- reactive({
    set.seed(1105)
    ccaPerm <- CCA.permute(x = input1(), z = input2(),
                           typex = "standard", typez = "standard", 
                           nperms = 30, niter = 5, standardize = T)
    penXtemp <- ccaPerm$bestpenaltyx
    penZtemp <- ccaPerm$bestpenaltyz
    ccaRslt <- CCA(x = input1(), z = input2(),
                   typex = "standard", typez = "standard",
                   penaltyx = penXtemp, penaltyz = penZtemp,
                   K = 2, niter = 5, standardize = T)
    ccaScoreU <- input1() %*% ccaRslt$u
    ccaScoreV <- input2() %*% ccaRslt$v
    ccaScores <- cbind(ccaScoreU, ccaScoreV)
    colnames(ccaScores) <- c("U1", "U2", "V1", "V2")
    ccaScores <- as.data.frame(ccaScores)
    ccaScores$type = c(rep("class A", input$type1), rep("class B", input$type2))
    ccaScores
  })
  
    myCCAPlot = function(x = U1, y = U2, col = V1, shape = type, data = ccaScores(),
                         xyName = input$name1, coloName = input$name2,
                         textVjust = -1.0, elliLev = 0.6, ...){
      jitterPara = list(...)
      if(!"height" %in% names(jitterPara)){
        jitterPara = c(jitterPara, height = 0.01) 
      } else if(!"width" %in% names(jitterPara)){
        jitterPara = c(jitterPara, width = 0.01)
      }
      x = deparse(substitute(x))
      y = deparse(substitute(y))
      col = deparse(substitute(col))
      shape = deparse(substitute(shape))
      myPlot1 = ggplot(data, aes(x = data[,x], y = data[,y],
                                 col = data[,col], shape = data[,shape])) +
        geom_point(size = 4) +
        scale_color_continuous(name = paste0("First Component \nScores of ",
                                             coloName),
                               low = "blue", high = "red") +
        geom_text(aes(label = rownames(data)),
                  col = "black", size = 5, vjust = textVjust,
                  position = do.call("position_jitter", args = jitterPara)) +
        ## The position_jitter will make the values within a group 
        ## a litter bit separate.
        ## On ther other hand, position_dodge will separate the values between groups.
        scale_x_continuous(paste0("First Component Scores of ",
                                  xyName)) +
        scale_y_continuous(paste0("Second Component Scores of ",
                                  xyName)) +
        labs(title = paste0("Sparse CCA Scores for ", xyName, " as Base")) +
        theme(legend.title = element_text(size = 12),
              plot.title = element_text(size = 16, vjust = 2.0, face = "bold"),
              legend.text = element_text(size = 10)) +
        stat_ellipse(aes(fill = data[,shape]), level = elliLev, alpha = 0.2,
                     geom = "polygon", linetype = 2) +
        scale_fill_discrete(name = "Class",
                            labels = c("class A", "class B")) +
        scale_shape_discrete(name = "Class",
                            labels = c("class A", "class B"))
      myPlot1
    }
  
   output$plot1 <- renderPlotly({
    ggplotly({
      
      myCCAPlot()
    })
    
  })
   
  output$plot2 <- renderPlotly({
    
    ggplotly({
      myCCAPlot(x=V1, y=V2, col=U1, xyName = input$name2, coloName = input$name1)
      
    })
  }) 
  }) 
}

shinyApp(ui = ui, server = server)
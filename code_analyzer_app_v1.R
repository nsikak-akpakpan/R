# install.packages("shiny")
library(shiny)
source("util.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose TXT File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    ),
    mainPanel(
      tableOutput("contents1"),
      tableOutput("contents2"),
      tableOutput("contents3"),
      tableOutput("contents4"),
      tableOutput("contents5"),
	tableOutput("contents6")
    )
  )
)


server <- function(input, output) {
  output$contents1 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    x1 = readLines(inFile$datapath)

    proc_commands = c("sql",
                      "sort",
                      "export",
                      "import",
                      "print",
                      "datasets",
                      "freq",
                      "surveyselect",
                      "contents")
    proc_commands = paste("proc", proc_commands)

    out1 = get_proc_summary(x1, key_cmds = proc_commands)
    names(out1)[1] = "Proc Commands"
    out1

  })
  
  output$contents2 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    x2 = readLines(inFile$datapath)

    out2 = get_proc_summary(x2, key_cmds = c("datafile", "outfile"))
    names(out2)[1] = "Inputs and Outputs"
    out2
    
  })
  
  output$contents3 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    x3 = readLines(inFile$datapath)
    
    out3 = get_proc_summary(x3, key_cmds = c("%include", "%macro"))
    names(out3)[1] = "Dependencies"
    out3
    
  })
  
  output$contents4 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    x4 = readLines(inFile$datapath)
    
    out4 = get_extensions(x4, pattern = "datafile")
    names(out4)[1] = "File Read Extensions"
    out4
    
  })
  
  output$contents5 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    x5 = readLines(inFile$datapath)
    
    out5 = get_extensions(x5, pattern = "outfile")
    names(out5)[1] = "File Write Extensions"
    out5
    
  })

  output$contents6 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    x6 = readLines(inFile$datapath)
    
    out6 = get_libnames(x6)
    # names(out5)[1] = "File Write Extensions"
    out6
    
  })
  
  
}

shinyApp(ui, server)

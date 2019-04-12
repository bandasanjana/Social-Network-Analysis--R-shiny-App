library(shiny)
library(shinydashboard)
library(networkD3)
library(visNetwork)

ui <- 
  dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "Sanjana Banda_Social_Network_Analysis", titleWidth = 400,
                    dropdownMenu(
                      type = "notifications", 
                      icon = icon("question-circle"),
                      badgeStatus = NULL,
                      headerText = "See also:",
                      
                      notificationItem("Viz Network", icon = icon("file"),
                                       href = 'https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html'),
                      notificationItem("shinydashboard", icon = icon("file"),
                                       href = "https://rstudio.github.io/shinydashboard/")
                    )
                    
    ),
    dashboardSidebar(
      sidebarMenu(id = "sidebarmenu",
                  menuItem("Upload Files", tabName = "upload"),
                  menuItem("Tables", tabName = "data"),
                  menuItem("Emails", tabName = "emails"),
                  menuItem("Connections", tabName = "connections"),
                  menuItem("2-hop Neighbours", tabName = "2hopNeighbours"),
                  menuItem("Degree Centrality", tabName = "Degree_Centrality" , 
                             menuSubItem('Table', tabName = 'dcsubtable'),
                           menuSubItem('Network', tabName = 'dcsubnetwork')),
                  menuItem("Betweeness Centrality", tabName = "Betweeness_Centrality", 
                           menuSubItem('Table', tabName = 'bcsubtable'),
                           menuSubItem('Network', tabName = 'bcsubnetwork')),
                  menuItem("Indegree Centrality", tabName = "Indegree_Centrality", 
                           menuSubItem('Table', tabName = 'idcsubtable'),
                           menuSubItem('Network', tabName = 'idcsubnetwork')),
                  menuItem("Department-Wise E-Mails", tabName = "EmailsDept", 
                           menuSubItem('Table', tabName = 'deptsubtable'),
                           menuSubItem('Network', tabName = 'deptsubnetwork')),
                  menuItem("Summary", tabName = "Summary")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "upload",
                fluidRow(
                  box(
                    fileInput("file1", "Upload EU-Core Table",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                    fileInput("file2", "Upload EU-Core-Department Table",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                  )
                )
        ),
        tabItem(tabName = "data",
                fluidRow(
                  box(radioButtons("data", "",c("EU Core" = "ecore","EU Core Department" = "ecoredep"))),
                  tableOutput("dataTable")
                )
        ),
        tabItem(tabName = "emails",
                fluidRow(
                  box(radioButtons("emails", "",c("Emails Sent" = "esent","Emails Received" = "ereceived")),
                      selectizeInput("inSelectEmails", "Select Sender/Receiver ID",c(), multiple = TRUE, selected = NULL)),
                  tableOutput("emailTable")
                )
        ),
        tabItem(tabName = "connections",
                fluidRow(
                  box(sliderInput(inputId = "num", label = "Select Number of Connections", value = 1 ,min = 1, max= 1000)),
                  visNetworkOutput("network0")
                )
        ),
        tabItem(tabName = "2hopNeighbours",
                fluidRow(
                  box(selectInput("inSelect2Hop", "1. Select Input - Top 10 Nodes with Highest Emails Sent/Received", c()),
                      selectizeInput("inSelect2Hop2", "2. Select Input/Multiple Inputs - Corresponding 1-Hop Neighbours of selected Node", c(), multiple = TRUE, selected = NULL)),
                  box(sliderInput(inputId = "num2", label = "3. Select Number of 2-hop Neigbours of the selected Node via selected 1-hop neighbours", value = 1 ,min = 1, max= 100)),
                  visNetworkOutput("network1")
                )
        ),
        tabItem(tabName = "dcsubnetwork",
                fluidRow(
                  box(selectInput("inSelectDC", "1. Select Input - Top 10 Nodes with Highest Degree_Centrality", c()),
                      selectizeInput("inSelectDC2", "2. Select Input/Multiple Inputs - Corresponding 1-Hop Neighbours of selected Node",c(), multiple = TRUE, selected = NULL)),
                  box(sliderInput(inputId = "num3", label = "3. Select Number of 2-hop Neigbours of the selected Node via selected 1-hop neighbours", value = 1 ,min = 1, max= 100)),
                  visNetworkOutput("network2")
                )
        ),
        tabItem(tabName = "dcsubtable",
                fluidRow(
                  tableOutput("dcTable")
                )
        ),
        tabItem(tabName = "bcsubtable",
                fluidRow(
                  tableOutput("bcTable")
                )
        ),
        tabItem(tabName = "idcsubtable",
                fluidRow(
                  tableOutput("idcTable")
                )
        ),
        tabItem(tabName = "deptsubtable",
                fluidRow(
                    
                  tableOutput("deptMailTable")
                )
        ),
        tabItem(tabName = "bcsubnetwork",
                fluidRow(
                  box(selectInput("inSelectBC", "1. Select Input - Top 10 Nodes with Highest Betweeness_Centrality", c()),
                      selectizeInput("inSelectBC2", "2. Select Input/Multiple Inputs - Corresponding 1-Hop Neighbours of selected Node",c(), multiple = TRUE, selected = NULL)),
                  box(sliderInput(inputId = "num4", label = "3. Select Number of 2-hop Neigbours of the selected Node via selected 1-hop neighbours", value = 1 ,min = 1, max= 100)),
                  visNetworkOutput("network3")
                )
        ),
        tabItem(tabName = "idcsubnetwork",
                fluidRow(
                  box(selectInput("inSelectIDC", "1. Select Input - Top 10 Nodes with Highest Indegree_Centrality", c()),
                      selectizeInput("inSelectIDC2", "2. Select Input/Multiple Inputs - Corresponding 1-Hop Neighbours of selected Node",c(), multiple = TRUE, selected = NULL)),
                  box(sliderInput(inputId = "num5", label = "3. Select Number of 2-hop Neigbours of the selected Node via selected 1-hop neighbours", value = 1 ,min = 1, max= 100)),
                  visNetworkOutput("network4")
                )
        ),
        tabItem(tabName = "deptsubnetwork",
                fluidRow(
                  box(selectInput("inSelectDeptwiseMail", "Select Sender Department", c()),
                      sliderInput(inputId = "num1", label = "Select Number of Receiver Departments", value = 1 ,min = 1, max= 100)),
                  visNetworkOutput("network5")
                )
        ),
        tabItem(tabName = "Summary",
                fluidRow(
                   h4(textOutput("txtout"))
                )
        )
      )
    )
  )



#Uploading the Files
server <- function(input, output, session) {

  Data1 <- reactive({
    if (input$file1 == "")
      return(NULL)
    a <- read.csv(input$file1$datapath)
    colnames(a)[1] <- "Sender"
    colnames(a)[2] <- "Reciever"
    return(a)
      })
  
  Data2 <- reactive({ 
    if (input$file2 == "")
      return(NULL)
   
    b <-read.csv(input$file2$datapath)
    colnames(b)[1] <- "ID"
    colnames(b)[2] <- "Department"
    return(b)
    })
  

#Updating the values Dynamically according to the user Input     
  observe({
    updateSelectInput(session, "inSelectEmails",label = lableEmails() ,choices = inSelectEmails())
  })
  observe({
    updateSelectInput(session, "inSelect2Hop", choices = inSelect2hop())
    updateSelectInput(session, "inSelectDC", choices = inSelectDC())
    updateSelectInput(session, "inSelectDCT", choices = inSelectDC())
    updateSelectInput(session, "inSelectBC", choices = inSelectBC())
    updateSelectInput(session, "inSelectIDC", choices = inSelectIDC())
    updateSelectInput(session, "inSelectDeptwiseMail", choices = inSelectDeptwiseMail())
    updateSelectInput(session, "inSelectDeptTable", choices = inSelectDeptwiseMail())
  })
  observe({
    updateSelectInput(session, "inSelect2Hop2", choices = inSelect2hop2())
    updateSelectInput(session, "inSelectDC2", choices = inSelectDC2())
    updateSelectInput(session, "inSelectBC2", choices = inSelectBC2())
    updateSelectInput(session, "inSelectIDC2", choices = inSelectIDC2())
  })
  observe({
    updateSliderInput(session, "num2", max = inSelectnum2())})
  observe({
    updateSliderInput(session, "num3", max = inSelectnum3())})
  observe({
    updateSliderInput(session, "num4", max = inSelectnum4())})
  observe({
    updateSliderInput(session, "num5", max = inSelectnum5())
  })
  observe({
    updateSliderInput(session, "num1", max = inSelectnum1())
  })
 
  
#Ouput for Data Table 
  output$dataTable <- renderTable({
    if(input$data == "ecore"){
      validate(
        need(input$file1, 'Please Upload EU-core file')
      )
      return(Data1()[1:25,])
    }
    else if(input$data == "ecoredep"){
      validate(
        need(input$file2, 'Please Upload EU-core-Department file')
      )
      return(Data2()[1:25,])
    }
    else{
      return(NULL)
    }
  })

  
#Output for Emails Table   
  output$emailTable <- renderTable({
    if(input$emails == "esent") {
      validate(
        need(input$file1, 'Please Upload EU-core file')
      )
      A <- as.data.frame(table(Data1()[,1]))
      colnames(A)[1] <- "Senders_ID"
      colnames(A)[2] <- "No._of_Emails_Sent"
      if(length(input$inSelectEmails) == 0){
        return(A)
      }
      else{
        inselect <- input$inSelectEmails
        return(subset(A, Senders_ID %in% inselect))
      }
    }
    else if(input$emails == "ereceived"){
      validate(
        need(input$file1, 'Please Upload EU-core file')
      )
      B <- as.data.frame(table(Data1()[,2]))
      colnames(B)[1] <- "Reciever_ID"
      colnames(B)[2] <- "No._of_Emails_Recieved"
      
      if(length(input$inSelectEmails) == 0){
        return(B)
      }
      else{
        inselect <- input$inSelectEmails
        return(subset(B, Reciever_ID %in% inselect))
      }
    }
    else{
      return(NULL)
    }
  })

  
#Output for Department-Wise-Email Table  
  output$deptMailTable <- renderTable({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    outputtable <- getDeptMailNetwork()
    inputsel <- input$inSelectDeptTable
    if(length(inputsel) == 0){
      return(outputtable)
    }
    else{
      return(subset(outputtable, Sender_Dept %in% inputsel))
    }
    
  })
  output$dcTable <- renderTable({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file'))
      getDCtable()
  })
  output$bcTable <- renderTable({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file'))
    getBCtable()
  })
  output$idcTable <- renderTable({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file'))
    getIDCtable()
  })


#Output to Display the Connections Depending on the user Input  
  output$network0 <- renderVisNetwork({
    
    validate(
      need(input$file1, 'Please Upload EU core file')
    )
    
    if(length(input$num) == 0){
      return(NULL)
    }
    else{
      New_Data <-rbind(data.frame(Sender = 0, Reciever = 0), Data1())
      x <- New_Data[1:(input$num+1),]
      g <- graph.data.frame(x)
      
      # minimal example
      net <- graph_from_data_frame(d=x, vertices=Data2(), directed=T)
      net <- simplify(g, remove.multiple = F, remove.loops = T)
      data <- toVisNetworkData(net)
      nodes <- data$nodes 
      edges <- data$edges
      
      
      
      visNetwork(nodes, edges) %>% visEdges(arrows = "to" )%>% visNodes(color = list(background = FALSE, border = FALSE,
                                                                                     highlight = "yellow"),shadow = list(enabled = FALSE))%>%visOptions( highlightNearest = TRUE)%>%visPhysics(stabilization = FALSE)
      
    }
    
  }) 
  

  # Output to display Department-wise-email network  
  
  output$network5 <- renderVisNetwork({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    
    #Adding an extra row at the top with zero input value
    final_Output <- getDeptMailNetwork()
    c1 <- vector("integer")
    c2 <- vector("integer")
    x<- length(final_Output$Sender_Dept)
    for(i in 1:x){
      if(input$inSelectDeptwiseMail ==final_Output$Sender_Dept[i]){
        c1 <-c(c1,final_Output$Receiver_Dept[i])
        c2 <- c(c2,final_Output$Number_Of_Emails[i])
      }
    }
    #Creating the Data frame
    s1 <- vector("integer")
    #c <- as.data.frame(value)
    s1 <- c(s1,rep(input$inSelectDeptwiseMail,length(c)))
    output <- as.data.frame(cbind(s1,c1,c2))#Final table with one node and its 2-hop neighbours in dataframe
    colnames(output)[1] <- "Sender_Dept"
    colnames(output)[2] <- "Receiver_Dept"
    colnames(output)[3] <- "Number_Of_Emails"
    
    outputLen <- length(c2)
    
    len1 <- if(input$num1 < outputLen) input$num1 else outputLen
    
    df3 <- output[1:(len1),]
    
    g <- graph.data.frame(df3)
    
  
    net <- graph_from_data_frame(d=df3, vertices=Data2(), directed=T)
    
    
    net <- simplify(g, remove.multiple = F, remove.loops = T)
    
    data <- toVisNetworkData(net)
    nodes <- data$nodes 
    edges <- data$edges
    colnames(edges)[3] <- "value"
   
    
    visNetwork(nodes, edges) %>% visEdges(arrows = "to" )%>% visNodes(color = list(background = FALSE, border = FALSE,
                                                                                   highlight = "yellow"),shadow = list(enabled = FALSE))%>%visOptions( highlightNearest = TRUE)%>%visPhysics(stabilization = FALSE)
    
    
    
  })
  
  #get1HopNeighbours Function
  
  get1HopNeighbours <- function(inputVal){
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    #Converting to another dataframe
    from <- Data1()$Sender
    to <-  Data1()$Reciever
    rel <- data.frame(from, to)
    
    
    #Using the function graph.data.frame display the connections
    g <- graph.data.frame(rel)
    
    validate(need(is.element(inputVal, from), "No connection exists for the node"))
    
    x1 <-unlist((neighborhood(g, 1, inputVal, mode="out")))
    x2 <-names(x1[-1])
    return (x2)
  }
  
  
  
  #get2HopNeighbours function
  
  get2HopNeighbours <- function(inputVal1, inputVal2,inputVal3){
    validate(
      need(inputVal1, ''),
      need(inputVal2, ''),
      need(inputVal3, '')
    )
    #Converting to another dataframe
    from <- Data1()$Sender
    to <-  Data1()$Reciever
    rel <- data.frame(from, to)
    
    
    #Using the function graph.data.frame display the connections
    g <- graph.data.frame(rel)
    
    x2 <- inputVal2
    Reciever <-data.frame(x2)
    sender1 <- as.vector(x2)
    
    s2 <- length(sender1)
    l <- nrow(Reciever)
 
    sender <- c(rep(inputVal1,l))
    
    sender<- as.data.frame(sender)
    
    part1 <-cbind(sender, Reciever)
 
    colnames(part1)[2] <- "Reciever"

    part1$Reciever <- as.integer(as.character((part1$Reciever)))

    
    m <- list()
    value <- vector('integer')
    
    for (i in  1:s2){
      if(is.element(sender1[i], from)){
        y1 <- unlist(neighborhood(g, 1, sender1[i], mode="out"))
        y2 <- names(y1[-1])
        y2 <- as.integer(y2)
        
        outputLen <- length(y2)
        len1 <- if(inputVal3 < outputLen) inputVal3 else outputLen
        
        l <- vector("integer")
        l <- y2[1:len1]
        value <- c(value,rep(sender1[i],length(l)))
        m[[i]] <- l
      }
    }
    
    validate(need(length(m) > 0, "2Hop Neighbour does not exists for this node"))
    df <- ldply (m, data.frame)
    colnames(df)[1] <- "twoHopNeigbours"#Changing the name of the column
    value1 <- as.data.frame(value)#Converting into dataframe
    part2 <- cbind(value1,df)
    colnames(part2)[1] <-"sender"
    colnames(part2)[2] <- 'Reciever'
    
    
    df3 <- rbind(part1,part2)
    
    df3
  }
  
  
  
  #getDeptMailNetwork Function
  
  getDeptMailNetwork <- function(){
    nodes <- Data2()
    links <- Data1()
    
    #Adding a column with Index values 
    links$Index <- seq.int(nrow(links))
    
    #Changing the Order of the column in the dataframe
    links <- links[c("Index", "Sender", "Reciever")]
    
    #Making the copies of dataframe
    links1 <- links
    links2 <- links
    
    #changing the name of the second columnof links1
    colnames(links1)[2] <- "ID"
    colnames(links2)[3] <- "ID"
    
    #Dropped the column receiver
    links1 = subset(links1, select = -c(Reciever) )
    links2 = subset(links2, select = -c(Sender))
    
    #Joining the two tables keeping them in same order using plyr  package
    data1 <-join(links1,nodes,'ID') 
    data2 <- join(links2,nodes,'ID')
    
    #change the column names of dept to send_Dept & Receiver_Dept
    colnames(data1)[3] <- "Send_Dept"
    colnames(data2)[3] <- "Receiver_Dept"
    
    #combining the data1 and data2 to final data
    Final <- cbind(data1,data2)
    
    #Dropping the unrequired columns
    Final =subset(Final, select = -c(Index,ID))
    Final =subset(Final, select = -c(Index,ID))
    
    #Counting the Frequency of between two variables in a table using plyr library
    Output <- ddply(Final, .(Final$Send_Dept, Final$Receiver_Dept), nrow)
    
    #Changing the names of the columns to desired names as below
    names(Output) <- c("Sender_Dept", "Receiver_Dept", "Number_Of_Emails")
    
    #Adding an extra row at the top with zero input value
    final_Output <- Output
    return(final_Output)
  }
  
  
  #getNetworkGraph function
  
  getNetworkGraph <- function(df){
    g <- graph.data.frame(df)
    
    # minimal example
    node1 <- Data2()
    net <- graph_from_data_frame(d=df, vertices=node1, directed=T)
    net <- simplify(g, remove.multiple = F, remove.loops = T)
    data <- toVisNetworkData(net)
    nodes = data$nodes
    
    colnames(node1)[1] <- "id"
    
    Final_Output2 <-merge(x = nodes, y = node1, by = "id", all.x = TRUE)
    colnames(Final_Output2)[3] <- 'group'
    y2 <- Final_Output2
    
    edges = data$edges
    
    visNetwork(y2, edges) %>% visEdges(arrows = "to" )%>% visNodes(color = list(background = FALSE, border = FALSE,
                                                                                highlight = "yellow"),shadow = list(enabled = FALSE))%>%visOptions( highlightNearest = TRUE)%>%visPhysics(stabilization = FALSE)
  }
  
  
  #Calculating Degree Centrality
  
  getDCtable <- function(){
    library(igraph)
    library(dplyr)
    library(plyr)
    
    nodes <- Data2()
    links <- Data1()
    
    #Coverting into another dataframe
    from <- Data1()$Sender
    to <-  Data1()$Reciever
    data <- data.frame(from, to)
    
    #Using the function graph.data.frame display the connections
    data<- graph.data.frame(data)
    
    #Finding the In-Degree Centrality
    y<-(degree(data, v = V(data), mode = c("all"),
               loops = TRUE, normalized = FALSE))
    
    #Coverting to table
    y <- as.table(y)
    
    #Converting to Dataframe
    L <-as.data.frame(y) 
    
    #Setting up the required column names in L and Nodes
    colnames(L)[2] <- "degree_centrality"
    colnames(L)[1] <- "Node"
    colnames(nodes)[1] <-"Node"
    
    #Arranging L in Descending Order
    Output <-L[order(-(L$degree_centrality)),c(1,2)]
    
    #Taking the Top 10 Rows in the output data
    Output<- Output[1:10,]
    Output
  }
  
 
  #Calculating Betweeness Centrality
  
  getBCtable <- function(){
    #Using Required Libaray
    library(igraph)
    library(dplyr)
    library(plyr)
    
    #Setting up the directory and Uploading the files
    
    nodes <- Data2()
    links <- Data1()
    
    #Coverting into another dataframe
    from <- Data1()$Sender
    to <-  Data1()$Reciever
    data <- data.frame(from, to)
    
    #Using the function graph.data.frame display the connections
    data<- graph.data.frame(data)
    
    #Finding the Betweeness Centrality
    y <-betweenness(data, v = V(data), directed = TRUE, weights = NULL,
                    nobigint = TRUE, normalized = FALSE)
    
    #Coverting to table
    y <- as.table(y)
    
    #Converting to Dataframe
    L <-as.data.frame(y) 
    
    #Setting up the required column names in L and Nodes
    colnames(L)[2] <- "Betweeness_centrality"
    colnames(L)[1] <- "Node"
    colnames(nodes)[1] <-"Node"
    
    #Arranging L in Descending Order
    Output <-L[order(-(L$Betweeness_centrality)),c(1,2)]
    
    #Taking the Top 10 Rows in the output data
    Output<- Output[1:10,]
    Output
  }
  
 
  #Calculating In-Degree Centrality
  
  getIDCtable <- function(){
    nodes <- Data2()
    links <- Data1()
    #Coverting into another dataframe
    from <- Data1()$Sender
    to <-  Data1()$Reciever
    data <- data.frame(from, to)
    
    #Using the function graph.data.frame display the connections
    data<- graph.data.frame(data)
    
    #Finding the In-Degree Centrality
    y<-(degree(data, v = V(data), mode = c("in"),
               loops = TRUE, normalized = FALSE))
    
    #Coverting to table
    y <- as.table(y)
    
    #Converting to Dataframe
    L <-as.data.frame(y) 
    
    #Setting up the required column names in L and Nodes
    colnames(L)[2] <- "in_degree_centrality"
    colnames(L)[1] <- "Node"
    colnames(nodes)[1] <-"Node"
    
    #Arranging L in Descending Order
    Output <-L[order(-(L$in_degree_centrality)),c(1,2)]
    
    #Taking the Top 10 Rows in the output data
    Output<- Output[1:10,]
    Output
  }
  
  
  lableEmails <- reactive({
    if(input$emails == "esent") {
      return("Select Sender ID")
    }
    else{
      return("Select Reciever ID")
    }
  })
  
  
  
  #Calculating the number of Emails  
  
  inSelect2hop <- reactive({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    #Setting up the directory and Uploading the files
    nodes <- Data2()
    links <- Data1()
    
    #Finding the Frequency of each cell in A
    A <- as.data.frame(table(links[,1]))
    
    #Setting up the required column names in A
    colnames(A)[1] <- "Senders_ID"
    colnames(A)[2] <- "No._of_Emails_Sent"
    
    #Arranging A in Descending Order
    a <-A[order(-(A$No._of_Emails_Sent)),c(1,2)]
    
    #Taking up the first 10 rows
    a1 <- a[1:10,1]
    
    #Converting the class of a1
    a1 <- as.integer(a1)
    
    #Finding the Frequency of each cell in B
    B <- as.data.frame(table(links[,2]))
    
    #Setting up the required column names in B
    colnames(B)[1] <- "Reciever_ID"
    colnames(B)[2] <- "No._of_Emails_Recieved"
    
    #Arranging B in Descending Order
    b <-B[order(-(B$No._of_Emails_Recieved)),c(1,2)]
    
    #Taking up the first 10 rows
    b1 <- b[1:10,1]
    
    #Converting the class of b1
    b1 <- as.integer(b1)
    
    #Creating a new Vector with Integer class
    values <- vector("integer")
    
    #Taking up the unique values in a1&b1
    values <- unique(c(a1,b1))
    values
  })
  
  
  
  ######## Inputs for Select 1 Options ######
  
  inSelectDC <- reactive({
    validate(
      need(input$file1, 'Please Upload-EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    nodes <- Data2()
    links <- Data1()
    
    Output <- getDCtable()
    #Joining the two tables to get the corresponding Departments for each node
    Final_Output <-join(Output,nodes) 
    #Taking up the unique values in a1&b1
    unique(Final_Output$Node)
  })
  
  
  inSelectBC <- reactive({
    validate(
      need(input$file1, 'Please Upload EU core file'),
      need(input$file2, 'Please Upload EU core department file')
    )
    
    nodes <- Data2()
    Output <- getBCtable()
    
    #Joining the two tables to get the corresponding Departments for each node
    Final_Output <-join(Output,nodes) 
    
    unique(Final_Output[,1])
  })
  
  
  inSelectIDC <- reactive({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    nodes <- Data2()
    Output <- getIDCtable()
    #Joining the two tables to get the corresponding Departments for each node
    Final_Output <-join(Output,nodes) 
    unique(Final_Output[,1])
  })
  
  
  inSelectDeptwiseMail <- reactive({
    validate(
      need(input$file1, 'Please Upload EU core file'),
      need(input$file2, 'Please Upload EU core department file')
    )
    c <-sort(unique(Data2()[,2]))
    
    return(c)
  })
  
  
  inSelectEmails <- reactive({
    if(input$emails == "esent") {
      validate(
        need(input$file1, 'Please Upload EU-core file')
      )
      A <- as.data.frame(table(Data1()[,1]))
      colnames(A)[1] <- "Senders_ID"
      colnames(A)[2] <- "No._of_Emails_Sent"
      return(A[,1])
    }
    else if(input$emails == "ereceived"){
      validate(
        need(input$file1, 'Please Upload EU-core file')
      )
      B <- as.data.frame(table(Data1()[,2]))
      colnames(B)[1] <- "Reciever_ID"
      colnames(B)[2] <- "No._of_Emails_Recieved"
      return(B[,1])
    }
    else{
      return(NULL)
    }
  })
  
  ######## Inputs for Select 2 Options ######  
  
  inSelect2hop2 <- reactive({
    get1HopNeighbours(input$inSelect2Hop)
  })
  inSelectDC2 <- reactive({
    get1HopNeighbours(input$inSelectDC)
  })
  inSelectBC2 <- reactive({
    get1HopNeighbours(input$inSelectBC)
  })
  inSelectIDC2 <- reactive({
    get1HopNeighbours(input$inSelectIDC)
  })
  
  inSelectnum1 <- reactive({
    length(inSelectDeptwiseMail())
  })
  
  
  ######## Inputs for slider Options ######    
  inSelectnum2 <- reactive({
    s2 <- input$inSelect2Hop2
    len <- 1
    for (i in  1:length(s2)){
      len1 <- length(get1HopNeighbours(s2[i]))
      if(len1 > len)
        len = len1
    }
    len
  })
  inSelectnum3 <- reactive({
    s2 <- input$inSelectDC2
    len <- 1
    for (i in  1:length(s2)){
      len1 <- length(get1HopNeighbours(s2[i]))
      if(len1 > len)
        len = len1
    }
    len
  })
  inSelectnum4 <- reactive({
    s2 <- input$inSelectBC2
    len <- 1
    for (i in  1:length(s2)){
      len1 <- length(get1HopNeighbours(s2[i]))
      if(len1 > len)
        len = len1
    }
    len
  })
  inSelectnum5 <- reactive({
    s2 <- input$inSelectIDC2
    len <- 1
    for (i in  1:length(s2)){
      len1 <- length(get1HopNeighbours(s2[i]))
      if(len1 > len)
        len = len1
    }
    len
  })
  
  
  
  #Output to Display the 2-hop Neighbours  
  
  output$network1 <- renderVisNetwork({
    library(igraph)
    library (plyr)
    library(networkD3)
    
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    
    df3 <- get2HopNeighbours(input$inSelect2Hop, input$inSelect2Hop2, input$num2)
    g <- graph.data.frame(df3)
    summary(g)
    
    net <- simplify(g, remove.multiple = F, remove.loops = T)
    data <- toVisNetworkData(net)
    nodes <- data$nodes 
    edges <- data$edges
    
    visNetwork(nodes, edges) %>% visEdges(arrows = "to" )%>% visNodes(color = list(background = FALSE, border = FALSE,
                                                                                   highlight = "yellow"),shadow = list(enabled = FALSE))%>%visOptions( highlightNearest = TRUE)%>%visPhysics(stabilization = FALSE)
    
  })
  
  #Output to display Degree Centrality 2-hop neighbours 
  
  output$network2 <- renderVisNetwork({
    library(igraph)
    library(dplyr)
    library(plyr)
    
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
      
    )
    
    df3 <- get2HopNeighbours(input$inSelectDC, input$inSelectDC2, input$num3)
    getNetworkGraph(df3)
    
  })
  
  #Output to display Betweeness Centrality 2-hop -neighbours
  
  output$network3 <- renderVisNetwork({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file')
    )
    df4 <- get2HopNeighbours(input$inSelectBC, input$inSelectBC2, input$num4)
    getNetworkGraph(df4)
    
  })
  
  #Output to display In-Degree Centrality 2-hop -neighbours 
  
  output$network4 <- renderVisNetwork({
    validate(
      need(input$file1, 'Please Upload EU-core file'),
      need(input$file2, 'Please Upload EU-core-Department file'))
    df5 <- get2HopNeighbours(input$inSelectIDC, input$inSelectIDC2, input$num5)
    getNetworkGraph(df5)
  })
  
  output$txtout <- renderText({"
             Summary:- 


               In the given network, analysis of emails data for a large European research Institution,It is evident that Person(NODE =160),          
               is having the highest indicators of centrality,Degree Centrality(IN&OUT) and betweeness Centrality.Thus, Node 160 plays the most 
               important vertex of the network.Hence, we can assume that the person (NODE=160) is the most influential person in the social network.
               It can be observed that persons (Nodes =121,107,62,86 and 129) are present in the top 10 nodes with highest degree centrality,
               betweeness centrality and indegree centrality.Thus, we can interpret that these people are highly important in organization.
               From the network display ,it is evident that the people (nodes) mentioned in before are most important people all belonging
               to same department (colour yellow), thus we can assume that department 36 is the most influential department in the organization.
               Person( Node = 82), has high out-degree centrality (sending emails to many people) but not high in-degree centrality (receiving emails from less
               number of people).Thus, we can assume that the person is mostly  responsible for one way communication.
               Persons (Node 86,121,62 and 107), belonging to department 36(most influential department) have the highest betweeness_centrality thus 
               these people also play a major role to bridge different departments to form  a network.
         
    "})
  }

shinyApp(ui = ui, server = server)



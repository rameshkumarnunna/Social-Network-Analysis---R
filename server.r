library(htmltools)
library(shiny)
# library(plyr)
library(DT)
library(networkD3)
library(igraph)
library(RANN)
library(data.table)
library(dplyr)
library(sqldf)
shinyServer(function(input,output) {
  
  #Inputfile 1
  datafile<-eventReactive(input$file1,{
    inFile <- data.frame(input$file1)

  if (is.null(inFile))
    return(NULL)

  emailinput<- read.csv(inFile$datapath, sep = input$sep, header = input$header,col.names = c("Emailssent","Emailsreceived"))})

  #Inputfile 2
  deptfile<-eventReactive(input$file2,{ 
    inFile <- data.frame(input$file2)
    
    if (is.null(inFile))
      return(NULL)
    
  deptinput<- read.csv(inFile$datapath, sep = input$sep, header = input$header,col.names = c("person","department"))})
  
  #Displaying Email network_data
  output$data <- DT::renderDataTable({
    DT :: datatable(datafile())
    
  })
  
  #Displaying department data
  output$dept <- DT::renderDataTable({
    DT :: datatable(deptfile())
    })
  
  #Dispaly N connections
  output$view <- renderSimpleNetwork({
    
    Emailnetwork_unique <- unique(datafile(),incomparables = FALSE)
    if(is.null(Emailnetwork_unique)){return()}
    sample_data = sample(1:nrow(Emailnetwork_unique),input$obs)
    networkData <-Emailnetwork_unique[sample_data,]
    simpleNetwork(networkData,height = 300,width = 400,fontSize = 14,fontFamily = "sans-serif",nodeColour = "#000000",linkColour = "red",zoom = TRUE,opacity = 1)
    
    })
  
  #Emails sent by each person
  output$sent <- DT :: renderDataTable({
   DT :: datatable(plyr :: count(datafile(),"Emailssent")) })
  
  #Emails received by each person
  output$received <- DT :: renderDataTable({
  emailsreceived <- DT :: datatable(plyr :: count(datafile(),"Emailsreceived"))
  return(emailsreceived)
  }) 
  
  # 2 hop neighbors
  neighbors_2_hop <- function(network_eu,top_ten){

    a1 <- filter(network_eu,Emailssent 
                 %in% top_ten)
    fil_neigh1 <- a1 %>% group_by(Emailssent) %>% top_n(n = input$ngbhr)
    r2 <- unlist(fil_neigh1[,2])
    a2 <- filter(network_eu,Emailssent %in% r2)
    fil_neigh2 <- a2 %>% group_by(Emailssent) %>% 
              top_n(n = input$ngbhr)
    
    neighborData <- merge(fil_neigh1,fil_neigh2, all = T)
    return(data.frame(neighborData))}
  
  #Top 10 emails sent _2 hop
  output$Two_hop_sent_emails <- renderSimpleNetwork({

    network_eu <- as.data.frame(datafile())
    emailssent <- plyr :: count(network_eu,"Emailssent") 
    
    names(emailssent) <- c("person","freq")
    emailssent_desc <- emailssent[order((rank(-emailssent$freq ,ties.method = "min")),decreasing = F),]
    
    Topten <- head(emailssent_desc[,1],10)
    Two_hop_sent_emails <- as.data.frame(neighbors_2_hop(network_eu,top_ten = unlist(Topten[]))) #calculating 2hop neighbors for Top10
    
    simpleNetwork(Two_hop_sent_emails,opacity = 3,nodeColour = "#000000",fontSize = 14,zoom = TRUE,linkColour = "red")

  })
  
  #Top 10 emails received _2 hop
  output$Two_hop_received_emails <- renderSimpleNetwork({
    
    network_eu <- as.data.frame(datafile())
    emailssent <- plyr :: count(network_eu,"Emailsreceived")
    names(emailssent) <- c("person","freq")
    
    emailssent_desc <- emailssent[order((rank(-emailssent$freq ,ties.method = "min")),decreasing = F),]
    Topten <- head(emailssent_desc[,1],10)
    # r <- unlist(Topten[])
    Two_hop_received_emails <- as.data.frame(neighbors_2_hop(network_eu,top_ten = unlist(Topten[]))) #calculating 2hop neighbors for Top10
    simpleNetwork(Two_hop_received_emails ,fontSize = 14 ,nodeColour = "#000000",linkColour = "red",zoom = TRUE,opacity = 3)
    
  })
  
  #Degree centrality
  Data_Degree_centrality<-eventReactive(input$file1,{
    inFile <- data.frame(input$file1)
    if (is.null(inFile))
      return(NULL)
  edge_list<- read.csv(inFile$datapath, sep = input$sep, header = input$header)
  graph_dc<- graph.data.frame(edge_list,directed = FALSE)
  degree_centrality<- data.frame(degree(graph_dc))
  degree_centrality_rownames<- cbind(newColName = rownames(degree_centrality), degree_centrality)})
  
  output$degree_centrality <- renderForceNetwork({ a_dc <- as.data.frame(Data_Degree_centrality())
  
  network_eu <- as.data.frame(datafile())
  names(a_dc) <- c("nodes","dc")
  a_dc_desc <- a_dc[order((rank(-a_dc$dc,ties.method = "min")),decreasing = F),]
  Topten <- head(a_dc_desc[,1],10)
  dc_Two_hop <- as.data.frame(neighbors_2_hop(network_eu,top_ten = unlist(Topten[]))) #calculating 2hop neighbors for Top10
  nodes <- as.data.frame(deptfile())
  forceNetwork(Links = dc_Two_hop, Nodes = nodes, Source = "Emailssent", Target = "Emailsreceived",
               height = 500, width = 1000,
               NodeID = "person", Group = "department",
               Value = 1,opacity = 3, zoom = TRUE, 
               fontSize = 20,charge = -15,
               fontFamily = "serif",
               linkWidth = 2,linkDistance = 75,
               legend = TRUE,
               arrows = FALSE , bounded = FALSE, opacityNoHover = 1,
               clickAction = NULL) }) 
              
  #Betweenness centrality
  Data_Betweenness_centrality<-eventReactive(input$file1,{
    inFile <- data.frame(input$file1)
    if (is.null(inFile))
      return(NULL)
    edge_list<- read.csv(inFile$datapath, sep = input$sep, header = input$header)
    graph_bc<- graph.data.frame(edge_list,directed = FALSE)
    betweenness_centrality<- data.frame(betweenness(graph_bc))
    degree_centrality_rownames<- cbind(newColName = rownames(betweenness_centrality), betweenness_centrality)})
  
  output$betweenness_centrality <- renderForceNetwork({
    a_bc <- as.data.frame(Data_Betweenness_centrality())
    
    network_eu <- as.data.frame(datafile())
    names(a_bc) <- c("nodes","bc")
    a_bc_desc <- a_bc[order((rank(-a_bc$bc ,ties.method = "min")),decreasing = F),]
    Topten <- head(a_bc_desc[,1],10)
    bc_Two_hop <- as.data.frame(neighbors_2_hop(network_eu,top_ten = unlist(Topten[]))) #calculating 2hop neighbors for Top10
    nodes <- as.data.frame(deptfile())
    
    forceNetwork(Links = bc_Two_hop, Nodes = nodes, Source = "Emailssent", Target = "Emailsreceived",
                 height = 300, width = 300,
                 # colourScale = JS('force.alpha(1); force.restart(); d3.scaleOrdinal(d3.schemeCategory20);'),
                 NodeID = "person", Group = "department",
                 Value = 1,
                 fontSize = 20,
                 fontFamily = "serif",
                 linkWidth = 2,linkDistance = 75, 
                 charge = -15,
                 opacity = 3, zoom = TRUE, 
                 legend = TRUE,
                 arrows = FALSE , bounded = FALSE, opacityNoHover = 1,
                 clickAction = NULL)  })
  
  #Indegree centrality
  Data_Indegree_centrality<-eventReactive(input$file1,{
    inFile <- data.frame(input$file1)
    if (is.null(inFile))
      return(NULL)
    edge_list<- read.csv(inFile$datapath, sep = input$sep, header = input$header)
    graph_Ic<- graph.data.frame(edge_list,directed = TRUE)
    Indegree_centrality<- data.frame(degree(graph_Ic,mode = "in"))
    Indegree_centrality_rownames<- cbind(newColName = rownames(Indegree_centrality), Indegree_centrality)})
  
  output$indegree_centrality <- renderForceNetwork({
    
    a_ic <- as.data.frame(Data_Indegree_centrality())
    
    network_eu <- as.data.frame(datafile())
    names(a_ic) <- c("nodes","ic")
  
    a_ic_desc <- a_ic[order((rank(-a_ic$ic ,ties.method = "min")),decreasing = F),]
    Topten <- head(a_ic_desc[,1],10)
    
    ic_Two_hop <- as.data.frame(neighbors_2_hop(network_eu,top_ten =unlist(Topten[]))) #calculating 2hop neighbors for Top10
  
    nodes <- as.data.frame(deptfile())
    forceNetwork(Links = ic_Two_hop, Nodes = nodes, Source = "Emailssent", Target = "Emailsreceived",
                 height = 300, width = 300,
                 NodeID = "person", Group = "department",
                 Value = 1,
                 fontSize = 20,
                 fontFamily = "serif",
                 linkWidth = 2,
                 linkDistance = 75, 
                 charge = -15,
                 opacity = 3, zoom = TRUE, 
                 legend = TRUE,
                 arrows = FALSE , bounded = FALSE, opacityNoHover = 1,
                 clickAction = NULL) 
  })
  
  
  #Department level table display
  output$bydept <- DT :: renderDataTable({

    emailssentjoin <- left_join(datafile(),deptfile(),by = c("Emailssent" = "person"))
    email_all <- left_join(emailssentjoin,deptfile(),by = c("Emailsreceived" = "person"))
    email_all_bydept <- (email_all[, -which(names(email_all) %in% c("Emailssent","Emailsreceived"))])
    colnames(email_all_bydept)[1] <- "Emailsfrom_dept"
    colnames(email_all_bydept)[2] <- "EmailsTo_dept"
    Totalemails_dept <- data.frame(sqldf("select Emailsfrom_dept,EmailsTo_dept,count(*) as Totalemailssent from email_all_bydept group by Emailsfrom_dept,EmailsTo_dept"))
    
    return(Totalemails_dept)
   
  })
  
 #Department level directed connections
  output$bydeptgraph <- renderForceNetwork({
  network_eu <- as.data.frame(datafile())

  head_network_eu <- head(network_eu,200)
  nodes <- as.data.frame(deptfile())
   
  forceNetwork(Links = head_network_eu, Nodes = nodes, Source ="Emailssent",Target = "Emailsreceived",
                 height = "500px", width = "100%", 
                 NodeID = "person", Group = "department",
                 Value = 1,
                 fontSize = 20,
                 fontFamily = "serif",
                 linkColour = "#666",
                 opacity = 2,
                 charge = -10,
                 arrows = TRUE,
                 clickAction = NULL,
                 linkDistance = 25,
                 legend = TRUE,
                 opacityNoHover = 1
                 )
    }) 
   
  #observations
  output$dc=renderPrint({
    print("Degree Centrality: Members 160,121,107,62,86,82 are very important people in the research instutution as they are receiving/sending large number of emails. People often mail those persons to give/ask essential information. The number of emails received/sent by each person vary in large number across the organization, as the persons 755,524,858,750 etc.,sent/received either zero or one email. 160,121,107,62,86,82,434,183,5,129 are the nodes with Top 10 degree centrality.Majority of the top 10 people belongs to department 36.More number of emails are being received/sent by 36,which is indeed very important department in the instutution")})
  output$bc=renderPrint({
    print("Between Centrality: Memebers 160,86,5,121,82,107,62,377,211,64 are the people with highest betweeness and majority of them are from department 36. These people might be at the middle level management where they pass important information from junior members to senior memebers in the instutution. Significant amount of information is being passed from department 36 from one department to other department. On the other hand members like 524,633,630,631,580 are the ones with lowest betweeness centrality.These people are at the edge of the network where they dont essentially pass information from one person to person.")})
  output$ic=renderPrint({
    print("Indegree Centrality: More number of emails are being received by members 160,62,107,121,86,434,183,129,64,128 in which majority of them are from department 36.These people can be assumed as junior members where they often receive work assignements from senior members. More number of emails are being sent to department number 36. Nodes 524,750 etc.,have lowest indegree centrality")})
  output$summary=renderPrint({
    print("To summarize, the whole email network is concentrated around department number 36 , which is very important department in the institutuion.Also, members like 160,86,62,121,107,434 etc.,must be very crucial people.Where as members like 633,750 and department 21 are not important in the organization")})
  
})



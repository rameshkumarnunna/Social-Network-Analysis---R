library(htmltools)
library(shinythemes)
library(shiny)
library(shinydashboard)
library(networkD3)
library(sqldf)
shinyUI(fluidPage(theme = shinytheme("cerulean"),
  #Title
  titlePanel(title = "RameshKumar Nunna_Social_Network_Analysis"),
  
  #sidebar panel
  sidebarLayout(
    sidebarPanel(
      
      #Fileinput
      fileInput("file1", "Upload email_network file",
                multiple = TRUE,
                accept = c("text/csv", ".csv",
                           "text/comma-separated-values,text/plain",
                           ".txt")),
      fileInput("file2", "Upload department_labels file",
                multiple = TRUE,                                                         
                accept = c("text/csv", ".csv",
                           "text/comma-separated-values,text/plain",
                           ".txt")),
      tags$hr(),
      checkboxInput("header", "Header", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Space='', Comma=',',Tab='\t',Semicolon=';'), selected = ''),
      numericInput(inputId = "ngbhr",
                   label = "Select no of neighbors (for 2 hop)",
                   value = 4)
      
    
    ),
    
    #mainpanel for outputs
    mainPanel(
      
      #tabset modelling output
      tabsetPanel(type = "tabs",
                  tabPanel("Data",tabsetPanel(type = "tabs",
                                              tabPanel("email_network", DT :: dataTableOutput("data")),
                                              tabPanel("dept_data", DT :: dataTableOutput("dept")))),
                  tabPanel("N connections",h4(numericInput(inputId = "obs",
                                                                    label = "connections",
                                                                    value = 10)),simpleNetworkOutput("view",width = "100%", height = "500px")),
                  tabPanel("Emails sent by each person",DT :: dataTableOutput("sent")),
                  tabPanel("Emails received by each person", DT :: dataTableOutput("received")),
                  tabPanel("2-hop neighbours",tabsetPanel(type = "tabs",tabPanel("emailssent",simpleNetworkOutput("Two_hop_sent_emails",width = "100%", height = "500px")),
                                                                        tabPanel("emailsreceived",simpleNetworkOutput("Two_hop_received_emails",width = "100%", height = "500px")))),
                  tabPanel("Degree_centrality", forceNetworkOutput("degree_centrality",width = "100%", height = "500px")),
                  tabPanel("Betweenness_centrality", forceNetworkOutput("betweenness_centrality",width = "100%", height = "500px")),
                  tabPanel("Indegree_centrality", forceNetworkOutput("indegree_centrality",width = "100%", height = "500px")),
                  tabPanel("Department level",tabsetPanel(type = "tabs",tabPanel("dept_table", DT :: dataTableOutput("bydept")),
                                                                        tabPanel("dept_visualization", forceNetworkOutput("bydeptgraph",width = "100%", height = "500px")))),
                  tabPanel("Observations", h4("Describe your observations when comparing the visualizations of Degree, Betweenness, Indegree centralities"),
                                           verbatimTextOutput("dc"),
                                           verbatimTextOutput("bc"),
                                           verbatimTextOutput("ic"),
                                           verbatimTextOutput("summary"))))
)))
  
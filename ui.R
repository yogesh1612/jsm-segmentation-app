
library(shiny)
library(kableExtra)
library('shinyFeedback')
library('dplyr')
# Define UI for application that draws a histogram
shinyUI(fluidPage(
   # useShinyjs(),
    # Application title
    useShinyFeedback(), # include shinyFeedback
    
    titlePanel("JSM-Segmentation App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            conditionalPanel(condition="input.tabselected==1",
                             
                             # Upload data:
                             #h5(p("Upload Responses")),
                             fileInput("file", "Upload survey data")
                             # upload secon data
                             # h5(p("Preference Data Input")),
                             #fileInput("file1", "Upload Preference Data")
                             
                             
                             ),
            conditionalPanel(condition="input.tabselected==2",
                             # 
                             selectInput("select", "Choose Segmentation Algo", 
                                         c("K-Means","Hierarchical","Model Based"), selected = "K-Means"),
                             numericInput("Clust", "Number of Segments:", 3)
                             
                             ),
            
            conditionalPanel(condition="input.tabselected==3",
                             numericInput('nattr',"Number of Attribute",2),
                             numericInput('nbrds',"Number of Brands",2),
                             
                             textInput('bname',label = "Enter Attribute Name (seperated by comma"),
                             textInput('aname',label = "Enter Brand Name (seperated by comma)"),
                             
                             # Variable selection:
                             h5(p("Parameter selection for perceptual map")),
                             numericInput("k0","Scaling Factor Entities",1.5),
                             #numericInput("k1","Scaling Factor User",1.5),
                             #h6(p("A -  Perceptual (Attributes)")),
                             #htmlOutput("varselect"),
                             #h6(p("B -  Perceptual (Firms - only for Spider Chart)")),
                             #htmlOutput("varselect2"),
                             
                             #h6(p("C -  Preference")),
                             # upload secon data
                             #htmlOutput("varselect1"),
                             # 
                             br())
            
            
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type='tabs',
                        id = 'tabselected',
                        #------------------------------------------#
                        tabPanel('Overview',title = "Overview",value = 1),
                        #-----------------------------------------#
                        tabPanel("Summary - Segmentation",value = 2,h3(textOutput("caption1")), h4(div(textOutput("caption2"),style = "color:Red")),
                        plotOutput("plotpca",height = 400, width = 500),verbatimTextOutput("summary"),htmlOutput("summary_table")),
                        #------------------------------------------#
                        tabPanel("JSM Plot",value = 3,
                                # htmlOutput("seg"),
                                uiOutput("segplots")
                                )
                        
                            
                        )
            )
)
)
)


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
            
            conditionalPanel(condition="input.tabselected==1|input.tabselected==2",
                             
                             # Upload data:
                             #h5(p("Upload Responses")),
                             fileInput("file", "Upload survey data"),
                             # upload secon data
                             # h5(p("Preference Data Input")),
                             #fileInput("file1", "Upload Preference Data")
                             numericInput('nattr',"Number of Attribute (including overall pref)",2),
                             numericInput('nbrds',"Number of Brands (including overall pref)",2),
                             
                             textInput('bname',label = "Enter Attribute Name (seperated by comma)"),
                             textInput('aname',label = "Enter Brand Name (seperated by comma)")
                             
                             
                             ),
            conditionalPanel(condition="input.tabselected==3",
                             # 
                             selectInput("select", "Choose Segmentation Algo", 
                                         c("K-Means"), selected = "K-Means"),
                             numericInput("Clust", "Number of Segments:", 3)
                             
                             ),
            
            conditionalPanel(condition="input.tabselected==4 | input.tabselected==5",
                             
                             
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
                        tabPanel('Overview',value=1, 
                                 h4(p("Perceptual mapping")),
                                 p("Perceptual Mapping is the use of graphs to identify the positioning of products/brands that consumers have, and find their preference. 
                                   The graphs layout an X and Y axis with variables and ranges from the most desirable to least desirable. For instance, the far right may be listed as 'Upper class' while the left side will be 'Lower Class'. 
                                   This allows for the placement of business names to help find the position that consumers place these businesses in relation to the variables listed."
                                   ,align="justify"),
                                 
                                 a(href="https://en.wikipedia.org/wiki/Perceptual_mapping","- Wikipedia"),
                                 
                                 h4(p("Data input")),
                                
                                 p("To plot segmentwise joint space map, this app needs input from the user. In left-sidebar panel, click on Browse and upload the survey responses. Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format and then proceed. 
                                    Make sure you have top row as variable names and first column as respondent id/name in csv file . As soon as upload is complete, this app will read the data and then segment users responses based on clustering algorithm (default 3 cluster) & plot the JSM Map for each segment  " , align="justify"),
                                 
                                 p(htmlOutput('dim'),
                                   
                                 
                                 p(strong('Sample Attribute Name:')),
                                 p('Worklife balance, Remuneration,Risk,Global Opportunities,Challanging Roles & Responsibility, Total'),
                                   
                                 p(strong('Sample Brand Name:')),
                                 p('Consulting Sector,Technology and IT sector,FMCG sector,Healthcare and Pharma,Banking and Finance,Startups or early stage ventures'),
                                   
                                 h4(p("Download Sample data")), 
                                 downloadButton('downloadData', 'Download sample survey responses')),
                                 p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                                 img(src = "example1.png")
                                 ),
                        #------------------------------------------#
                        tabPanel('Summary Statistics',value = 2, DT::dataTableOutput("summ")),
                        #-----------------------------------------#
                        tabPanel("Summary - Segmentation",value = 3,h3(textOutput("caption1")), h4(div(textOutput("caption2"),style = "color:Red")),
                        plotOutput("plotpca",height = 400, width = 500),verbatimTextOutput("summary"),htmlOutput("summary_table")),
                        #------------------------------------------#
                        tabPanel("JSM Plot",value = 4,
                                # htmlOutput("seg"),
                                uiOutput("segplots")
                                ),
                        tabPanel('Overall JSM',value = 5,plotOutput('plotjsm',height = 700, width = 700))
                        
                            
                        )
            )
)
)
)

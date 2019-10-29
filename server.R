#------ Functions for Clustering --------------#
# brnd_attr_mat <- function(data,k,nbrds,nattr,seg){
#     #check if missing values are there
#     
#     }
#     fit <- kmeans(data,k)
#     Segment.Membership =  fit$cluster
#     d = data.frame(r.name = row.names(seg_data),Segment.Membership,seg_data)
#     Segment.Membership = as.character(fit$cluster)
#     clustmeans = aggregate(seg_data,by = list(Segment.Membership), FUN = mean)
#     clustmeans$Group.1 <- paste0('Segment','_',clustmeans$Group.1)
#     inp_mat = matrix(focal_row, ncol=nbrds, nrow=nattribs, byrow=TRUE)
#     inp_mat = as.data.frame(inp_mat)
#     focal_row = clustmeans[seg, 2:ncol(clustmeans)]
#     return(inp_mat)  
# }
# 

JSM <- function(inp1,nbrds,nattr,k0,bname=NULL,aname=NULL,i=NULL){
  
  inp_mat = matrix(unlist(inp1), ncol=nattr, nrow=nbrds, byrow=TRUE)
  inp1 = inp_mat
  
  # inp1 = perception matrix with row and column headers
  # brands in rows and attributes in columns
  # prefs = preferences matrix
  
  if(!is.null(i)){
   plot_title =  paste0("Joint Space map for Segment - ",i)
  }else{
    plot_title = "Join Space map"
  }
  
  par(pty="m")
  
  fit = prcomp(inp1, scale.=TRUE) # extract prin compts
  
  plot(fit$rotation[,1:2], # use only top 2 prinComps
       
       type ="n",xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
       
       main = plot_title )#paste0("Joint Space map for Segment - ",i)) # plot title
  
  abline(h=0); abline(v=0) # build horiz & vert axes
  
  if(length(aname)==0){
    attribnames = paste0('a',1:nattr)
  }else{
    attribnames <- aname
  }
  if(length(bname)==0){
    brdnames = paste0('b',1:nbrds)
  }else{
    brdnames <- bname
  }
  
  
  
  # <-- insert attrib vectors as arrows--
  
  for (i1 in 1:nrow(fit$rotation)){
    
    arrows(0,0, x1=fit$rotation[i1,1]*fit$sdev[1], y1=fit$rotation[i1,2]*fit$sdev[2], col="blue", lwd=1.5);
    
    text(x=fit$rotation[i1,1]*fit$sdev[1],y=fit$rotation[i1,2]*fit$sdev[2], labels=attribnames[i1],col="blue", cex=1.1)}
  
  # <--- make co-ords within (-1,1) frame #
  
  fit1 = fit
  
  fit1$x[,1] = fit$x[,1]/apply(abs(fit$x),2,sum)[1]
  
  fit1$x[,2] = fit$x[,2]/apply(abs(fit$x),2,sum)[2]
  
  points(x = fit1$x[,1]*k0, y = fit1$x[,2]*k0, pch = 19, col ="red")
  
  text(x = fit1$x[,1]*k0, y = fit1$x[,2]*k0, labels = brdnames,col ="black", cex = 1.1)
  
}



impute.mean <- function(x) replace(x, is.na(x) | is.nan(x) | is.infinite(x), mean(x, na.rm = TRUE))

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

#---------Data import-------------------------------------#
    Dataset <- reactive({
        if (is.null(input$file)) {
            # User has not uploaded a file yet
            return(data.frame())
        }
            
        Dataset <- read.csv(input$file$datapath ,header=TRUE)
        # for(i in 1:ncol(Dataset)){
        #             Dataset[is.na(Dataset[,i]), i] <- mean(Dataset[,i], na.rm = TRUE)
        #         }
        row.names(Dataset)<- Dataset[,1]
        #Dataset[,2:6]<- NULL
        Dataset[,1]<- NULL
        Dataset <- as.data.frame(apply(Dataset,2,impute.mean))
        Dataset <- apply(Dataset, 2, impute.mean)
        return(Dataset)
        
        # row.names(Dataset)<- Dataset[,1]
        # return(Dataset)
    
    
    })

#-------------Summary Table--------------------------#
    
output$dim <- renderText({
              if (is.null(input$file)) {
                # User has not uploaded a file yet
              return(data.frame())}else{
                paste0('*** Uploaded dataset contains ',
                       "<font color=\"\"><b>",
                       nrow(Dataset()),
                       ' responses on ',
                       ncol(Dataset()), 
                       '  variables.',
                       "</b></font>",
                       "Please make sure that product of no. of brand & attribute should be equal to total columns in dataset***")
              }
  
  
            })
    
output$summ <- DT::renderDataTable({
                 if (is.null(input$file)) {
                 # User has not uploaded a file yet
               return(data.frame())
                 }else{
                      summarytools::descr(Dataset(),transpose = TRUE,stats = c('min','mean','max','sd','n.valid'))%>%round(2)
                 }
                      })
    
        
#----------------------------------------------------#
    observeEvent(c(input$nbrds,input$nattr), {
      feedbackWarning(
        inputId = "nattr",
        condition = prod(input$nbrds,input$nattr) != ncol(Dataset()),
        text = paste0("Product of no.of brands & attribute should be equal to no. of col")
      )
    })
    
    # observeEvent(input$nbrds, {
    #   feedbackWarning(
    #     inputId = "nbrds",
    #     condition = prod(input$nbrds,input$nattr) != ncol(Dataset()),
    #     text = paste0("Product should be equal to ncol i.e." , ncol(Dataset()))
    #   )
    # })
#---------------------------------------------------------#
    # Download data files
    output$downloadData <- downloadHandler(
      filename = function() { "jsm survey response.csv" },
      content = function(file) {
        write.csv(read.csv("Dataset/jsm data for segmentation.csv"), file, row.names=F)
      }
    )
#------------------------------------------------------------#
  output$plotjsm <- renderPlot({ 
    
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else {
      mean_data <- colMeans(Dataset())
      JSM(mean_data,k0 =input$k0 ,nbrds =input$nbrds ,nattr =input$nattr,aname(),bname(),i = NULL )
      
    }
  }) 
    
#-----------------------------------------------------------#    
    fit <- reactive({
        set.seed(12345)
        
        if (input$select == "K-Means") ({
            
            if (is.null(input$file)) {
                # User has not uploaded a file yet
                return(data.frame())
            }
            
            else {
                fit <- kmeans(Dataset(),input$Clust)
                return(fit)
            }
            
        })
    })
    
#----------------------------------------------------------#    
    
    
    output$caption1 <- renderText({
      if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
      else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
      else return (NULL)
    })
#-------------------------------------------------------------------#

segmnts<-reactive({paste0("Segment_",names(table(fit()$cluster)))})

output$segselect <- renderUI({
                    if (length(segmnts())==0) return(NULL)
                    selectInput("seg","Select Segment",choices = segmnts(),selected = segmnts()[1])})

#-------------------------------------------------------------------#    
output$plotpca = renderPlot({ 
  
  if (is.null(input$file)) {
    # User has not uploaded a file yet
    return(data.frame())
  }
  else {
    data.pca <- prcomp(Dataset(),center = TRUE,scale. = TRUE)
    plot(data.pca, type = "l"); abline(h=1)    
  }
})
#-------------------------------------------------------------------#    
    output$summary <- renderPrint({
        if (input$select == "K-Means") ({
            
            if (is.null(input$file)) {
                # User has not uploaded a file yet
                return(data.frame())
            }
            else {
                Segment.Membership =  fit()$cluster
                d = data.frame(r.name = row.names(Dataset()),Segment.Membership,Dataset())
                Segment.Membership = as.character(fit()$cluster)
                clustmeans = aggregate(Dataset(),by = list(Segment.Membership), FUN = mean)
                clustmeans$Group.1 <- paste0('Segment','_',clustmeans$Group.1)
                # fit = kmeans(Dataset(),input$Clust)
                # Segment.Membership = as.character(fit$cluster)
                # clustmeans = aggregate(Dataset(),by = list(Segment.Membership), FUN = mean)
                Summary = list(#Segment.Membership = Segment.Membership
                               #,SegMeans =clustmeans,
                               Count = table(Segment.Membership) )
                Summary
            }
        })
        
        })
    
#------------------------------------------------------------#    
output$summary_table <- renderText({
      
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          # 
          # fit = kmeans(Dataset(),input$Clust)
          # Segment.Membership = as.character(fit$cluster)
          # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
          # Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership) )
          # 
          # 
          
          
          Segment.Membership =  fit()$cluster
          d = data.frame(r.name = row.names(Dataset()),Segment.Membership,Dataset())
          Summary<-d[-1] %>% 
            #mutate(Group = as.factor(Segment.Membership)) %>%
            group_by(Segment.Membership) %>%
            summarize_all(.funs = list(mean)) %>%
            arrange(Segment.Membership) %>%
            round(2)%>% 
            # t()%>%
            # mutate_if(is.numeric, function(x) {
            #   cell_spec(x, bold = T)
            #            # color = spec_color(x, end = 0.9),
            #            # font_size = spec_font_size(x,begin = 14,end = 20))
            # })%>%
            kable(escape = F, align = "c") %>%
            kable_styling(c("striped", "condensed"), full_width = F)%>%
            footnote(general = "Mean value of all variables within each cluster. ")
          return(Summary)
        }
      })
})

    
#-----------------------------------------------------------#   
    
    

    
inp_mat<- reactive(
    if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
            # User has not uploaded a file yet
            return(data.frame())
        }
        
        else {
            fit <- kmeans(Dataset(),input$Clust)
            Segment.Membership =  fit()$cluster
            d = data.frame(r.name = row.names(Dataset()),Segment.Membership,Dataset())
            Segment.Membership = as.character(fit()$cluster)
            clustmeans = aggregate(Dataset(),by = list(Segment.Membership), FUN = mean)
            clustmeans$Group.1 <- paste0('Segment','_',clustmeans$Group.1)
           
            mat_list=list()
            for (i in 1:input$Clust){
              focal_row <- data.matrix(clustmeans[i,2:ncol(clustmeans)])
              #inp_mat = matrix(focal_row, ncol=input$nbrds, nrow=input$nattr, byrow=TRUE)
              mat_list[[i]]<-focal_row
              
            }
            #inp_mat = as.data.frame(inp_mat),
            #focal_row = clustmeans[seg, 2:ncol(clustmeans)]
            return(mat_list)
            
            
            }
        
    })
      
       
    )

#------------------------------------------------------------#  
   
# pref<-reactive(
#     if (is.null(input$file1)){
#         pref = matrix(0,2,nrow(inp_mat()))
#     }
#     
#     else {
#         pref = read.csv(input$file1$datapath ,header=TRUE)
#         row.names(pref) = pref[,1]
#         pref = pref[,2:ncol(pref)]
#         pref = pref[input$users,]
#     }
#     
# )  
###---------------------- ###


# Select variables:
output$varselect1 <- renderUI({
    if (identical(Dataset1(), '') || identical(Dataset1(),data.frame())) return(NULL)
    # Variable selection:
    
    checkboxGroupInput("users", "Choose Users (At least 1 user must be selected)",
                       rownames(Dataset1()), head(rownames(Dataset1())))
    
})

# output$seg <- renderUI({
#     if (identical(inp_mat(), '') || identical(inp_mat(),matrix())) return(NULL)
#     # Variable selection:
#     
#     selectInput("segmnt",label = paste0())
#     
# })


#----------------Brand Name--------------------------#
bname <- reactive({
  if(length(strsplit(input$bname,',')[[1]])==0){return(NULL)}
  else{
    return(strsplit(input$bname,',')[[1]])
  }
})
  
#---------update bname default value based on cluster input-------#
# observe({
#   # We'll use the input$Clus variable multiple times, so save it as x
#   # for convenience.
#   x <- input$Clust
#   
#   # This will change the value of input$inText, based on x
#   updateTextInput(session, "bname", value = paste0('a',1:x,collapse = ','))
#   
#   # Can also set the label, this time for input$inText2
#   # updateTextInput(session, "aname",
#   #                 label = paste("New label", x),
#   #                 value = paste("New text", x))
# })
#------------------------------------------------------------------#

aname <- reactive({
  if(length(strsplit(input$aname,',')[[1]])==0){return(NULL)}
  else{
    return(strsplit(input$aname,',')[[1]])
  }
  
   
})

#---------------JSM Plot------------------------------#

plotInput <- reactive({
  n_plot <- input$Clust
  #total_data <- lapply(1:n_plot, function(i){inp_mat()})
  return (list("n_plot"=n_plot, "total_data"=inp_mat()))
})



#Insert the right number of plot output objects into the web page
output$segplots <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {

    plot_output_list <- lapply(1:input$Clust, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 700, width = 700)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }

})




#  max_plots = reactive({input$Clust})

  # for (i in 1:max_plots()) {
  #   # Need local so that each item gets its own number. Without it, the value
  #   # of i in the renderPlot() will be the same across all instances, because
  #   # of when the expression is evaluated.
  #   local({
  # 
  #     my_i <- i
  #     plotname <- paste("plot", my_i, sep="")
  # 
  #     output[[plotname]] <- renderPlot({
  #       
  #               JSM(inp_mat()[[i]],k0 =input$k0 ,k1 = input$k1,nbrds =input$nbrds ,nattr =input$nattr,NULL,NULL )
  # 
  #     })
  #   })
  # }

observe({
  lapply(1:plotInput()$n_plot, function(i){
    output[[paste("plot", i, sep="") ]] <- renderPlot({
      # hist(plotInput()$total_data[[i]], main = paste("Histogram Nr", i))
      JSM(plotInput()$total_data[[i]],k0 =input$k0 ,nbrds =input$nbrds ,nattr =input$nattr,aname(),bname(),i )
      
    })
    
    
  })
})

})

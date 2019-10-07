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


JSM <- function(inp1,k0,k1,bname,aname){
    
    # inp1 = perception matrix with row and column headers
    # brands in rows and attributes in columns
    # prefs = preferences matrix
    
    par(pty="m")
    
    fit = prcomp(inp1, scale.=TRUE) # extract prin compts
    
    plot(fit$rotation[,1:2], # use only top 2 prinComps
         
         type ="n",xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
         
         main ="Joint Space map ") # plot title
    
    abline(h=0); abline(v=0) # build horiz & vert axes
    
    attribnames = aname
    
    brdnames = bname
    
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
    
    # # --- add preferences to map ---#
    # 
    # k1 = k1; #scale-down factor
    # 
    # pref = data.matrix(prefs)# make data compatible
    # 
    # pref1 = pref %*% fit1$x[, 1:2]
    # 
    # for (i1 in 1:nrow(pref1)){
    #     
    #     segments(0, 0, x1 = pref1[i1,1]*k1, y1 = pref1[i1,2]*k1, col="maroon2", lwd=1.25)
    #     
    #     points(x = pref1[i1,1]*k1, y = pref1[i1,2]*k1, pch=19, col="maroon2")
    #     
    #     text(x = pref1[i1,1]*k1, y = pref1[i1,2]*k1, labels = rownames(pref)[i1], adj = c(0.5, 0.5), col ="maroon2", cex = 1.1)
    #     
    # }
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
    
    
    
#---------------------------------------------------------#
    Dataset1 <- reactive({
        if (is.null(input$file1)) {
            # User has not uploaded a file yet
            return(data.frame())
        }
        Dataset1 <- read.csv(input$file1$datapath ,header=TRUE)
        row.names(Dataset1) = Dataset1[,1]; Dataset1= Dataset1[,2:ncol(Dataset1)]
        return(Dataset1)
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
#-------------------------------------------------------------------#

segmnts<-reactive({paste0("Segment_",names(table(fit()$cluster)))})

output$segselect <- renderUI({
                    if (length(segmnts())==0) return(NULL)
                    selectInput("seg","Select Segment",choices = segmnts(),selected = segmnts()[1])})
    
    
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
                Summary = list(Segment.Membership = Segment.Membership
                               #,SegMeans =clustmeans,
                               ,Count = table(Segment.Membership) )
                Summary
            }
        })
        
        })
    
#------------------------------------------------------------#    
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
#----------------------------------------------------------#    
    
    
    output$caption1 <- renderText({
        if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
        else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
        else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
        else return (NULL)
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
            
            focal_row <- data.matrix(clustmeans[clustmeans$Group.1==input$seg,2:ncol(clustmeans)])
            inp_mat = matrix(focal_row, ncol=input$nbrds, nrow=input$nattr, byrow=TRUE)
            #inp_mat = as.data.frame(inp_mat),
            #focal_row = clustmeans[seg, 2:ncol(clustmeans)]
            return(inp_mat)
            
            
            }
        
    })
      
       
    )

#------------------------------------------------------------#  
   
pref<-reactive(
    if (is.null(input$file1)){
        pref = matrix(0,2,nrow(inp_mat()))
    }
    
    else {
        pref = read.csv(input$file1$datapath ,header=TRUE)
        row.names(pref) = pref[,1]
        pref = pref[,2:ncol(pref)]
        pref = pref[input$users,]
    }
    
)  
###---------------------- ###


# Select variables:
output$varselect1 <- renderUI({
    if (identical(Dataset1(), '') || identical(Dataset1(),data.frame())) return(NULL)
    # Variable selection:
    
    checkboxGroupInput("users", "Choose Users (At least 1 user must be selected)",
                       rownames(Dataset1()), head(rownames(Dataset1())))
    
})

output$seg <- renderUI({
    if (identical(inp_mat(), '') || identical(inp_mat(),matrix())) return(NULL)
    # Variable selection:
    
    selectInput("segmnt",label = paste0())
    
})


#----------------Brand Name--------------------------#
bname <- reactive({
  if(length(strsplit(input$bname,',')[[1]])==0){return(NULL)}
  else{
    return(strsplit(input$bname,',')[[1]])
  }
})
  


aname <- reactive({
  if(length(strsplit(input$aname,',')[[1]])==0){return(NULL)}
  else{
    return(strsplit(input$aname,',')[[1]])
  }
  
   
})

#---------------JSM Plot------------------------------#


output$plot<-renderPlot({
  fit = prcomp(inp_mat(), scale.=TRUE) # extract prin compts
  #rownames(fit$rotation)<-bname()
  #colnames(fit$rotation)<-fname()
  #fviz_pca_biplot(fit,label="all",title = "Joint Space Map ")
  
  JSM(inp_mat(),k0 =input$k0 ,k1 = input$k1,bname(),aname())
})


})
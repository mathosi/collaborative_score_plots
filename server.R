
library(rdrop2)

# variablese to modify ----------------------------------------------------

embeddings_csv <- 'embeddings_example.csv'
dropbox_path <- 'Regensburg/cell_selection'


# Running instructions ----------------------------------------------------

#instruction: put embeddings.csv into collaborative_score_plots/
#start shiny locally:
#library(shiny)
#setwd('/path/to/collaborative_score_plots/')
#runApp()

#deploy the shinyapp so that collaborators can use it:
#rsconnect::deployApp()
#https://www.shinyapps.io/admin/#/dashboard


# main functions ----------------------------------------------------------

msMakeDfFromVectors <- function (vectorList, fillWith = NA) 
{
  maxLength <- max(unlist(lapply(vectorList, length)))
  eqLengthVectorlist <- lapply(vectorList, function(x) c(x, rep(NA, maxLength - length(x))))
  filledDf <- do.call(cbind, eqLengthVectorlist)
  if (!is.na(fillWith)) {
    filledDf[is.na(filledDf)] <- fillWith
  }
  Df <- as.data.frame(filledDf)
  rownames(Df) <- NULL
  return(Df)
}

server <- function(input, output, session, dropboxPath = dropbox_path, embeddings = embeddings_csv) {
  subsample_cells <- FALSE
  dr_df <- read.csv(embeddings)
  rownames(dr_df) <- dr_df$Barcode
  if(subsample_cells){
    set.seed(1)
    dr_df <- dr_df[sample(1:nrow(dr_df), 1000, replace = F), ]
  }
  #myCols <- mapSampleNames(returnCols = T)
  
  output$plot <- renderPlotly({
    p <- ggplot(dr_df, aes(x=UMAP1, y=UMAP2, key = Barcode)) +
      geom_point(aes(colour=Tissue), size=0.7) +
      #scale_colour_manual(values = myCols) +
      coord_fixed() +
      guides(colour = guide_legend(override.aes = list(size=5)))
    ggplotly(p, tooltip = '') %>% layout(dragmode = "lasso")
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (!is.null(d)) dr_df[dr_df$cells == d$key, ]
  })
  
  dd <-  renderPrint({
    d <- event_data("plotly_selected")
    if (!is.null(d)) {
        d
    }
  })
  
  reactRes <- reactiveValues(response=list())
  observeEvent(input$button, {
    groupName <- renderText({input$caption})
    d <- event_data("plotly_selected")
    if (!is.null(d)) {
      d_out <- d$key
      response <- list()
      response[[input$caption]] <- d_out
      oldRes <- reactRes$response
      reactRes$response <- c(oldRes,response)
      output$responseList <- renderPrint(dplyr::glimpse(reactRes$response))
    }
  })
  
  observeEvent(input$button2, {
    selectionList <- reactRes$response
    if (is.list(selectionList)) {
      # Create a unique file name
      fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
      # Write the data to a temporary file locally
      filePath <- file.path(tempdir(), fileName)
      output$messages <- renderText(sprintf('Saving temporary file under %s', filePath))
      data_df <- msMakeDfFromVectors(selectionList)
      write.csv(data_df, filePath, row.names = FALSE, quote = TRUE)
      # Upload the file to Dropbox
      drop_upload(filePath, path = dropboxPath)
    }
  })
  
  observeEvent(input$button3, {
    reactRes$response <- list()
    output$responseList <- NULL
  })
}

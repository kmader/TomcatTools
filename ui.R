library(shiny)
library(ggplot2)

rimageOutput<-function (outputId, width = "100%", height = "400px", clickId = NULL, 
          hoverId = NULL, hoverDelay = 300, hoverDelayType = c("debounce", 
                                                               "throttle")) 
{
  if (is.null(clickId) && is.null(hoverId)) {
    hoverDelay <- NULL
    hoverDelayType <- NULL
  }
  else {
    hoverDelayType <- match.arg(hoverDelayType)[[1]]
  }
  style <- paste("width:", validateCssUnit(width), ";", "height:", 
                 validateCssUnit(height))
  div(id = outputId, class = "shiny-image-output", style = style, 
      `data-click-id` = clickId, `data-hover-id` = hoverId, 
      `data-hover-delay` = hoverDelay, `data-hover-delay-type` = hoverDelayType)
}

shinyUI(pageWithSidebar(
  
  headerPanel("Recomanager Test"),
  
  sidebarPanel(
    uiOutput('img_selector'),
    selectInput('facet_row', 'Algorithm', c(None='.', "Gridrec","FBP")),
    uiOutput('minmaxselector'),
    plotOutput('hist')
  ),
  
  mainPanel(
    rimageOutput('preview',clickId='previewClick')
  )
))
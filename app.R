#load libraries
library(shiny)
library(leaflet)

#load data
load("Data/data.RData")

#user interface
ui <- fluidPage(
  
  titlePanel("Simulated COVID Cases in Bangladesh"),
  
  sidebarLayout(
    sidebarPanel(
      
      p("Predicted case numbers can be based on posterior simulations from one of the following models:"),
      br(),
      radioButtons("mod",
                  label="Choose model:",
                  choices=c("non-spatial model",
                            "spatial model with covariates"),
                  selected="spatial model with covariates"),
      radioButtons("mapfill",
                   label="Choose map to view:",
                   choices=c("observed rate",
                             "predicted rate",
                             "95% uncertainty in predicted rate"),
                   selected="predicted rate")
      
    ),
  
  mainPanel(
    
    textOutput("model.description"),
    uiOutput("formula"),
    br(),
    br(),
    leafletOutput("map"),
    br(),
    textOutput("density.description"),
    br(),
    plotOutput("density"),
    br(),
    br()
    
    )
  )
)


#server
server <- function(input, output){
  
  #model description output
  output$model.description <- renderText({
    if(input$mapfill=="observed rate"){
      paste("The map below shows the rate and number of COVID cases observed over the period 01/10/2020 - 29/04/2021.")
    } else if(input$mod=="spatial model with covariates"){
      paste("Predicted rates and COVID case numbers shown in the map below are based on the following 
      spatial model using the conditional autoregressive prior distribution for the saptial random effects
      as specified by Besag, York and Mollie (1991):")
    } else if (input$mod=="non-spatial model"){
      paste("Predicted rates and COVID case numbers shown in the map below are based on the following
      model with intercept and offset:")
    }
  })
  
  #formula output
  output$formula <- renderUI({
    if(input$mapfill=="observed rate"){
      
    } else if (input$mod=="spatial model with covariates"){
      withMathJax(paste0("$$y_{i}\\sim\\text{Poisson}(\\lambda_{i})$$"),
                  paste0("$$\\log(\\rho_{i})=\\boldsymbol{x}_i\\boldsymbol{\\beta}+\\log(\\text{tests}_i)+u_i+v_i$$"),
                  paste0("$$u_i|\\boldsymbol{u}_{-i}\\sim N\\bigg(\\frac{\\sum_{j\\in \\delta_i}u_j}{n_{\\delta_i}}\\text{,}\\frac{\\sigma_{u}^{2}}{n_{\\delta_i}}\\bigg)$$"),
                  paste0("$$v_i\\sim N(0,\\sigma^2_v)$$"),
                  paste0("$$\\log\\Big(\\textstyle{\\frac{1}{\\sigma^2_u}}\\Big)\\sim\\text{log-Gamma}(1, 0.001)$$"),
                  paste0("$$\\log\\big(\\textstyle{\\frac{1}{\\sigma^2_v}}\\big)\\sim\\text{log-Gamma}(1, 0.001)$$")
                )
    } else if (input$mod=="non-spatial model"){
      withMathJax(paste0("$$y_{i}\\sim\\text{Poisson}(\\lambda_{i})$$"),
                  paste0("$$\\log(\\rho_{i})=\\beta_{0}+\\log(\\text{tests}_{i})+v_{i}$$"),
                  paste0("$$v_{i}\\sim N(0,\\sigma_v^2)$$"),
                  paste0("$$\\log\\big(\\textstyle{\\frac{1}{\\sigma^2_v}}\\big)\\sim\\text{log-Gamma}(1, 0.001)$$"))
    }
   
  })
  
  #leaflet map output
  output$map <- renderLeaflet({
    
    if(input$mapfill=="observed rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang3$rate)
      labels <- sprintf("<strong> %s </strong> <br/> Observed Rate: %s <br/> Observed Cases: %s
                  <br/> Population: %s",
                        bang3$DISTNAME, round(bang3$rate, 3), bang3$cases, bang3$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang3)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(rate), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~rate, opacity=0.6, title="Observed Rate",
                  position="bottomright")
      
    } else if (input$mod=="spatial model with covariates" & input$mapfill=="predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang$pred.rate)
      labels <- sprintf("<strong> %s </strong> <br/> Predicted Rate: %s <br/> Predicted Cases: %s
                  <br/> Population: %s",
                        bang$DISTNAME, round(bang$pred.rate, 3), round(bang$pred.cases), bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(pred.rate), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~pred.rate, opacity=0.6, title="Predicted Rate",
                  position="bottomright")
    } else if (input$mod=="non-spatial model" & input$mapfill=="predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang2$pred.rate)
      labels <- sprintf("<strong> %s </strong> <br/> Predicted Rate: %s <br/> Predicted Cases: %s
                  <br/> Population: %s",
                        bang2$DISTNAME, round(bang2$pred.rate, 3), round(bang2$pred.cases), bang2$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang2)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(pred.rate), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~pred.rate, opacity=0.6, title="Predicted Rate",
                  position="bottomright")
    } else if (input$mod=="spatial model with covariates" & input$mapfill=="95% uncertainty in predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang$uncertainty)
      labels <- sprintf("<strong> %s </strong> <br/> Interval: %s (%s, %s)
                  <br/> Interval Width: %s <br/> Population: %s",
                        bang$DISTNAME, round(bang$pred.rate, 3), round(bang$LL, 3), round(bang$UL, 3),
                        round(bang$uncertainty, 3), bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(uncertainty), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~uncertainty, opacity=0.6, title="95% CI Width",
                  position="bottomright")
    } else if (input$mod=="non-spatial model" & input$mapfill=="95% uncertainty in predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang2$uncertainty)
      labels <- sprintf("<strong> %s </strong> <br/> Interval: %s (%s, %s)
                  <br/> Interval Width: %s <br/> Population: %s",
                        bang2$DISTNAME, round(bang2$pred.rate, 3), round(bang2$LL, 3), round(bang2$UL, 3),
                        round(bang2$uncertainty, 3), bang2$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang2)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(uncertainty), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~uncertainty, opacity=0.6, title="95% CI Width",
                  position="bottomright")
    }
  })
  
  #density description output
  output$density.description <- renderText({
    if (input$mapfill=="predicted rate" | input$mapfill=="95% uncertainty in predicted rate"){
      paste("The density for the predicted number of cases is shown below along with the true observed
            number of cases.")
    }
  })
  
  #density plot output
  output$density <- renderPlot({
    par(mfrow=c(4, 4), mar=c(4,4.5,1,0.5))
    if(input$mapfill=="observed rate"){
      
    } else if (input$mod=="spatial model with covariates"){
        for(i in 1:length(unique.districts)){
          plot(density(samples.link[[i]]), main="")
          title(paste0("Cases in ", unique.districts[i]))
          abline(v=data.spat.2c$cases[i], col="red")
        }
      } else if (input$mod=="non-spatial model"){
        for(i in 1:length(unique.districts)){
          plot(density(spat.samples.link[[i]]), main="")
          title(paste0("Cases in ", unique.districts[i]))
          abline(v=data.spat.2c$cases[i], col="red")
        }
     }
  })
  
  
}

#run the app
shinyApp(ui, server)









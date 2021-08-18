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
                            "spatial model with covariates",
                            "spatio-temporal model"),
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
    conditionalPanel("input.mod=='spatio-temporal model'",
                     sliderInput("week",
                                 label="Choose a week to view predicted rates and case numbers for;",
                                 min=as.Date("01/10/2020", "%d/%m/%Y"),
                                 max=as.Date("29/04/2021", "%d/%m/%Y"),
                                 value=as.Date("01/10/2020", "%d/%m/%Y"),
                                 timeFormat="%d/%m/%Y",
                                 width="100%",
                                 step=7)),
    br(),
    br(),
    leafletOutput("map"),
    br(),
    textOutput("density.description"),
    br(),
    plotOutput("density1"),
    plotOutput("density2"),
    plotOutput("density3"),
    plotOutput("density4"),
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
      paste("The map below shows the rate and number of COVID cases observed over the period 01/10/2020
            - 29/04/2021.")
    } else if(input$mod=="spatial model with covariates"){
      paste("Predicted rates and COVID case numbers shown in the map below are based on the following 
            spatial model using the conditional autoregressive prior distribution for the spatial random 
            effects as specified by Besag, York and Mollie (1991):")
    } else if (input$mod=="non-spatial model"){
      paste("Predicted rates and COVID case numbers shown in the map below are based on the following
            model with intercept and offset:")
    } else if (input$mod=="spatio-temporal model"){
      paste("Predicted rates and COVID case numbers shown in the map below are based on the following
            spatio-temporal model using the conditional autoregressive prior distribution for the spatial
            random effects as specified by Besag, York and Mollie (1991) and a random walk of order 2 for 
            the temporal random effects:")
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
                  paste0("$$\\log\\big(\\textstyle{\\frac{1}{\\sigma^2_v}}\\big)\\sim\\text{log-Gamma}(1, 0.001)$$"))
    } else if (input$mod=="non-spatial model"){
      withMathJax(paste0("$$y_{i}\\sim\\text{Poisson}(\\lambda_{i})$$"),
                  paste0("$$\\log(\\rho_{i})=\\beta_{0}+\\log(\\text{tests}_{i})+v_{i}$$"),
                  paste0("$$v_{i}\\sim N(0,\\sigma_v^2)$$"),
                  paste0("$$\\log\\big(\\textstyle{\\frac{1}{\\sigma^2_v}}\\big)\\sim\\text{log-Gamma}(1, 0.001)$$"))
    } else if (input$mod=="spatio-temporal model"){
      withMathJax(paste0("$$y_{i}\\sim\\text{Poisson}(\\lambda_{it})$$"),
                  paste0("$$\\log(\\rho_{it})=\\beta_{0}+\\log(\\text{tests}_{it}+0.001)+u_{i}+v_{i}+\\gamma_{t}+\\phi_{t}+\\delta_{it}$$"),
                  paste0("$$u_i|\\boldsymbol{u}_{-i}\\sim N\\bigg(\\frac{\\sum_{j\\in \\delta_i}u_j}{n_{\\delta_i}}\\text{,}\\frac{\\sigma_{u}^{2}}{n_{\\delta_i}}\\bigg)$$"),
                  paste0("$$v_i\\sim N(0,\\sigma^2_v)$$"),
                  paste0("$$\\gamma_{t}|\\gamma_{t-1},\\gamma_{t-2}\\sim N(2\\gamma_{t-1}+\\gamma_{t-1},\\sigma_{\\gamma}^2)$$"),
                  paste0("$$\\phi_{t}\\sim N(0,\\sigma_{\\phi}^2)$$"),
                  paste0("$$\\delta_{it}\\sim N\\big(0,\\textstyle{\\frac{1}{\\tau_{\\delta}}}\\big)$$"),
                  paste0("$$\\log\\Big(\\textstyle{\\frac{1}{\\sigma^2_u}}\\Big)\\sim\\text{log-Gamma}(1, 0.001)$$"),
                  paste0("$$\\log\\Big(\\textstyle{\\frac{1}{\\sigma^2_v}}\\Big)\\sim\\text{log-Gamma}(1, 0.001)$$"),
                  paste0("$$\\log\\Big(\\textstyle{\\frac{1}{\\sigma^2_{\\gamma}}}\\Big)\\sim\\text{log-Gamma}(1, 5\\times 10^{-5})$$"),
                  paste0("$$\\log\\Big(\\textstyle{\\frac{1}{\\sigma^2_{\\phi}}}\\Big)\\sim\\text{log-Gamma}(1, 5\\times 10^{-5})$$"),
                  paste0("$$\\log\\Big(\\textstyle{\\frac{1}{\\tau_{\\delta}}}\\Big)\\sim\\text{log-Gamma}(1, 5\\times 10^{-5})$$"))
    }
  })
  
  #leaflet map output
  output$map <- renderLeaflet({

    if(input$mapfill=="observed rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang$obs.rate)
      labels <- sprintf("<strong> %s </strong> <br/> Observed Rate: %s <br/> Observed Cases: %s
                  <br/> Population: %s",
                        bang$DISTNAME, round(bang$obs.rate, 3), bang$obs.cases, bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(obs.rate), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~obs.rate, opacity=0.6, title="Observed Rate",
                  position="bottomright")
    } else if (input$mod=="spatial model with covariates" & input$mapfill=="predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang$spat.pred.rate)
      labels <- sprintf("<strong> %s </strong> <br/> Predicted Rate: %s <br/> Predicted Cases: %s
                  <br/> Population: %s",
                        bang$DISTNAME, round(bang$spat.pred.rate, 3), round(bang$spat.pred.cases), 
                        bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(spat.pred.rate), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~spat.pred.rate, opacity=0.6, title="Predicted Rate",
                  position="bottomright")
    } else if (input$mod=="non-spatial model" & input$mapfill=="predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang$ns.pred.rate)
      labels <- sprintf("<strong> %s </strong> <br/> Predicted Rate: %s <br/> Predicted Cases: %s
                  <br/> Population: %s",
                        bang$DISTNAME, round(bang$ns.pred.rate, 3), round(bang$ns.pred.cases), bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(ns.pred.rate), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~ns.pred.rate, opacity=0.6, title="Predicted Rate",
                  position="bottomright")
    } else if (input$mod=="spatial model with covariates" & input$mapfill=="95% uncertainty in predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang$spat.uncertainty)
      labels <- sprintf("<strong> %s </strong> <br/> Interval: %s (%s, %s)
                  <br/> Interval Width: %s <br/> Population: %s",
                        bang$DISTNAME, round(bang$spat.pred.rate, 3), round(bang$spat.LL, 3),
                        round(bang$spat.UL, 3), round(bang$spat.uncertainty, 3), bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(spat.uncertainty), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~spat.uncertainty, opacity=0.6, title="95% CI Width",
                  position="bottomright")
    } else if (input$mod=="non-spatial model" & input$mapfill=="95% uncertainty in predicted rate"){
      pal <- colorNumeric(palette="YlOrRd",domain=bang$ns.uncertainty)
      labels <- sprintf("<strong> %s </strong> <br/> Interval: %s (%s, %s)
                  <br/> Interval Width: %s <br/> Population: %s",
                        bang$DISTNAME, round(bang$ns.pred.rate, 3), round(bang$ns.LL, 3), round(bang$ns.UL, 3),
                        round(bang$ns.uncertainty, 3), bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(ns.uncertainty), fillOpacity=0.6,
                    highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~ns.uncertainty, opacity=0.6, title="95% CI Width",
                  position="bottomright")
    } else if (input$mod=="spatio-temporal model" & input$mapfill=="predicted rate"){
      pal <- colorNumeric(palette="YlOrRd", domain=st.int1.bang@data[, paste0("Pred.Rate", (as.numeric(input$week)-18536)/7+1)])
      labels <- sprintf("<strong> %s </strong> <br/> Predicted Rate: %s <br/> Predicted Cases: %s
                  <br/> Population: %s",
                        st.int1.bang$DISTNAME,
                        round(st.int1.bang@data[, paste0("Pred.Rate", (as.numeric(input$week)-18536)/7+1)], 3), 
                        round(st.int1.bang@data[, paste0("Pred.Cases", (as.numeric(input$week)-18536)/7+1)]),
                        st.int1.bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(st.int1.bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1,
                    fillColor=~pal(st.int1.bang@data[, paste0("Pred.Rate", (as.numeric(input$week)-18536)/7+1)]), 
                    fillOpacity=0.6, highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~st.int1.bang@data[, paste0("Pred.Rate", (as.numeric(input$week)-18536)/7+1)],
                  opacity=0.6, title="Predicted Rate", position="bottomright")
    } else if (input$mod=="spatio-temporal model" & input$mapfill=="95% uncertainty in predicted rate"){
      pal <- colorNumeric(palette="YlOrRd", domain=st.int1.bang@data[, paste0("Range", (as.numeric(input$week)-18536)/7+1)])
      labels <- sprintf("<strong> %s </strong> <br/> Interval: %s (%s, %s)
                  <br/> Interval Width: %s <br/> Population: %s",
                        st.int1.bang$DISTNAME, round(st.int1.bang@data[, paste0("Pred.Rate", (as.numeric(input$week)-18536)/7+1)], 3), 
                        round(st.int1.bang@data[, paste0("LL", (as.numeric(input$week)-18536)/7+1)], 3), 
                        round(st.int1.bang@data[, paste0("UL", (as.numeric(input$week)-18536)/7+1)], 3), 
                        round(st.int1.bang@data[, paste0("Range", (as.numeric(input$week)-18536)/7+1)], 3),
                        st.int1.bang$pop)%>%
        lapply(htmltools::HTML)
      leaflet(st.int1.bang)%>%
        addTiles()%>%
        addPolygons(color="grey", weight=1, fillColor=~pal(st.int1.bang@data[, paste0("Range", (as.numeric(input$week)-18536)/7+1)]), 
                    fillOpacity=0.6, highlightOptions=highlightOptions(weight=4),label=labels,
                    labelOptions=labelOptions(style=list("font-weight"="normal",padding="3px 8px"),
                                              textsize="15px",
                                              direction="auto"))%>%
        addLegend(pal=pal, values=~st.int1.bang@data[, paste0("Range", (as.numeric(input$week)-18536)/7+1)], 
                  opacity=0.6, title="95% CI Width", position="bottomright")
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
  output$density1 <- renderPlot({
    par(mfrow=c(4, 4), mar=c(4,4.5,1,0.5))
    if(input$mapfill=="observed rate"){
      
    } else if (input$mod=="spatial model with covariates"){
      for(i in 1:16){
        plot(density(samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }
    } else if (input$mod=="non-spatial model"){
      for(i in 1:16){
        plot(density(ns.samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }  
    } else if (input$mod=="spatio-temporal model"){
      for(i in 1:16){
        plot(density(st.int1.samples.link[[(31*(i-1)+(as.numeric(input$week)-18536)/7+1)]]), main="")    #for any other week change +1 (week 17 change to +17)
        title(paste0("Cases in ", st.int1.bang@data$DISTNAME[i]))
        abline(v=st.int1.bang@data[i, paste0("Cases", (as.numeric(input$week)-18536)/7+1)], col="red")
      }
    }
  })
  output$density2 <- renderPlot({
    par(mfrow=c(4, 4), mar=c(4,4.5,1,0.5))
    if(input$mapfill=="observed rate"){
      
    } else if (input$mod=="spatial model with covariates"){
      for(i in 17:32){
        plot(density(samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }
    } else if (input$mod=="non-spatial model"){
      for(i in 17:32){
        plot(density(ns.samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }  
    } else if (input$mod=="spatio-temporal model"){
      for(i in 17:32){
        plot(density(st.int1.samples.link[[(31*(i-1)+(as.numeric(input$week)-18536)/7+1)]]), main="")    #for any other week change +1 (week 17 change to +17)
        title(paste0("Cases in ", st.int1.bang@data$DISTNAME[i]))
        abline(v=st.int1.bang@data[i, paste0("Cases", (as.numeric(input$week)-18536)/7+1)], col="red")
      }
    }
  })
  output$density3 <- renderPlot({
    par(mfrow=c(4, 4), mar=c(4,4.5,1,0.5))
    if(input$mapfill=="observed rate"){
      
    } else if (input$mod=="spatial model with covariates"){
      for(i in 33:48){
        plot(density(samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }
    } else if (input$mod=="non-spatial model"){
      for(i in 33:48){
        plot(density(ns.samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }  
    } else if (input$mod=="spatio-temporal model"){
      for(i in 33:48){
        plot(density(st.int1.samples.link[[(31*(i-1)+(as.numeric(input$week)-18536)/7+1)]]), main="")    #for any other week change +1 (week 17 change to +17)
        title(paste0("Cases in ", st.int1.bang@data$DISTNAME[i]))
        abline(v=st.int1.bang@data[i, paste0("Cases", (as.numeric(input$week)-18536)/7+1)], col="red")
      }
    }
  })
  output$density4 <- renderPlot({
    par(mfrow=c(4, 4), mar=c(4,4.5,1,0.5))
    if(input$mapfill=="observed rate"){
      
    } else if (input$mod=="spatial model with covariates"){
      for(i in 49:64){
        plot(density(samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }
    } else if (input$mod=="non-spatial model"){
      for(i in 49:64){
        plot(density(ns.samples.link[[i]]), main="")
        title(paste0("Cases in ", bang@data$DISTNAME[i]))
        abline(v=bang@data$obs.cases[i], col="red")
      }  
    } else if (input$mod=="spatio-temporal model"){
      for(i in 49:64){
        plot(density(st.int1.samples.link[[(31*(i-1)+(as.numeric(input$week)-18536)/7+1)]]), main="")    #for any other week change +1 (week 17 change to +17)
        title(paste0("Cases in ", st.int1.bang@data$DISTNAME[i]))
        abline(v=st.int1.bang@data[i, paste0("Cases", (as.numeric(input$week)-18536)/7+1)], col="red")
      }
    }
  })
  
  
}

#run the app
shinyApp(ui, server)









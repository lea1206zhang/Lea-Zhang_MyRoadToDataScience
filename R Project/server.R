#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

## server.R ##

function(input, output, session) {
  
  # Map rendering -----------------------------------------------------------------------------
  # Initializzation
  output$mymap = renderLeaflet({ leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -73.8, lat = 40.65, zoom =11)
  })
 
  # Reactively receive instructions from UI----------------------------------------------
  # reactives for the map
  #bins <- reactive({
  #  if (input$element == 'Volume') {
  #    bins = c(0,2,5,10,20,30,40,50,60,80,90,100,150) # color bin for volume
  #  } else if (input$element == 'Median Price') {
  #    bins = c(0, 1000000, 1250000, 1500000, 1750000, 2000000, Inf) # color bin for price
  #  }
  #})
 
  select <- reactive({
    if (input$element == 'Volume') {
      select = map_data@data$n # select column n for map vlaue input
    } else if (input$element == 'Median Price') {
      select = map_data@data$med_rev # select column price for value input
    }
  })

  # reactives for the donut chart 
  selectDonutAll <- reactive({
    if (input$element == 'Volume') {
      selectDonutAll = donutAll$n 
    } else if (input$element == 'Median Price') {
      selectDonutAll = donutAll$price
    }  
  })  
  
  selectDonut <- reactive({
    selectDonut = donut %>% filter(borough == input$borough) 
  })  
  
  selected <- reactive({
    if (input$element == 'Volume') {
      selected = "n"
    } else if (input$element == 'Median Price') {
      selected = "price"
    }  
  })  
  # reactive data frame for bar chart
  selectZp <- reactive({
    selectZp = chartZipCode %>% filter(borough == input$borough) 
  })
  
  # reactive for time chart 
  selectTimeAll <- reactive({
    selectTime = chartAll %>% filter(time <= as.Date(paste(input$time,"-01",sep=""))) 
  })
  
  selectTime <- reactive({
    selectTime = chartAll %>% filter(time <= as.Date(paste(input$time,"-01",sep="")),borough == input$county) 
  })
  
  # Reactively update map with from received UI instruction----------------------------------------------------------------
  observe({
    # Leaflet New York Map
    #pal = colorBin("viridis", domain = select(), bins = bins()) # set fill color
    
    labels <- paste("<strong>",map_data@data$zipcode,"</strong>",'<br/>',
                    "Total Number of Properties:", map_data@data$n, '<br/>',
                    "Median Price:", '$',format(map_data@data$med_rev, big.mark = ',')
    ) %>% lapply(htmltools::HTML)

    pal <- colorNumeric("viridis", NULL)
    #output$mymap <- renderLeaflet({
    #  leaflet(map_data) %>%
    #    addTiles() %>%
    #    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
    #                fillColor = ~pal(select()),
    #                highlight = highlightOptions(weight = 2, color = "white", 
    #                                            fillOpacity = 1,bringToFront = TRUE)
                  #label = ~paste0(county, ": ", formatC(pop, big.mark = ","))
    #    ) %>%
    #    addLegend(pal = pal, values = select(), opacity = 1.0
                #labFormat = labelFormat(transform = function(x) round(10^x))
    #    )
    #})
         
    leafletProxy( 'mymap',
                  data = map_data) %>%
      addPolygons(
        smoothFactor = 0.5, fillOpacity = 0.7, fillColor = ~ pal(select()),
        color = "white", dashArray = "1", weight = 1,
        highlight = highlightOptions(weight = 2, color = "white", 
                                     fillOpacity = 1,bringToFront = TRUE),
        label = ~ labels
        ) %>%
      
      clearControls() %>%
      
      addLegend(
        "bottomleft", pal = pal, values = select(),
        title = input$element,
        labFormat = labelFormat(prefix = " "),
        opacity = 0.75)

# Plotly Donut Chart for building class
output$Donut <- renderPlotly({
  if (input$borough == "All") {
    donutAll %>%
      plot_ly(labels = ~ property_type, values = selectDonutAll()) %>% 
      add_pie(hole = 0.3,textposition = 'inside',
              textinfo = 'percent') %>%
      layout(title = paste(input$element ,"By Property Type for All"),  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
  } else if (selected() == "n"){
    
    plot_ly(data = selectDonut(), labels = ~ property_type, values = ~ n) %>% 
      add_pie(hole = 0.3,textposition = 'inside',
              textinfo = 'percent') %>%
      layout(title = paste('Volumn By Property Type for',input$borough),  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
    
  } else {
    plot_ly(data = selectDonut(), labels = ~ property_type, values = ~ price) %>% 
      add_pie(hole = 0.3,textposition = 'inside',
              textinfo = 'percent') %>%
      layout(title = paste('Median Price By Property Type for',input$borough),  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
  }
})

# Plotly bar chart by zip code
output$zp <- renderPlotly({
  if (input$borough == "All") {
    if(input$element == 'Volume') {
      chartZipCodeAll %>% plot_ly(x = ~zipcode, y = ~n, type = 'bar', name = 'Median Price', 
                               marker = list(color = 'rgb(49,130,189)')) %>%
        layout(xaxis = list(title = "", tickangle = -45),
               yaxis = list(title = ""),
               title = 'Total Properties By Zip Code for All',
               margin = list(b = 50),
               barmode = 'group')
    } else if (input$element == 'Median Price') {
      chartZipCodeAll %>% plot_ly(x = ~zipcode, y = ~price, type = 'bar', name = 'Median Price', 
                               marker = list(color = 'rgb(49,130,189)')) %>%
        add_trace(y = ~avgPrice, name = 'Avg Price', 
                  marker = list(color = 'rgb(204,204,204)')) %>%
        layout(xaxis = list(title = "", tickangle = -45),
               yaxis = list(title = ""),
               title = 'Median/Average Price By Zip Code for All',
               legend = list(orientation = 'h',x = 0, y = 0.9),
               margin = list(b = 50),
               barmode = 'group')
    }
  } else if (selected() == "n") {
    selectZp() %>% plot_ly(x = ~zipcode, y = ~n, type = 'bar', name = 'Median Price', 
                             marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = ""),
             title = paste('Total Properties By Zip Code for',input$borough),
             margin = list(b = 50),
             barmode = 'group')
    
  } else {
    selectZp() %>% plot_ly(x = ~zipcode, y = ~n, type = 'bar', name = 'Median Price', 
                             marker = list(color = 'rgb(49,130,189)')) %>%
      add_trace(y = ~avgPrice, name = 'Avg Price', 
                marker = list(color = 'rgb(204,204,204)')) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = ""),
             title = paste('Median/Average Price By Zip Code for',input$borough),
             legend = list(orientation = 'h',x = 0, y = 0.9),
             margin = list(b = 50),
             barmode = 'group')
  }
})
# Plotly line chart by time and brough
output$Time <- renderPlotly({ 
  if (input$county == "All") {
    selectTimeAll() %>% plot_ly( x = ~time, y = ~price,mode = 'lines',color = ~RegionName) 
  } else {
    selectTime() %>% plot_ly( x = ~time, y = ~price,mode = 'lines',color = ~RegionName)
  }
              })


  
}) # end of observe 
  
  # Zillow Map rendering-------------------------------------------------------------------------------
  
  # By ordering by sales price, we ensure that the comparatively rare higher prices
  # will be drawn last and thus be easier to see
  all_zp <- all_zp[order(all_zp$cost),]
  
  # set color for circles
  pal2 = colorNumeric( palette="YlOrRd", domain=all_zp$`2017-06`, 
                   na.color="transparent")
  
  # style popup label
  labels2 = paste(all_zp$zipcode, '<br/>',
                  tolower(all_zp$neighbourhood), '<br/>',
                  '$',format(all_zp$cost, big.mark = ',')) %>% lapply(htmltools::HTML)
  
  output$mymap2 = renderLeaflet({ leaflet(all_zp) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = -73.8569, lat = 40.7061, zoom = 11) %>%
      addCircleMarkers(~long, ~lat,
                       fillColor = ~pal2(cost), fillOpacity = 0.7, color="white", 
                       radius=~log(all_zp$cost), # set radius for circle
                       stroke=FALSE,
                       label = labels2,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), 
                                                    textsize = "13px", direction = "auto")
      ) %>%
      addLegend( pal=pal2, values=~cost, opacity=0.9, title = "Sale Price", position = "bottomleft" )
    
  })
  # line chart rendering-------------------------------------------------------------------------------
  #output$Time <- renderPlotly({ 
  #  plot_ly(price, x = ~time, y = ~price,mode = 'lines',color = ~RegionName)
  #})
  # datatable rendering--------------------------------------------------------------------------------
  output$table <- DT::renderDataTable ({
    DT::datatable(top_n,container = sketch,rownames = FALSE) 
  })
  # Cap Rate Vs Total Reviews rendering--------------------------------------------------------------------------------
  output$be = renderPlotly({
    p <- ggplot(all_zp, aes(x = beperiod, y = tot_review,color=as.factor(zipcode))) +
      geom_point()+
      labs(x="Breakeven Period/Year", y="Number of Reviews")
    ggplotly(p)
    })
}


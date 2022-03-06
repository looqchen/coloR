library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)
library(gplots) #for col2hex
library(scales) #for col2hcl

ui <- fluidPage(  
    titlePanel(
        HTML("<h1><small><b>COLO</b></small>R</h1>")
    ),
    
    fluidRow(
        column(6, style = "background-color:#A7D70033",
               h4("Built-in Colors in R"),
               htmlOutput("header"),
               plotlyOutput("plot657", height="500px"),
               tableOutput("click")     
        ),
        
        column(6, style = "background-color:#E6E60033",
               h4("Color Palettes"),
               fluidRow(
                   column(6,
                          sliderInput("numcol", label="Number of colors", min=1, max=20, value=5)),
                   column(6,
                          sliderInput("alpha", label="Opacity", min=0, max=1, value=1, step=0.1))),
               fluidRow(
                   column(6,
                          sliderInput("s", label="Saturation", min=0, max=1, value=1, step=0.1)),
                   column(6,
                          sliderInput("v", label="Value", min=0, max=1, value=1, step=0.1)),
                   HTML("<i>Saturation and Value only apply to</i> <code>rainbow()</code>")),
               plotlyOutput("cpPlot", height="200px")
        )
    )
    
    
    # sidebarLayout(
    #     sidebarPanel(),
    #     mainPanel(
    #         htmlOutput("header"),
    #         plotlyOutput("plot2"),
    #         tableOutput("click")
    #     )
    # )
)

server <- function(input, output, session) {
    
    output$header <- renderUI({
        HTML("<b>R has 657 built in color names.</b><br>
              Click each color to copy information.<br>
              Select an area (brush) to zoom in.<br>
              Use <code>alpha=</code> in <code>rgb()</code> and <code>hsv()</code> to customize the opacity of a specific color.")
    })
    
    output$plot657 <- renderPlotly({
        #load("plot657.RData")
        xy <- expand.grid(y=37:1,x=1:15)[1:555,]
        coll <- data.frame(t(rgb2hsv(unlist(col2rgb(colors())))),
                           o=1:657,
                           color=colors())
        col <- coll %>%
            filter(str_detect(color, "^gray", negate=T)) %>%
            separate(color, remove=F,
                     into = c("text", "num"),
                     sep = "(?<=[A-Za-z])(?=[0-9])", convert=T) %>%
            replace_na(list(num= 0)) %>%
            add_column(xy) %>%
            mutate(HEX=col2hex(color),
                   RGB=apply(col2rgb(color),2,paste,collapse = ', ')) %>%
            mutate(HSV=apply(round(rgb2hsv(col2rgb(color)),3),
                             MARGIN=2, FUN=paste, collapse = ', '))
        col$color <- as_factor(col$color)
        
        ggp <- ggplotly(
            ggplot(col,aes(x=x,y=y,
                           text=paste('<b>Color</b>: ', color,
                                      '<br><b>HEX</b>: ', HEX,
                                      '<br><b>RGB</b>: ', RGB,
                                      '<br><b>HSV</b>: ', HSV))) + 
                geom_tile(aes(fill=color),colour="white",size=0.5)+ 
                scale_fill_identity()+
                #scale_y_reverse(limits = c(37, 0))+
                theme(legend.position = "none",
                      line = element_blank(),
                      text = element_blank(),
                      title = element_blank(),
                      panel.background = element_rect(fill="transparent"),
                      plot.background = element_rect(fill="transparent"),
                      plot.margin=unit(c(0,0,0,0), "mm")),
            tooltip="text", source="plotly1") %>%
            layout(xaxis = list(autorange = TRUE),
                   yaxis = list(autorange = TRUE))
        ggp
    })
    
    output$click <- renderTable({
        d <- event_data("plotly_click", source="plotly1")
        req(d)
        d <- d[1,1]+1
        out <- col[d, c("color","HEX","RGB","HSV")]
        colnames(out)[1] <- "Color"
        out
    }, colnames = T, rownames = F, hover = T)
    
    output$cpPlot <- renderPlotly({
        nc <- input$numcol
        al <- input$alpha
        sa <- input$s
        va <- input$v
        xy <- expand_grid(x=1:5, y=1:nc)
        col <- data.frame(xy, color= c(rainbow(nc,alpha=al,s=sa,v=va),
                                       heat.colors(nc,alpha=al),
                                       terrain.colors(nc,alpha=al),
                                       topo.colors(nc,alpha=al),
                                       cm.colors(nc,alpha=al)))
        #col$color <- as_factor(col$color)
        cp <- ggplotly(
            ggplot(col,aes(x=y,y=x, fill=color)) + 
            geom_tile(colour="white", size=0.6, height=0.8)+ 
            scale_fill_identity()+
            #scale_y_continuous()+
            scale_y_reverse(breaks=1:5,
                            labels=c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"))+
            theme(legend.position = "none",
                  line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(face="bold",
                                             size=10),
                  title = element_blank(),
                  panel.background = element_rect(fill="transparent"),
                  plot.background = element_rect(fill="transparent"),
                  plot.margin=unit(c(0,0,0,0), "mm"))
        ) %>%
            layout(xaxis = list(autorange = TRUE),
                   yaxis = list(autorange = TRUE))
        
        cp
    })
    
}

runApp(shinyApp(ui, server))
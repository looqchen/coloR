library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)
library(gplots) #for col2hex
library(scales) #for col2hcl

ui <- fluidPage(  
    titlePanel("coloR"),
    
    fluidRow(
        column(6,
               h4("Built-in Colors in R"),
               htmlOutput("header"),
               plotlyOutput("plot657", height="500px"),
               tableOutput("click")     
        ),
        
        column(6,
               h4("Color Palettes"),
               sliderInput("numcol", label="Number of colors", min=1, max=20, value=5),
               sliderInput("alpha", label="Opacity", min=0, max=1, value=1, step=0.1),
               plotOutput("cpPlot", height="300px")
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
        xy <- expand.grid(x=1:37,y=1:15)[1:555,]
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
        
        ggp <- ggplotly(
            ggplot(col,aes(x=y,y=x, fill=color,
                           text=paste('<b>Color</b>: ', color,
                                      '<br><b>HEX</b>: ', HEX,
                                      '<br><b>RGB</b>: ', RGB,
                                      '<br><b>HSV</b>: ', HSV))) + 
                geom_tile(colour="white",size=0.5)+ 
                scale_fill_manual(values = sort(col$color))+
                scale_y_reverse(limits = c(37, 0))+
                theme(legend.position = "none",
                      line = element_blank(),
                      text = element_blank(),
                      title = element_blank(),
                      panel.background = element_blank(),
                      plot.margin=unit(c(0,0,0,0), "mm")),
            tooltip="text") %>%
            layout(xaxis = list(autorange = TRUE),
                   yaxis = list(autorange = TRUE))
        
        output$click <- renderTable({
            d <- event_data("plotly_click")
            req(d)
            d <- d[1,1]
            out <- col[d, c("color","HEX","RGB","HSV")]
            colnames(out)[1] <- "Color"
            out
        }, colnames = T, rownames = F, hover = T)
        
        ggp
    })
    
    output$cpPlot <- renderPlot({
        nc <- input$numcol
        al <- input$alpha
        xy <- expand_grid(x=1:5, y=1:nc)
        col <- data.frame(xy, color= c(rainbow(nc,alpha=al),
                                       heat.colors(nc,alpha=al),
                                       terrain.colors(nc,alpha=al),
                                       topo.colors(nc,alpha=al),
                                       cm.colors(nc,alpha=al)))
        ggplot(col,aes(x=y,y=x, fill=color)) + 
            geom_tile(colour="white",size=2, height=0.6)+ 
            scale_fill_identity()+
            #scale_y_continuous()+
            scale_y_reverse(breaks=1:5,
                            labels=c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"))+
            theme(legend.position = "none",
                  line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(face="bold",
                                             size=14),
                  title = element_blank(),
                  panel.background = element_blank(),
                  plot.margin=unit(c(0,0,0,0), "mm"))
    })
    
}

shinyApp(ui, server)
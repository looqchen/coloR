library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)
library(gplots) #for col2hex
library(scales) #for col2hcl

ui <- fluidPage(  
    tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Carter+One&display=swap');
      body {
        background-color: white;
        color: black;
      }
      h2 {
        font-family: 'Carter One';
        font-size: 60px;
        background: -webkit-linear-gradient(left, #FFAEB9, #EEEE00 5%, #53868B 10%, #FFAEB9);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      .shiny-input-container {
        color: #474747;
      }"))
    ),
    titlePanel("coloR"),
    
    navbarPage("",
    tabPanel("Base R",
    fluidRow(
        column(6, style = "border: 0.3rem solid;
               border-color:#53868B;
               border-radius: 20px;
               padding: 10px",
               h4("Built-in Colors in R"),
               htmlOutput("header"),
               plotlyOutput("plot657", height="400px"),
               tableOutput("click")     
        ),
        
        column(6, style="border: 0.3rem solid;
               border-color:#53868B;
               border-radius: 20px;
               padding: 10px",
               h4("Color Palettes"),
               fluidRow(
                   column(6,
                          sliderInput("numcol", label="Number of colors", min=1, max=20, value=10)),
                   column(6,
                          sliderInput("alpha", label="Opacity", min=0, max=1, value=1, step=0.1))),
               fluidRow(
                   column(6,
                          sliderInput("s", label="Saturation", min=0, max=1, value=1, step=0.1)),
                   column(6,
                          sliderInput("v", label="Value", min=0, max=1, value=1, step=0.1)),
                   HTML("&nbsp;&nbsp;&nbsp;&nbsp;<i>Saturation and Value only apply to</i> <code>rainbow()</code>")),
               plotlyOutput("cpPlot", height="200px"),
               htmlOutput("colfuncode")
        )
    )
    ),
    
    tabPanel("Color Space"),
    tags$style(HTML(".navbar-brand {display:none;}
                    .navbar-default {
                    background: -webkit-linear-gradient(left, #FFAEB9, #EEEE00, #53868B);
                    border: none;
                    border-radius: 10px;
                    font-size: 18px;
                    }
                    .navbar-default .navbar-nav > .active > a,
                    .navbar-default .navbar-nav > .active > a:focus,
                    .navbar-default .navbar-nav > .active > a:hover {
                    color: #555;
                    background-color: #1A1A1A1A;
                    }"))
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
        xy <- expand.grid(y=28:1,x=1:20)[1:555,]
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
        
        output$click <- renderTable({
          d <- event_data("plotly_click", source="plotly1")
          req(d)
          d <- d[1,1]+1
          out <- col[d, c("color","HEX","RGB","HSV")]
          colnames(out)[1] <- "Color"
          out
        }, colnames = T, rownames = F, hover = T)
        
        ggp
    })
    

    
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
        rf <- c("rainbow", "heat.colors", "terrain.colors", "topoo.colors", "cm.colors")
        rfunc <- paste(rf, "(", nc, ifelse(rf=="rainbow", paste(", s=",sa,", v=",va,sep=""),""), ", alpha=", al, ")", sep="")
        
        #col$color <- as_factor(col$color)
        cp <- ggplotly(
            ggplot(col,aes(x=y,y=x, fill=color,
                           text=paste('<b>Color</b>: ', color))) + 
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
                  plot.margin=unit(c(0,0,0,0), "mm")),
            tooltip="text") %>%
            layout(xaxis = list(autorange = TRUE),
                   yaxis = list(autorange = TRUE))
        
        output$colfuncode <- renderUI({
          HTML("<b>R codes:</b><br>", paste("<code>",rfunc,"</code>",
                     c(rep("<br>",4),""),sep="",collapse = ""))
        })
        
        cp
    })
    
}

runApp(shinyApp(ui, server))
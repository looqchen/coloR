library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)
library(gplots) #for col2hex
library(scales) #for col2hcl
library(RColorBrewer)
library(colorspace)
#library(shinyjs) #for hiding output
library(ggsci)
library(ghibli)
library(LaCroixColoR)

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
                        column(6, #style = "border: 0.3rem solid; border-color:#53868B; border-radius: 20px; padding: 10px",
                               h4("Built-in Colors in R"),
                               htmlOutput("header"),
                               plotlyOutput("plot657", height="350px"),
                               tableOutput("click")     
                        ),
                        
                        column(6, #style="border: 0.3rem solid; border-color:#53868B; border-radius: 20px; padding: 10px",
                               style = "border-left: double #CCCCCC;",
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
             
             tabPanel("RColorBrewer",
                      fluidRow(
                        column(5,
                               checkboxInput("colblind",
                                             label="Display color-blind friendly palettes only.",
                                             value=FALSE),
                               radioButtons("rcb_choose", 
                                            label="Display Palettes", 
                                            choices=c("All"="all",
                                                      "Diverging (max 11 colors)"="div",
                                                      "Qualitative (max 8~12 colors)"="qual",
                                                      "Sequential (max 9 colors)"="seq",
                                                      "Select one or more palettes"="one"
                                            )),
                               conditionalPanel(
                                 condition = "input.rcb_choose == 'one'",
                                 selectInput("selectone",
                                             label="Select one or more palettes",
                                             choices = rownames(brewer.pal.info),
                                             multiple=TRUE),
                                 style = "background: #E5E5E5; border-radius: 3px;"
                               ),
                               sliderInput("numcol2", label="Number of colors", min=1, max=12, value=12),
                               HTML("<h4>R code examples</h4>
                           8 colors from the <i>YlGn</i> Palette:<br>
                           <code>brewer_pal(palette='YlGn')(8)</code><br>
                           5 colors from the 2nd of qualitative palettes, which is <i>Dark2</i>, in a reverse order:<br>
                           <code>brewer_pal(type='seq', palette=2, direction=-1)(5)</code>")
                        ),
                        column(7,
                               plotOutput("rcb_plot", height="500px"))
                      )),
             tabPanel("ColorSpace",
                      # div(style="display:inline-block",actionButton("cs_all",label="All",style="font-size: 110%;")),
                      fluidRow(style="font-size: 110%; padding-left: 15px;",
                        radioButtons("cs_choose", 
                                   label=NULL, #"Display Palettes", 
                                   choices=c("All"="cs_all",
                                             "Qualitative"="qualitative",
                                             "Sequential (single-hue)"="sequential (single-hue)",
                                             "Sequential (multi-hue)"="sequential (multi-hue)",
                                             "Diverging"="diverging",
                                             "Select one or more palettes"="cs_select"
                                   ), inline=T)),
                      conditionalPanel(condition="input.cs_choose == 'cs_select'",
                                       column(4,
                                              HTML("<br>"),
                                              sliderInput("cs_numcol", label="Number of colors", min=1, max=20, value=5),
                                              selectInput("cs_selectone",
                                                          label="Select one or more palettes",
                                                          choices = rownames(data.frame(hcl_palettes())),
                                                          multiple=TRUE),
                                              htmlOutput("cs_code")),
                                       column(8,
                                              plotOutput("colorspaceplot2"))),
                      plotOutput("colorspaceplot", height = "500px")
                      ),
             tabPanel("More Palettes",
                      navlistPanel(
                        "Packages",
                        tabPanel("GGSCI",
                                 fluidRow(
                                     column(4,
                                            sliderInput("ggsci_numcol",
                                                        label="Number of colors",
                                                        min=1,max=20,value=5)),
                                     column(8,
                                            HTML("<b>R code examples</b><br>
                                            <code>library(ggsci)</code><br>
                                            <code>pal_aaas()(5)</code><br>
                                            <code>pal_jama(alpha=0.5)(7)</code><br>
                                            <code>pal_uchicago('dark', alpha=0.7)(10)</code>"))
                                 ),
                                 plotOutput("ggsci_plot", height="500px")),
                        tabPanel("GHIBLI",
                                 fluidRow(
                                   column(4,
                                          sliderInput("ghibli_numcol",
                                                      label="Number of colors",
                                                      min=1,max=15,value=7)),
                                   column(8,
                                          HTML("<b>R code examples</b><br>
                                            <code>library(ghibli)</code><br>
                                            <code>ghibli_palette('PonyoLight')
</code><br>
                                            <code>ghibli_palette('MarnieMedium2', 3)
</code><br>
                                            <code>ghibli_palette(name = 'YesterdayLight', n = 21, type = 'continuous')</code>"))
                                 ),
                                 plotOutput("ghibli_plot")),
                        tabPanel("CANVA",
                                 fluidRow(
                                   column(4,
                                          p(strong("Canva Palettes"), br(), "These are 150 ", strong("four-color"), ' palettes by the ', a(href = 'canva.com', 'canva.com',.noWS = "outside"), ' design school and available with the ', code("ggthemes"), " package.", .noWS = c("after-begin", "before-end"))),
                                   column(8,
                                          HTML("<b>R code examples</b><br>
                                            <code>library(ggthems)</code><br>
                                            <code>canva_palettes</code><br>
                                            <code>canva_pal(palette = 'Fresh and bright')(4)</code>"))
                                 ),
                                 plotOutput("canva_plot", height="800px")),
                        tabPanel("NineteenEightyR",
                                 fluidRow(
                                   column(4,
                                          p(strong("Canva Palettes"), br(), "These are 150 ", strong("four-color"), ' palettes by the ', a(href = 'canva.com', 'canva.com',.noWS = "outside"), ' design school and available with the ', code("ggthemes"), " package.", .noWS = c("after-begin", "before-end"))),
                                   column(8,
                                          HTML("<b>R code examples</b><br>
                                            <code>library(ggthems)</code><br>
                                            <code>canva_palettes</code><br>
                                            <code>canva_pal(palette = 'Fresh and bright')(4)</code>"))
                                 ),
                                 plotOutput("r1980_plot", height="800px")),
                        #tabPanel("Component 6"),
                        #tabPanel("Component 7"),
                        #tabPanel("Component 8"),
                        #tabPanel("Component 9"),
                        #tabPanel("Component 10"),
                        #tabPanel("Component 11"),
                        #tabPanel("Component 12"),
                        #tabPanel("Component 13"),
                        #tabPanel("Component 14"),
                        #tabPanel("Component 15"),
                        well=T, widths=c(2,10)
                      )),
             tags$style(HTML(".navbar-brand {display:none;}
                    .navbar-default {
                        background: -webkit-linear-gradient(left, #FFAEB9, #EEEE00, #53868B);
                        border: none;
                        border-radius: 3px;
                        font-size: 18px;
                    }
                    .navbar-default .navbar-nav > .active > a,
                    .navbar-default .navbar-nav > .active > a:focus,
                    .navbar-default .navbar-nav > .active > a:hover {
                        color: #555;
                        background-color: #1A1A1A1A;
                    }
                    input[type='radio'] {transform: scale(1.5);}
                    input[type='radio']:checked+span {font-weight:bold;}
                    input[type='checkbox'] {transform: scale(1.5);}
                    input[type='checkbox']:checked+span {font-weight:bold;}
                  
                    .nav.nav-pills.nav-stacked > li > a {
                        padding: 7px 10px 7px 15px;
                    }
                    .nav.nav-pills.nav-stacked > .active > a,
                    .nav.nav-pills.nav-stacked > .active > a:focus,
                    .nav.nav-pills.nav-stacked > .active > a:hover {
                        border-radius: 2px;
                    }
                    .well{
                        border-radius: 3px;
                        border: none;
                        padding: 0 0 0 0;
                    }")),
             collapsible = TRUE)
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
        geom_tile(aes(fill=color),colour="grey80",size=0.2)+ 
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
        geom_tile(colour="grey80", size=0.2, height=0.8)+ 
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
      HTML("<b>R codes:</b><br>", paste("<code>",rfunc,"</code><br>",sep="",collapse = ""), "<br>")
    })
    
    cp
  })
  
  observeEvent(input$colblind,{
    updateSelectInput(session,'selectone',
                      choices=rownames(brewer.pal.info)[as.logical(brewer.pal.info$colorblind^input$colblind)])})
  
  output$rcb_plot <- renderPlot({
    source("coloR_func.R")
    if(input$rcb_choose=="one"){
      typ <- "all"
      cblind <- FALSE
      slct <- input$selectone
    }else{
      typ <- input$rcb_choose
      cblind <- FALSE
      slct <- NULL
    }
    mydisplay.brewer.all(n=input$numcol2,
                         type=typ,
                         colorblindFriendly = input$colblind,
                         select = slct,
                         exact.n=F)
  })
  
  output$colorspaceplot <- renderPlot({
    cs_tp <- NULL
    if(input$cs_choose == "cs_all"){
      cs_tp <- NULL
    }else if(input$cs_choose == "cs_select"){
      cs_tp <- NULL
    }else{
      cs_tp <- input$cs_choose
    }
    hcl_palettes(type=cs_tp, plot=T)
  })
  
  output$colorspaceplot2 <- renderPlot({
    pnames <- input$cs_selectone
    if(length(pnames)>0){
      hcl_palettes(n=input$cs_numcol, palette=input$cs_selectone, plot=T)
    }
  })
  
  output$cs_code <- renderUI({
    pnames <- input$cs_selectone
    paldf <- data.frame(hcl_palettes())
    cs_func <- NULL
    if(length(pnames)>0){
      tmp <- match(pnames,rownames(paldf))
      paldf2 <- data.frame(x=tolower(as.character(paldf[sort(tmp),1])))
      paldf2 <- suppressWarnings(paldf2 %>% separate(x, c("A","B")))[,1]
      cs_func <- paste(paldf2, "_hcl(n = ", input$cs_numcol,", palette = '", pnames[order(tmp)], "')",sep="")
    }
    HTML("<b>R codes:</b><br>", paste("<code>",cs_func,"</code><br>",sep="",collapse = ""), "<br>")
  })
  
  output$ggsci_plot <- renderPlot({
    ncol <- input$ggsci_numcol
    colfunc <- sapply(ggsci_func_names <- ls("package:ggsci")[1:18], FUN=get)
    fx <- function(x) as.character(as.list(args(get(x)))$palette)[-1]
    opt.list <- sapply(ggsci_func_names, FUN=fx)
    opt.num <- sapply(opt.list, FUN=length)
    ggsci_func_namesU <- toupper(matrix(unlist(strsplit(ggsci_func_names,split="_")),ncol=2,byrow=T)[,2])
    df <- data.frame(funcName=rep(ggsci_func_names,opt.num),
                     Name=rep(ggsci_func_namesU,opt.num),
                     Option=unlist(opt.list))
    fxgetcol <- function(funcname, option){
      suppressWarnings(get(funcname)(palette=option)(ncol))
    }
    df <- df %>%
      add_column(wrap=rep(1:3,each=14)) %>%
      rowwise() %>%
      mutate(color = list(fxgetcol(funcName, Option)))
    ggdf <- data.frame(y=rep(rep(14:1,3),each=ncol),
                       x=rep(1:ncol,42),
                       Name=rep(df$Name, each=ncol),
                       Option=rep(df$Option, each=ncol),
                       wrap=rep(df$wrap, each=ncol),
                       Color=unlist(df$color))
    ann_text <- df %>%
      add_column(x=rep(0.5,42),
                 y=rep(14:1,3),
                 label=paste(df$Name,df$Option,sep="."))

    ggplot(ggdf,aes(x=x,y=y)) + 
      geom_tile(aes(fill=Color),height=0.6)+ 
      scale_fill_identity()+
      facet_wrap(~wrap)+
      geom_text(data=ann_text,label=ann_text$label,hjust=0,vjust=-1.6)+
      theme(legend.position = "none",
            line = element_blank(),
            axis.text = element_blank(),
            title = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            panel.background = element_rect(fill="transparent"),
            plot.background = element_rect(fill="transparent"),
            plot.margin=unit(c(0,0,0,0), "mm"))

  })
  
  output$ghibli_plot <- renderPlot({
    #discon <- input$ghibli_type
    #if(discon=="con"){
    #  ncol <- 50
    #}else if(discon=="dis"){
      ncol <- input$ghibli_numcol
    #}
    
    gname <- names(ghibli_palettes)
    np <- length(gname)
    ghdf <- data.frame(Palette=gname,
                       x=rep(0.5,np),
                       y=rep(9:1,each=3))
    ghdf <- ghdf %>%
      add_column(wrap=rep(1:3,9))
    
    # if(discon=="con"){
    #   ff <- function(x) c(ghibli_palette(x,n=ncol,type="continuous"))
    #   ggghdf <- data.frame(y=rep(9:1,each=ncol*3),
    #                        x=rep(1:ncol,np),
    #                        wrap=rep(ghdf$wrap,each=ncol),
    #                        Color=c(sapply(gname, FUN=ff)))
    #   ggplot(ggghdf, aes(x=x,y=y))+
    #     geom_tile(aes(fill=Color),height=0.6)+
    #     scale_fill_identity()+
    #     facet_wrap(~wrap)+
    #     geom_text(data=ghdf,label=ghdf$Palette,hjust=0, nudge_y=0.5)+
    #     theme(legend.position = "none",
    #           line = element_blank(),
    #           axis.text = element_blank(),
    #           title = element_blank(),
    #           strip.background = element_blank(),
    #           strip.text.x = element_blank(),
    #           panel.background = element_rect(fill="transparent"),
    #           plot.background = element_rect(fill="transparent"),
    #           plot.margin=unit(c(0,0,0,0), "mm"))
    #}else if(discon=="dis"){
      
    if(ncol <= 7){
      ggghdf <- data.frame(y=rep(9:1,each=ncol*3),
                           x=rep(1:ncol,np),
                           wrap=rep(ghdf$wrap,each=ncol),
                           Color=c(sapply(ghibli_palettes, FUN=function(x){x[1:ncol]})))
    }else{
      ff <- function(x) c(ghibli_palette(x,n=ncol,type="continuous"))
      ggghdf <- data.frame(y=rep(9:1,each=ncol*3),
                           x=rep(1:ncol,np),
                           wrap=rep(ghdf$wrap,each=ncol),
                           Color=c(sapply(gname, FUN=ff)))
    }
    
      ggplot(ggghdf, aes(x=x,y=y))+
        geom_tile(aes(fill=Color),height=0.6)+
        scale_fill_identity()+
        facet_wrap(~wrap)+
        geom_text(data=ghdf,label=ghdf$Palette,hjust=0, nudge_y=0.5)+
        theme(legend.position = "none",
              line = element_blank(),
              axis.text = element_blank(),
              title = element_blank(),
              strip.background = element_blank(),
              strip.text.x = element_blank(),
              panel.background = element_rect(fill="transparent"),
              plot.background = element_rect(fill="transparent"),
              plot.margin=unit(c(0,0,0,0), "mm"))
    #}
  })
  
  output$canva_plot <- renderPlot({
    ncol <- 4
    canvaname <- names(canva_palettes)
    np <- length(canvaname)
    cl <- 6; rw <- 150/cl
    canvadf <- data.frame(Palette=canvaname,
                          x=rep(0.5,np),
                          y=rep(rw:1,each=cl),
                          wrap=rep(1:cl,rw))
    ggcanvadf <- data.frame(y=rep(rw:1,each=ncol*cl),
                            x=rep(1:ncol,np),
                            wrap=rep(canvadf$wrap,each=ncol),
                            Color=as.character(unlist(canva_palettes)))
    canvap <- ggplot(ggcanvadf, aes(x=x,y=y))+
      geom_tile(aes(fill=Color),height=0.6)+
      scale_fill_identity()+
      facet_grid(.~wrap)+
      geom_text(data=canvadf,label=canvadf$Palette,hjust=0, nudge_y=0.5, size=3)+
      theme(legend.position = "none",
            line = element_blank(),
            axis.text = element_blank(),
            title = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            panel.background = element_rect(fill="transparent"),
            plot.background = element_rect(fill="transparent"),
            plot.margin=unit(c(0,0,0,0), "mm"))
    canvap
  })

  output$r1980_plot <- renderPlot({
    #r1980_func <- sapply(r1980_func_names <- ls("package:NineteenEightyR"), FUN=get)
    
  })
}

shinyApp(ui, server)
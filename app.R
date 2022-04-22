library(shiny)
{
library(shinyWidgets)
library(fontawesome)
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
#library(LaCroixColoR)
library(NineteenEightyR)
library(nord)
library(dichromat)
library(gcookbook)
library(maps)
}

ui <- fluidPage(  
  tags$head(
    tags$style({HTML("
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
      }")})
  ),
  
  titlePanel("coloR"),
  
  navbarPage("",
             # About
             {tabPanel("About",
                      style = "font-size: 12pt",
                      p(strong("Welcome", style="color:#cb6318;font-size: 16pt"),
                        strong("to", style="color:#7caa2d;font-size: 16pt"),
                        strong("coloR", style="color:#34888c;font-size: 16pt"),
                        strong("!", style="color:#f5e356;font-size: 20pt"),
                        .noWS = c("after-begin", "before-end")),#hr(),
                      p("This Shiny App intends to provide an comprehensive list of existing colors and palettes in R to help color picking in data visualization. It is in development and new contents are added everyday."),
                      p("You may use the top menu bar to navigate through the following sections:"),
                      tags$ul(
                        tags$li(strong("Color Names"), p("This section lists 657 built-in colors in base R with their names, HEX codes, RGB and HSV values.")), 
                        tags$li(strong("Palettes"), p("This section includes popular color palettes packages available in R. They are",
                           tags$ul(
                             tags$li(a(href="https://cran.r-project.org/web/packages/RGraphics/index.html","grDevices"),": 5 palettes in base R, no package installation needed.",.noWS = c("after-begin", "before-end")),
                             tags$li(a(href="https://jrnold.github.io/ggthemes/reference/canva_palettes.html","Canva"),": 150 four-color palettes by the ", a(href="canva.com", "canva.com"), "design school, ", code("install.packages('ggthemes')"), .noWS = c("after-begin", "before-end")),
                             tags$li(a(href="https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html","ColorSpace"),": 97 palettes, ", code("install.packages('colorspace')"), .noWS = c("after-begin", "before-end")),
                             tags$li(a(href="https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html","GGSCI"),": 42 scientific journal and sci-fi themed palettes, ", code("install.packages('ggsci')"), .noWS = c("after-begin", "before-end")),
                             tags$li(a(href="https://cran.r-project.org/web/packages/ghibli/vignettes/ghibli.html","GHIBLI"),": 27 palettes inspired by ", a(href="https://en.wikipedia.org/wiki/Studio_Ghibli","Studio Ghibli"), " films, ", code("install.packages('ghibli')"), .noWS = c("after-begin", "before-end")),
                             tags$li(a(href="https://github.com/m-clark/NineteenEightyR","NineteenEightyR"),": 12 palettes inspired by Sonny Crockett, Malibu, Miami, the movie Cobra, and more. This is a developmental version and can be installed via", code("devtools::install_github('m-clark/NineteenEightyR')"), .noWS = c("after-begin", "before-end")),
                             tags$li(a(href="https://cran.r-project.org/web/packages/nord/readme/README.html","NORD"),": 16 arctic, north-bluish color palettes, ", code("install.packages('nord')"), .noWS = c("after-begin", "before-end")),
                             tags$li(a(href="https://cran.r-project.org/web/packages/RColorBrewer/index.html","RColorBrewer"),": 35 palettes, ", code("install.packages('RColorBrewer')"), .noWS = c("after-begin", "before-end"))
                           ))), 
                        tags$li(strong("Visualization"),
                                p("In this section, you can visualize a color palette of your choice in different", strong("plot types"),.noWS = c("after-begin", "before-end")),
                                tags$ul(
                                  tags$li("Bar plot"),
                                  tags$li("Box plot"),
                                  tags$li("Histogram"),
                                  tags$li("Scatter plot"),
                                  tags$li("Line chart"),
                                  tags$li("Map")
                                ),
                                br(),
                                p("with", strong("options"), "to view it in",.noWS = c("after-begin", "before-end")),
                                tags$ul(
                                  tags$li("Regular setting"),
                                  tags$li("Black-white printing"),
                                  tags$li("Color blindess: deuteranopia and protanopia (red-green color blindness), and tritanopia (green-blue color blindness)")
                                )
                      )),
                      hr()
                      )},
             
             tabPanel("Color Names",
                      {fluidRow(
                        column(5,
                               br(),
                               h4("Built-in Colors in R"),
                               HTML("<b>R has 657 built in color names.</b><br>
              Click each color to copy information.<br>
              Select an area (brush) to zoom in.<br>
              Use <code>alpha=</code> in <code>rgb()</code> and <code>hsv()</code> to customize the opacity of a specific color."),
                               br(),br(),br(),
                               tableOutput("click")),
                        column(7,
                               plotlyOutput("plot657", height="500px"))
                      )}),
             
             tabPanel("Palettes",
                      navlistPanel(
                        "Packages",
                        
                        tabPanel("Base R",
                                 {fluidRow(
                                   column(4,
                                     strong("Color palettes in base R"),
                                     sliderInput("numcol", label="Number of colors", min=1, max=20, value=10),
                                     sliderInput("alpha", label="Opacity", min=0, max=1, value=1, step=0.1),
                                     sliderInput("s", label="Saturation", min=0, max=1, value=1, step=0.1),
                                     sliderInput("v", label="Value", min=0, max=1, value=1, step=0.1),
                                     HTML("<i>Saturation and Value only apply to</i> <code>rainbow()</code>")),
                                   column(8,
                                          htmlOutput("colfuncode"),
                                          plotlyOutput("cpPlot", height="250px")
                                   ))}),
                        
                        tabPanel("CANVA",
                                 {fluidRow(
                                   column(4,
                                          p(strong("Canva Palettes"), br(), "These are 150 ", strong("four-color"), ' palettes by the ', a(href = 'canva.com', 'canva.com',.noWS = "outside"), ' design school and available with the ', code("ggthemes"), " package.", .noWS = c("after-begin", "before-end"))),
                                   column(8,
                                          HTML("<b>R codes</b><br>
                                            <code>library(ggthemes)</code><br>
                                            <code>canva_palettes</code><br>
                                            <code>canva_pal(palette = 'Fresh and bright')(4)</code>"))
                                 )},
                                 plotOutput("canva_plot", height="800px")),
                        
                        tabPanel("ColorSpace",
                                 {# div(style="display:inline-block",actionButton("cs_all",label="All",style="font-size: 110%;")),
                                 fluidRow(style="font-size: 110%; padding-left: 15px;",
                                          prettyRadioButtons("cs_choose", 
                                                       label=NULL, #"Display Palettes", 
                                                       choices=c("All"="cs_all",
                                                                 "Qualitative"="qualitative",
                                                                 "Sequential (single-hue)"="sequential (single-hue)",
                                                                 "Sequential (multi-hue)"="sequential (multi-hue)",
                                                                 "Diverging"="diverging",
                                                                 "Select one or more palettes"="cs_select"
                                                       ), inline=T))},
                                 {conditionalPanel(condition="input.cs_choose == 'cs_select'",
                                                  column(4,
                                                         HTML("<br>"),
                                                         sliderInput("cs_numcol", label="Number of colors", min=1, max=20, value=5),
                                                         selectInput(
                                                           inputId = "cs_selectone",
                                                           label = "Select one or more palettes", 
                                                           choices = rownames(data.frame(hcl_palettes())),
                                                           multiple = TRUE
                                                         ),
                                                         
                                                         htmlOutput("cs_code")),
                                                  column(8,
                                                         plotOutput("colorspaceplot2")))},
                                 plotOutput("colorspaceplot", height = "500px")
                        ),
                        
                        tabPanel("GGSCI",
                                 {fluidRow(
                                     column(4,
                                            sliderInput("ggsci_numcol",
                                                        label="Number of colors",
                                                        min=1,max=20,value=5)),
                                     column(8,
                                            HTML("<b>R codes</b><br>
                                            <code>library(ggsci)</code><br>
                                            <code>pal_aaas()(5)</code><br>
                                            <code>pal_jama(alpha=0.5)(7)</code><br>
                                            <code>pal_uchicago('dark', alpha=0.7)(10)</code>"))
                                 )},
                                 plotOutput("ggsci_plot", height="500px")),
                        
                        tabPanel("GHIBLI",
                                 {fluidRow(
                                   column(4,
                                          sliderInput("ghibli_numcol",
                                                      label="Number of colors",
                                                      min=1,max=15,value=7)),
                                   column(8,
                                          HTML("<b>R codes</b><br>
                                            <code>library(ghibli)</code><br>
                                            <code>ghibli_palette('PonyoLight')
</code><br>
                                            <code>ghibli_palette('MarnieMedium2', 3)
</code><br>
                                            <code>ghibli_palette(name = 'YesterdayLight', n = 21, type = 'continuous')</code>"))
                                 )},
                                 plotOutput("ghibli_plot")),
                        
                        tabPanel("NineteenEightyR",
                                 {fluidRow(
                                   column(4,
                                          p(strong("NineteenEightyR Palettes"), br(), "Colors inspired by Sonny Crockett, Malibu, Miami, the movie Cobra, and more. This is a develomental version and can be installed via ", br(), code("devtools::install_github('m-clark/NineteenEightyR')"), .noWS = c("after-begin", "before-end"))),
                                   column(8,
                                          HTML("<b>R codes</b><br>
                                            <code>library(NineteenEightyR)</code><br>
                                            <code>youngturqs(n = 12, alpha = 1)</code><br>
                                            <code>seventies_aint_done_yet(n = 5, alpha = 1)</code>"))
                                 )},
                                 plotOutput("r1980_plot", height="450px")),
                        
                        tabPanel("NORD",
                                 {fluidRow(
                                   column(4,
                                          sliderInput("nord_numcol",
                                                      label="Number of colors",
                                                      min=1,max=20,value=5)),
                                   column(8,
                                          HTML("<b>R codes</b><br>
                                            <code>library(nord)</code><br>
                                            <code>nord(palette = 'aurora', n = 5)</code><br>
                                            <code>nord(palette = 'baie_mouton', n = 7, alpha=0.8)</code>")))},
                                 plotOutput("nord_plot")),
                        
                        tabPanel("RColorBrewer",
                                 {fluidRow(
                                   column(5,
                                          
                                          prettySwitch(
                                            inputId = "colblind",
                                            label = "Color-blind friendly palettes only", 
                                            status = "primary"
                                          ),
                                          prettyRadioButtons(
                                            "rcb_choose", 
                                            label="Display Palettes", 
                                            choices=c("All"="all",
                                                      "Diverging (max 11 colors)"="div",
                                                      "Qualitative (max 8~12 colors)"="qual",
                                                      "Sequential (max 9 colors)"="seq",
                                                      "Select one or more palettes"="one"
                                            ),
                                            status="primary"
                                          ),
                                        
                                          conditionalPanel(
                                            condition = "input.rcb_choose == 'one'",
                                            selectInput(
                                              inputId = "selectone",
                                              label = "Select one or more palettes", 
                                              choices = rownames(brewer.pal.info),
                                              multiple = TRUE
                                            )),
                                            
                                          sliderInput("numcol2", label="Number of colors", min=1, max=12, value=12),
                                          HTML("<br><b>R codes</b><br>
                                          <code>library(RColorBrewer)</code><br>
                           <code>brewer_pal(palette='YlGn')(8)</code><br>
                           <code>brewer_pal(type='seq', palette=2)(5)</code>")
                                   ),
                                   column(7,
                                          plotOutput("rcb_plot", height="500px"))
                                 )}),
                        tabPanel("...More to come...",
                                 hr(),
                                 strong("More palettes to come:"),
                                 tags$ul(
                                   tags$li("ochRe"),
                                   tags$li("palettetown"),
                                   tags$li("Polychrome"),
                                   tags$li("quickoalette"),
                                   tags$li("rcartocolor"),
                                   tags$li("redmonder"),
                                   tags$li("RSkittleBrewer"),
                                   tags$li("wesanderson"),
                                   tags$li("yarr")
                                 ),
                                 hr()
                                 ),
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
             
             tabPanel("Visualization",
                      {fluidRow(
                        column(4,
                          radioGroupButtons("plottype",
                                            label="Type of plot",
                                            choiceValues=c("barplot",
                                                           "boxplot",
                                                           "histogram",
                                                           "scatterplot",
                                                           "linechart",
                                                           "map"),
                                            choiceNames = list(icon("chart-bar"),
                                                               icon("sliders-h",
                                                                    "fa-rotate-90"),
                                                               icon("chart-area"),
                                                               icon("ellipsis-h",
                                                                    "fa-rotate-135"),
                                                               icon("chart-line"),
                                                               icon("globe-americas")),
                                            justified = F,
                                            status = "primary",
                                            individual = T),
                          selectInput("package",
                                      label="Choose a package",
                                      choices=c("grDevices/Base R"="baser",
                                                "Canva (ggthemes)"="canva",
                                                "ColorSpace"="colorspace",
                                                "GGSCI"="ggsci",
                                                "GHIBLI"="ghibli",
                                                "NineteenEightyR"="r1980",
                                                "NORD"="nord",
                                                "RColorBrewer"="rcolorbrewer"),
                                      selected="canva"),
                          
                          pickerInput(
                            inputId = "palette",
                            label = "Choose a palette", 
                            choices = letters[1:4],
                            options = list(`live-search` = TRUE)
                          ),
                          selectInput("option",
                                      label="Choose an option",
                                      choices=c("None"="none")),
                          sliderInput("visnumcol",
                                      label="Number of colors to display",
                                      min=2, max=10, value=4, step=1),
                          htmlOutput("maxnum.message"),
                          htmlOutput("map.message")
                        ),
                        column(
                          8,
                          column(4,
                            prettyCheckboxGroup(
                              inputId = "display",
                              label = "Display", 
                              choices = c("Black-white printing"="bw",
                                          "Colorblindness"="cb"),
                              status = "primary",
                              inline = T,
                              icon=icon("check")
                            )),
                          column(8,
                            conditionalPanel(condition="input.display.includes('cb')",
                            prettyRadioButtons(
                              inputId = "colorblind",
                              label = "Colorblindness",
                              choices = c("Deuteranopia (red-green blind)"="deutan",
                                          "Protanopia (red-green blind)"="protan",
                                          "Tritanopia (green-blue blind)"="tritan"),
                              status = "primary",
                              icon=icon("check"),
                              inline=T
                            ))),
                          
                          plotOutput("visplot", height="420px")
                        )
                      )}),
             
             tags$style({HTML(".navbar-brand {font-size:14px;font-style:italic;}
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
                    
                    .fa-rotate-135 {
                    -webkit-transform: rotate(135deg);
                    -moz-transform: rotate(135deg);
                    -ms-transform: rotate(135deg);
                    -o-transform: rotate(135deg);
                    transform: rotate(135deg);
                    }
                    input[type='radio'] {transform: scale(1.2);}
                    input[type='radio']:checked+span {color:#1e77c5; font-weight:bold;}
                    input[type='checkbox'] {transform: scale(1.2);}
                    input[type='checkbox']:checked+span {color:#1e77c5; font-weight:bold;}
                  
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
                    }")}),
             collapsible = F)
)

server <- function(input, output, session) {
  
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
        geom_tile(aes(fill=color),colour="white",size=0.6)+ 
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
        geom_tile(height=0.8)+ 
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
      HTML("<b>R codes</b><br>", paste("<code>",rfunc,"</code><br>",sep="",collapse = ""), "<br>")
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
    HTML("<b>R codes</b><br><code>library(colorspace)</code><br>", paste("<code>",cs_func,"</code><br>",sep="",collapse = ""), "<br>")
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
    r1980_func <- sapply(r1980_func_names <- ls("package:NineteenEightyR"), FUN=get)
    ny <- length(r1980_func)
    nx <- sapply(r1980_func, FUN=function(x) as.list(args(x))$n)
    r1980df <- data.frame(y=ny:1,
                          x=0,
                          Palette=r1980_func_names)
    ggr1980df <- data.frame(y=unlist(apply(data.frame(ny:1,nx),1,FUN=function(x) rep(x[1],x[2]))),
                            x=unlist(sapply(nx, FUN=function(x) 1:x)),
                            Color=unlist(sapply(r1980_func_names,FUN=function(x) get(x)())))
    
    ggplot(ggr1980df, aes(x=x,y=y))+
      geom_tile(aes(fill=Color),height=0.8)+
      scale_fill_identity()+
      geom_text(data=r1980df,label=r1980df$Palette,hjust=1, nudge_x=0.3,size=6)+
      xlim(-4,13)+
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
  
  output$nord_plot <- renderPlot({
    ncol <- input$nord_numcol
    nordnames <- names(nord_palettes)
    np <- length(nordnames)
    ny <- np/2
    norddf <- data.frame(Palette=nordnames,
                         x=rep(0.5,np),
                         y=rep(ny:1,each=2),
                         wrap=rep(1:2,ny))
    ff <- function(x) nord(x,n=ncol)
    ggnorddf <- data.frame(y=rep(ny:1,each=ncol*2),
                         x=rep(1:ncol,np),
                         wrap=rep(norddf$wrap,each=ncol),
                         Color=c(sapply(nordnames, FUN=ff)))
    
    ggplot(ggnorddf, aes(x=x,y=y))+
      geom_tile(aes(fill=Color),height=0.6)+
      scale_fill_identity()+
      facet_wrap(~wrap)+
      geom_text(data=norddf,label=norddf$Palette,hjust=0, nudge_y=0.5, size=5)+
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
  
  ##Visualization
  {
  r1980_func <- sapply(r1980_func_names <- ls("package:NineteenEightyR"), FUN=get)
  r1980nx <- sapply(r1980_func, FUN=function(x) as.list(args(x))$n)
  palettelist <- list(baser=c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"),
                   canva=unique(c("Shades of citrus",sort(names(canva_palettes)))),
                   colorspace=rownames(data.frame(hcl_palettes())),
                   ggsci=toupper(matrix(unlist(strsplit(ls("package:ggsci")[1:18],split="_")),ncol=2,byrow=T)[,2]),
                   ghibli=names(ghibli_palettes),
                   r1980=r1980_func_names,
                   nord=names(nord_palettes),
                   rcolorbrewer=rownames(brewer.pal.info))
  
  ggsci_func_names <- ls("package:ggsci")[1:18]
  fx <- function(x) as.character(as.list(args(get(x)))$palette)[-1]
  opt.list <- sapply(ggsci_func_names, FUN=fx)
  optlist4 <- opt.list[paste0("pal_",moreopt <- c("d3","igv","material","uchicago"))]
  maxnumlist <- list(
    baser=NA,
    canva=4,
    colorspace=NA,
    ggsci=list(10,list(category10=10,category20=20,
                       category20b=20,category20c=20),
               12,12,list(default=51,alternating=2),
               7,10,9,7,10,8,10,12,16,7,7,9,26),
    r1980=r1980nx,
    nord=NA,
    rcolorbrewer=c(rep(11,9),8,8,12,9,8,9,8,12,rep(9,18))
  )
  names(maxnumlist$ggsci) <- ggsci_func_names
  names(maxnumlist$rcolorbrewer) <- rownames(brewer.pal.info)
  my_theme <- list(
    ggtitle("Diamonds"),
    theme_classic(),
    theme(text = element_text(size = 16),
          axis.line = element_line(colour = "grey70", 
                                   size = 2, linetype = "solid")))
  }
  
  observeEvent(input$package,{
    updatePickerInput(session,'palette',
                      choices=palettelist[input$package][[1]])})
  
  observeEvent(input$palette,{
    if(input$palette %in% toupper(moreopt)){
      updateSelectInput(session,'option',
                        choices=optlist4[paste0("pal_", tolower(input$palette))][[1]])
    }else{
      updateSelectInput(session,'option',
                        choices="None")
    }
    })
  
  output$visplot <- renderPlot({
    ncol <- input$visnumcol
    maxnumcol <- NA
    if(input$package=="baser"){
      col <- get(input$palette)(n=ncol)
    }else 
      if(input$package=="canva"){
      col <- canva_pal(palette = input$palette)(ncol)
      maxnumcol <- 4
    }else 
      if(input$package=="colorspace"){
      col <- hcl.colors(n=ncol, palette=input$palette)
    }else 
      if(input$package=="ggsci"){
      ggscipn <- paste0("pal_",tolower(input$palette))
      maxnumcol <- maxnumlist$ggsci[ggscipn][[1]]
      if(tolower(input$palette) %in% moreopt){
        col <- get(ggscipn)(palette=input$option)(ncol)
        if(input$palette %in% c("D3","IGV")){
          maxnumcol <- maxnumcol[input$option]
        }
      }else{
        col <- get(paste0("pal_",tolower(input$palette)))()(ncol)
      }
    }else 
      if(input$package=="ghibli"){
      if(ncol <=7){
        col <- ghibli_palette(name=input$palette, n=ncol)
      }else if(ncol > 7){
        col <- ghibli_palette(name=input$palette, n=ncol, type="continuous")
      }
    }else 
      if(input$package=="r1980"){
      col <- get(input$palette)(n=ncol)
      maxnumcol <- maxnumlist$r1980[input$palette][[1]]
    }else 
      if(input$package=="nord"){
      col <- nord(palette=input$palette, n=ncol)
    }else 
      if(input$package=="rcolorbrewer"){
      col <- brewer_pal(palette=input$palette)(ncol)
      maxnumcol <- maxnumlist$rcolorbrewer[input$palette][[1]]
    }
    
    if("cb" %in% input$display){
      col <- dichromat(col, type=input$colorblind)
    }
    if("bw" %in% input$display){
      col <- desaturate(col)
    }
    nncol <- length(col)
    plottype <- input$plottype
    
    if(plottype=="barplot"){
      out <- ggplot(data=subset(diamonds,color %in% levels(diamonds$color)[1:nncol]),
                    aes(cut))+
        geom_bar(aes(fill=color), position=position_dodge())+
        scale_fill_manual(values=col)+my_theme
      
    }else
      if(plottype=="boxplot"){
      # updateSelectInput(session, "package",
      #                   selected="ghibli")
      # updatePickerInput(session, "palette",
      #                   selected="PonyoMedium")
      # updateSliderInput(session, "visnumcol",
      #                   value=7)
      out <- ggplot(data=subset(diamonds,clarity %in% levels(diamonds$clarity)[1:nncol]),
                    aes(carat>0.7, price, fill=clarity, color=clarity))+
        geom_boxplot(notch=T)+
        scale_y_log10()+
        scale_x_discrete("Carat", labels=c(expression("Carat" <= "0.7"), "Carat > 0.7"))+
        scale_fill_manual(values=col)+
        scale_color_manual(values=darken(col, 0.4))+my_theme
      
    }else 
      if(plottype=="histogram"){
      diamondsN <- diamonds %>% add_column(Carat=cut(diamonds$carat, breaks=c(-Inf, 0.4, 0.7, 1, Inf)))
      out <- ggplot(data=subset(diamondsN, Carat %in% levels(diamondsN$Carat)[1:nncol]),
                    aes(price, fill=Carat, color=Carat))+
        geom_histogram(aes(y=..density..), position="identity", alpha=0.2, bins=50, size=1)+
        geom_density(adjust=2,aes(color=Carat),alpha=0.5)+
        scale_x_log10()+
        scale_fill_manual(values=col,labels = c(expression(""<=0.4), "0.4~0.7", "0.7~1.0",expression("">1.0)))+
        scale_color_manual(values=col,labels = c(expression(""<=0.4), "0.4~0.7", "0.7~1.0",expression("">1.0)))+my_theme
      
    }else 
      if(plottype=="scatterplot"){
      mpg$class <- factor(mpg$class, levels=sort(unique(mpg$class), decreasing =T))
      out <- ggplot(data=subset(mpg, class %in% levels(mpg$class)[1:nncol]), aes(displ, hwy, fill=class, color = class)) +
        geom_point(alpha=0.7, size=4)+
        geom_smooth(se = FALSE, method = lm)+
        scale_y_log10()+
        scale_fill_manual(values=col,name="Class")+
        scale_color_manual(values=col,name="Class")+my_theme+
        ggtitle("Fuel Economy")+xlab("Engine Displacement")+
        ylab("Highway Miles per Gallon")
      
    }else 
      if(plottype=="linechart"){
      out <- ggplot(data=subset(uspopage, AgeGroup %in% levels(uspopage$AgeGroup)[1:nncol]), aes(Year, Thousands, fill=AgeGroup, color=AgeGroup))+
        geom_area(alpha=0.5,size=1)+
        scale_fill_manual(values=col)+
        scale_color_manual(values=col)+my_theme+
        ggtitle("US Population by Age Groups")+
        ylab("Population (in thousands)")
      
    }else 
      if(plottype=="map"){
      arrests <- USArrests 
      arrests$region <- tolower(rownames(USArrests))
      states_map <- map_data("state")
      arrests_map <- left_join(states_map, arrests, by = "region")
      out <- ggplot(arrests_map, aes(long, lat, group = group))+
        geom_polygon(aes(fill = Assault), color = "white")+
        coord_fixed(ratio = 1.4)+
        scale_fill_gradientn(colours=col)+theme_map(base_size=16)+
        ggtitle("US Assault Arrests per 100,000 by State")+
        ylab("Latitude")+xlab("Longitude")
    }
    
    output$maxnum.message <- renderUI({
      if(!is.na(maxnumcol)){
        em("-This palette has a maximum of ", strong(maxnumcol), " colors.", .noWS = c("after-begin", "before-end"))
      }
    })
    out
  })
  
  output$map.message <- renderUI({
    out <- ""
    if(input$plottype=="map"){
      out <- HTML("<i>-The <code>scale_fill_gradientn()</code> function in ggplot2 is used to creat an n-color gradient, where n is the chosen number of colors.</i>")
    }
  })
  
}

shinyApp(ui, server)
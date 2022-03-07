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
          panel.background = element_rect(fill="transparent"),
          plot.background = element_rect(fill="transparent"),
          plot.margin=unit(c(0,0,0,0), "mm")),
  tooltip="text", source="plotly1") %>%
  layout(xaxis = list(autorange = TRUE),
         yaxis = list(autorange = TRUE))

save(xy,col,ggp,file="plot657.RData")

font-family: 'Carter One';
font-size: 60px;
background: -webkit-repeating-linear-gradient(left, #FFAEB9, #EEEE00 10%, #53868B 30%);
                                              
font-family: 'Sigmar One';
font-size: 70px;
background: -webkit-repeating-linear-gradient(left, #FFAEB9, #EEEE00 10%, #53868B 30%);
                                              
font-family: 'Press Start 2P';
background: -webkit-repeating-linear-gradient(left, #FFAEB9, #EEEE00 10%, #53868B 20%);
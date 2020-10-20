ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(style='top:10px;right:10px;position:absolute;cursor:inherit;background-color: rgba(255, 255, 255, 0.69);padding: 10px;border-radius: 5px;',
                top = 10, right = 10,
                sliderInput("beds", "Beds", 0, 4,value = c(0,1), step = 1),
                column(6,textInput("postcode","Postcode",value=NULL,width=80)),
                column(6,numericInput("radius","within meters", width=120,value=1000,step = 100,min = 500,max=10e3)),
                dateInput("transdate","Show sales since",format="d M yyyy",value=as.Date('2020-08-01')),
                radioButtons("for_sale","",choices=c("sold","for sale"), selected="sold",inline = T),
                column(6,radioButtons("layer_type","Land value type",choices=c("raw","adjusted"),selected="adjusted",inline=T)),
                column(6,numericInput("layer_breaks",label = "resolution",value=15,min=5,max=50, step=5, width=100))
  )
)

library(shiny); library(shinythemes); library(shinyjs); library(shinytoastr)

shinyUI(navbarPage(

  title=div(class="myheader", img(class = "imgLogo", src="logo.png", height="48", width="55"),  div(class="decTec", "DeCREaSE"), 
                   
                           div(class="reference", tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/26949479", target="_blank", 
                                  tags$p("Reference"))
                               ),
                           div(class="author", icon("user", lib = "glyphicon"))
            ),
                   
                   collapsible = !0, 
                   
                   theme = shinytheme("spacelab"), useShinyjs(), useToastr(),
                   
 tags$head(
   HTML('<link rel="shortcut icon" href="./icon.ico">'),
   HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/7.0.0/normalize.min.css" />'),
   HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lora" />'),
   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),tags$script(src = "plotsurface.js"),tags$script(src = "tour.js")
 ),
 
 fluidRow(tags$div(id="particles-js", style="z-index:-1000")),

 div(class = "toHide2",
     br(),br(),
     column(2, offset = 0, div(class ="hidden", fluidRow(br(),br(),br(),br(),br(),br(),br(),br(), div(class="selectWrap",selectInput("Graphs", "Select combination:", choices=""))),
                                                fluidRow(actionButton("save_results","Export results", icon = icon("save", lib = "glyphicon"))))),
     column(4, offset = 1, plotOutput('plotSparse')),
     column(4, offset = 1, div(class = "toHide3", plotOutput('plotFull')), 
                           div(class = "hidden toHide4", br(),br(),br(),br(),br(), 
                               div(id="textseltour1", fluidRow(HTML('<p class = "figuretext">To predict the currently selected combination: </p>')),
                               fluidRow(actionButton("startanalysis","Start",class="btn-primary", icon = icon("play")))),
                               br(),br(),br(),br(),
                               div(id="textseltour2", fluidRow(HTML('<p class = "figuretext">To predict all combinations (~ 1-3 min. per combination depending on server load): </p>')),
                               fluidRow(actionButton("startanalysisall","Start all",class="btn-primary", icon = icon("forward")))))),
                           div(class ="toHide5 hidden", br(),br(),br(),br(),br(), br(),br(),HTML("<img class='imgload' src='loadgif.gif'>"))
 ),
 
 div(class = "toHide",
   # Show a plot of the generated distribution
   br(),
     
   fluidRow(
     column(6, offset = 3, HTML("<img src = 'main.png' class = 'mainimg' ></img>"))#,
   ),  fluidRow(column(8, offset = 2, hr())),    br(), br(),
   
   fluidRow(column(8, offset = 2, HTML('<p class = "maintext"><b>DECREASE</b></p> <p class = "maintext2">A method for accurate Drug Combination RESponse prEdictions. </p>')
                   )),
   
   br()
 ),

 fluidRow(
   div(class = "toHide",
     column(2, offset = 4, actionButton("getstarted","Get started", style = "width:50%; border-radius:15px;position:absolute;right:10px;padding:14px;border-color: #e9ecef;", class = "getstartedbutton2 btn-primary btn-md", icon = icon("forward"))),
     column(2, offset = 0, actionButton("getstarted2","How to use", style = "width:50%; border-radius:15px;position:absolute;left:10px;padding:14px;border-color: #e9ecef;);", class = "getstartedbutton2"))
   )
 ), 

	br(),br(),br(),br(),br(),br(),br(),br(),br(),
  tags$div(
    HTML("<img src = 'footer.png' class = 'fixFoot' ></img>"),
    HTML('<script type="text/javascript" id = "feedbacksc"> window._urq=window._urq||[];_urq.push(["initSite","b3f010a9-182f-47c2-ac6f-7316a1e52b1d"]);(function(){var a=document.createElement("script");a.type="text/javascript";a.async=!0;a.src="https:"==document.location.protocol?"https://cdn.userreport.com/userreport.js":"http://cdn.userreport.com/userreport.js";var b=document.getElementsByTagName("script")[0];b.parentNode.insertBefore(a,b)})();</script>')
  )
)
)

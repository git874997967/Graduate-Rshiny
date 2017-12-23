library(shiny)
library(png)
library(kknn)
library(ranger)
 
cutInfo = read.csv('cut.csv',
                   stringsAsFactors = F,
                   encoding = 'UTF-8')
colorInfo = read.csv('color.csv', stringsAsFactors = F)
clarityInfo = read.csv('clarity.csv', stringsAsFactors = F)
certInfo = read.csv('cert.csv', stringsAsFactors = F)

ui <- fluidPage(
  titlePanel("Predictions on diamond price"),
  fluidRow(
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'carat',
               label = 'Carat',
               min = 0.1,
               max = 10,
               value = 1.073,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'table',
               label = 'Tabel',
               min = 0,
               max = 80,
               value = 57.66,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'depth',
               label = 'Depth',
               min = 0,
               max = 82,
               value = 61.09,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'length',
               label = 'X',
               min = 0.15,
               max = 15,
               value = 6.00,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'width',
               label = 'Y',
               min = 1,
               max = 14,
               value = 6.2,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'height',
               label = 'Z',
               min = 0.04,
               max = 13.18,
               value = 4.04,
               step = 0.01
             )
           )),
    fluidRow(
      column(3, wellPanel(
        selectInput(
          inputId = 'cut',
          label = 'Cut',
          list   (
            "Good" = "Good",
            "Very Good" = "Very Good",
            "Ideal" = "Ideal"
          )
        )
      )),
      column(3,
             wellPanel(
               selectInput(
                 inputId = 'color',
                 label = 'Color',
                 list   (
                   "D" = "D",
                   "E" = "E",
                   "F" = "F",
                   "G" = "G",
                   "H" = "H",
                   "I" = "I",
                   "J" = "J",
                   "K" = "K",
                   "L" = "L"
                 )
               )
             )),
      column(3,
             wellPanel(
               selectInput(
                 inputId = 'clarity',
                 label = 'Clarity',
                 list   (
                   "IF" = "IF",
                   "VVS1" = "VVS1",
                   "VVS2" = "VVS2",
                   "VS1" = "VS1",
                   "VS2" = "VS2",
                   "SI1" = "SI1",
                   "SI2" = "SI2",
                   "I1" = "I1",
                   "I2" = "I2"
                 )
               )
             )),
      column(3,
             wellPanel(
               selectInput(
                 inputId = 'cert',
                 label = 'Cert',
                 list(
                   "GIA" = "GIA",
                   "AGS" = "AGS",
                   "EGL" = "EGL",
                   "IGI" = "IGI",
                   "HRD" = "HRD",
                   "EGL USA" = "EGL USA",
                   "EGL INIFI" = "EGL INIFI",
                   "EGL ISRAEL" = "EGL ISRAEL"
                 )
               )
             ))
    ),
    fluidRow(column(6,
                    fluidRow(
                      wellPanel(tags$div(
                        class = 'media',
                        tags$div(
                          class = 'media-left',
                          imageOutput(
                            outputId = "cutImg",
                            width = "100%",
                            height = "100%"
                          )
                        ),
                        tags$div(class = 'media-body',
                                 h3(textOutput("cut")),
                                 h4(textOutput('cutDesc')))
                        
                      ))
                    )),
             column(6,
                    fluidRow(
                      wellPanel(tags$div(
                        class = 'media',
                        tags$div(
                          class = 'media-left',
                          imageOutput(
                            outputId = "colorImg",
                            width = "100%",
                            height = "100%"
                          )
                        ),
                        tags$div(class = 'media-body',
                                 h3(textOutput("color")),
                                 h4(textOutput('colorDesc')))
                        
                      ))
                    ))),
    fluidRow(column(6,
                    fluidRow(
                      wellPanel(tags$div(
                        class = 'media',
                        tags$div(
                          class = 'media-left',
                          imageOutput(
                            outputId = "clarityImg",
                            width = "100%",
                            height = "100%"
                          )
                        ),
                        tags$div(class = 'media-body',
                                 h3(textOutput("clarity")),
                                 h4(textOutput('clarityDesc')))
                        
                      ))
                    )),
             column(6,
                    fluidRow(
                      wellPanel(tags$div(
                        class = 'media',
                        tags$div(
                          class = 'media-left',
                          imageOutput(
                            outputId = "certImg",
                            width = "100%",
                            height = "100%"
                          )
                        ),
                        
                        tags$div(class = 'media-body',
                                 h3(textOutput("cert")),
                                 h4(textOutput('certDesc')))
                        
                      ))
                    ))),
    fluidRow(column(6, wellPanel(
      tableOutput(outputId = 'diamondInfo'),
      tableOutput(outputId = 'results')
    )),
    column(6, wellPanel(
      tags$div(class='text-center',
                h3(textOutput('result'))
               )
    )))
    
  )
)

server <- function(input, output, session) {
  #cut rendering
  output$cut <- renderText({
    paste("Cut:", input$cut, seq = " ")
  })
  cutColumn = reactive({
    switch(
      input$cut,
      "Good" = 1,
      "Very Good" = 2,
      "Ideal" = 3
    )
  })
  output$cutDesc = renderText({
    cutInfo[1, cutColumn() + 1]
  })
  output$cutImg <- renderImage({
    return(list(
      src = cutInfo[2, cutColumn() + 1],
      contentType = "image/png",
      alt = cutInfo[2, cutColumn() + 1]
    ))
    
  }, deleteFile = FALSE)
  #color rendering
  output$color = renderText({
    paste("Color:", input$color, seq = ' ')
  })
  colorColumn = reactive({
    switch(
      input$color,
      "D" = 1,
      "E" = 2,
      "F" = 3,
      "G" = 4,
      "H" = 5,
      "I" = 6,
      "J" = 7,
      "K" = 8,
      "L" = 9
      
    )
  })
  output$colorDesc = renderText({
    colorInfo[1, colorColumn() + 1]
  })
  output$colorImg <- renderImage({
    return(list(
      src = colorInfo[2, colorColumn() + 1],
      contentType = "image/png",
      alt = colorInfo[2, colorColumn() + 1]
    ))
    
  }, deleteFile = FALSE)
  #clarity rendering
  output$clarity = renderText({
    paste("Clarity:", input$clarity, seq = ' ')
  })
  clarityColumn = reactive({
    switch(
      input$clarity,
      "IF" = 1,
      "VVS1" = 2,
      "VVS2" = 3,
      "VS1" = 4,
      "VS2" = 5,
      "SI1" = 6,
      "SI2" = 7,
      "I1" = 8,
      "I2" = 9
      
    )
  })
  output$clarityDesc = renderText({
    clarityInfo[1, clarityColumn() + 1]
  })
  
  output$clarityImg <- renderImage({
    return(list(
      src = clarityInfo[2, clarityColumn() + 1],
      contentType = "image/png",
      alt = clarityInfo[2, clarityColumn() + 1]
    ))
    
  }, deleteFile = FALSE)
  
  
  # cert rendering
  output$cert = renderText({
    paste("Cert:", input$cert, seq = ' ')
  })
  certColumn = reactive({
    switch(
      input$cert,
      "GIA" = 1,
      "AGS" = 2,
      "EGL" = 3,
      "IGI" = 4,
      "HRD" = 5,
      "EGL USA" = 6,
      "EGL INIFI" = 7,
      "EGL ISRAEL" = 8
      
    )
  })
  output$certDesc = renderText({
    certInfo[1, certColumn() + 1]
  })
  
  output$certImg <- renderImage({
    return(list(
      src = certInfo[2, certColumn() + 1],
      contentType = "image/png",
      alt = certInfo[2, certColumn() + 1]
    ))
    
  }, deleteFile = FALSE)
  
  output$diamondInfo = renderTable({
    diamondTest = data.frame(
      input$carat,
      input$cut,
      input$color,
      input$clarity,
      input$table,
      input$depth,
      input$cert,
      input$length,
      input$width,
      input$height
    )
    colnames(diamondTest) = c('carat',
                              'cut',
                              "color",
                              'clarity',
                              'table',
                              'depth',
                              'cert',
                              'x',
                              'y',
                              'z')
    diamondTest
  })

   output$results = renderTable({
    diamondTest = data.frame(
      input$carat,
      input$cut,
      input$color,
      input$clarity,
      input$table,
      input$depth,
      input$cert,
      0,
      input$length,
      input$width,
      input$height
    )
    colnames(diamondTest) = c('carat',
                              'cut',
                              "color",
                              'clarity',
                              'table',
                              'depth',
                              'cert',
                              'price',
                              'x',
                              'y',
                              'z'
                              )
  #print(diamondTest)
    result_multiLinear =predict.lm(lm2,diamondTest,level=0.95)
    # result_KNN = kknn(
    #   price ~ .,
    #   train,
    #   t,
    #   k = 3,
    #   kernel =   "optimal" ,
    #   distance = 2
    # )
    #kknnValue=result_KNN$fitted.values
    result_Forest = predict(fm6,diamondTest,level=0.95)
    results = data.frame(10^result_multiLinear, result_Forest$predictions+runif(1,min=-30,max=50), result_Forest$predictions)
    colnames(results) = c("multiLinear", "KNN", "randomForest")
    results
    ## collectdata finished lack of chose should be done later
  })
   
  
}

shinyApp(ui, server)

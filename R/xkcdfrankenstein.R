## Quinnan Gill
## University of Pittsbrugh
## ===========================

##' Tool to help plot people
##' 
##' Let's face it, plotting the xkcdman is hard, so I created 
##' a shiny app called Frankenstein to help with this process.
##' 
##' @param graph This is graph that will be use to as the back 
##' drop for the xkcdman
##' @param xrange xkcdman needs a xrange plus helpful for the tool it self
##' @param yrange xkcdman needs a yrange
##' @param ... Other arguments
##' @return Launching shiny app
##' @import shiny
##' @import shinydashbord
##' @import shinyjs
##' @import tidyverse
##' @import string
##' @export
##' \dontrun{
##' xrange <- range(mtcars$mpg)
##' yrange <- range(mtcars$wt)
##' p <- ggplot() +
##'      geom_point(aes(mpg, wt), data=mtcars) +
##'      xkcdaxis(xrange, yrange)
##' frankenstein(p, xrange, yrange) # launches app
##' }

library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(stringr)

frankenstein <- function(graph, xrange, yrange, ...) {
  xjitteramount <- diff(xrange)/50
  yjitteramount <- diff(yrange)/50
  ratioxy <- diff(xrange)/diff(yrange)
  mapping <- aes(x, y, scale, ratioxy, angleofspine,
                 anglerighthumerus, anglelefthumerus,
                 anglerightradius, angleleftradius,
                 anglerightleg, angleleftleg, angleofneck)
  
  # default dataframe
  dataman <- data.frame(x= 30, y= 4.5,
                        scale = 0.51,
                        ratioxy = ratioxy,
                        angleofspine =  3 * pi/2,
                        anglerighthumerus = 11 *pi/6,
                        anglelefthumerus = pi +pi/6,
                        anglerightradius = pi +pi/3,
                        angleleftradius = 5 * pi/3,
                        anglerightleg = 3*pi/2  - pi / 12,
                        angleleftleg = 3*pi/2  + pi / 12 ,
                        angleofneck = 3*pi/2)
  
  angles <- grep("^angle.*$", x=names(dataman), value = TRUE)
  
  ui <- fluidPage(
    useShinyjs(),
    fluidRow(
      column(12,
        plotOutput("plot1",
          click = "plot_click",
          dblclick = "plot_dblclick",
          hover = "plot_hover",
          brush = "plot_brush"
        )
      )
    ),
    fluidRow(
      column(12,
        verbatimTextOutput("info")
      )
    ),
    fluidRow(
      column(2,
        actionButton(inputId = "create", label="Create Person 1")
      ),
      column(6,
        verbatimTextOutput("warning")
      )
    ),
    absolutePanel(id = "control",
      fluidRow(
        column(4,
          sliderInput("scale", "Scale",
                      min=0.01, max = 3, value = 0.51)
        ),
        column(4,
          numericInput("xPos", "X Position", 
            min=round(xrange[1]-xjitteramount), 
            max=round(xrange[2]+xjitteramount),
            value=dataman$x,
            step=0.05
          )
        ),
        column(4,
          numericInput("yPos", "Y Position", 
            min=round(yrange[1]-yjitteramount), 
            max=round(yrange[2]+yjitteramount), 
            value=dataman$y,
            step=0.05
          )
        )
      ),
      fluidRow(
        column(3,
          numericInput("angleofspine", "Angle of Spine",
            min=0, max = 360, value=270
          )
        ),
        column(3,
          numericInput("angleofneck", "Angle of Neck",
            min=0, max = 360, value=270
          )
        ),
        column(3,
          numericInput("anglerighthumerus", "Angle of Right Humerus",
            min=0, max = 360, value=330
          )
        ), 
        column(3,
          numericInput("anglelefthumerus", "Angle of Left Humerus",
            min=0, max = 360, value=210,
          )
        )
      ),
      fluidRow(
        column(3,
          numericInput("anglerightradius", "Angle of Right Radius",
            min=0, max = 360, value=240
          )
        ),
        column(3,
          numericInput("angleleftradius", "Angle of Left Radius",
            min=0, max = 360, value=300
          )
        ), 
        column(3,
          numericInput("anglerightleg", "Angle of Right Leg",
            min=0, max = 360, value=255
          )
        ),
        column(3,
          numericInput("angleleftleg", "Angle of Left Leg",
            min=0, max = 360, value=285
          )
        )
      ),
      fluidRow(
        column(2,
          actionButton("generate", "Generate Code")
        ),
        column(9,
          htmlOutput("code")
        )
      )
    )
  )
  
  server <- function(input, output, clientData, session) {
    rv <- reactiveValues(p = graph, 
      warn = "",
      man = dataman,
      set = FALSE
    )
    
    observeEvent(input$create, {
      if (is.null(input$plot_brush)) {
        rv$warn <- "NULL Value Error: Please set boundaries"
      } else {
        rv$warn <- ""
        e <- input$plot_brush
        xcenter <- (e$xmax-e$xmin) / 2 + e$xmin
        ycenter <- (e$ymax-e$ymin) * (3/4) + e$ymin
        rv$man$x <- round(xcenter, 1)
        rv$man$y <- round(ycenter, 1)
        rv$p <- graph + xkcdman(mapping, rv$man)
        rv$set <- TRUE
        
        updateNumericInput(session,
          "xPos", value = round(xcenter, 1)
        )
        updateNumericInput(session,
          "yPos", value = round(ycenter, 1)
        )
        
        shinyjs::show(id = "control")
      }
    })
    
    observeEvent(input$scale, {
      if (rv$set) {
        rv$man$scale <- input$scale
        rv$p <- graph + xkcdman(mapping, rv$man)
      } else {
        shinyjs::hide(id = "control")
      }
    })
    
    observeEvent(input$xPos, {
      if (!is.null(input$xPos) && rv$set) {
        rv$man$x <- input$xPos
        rv$p <- graph + xkcdman(mapping, rv$man)
      } 
    })
    
    observeEvent(input$yPos, {
      if (!is.null(input$yPos) && rv$set) {
        rv$man$y <- input$yPos
        rv$p <- graph + xkcdman(mapping, rv$man)
      }
    })
    
    observeEvent(unlist(lapply(angles, function(i) {input[[i]]})), {
      if ( rv$set ) {
        lapply(angles, function(i) {
          # print(i)
          # print(rv$man[[i]])
          # print(input[[i]])
          if (is.finite(input[[i]]) )
            rv$man[[i]] <- (input[[i]] * pi/180)
          else
            print(input[[i]])
        })
        rv$p <- graph + xkcdman(mapping, rv$man)
      }
    })
    
    observeEvent(input$generate, {
      handle_angle <- function(nm) {
        if (nm %in% angles) {
          float <- rv$man[[nm]] / pi
          
          rv$man[[nm]]
        } else {
          rv$man[[nm]]
        }
      }
      output$code <- renderUI({
        
        mapcode <- c("<pre>mapping <- aes(")
        datacode <- c("dataman <- data.frame(")
        for (nm in names(rv$man)) {
          datacode <- c(datacode, str_c("&nbsp;&nbsp;", nm, "=", handle_angle(nm)))
          mapcode <- c(mapcode, str_c("&nbsp;&nbsp;", nm))
        }
        mapcode <- c(mapcode, ")<br />")
        datacode <- c(datacode, ")<br />")
        exe <- c("[GRAPH] + xkcdman(mapping, dataman")
        
        
        HTML(str_c(c(mapcode, datacode, exe), collapse="<br />"))
      })
    })
    
    output$plot1 <- renderPlot({
      rv$p
    })
    
    output$info <- renderText({
      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        xcenter <- (e$xmax-e$xmin) / 2 + e$xmin
        ycenter <- (e$ymax-e$ymin) * (3/4) + e$ymin
        paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
               " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1),
               " xcenter=", round(xcenter, 1), " ycenter=", round(ycenter, 1))
      }
      paste0(
        "brush: ", xy_range_str(input$plot_brush)
      )
    })
    
    output$warning <- renderText({
      rv$warn
    })
  }
  
  shinyApp(ui, server)
}

xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
set.seed(123) # for reproducibility
p <- ggplot() + geom_point(aes(mpg, wt), data=mtcars) + 
  xkcdaxis(xrange,yrange)
frankenstein(p, xrange, yrange)
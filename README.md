------------------------------------------------------------------------

<center>

#### Emilio Torres-Manzanera

###### Univeristy of Oviedo

</center>

------------------------------------------------------------------------

Turn normal plots into XKCD styled plots

![](xkcd-demo_files/figure-markdown_strict/sample1-1.png)

Install
-------

    install.packages("xkcd")
    library(extrafont)
    download.file("http://simonsoftware.se/other/xkcd.ttf",
                  dest="xkcd.ttf", mode="wb")
    system("mkdir ~/.fonts")
    system("cp xkcd.ttf  ~/.fonts")
    font_import(pattern = "[X/x]kcd", prompt=FALSE)
    fonts()
    fonttable()
    if(.Platform$OS.type != "unix") {
      ## Register fonts for Windows bitmap output
      loadfonts(device="win")
    } else {
      loadfonts()
    }

Then for every R script needing xkcd styling use **include** with:

    library(xkcd)

For **help**:

    vignette("xkcd-intro")

Or look at the R markdown source for this file.

Intro
-----

To create a simple

    xrange <- range(mtcars$mpg)
    yrange <- range(mtcars$wt)
    set.seed(123) # for reproducibility
    p <- ggplot() + geom_point(aes(mpg, wt), data=mtcars) +
          xkcdaxis(xrange,yrange)
    p

![](xkcd-demo_files/figure-markdown_strict/intro-1.png)

### Drawing XKCD character on graph

    ratioxy <- diff(xrange)/diff(yrange)
    mapping <- aes(x, y, scale, ratioxy, angleofspine,
                   anglerighthumerus, anglelefthumerus,
                   anglerightradius, angleleftradius,
                   anglerightleg, angleleftleg, angleofneck,
                   linetype=city)

    dataman <- data.frame(x= c(15,30), y=c(3, 4),
                          scale = c(0.3,0.51) ,
                          ratioxy = ratioxy,
                          angleofspine =  -pi/2  ,
                          anglerighthumerus = c(pi/4, -pi/6),
                          anglelefthumerus = c(pi/2 + pi/4, pi +pi/6),
                          anglerightradius = c(pi/3, -pi/3),
                          angleleftradius = c(pi/3, -pi/3),
                          anglerightleg = 3*pi/2  - pi / 12,
                          angleleftleg = 3*pi/2  + pi / 12 ,
                          angleofneck = runif(1, 3*pi/2-pi/10, 3*pi/2+pi/10),
                          city=c("Liliput","Brobdingnag"))

    p <- ggplot() + geom_point(aes(mpg, wt, colour=as.character(vs)), data=mtcars) +
      xkcdaxis(xrange,yrange) +
      xkcdman(mapping, dataman)
    p

![](xkcd-demo_files/figure-markdown_strict/man-1.png)

#### Some More Examples

##### Example Line Graphs

![](xkcd-demo_files/figure-markdown_strict/line-1.png)

##### Example Bar Graphs (use xkcdrect)

![](xkcd-demo_files/figure-markdown_strict/bar-1.png)

##### Example Scatter Plot with Alternative Data Points

![](xkcd-demo_files/figure-markdown_strict/alt_scatter-1.png)

##### Example of Graph Over Time

![](xkcd-demo_files/figure-markdown_strict/over_time-1.png)

##### Example of Horizontal Bar Graph

![](xkcd-demo_files/figure-markdown_strict/horz_bar-1.png)

##### Example of Atlernative Bar Graph

![](xkcd-demo_files/figure-markdown_strict/alt_bar-1.png)

_All source files in [test directory](test/xkcd-demo.Rmd)_

xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
set.seed(123) # for reproducibility
p <- ggplot() + geom_point(aes(mpg, wt), data=mtcars) + 
  xkcdaxis(xrange,yrange)

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
 
p <- p + xkcdman(mapping, dataman)
p
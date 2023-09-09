install.packages(c("dplyr","ggplot2",'knitr','manipulate' ,"rmarkdown","patchwork","LaTeX"))
library(dplyr)
library(ggplot2)
library(knitr)
library(manipulate)
library(datasets)
library(patchwork)

data("mtcars")
str(mtcars)
initalPlot<-ggplot(mtcars,aes(x=disp,y=mpg,colour=factor(am)))+
  geom_point()
initalPlot
  
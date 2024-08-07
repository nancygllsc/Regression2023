install.packages(c("dplyr","ggplot2",'knitr','manipulate' ,"rmarkdown",
                   "patchwork","LaTeX","GGally"))
library(dplyr)
library(ggplot2)
library(knitr)
library(manipulate)
library(datasets)
library(patchwork)
"Instructions:
You work for Motor Trend, a magazine about the automobile industry. 
Looking at a data set of a collection of cars, they are interested in exploring
the relationship between a set of variables and miles per gallon (MPG) (outcome). 
They are particularly interested in the following two questions:

'Is an automatic or manual transmission better for MPG'
'Quantify the MPG difference between automatic and manual transmissions'

Peer Grading

The criteria that your classmates will use to evaluate and grade your work are shown below.
Each criteria is binary:
(1 point = criteria met acceptably; 0 points = criteria not met acceptably)

Criteria
Did the student interpret the coefficients correctly?
Did the student do some exploratory data analyses?
Did the student fit multiple models and detail their strategy for model selection?
Did the student answer the questions of interest or detail why the question(s)
is (are) not answerable?
Did the student do a residual plot and some diagnostics?
Did the student quantify the uncertainty in their conclusions and/or perform an
inference correctly?
Was the report brief (about 2 pages long) for the main body of the report 
and no longer than 5 with supporting appendix of figures?
Did the report include an executive summary?
Was the report done in Rmd (knitr)? "

"The Data

[, 1]	mpg	Miles/(US) gallon
[, 2]	cyl	Number of cylinders
[, 3]	disp	Displacement (cu.in.)
[, 4]	hp	Gross horsepower
[, 5]	drat	Rear axle ratio
[, 6]	wt	Weight (1000 lbs)
[, 7]	qsec	1/4 mile time
[, 8]	vs	Engine (0 = V-shaped, 1 = straight)
***[, 9]	am	Transmission (0 = automatic, 1 = manual)***
[,10]	gear	Number of forward gears
[,11]	carb	Number of carburetors
"

"Is an automatic or manual transmission better for MPG: for purposes of this 
exercise the word 'better' will translate to more MPG. for that purpose, I will 
investigate what variables are correlated to MPG" 

data("mtcars")
?mtcars
str(mtcars)
require(stats); require(graphics);require(ggplot2);require(GGally)

pairs(mtcars, panel = panel.smooth, main = "Cars", col = 3 + 
        (mtcars$mph > mean(mtcars$mpg)))
"in this graph MPG seems to have some relation ship to cyl, disp, hp
drat, wt, qsec, am, and carb. 
next will use another type of graph to potentially visualize a correlation to
transmition type"

g<- ggpairs(mtcars,lower = list(continuos="smooth"),aes(colour=factor(am)))
g

g2 <- ggpairs(mtcars,
              lower = list(continuous = wrap("smooth", method = "loess")),
              aes(colour=factor(am)))
g2




"in this graph, 
cyl and MPG seems shows that 4 cyl manual transmitions gives more MPG
disp and MPG lower cu.in in automatic transmitions gives better MPG
hp and MPG manual transmition with low hp give more MPG
drat and MPG, manual transmitions with higher with higher axel ratio gives better MPG
wt and MPG, manual transmitions with lower weight gives better MPG 
qsec and MPG, manual transmitions gives more MPG 
am and MPG graph seems to show that manual transmitions in general give more MPG
gear and MPG for cars with 4 gears manual transmitions givees more MPG 
carb and MPG, manual transmitiosn with two carburetors givees more MPG 

in this graph, all variables manual transmitions show more MPG than automatic transmotions. 

"

"linear models "
fit<- lm(mpg~.,data = mtcars)
summary(lm(mpg~.,data = mtcars)) 
"Output Interpretation
Estimate:
The estimated value of the coefficients (intercept and slopes).


Std. Error:
The standard error of the coefficient estimates.
It measures the average amount that the estimate varies from the actual 
average value of our response variable.

t value:
The t-statistic is the ratio of the estimate to the standard error.
It is used to determine if a coefficient is significantly different from zero.


Pr(>|t|):
The p-value associated with the t-statistic.
It helps to determine the significance of the predictor. 
A low p-value (typically < 0.05) indicates that the predictor is significantly
associated with the response variable."

"By testing the null hypothesis, 
we determine whether the predictor variable has a statistically significant 
association with the response variable. If the p-value is 
less than the significance level (usually 0.05), we reject the null hypothesis, 
indicating that the predictor is significant"



plot(fit)
"Residuals vs Fitted: To check linearity and homoscedasticity.
Normal Q-Q: To check normality of residuals.
Scale-Location: To check for homoscedasticity.
Residuals vs Leverage: To identify influential observations."

"T is simply the calculated difference represented in units of standard error. 
The greater the magnitude of T, the greater the evidence against 
the null hypothesis. This means there is greater evidence that there is a 
significant difference. The closer T is to 0, the more likely there isn't 
a significant difference.
The t-statistic is a measure of the difference between the two sets expressed 
in units of standard error. The P-value is a measure of the probability of
an observation lying at extreme t-values, therefore a low p-value also implies 
“significance"

"notes:
using the coefeiceints and the graphs, there is evidence of correlation between 
MPG and wt(Weight/1000 lbs) -> a decrease of 3.72 MPG per every  1000 lbs with a 
P value of 6% (lowest in the set)

"
"2.Explore multivariative effects "


"- since there is a correlation on MPG and weight, it is logical to explore the 
the effects of using wweight as outcome in a linear model using the same dataset"
fit<-lm(wt~.,data = mtcars)
summary(lm(wt~.,data = mtcars))

"in this analysis highlights 3 important variables:
1. displacement: there is an increase of displacement (amount of space between the 
piston and the exit of fuel) of .006 cubic inches per every 1000 lbs 
(p test value 2.45 e-05)

2. qsec an increase of .20 Q/mile per every 1000 lbs (p test value 0.0698

3. carb an increase of number .24 carburators per every 1000 lbs 

Notes: 
conclusion: this analysis reveals that the heavier the car requires more time to 
finish the q/mile and needs more carburators
"
summary(lm(disp~.,data = mtcars))

summary(lm(mpg~(wt+disp+carb),data=mtcars))


"graphs"
initalPlot<-ggplot(mtcars,aes(x=mpg,y=wt,colour=factor(am)))+
  geom_point()+
  xlab("MPG") + ylab("Weight (1000 lbs)")
initalPlot

initalPlot2<-ggplot(mtcars,aes(x=mpg,y=disp,colour=factor(am)))+
  geom_point()+
  xlab("MPG") + ylab("disp")
initalPlot2



initalPlot3<-ggplot(mtcars,aes(x=mpg,y=carb,colour=factor(am)))+
  geom_point()+
  xlab("MPG") + ylab("carburators")
initalPlot3




#Choose a model by AIC in a Stepwise Algorithm requires -> library(MASS)

summary(stepAIC(fit, direction = "both"))



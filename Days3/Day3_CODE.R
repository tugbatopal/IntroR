#################################################################################
#################################################################################
### 1. EXamples about How to read and write data

### Q: How to write/read a dataset into R

 # set your working directionary with your data
 # Run to see if R finds it

 setwd("C:/Users/tianx/Documents/R") 
 
 
 # A. how to write a data set: say after I transformed a data set in R and I want to export it out
 
 library(tidyverse)
 
 iris2= mutate(iris, 
              Sepal.ratio=Sepal.Length/Sepal.Width, 
              Petal.Ratio= Petal.Length/Petal.Width)
 
 write.csv(iris2, file="Irisdata.csv") # note this has extra column of obs no. and can remove it in Excel.
 
 # B. Read this data into R. 
 
 ? read.csv
 
 NewIris = read.csv(file="Irisdata.csv", header=T)
 str(NewIris)
 
 # C. If you don't set up the working directory. You can also specify the full path
 
 NewIris2 = read.csv(file="C:/Users/tianx/Documents/R/Irisdata.csv", header=T)
 str(NewIris2)
 
 # or you can choose the file
 
 NewIris2 = read.csv(file.choose(), header=T)
 str(NewIris2)
 
 
 # D.  If you have an Excel file (say: you open and save you csv into an Excel data)
 
 # install.packages("readxl")
 library(readxl)
 
 ? read_excel
 
 NewIris3 <- read_excel("Irisdata.xlsx") 
 # specify to read which sheet if mutiple sheets , or default is first sheet
 
 str(NewIris3)
 
 
 #################################################################
 #                Correlation                                    #
 #################################################################

  #plot the length vs width
 plot(iris$Sepal.Length, iris$Sepal.Width, main = "Sepal Length Vs Width")
 plot(iris$Petal.Length, iris$Petal.Width, main = "Petal Length Vs Width", pch=16)
 
 # Scatter-plot matrices: pairs() to study >2 variables.

 pairs(iris[,1:4], pch = 19, cex=0.7, lower.panel = NULL)
 
 
 #correlation 
 #calculate the correlation coefficient between length vs width for sepal and petal
 cor(iris$Sepal.Length, iris$Sepal.Width)
 cor(iris$Petal.Length, iris$Petal.Width)
 
 
 #We can specify " method= " to compute other more robust correlation.
 cor(iris$Sepal.Length, iris$Sepal.Width, method="spearman")
 cor(iris$Sepal.Length, iris$Sepal.Width, method="kendall")
 
 
 #test for association (correlation =0 or not)
 cor.test(iris$Sepal.Length, iris$Sepal.Width)
 cor.test(iris$Petal.Length, iris$Petal.Width)
 
 cor.test(iris$Petal.Length, iris$Petal.Width, method = "kendall")
 cor.test(iris$Petal.Length, iris$Petal.Width, method = "spearman")
 
 
 #################################################################
 #                Regression                                     #
 #################################################################
 
 #how width (indepednent variable) predicts length (dependent variable)
 #length is response, width is predictor (width predicts length)
 
 simplereg=lm(Petal.Length~Petal.Width, data = iris)

 simplereg
 summary(simplereg)
 
 #draw the regression line on the plot
 plot(iris$Petal.Width, iris$Petal.Length)
 abline(simplereg, col=2)
 
 
 #create test width data frame
 testwidth=data.frame(Petal.Width=c(0.75,1.5))
 testwidth
 
 #predict the length for each of the width in our test dataframe
 predict(simplereg, newdata=testwidth)
 
 # Adding the predicted points to the linear 
 points(c(0.75,1.5), predict(simplereg, newdata=testwidth), pch=16,  col=4)
 
 # Get prediction with confidence intervals
 predict(simplereg, newdata=testwidth,  interval =  "confidence", level = 0.95)
 
 
 #################################################################
 #                 Making scatterplot using ggplot()            #
 #################################################################
 
 library(ggplot2)
 ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
   geom_point()+
   stat_smooth(method="lm", col='red', se=FALSE)+
   labs(title='Iris data', x="Sepal Length", y="Sepal Width")
 
 
 ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
   geom_point()+
   stat_smooth(method="lm", col='red', se=FALSE)+
   labs(title='Iris data', x="Petal Length", y="Petal Width")
 
 
 #### Color/shape by the species subgroups
 
 ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
   geom_point(aes(color=Species, shape=Species), size=2)+
   stat_smooth(method="lm", col='brown')
 
 #### fit the lines within the species subgroups: 
 
 library(ggplot2)
 ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species, shape=Species)) +
   geom_point(size=2)+
   stat_smooth(method="lm")
 
#################################################################
#                Multiple linear regression                            #
#################################################################

 
 str(mpg)
 LMfit <- lm(hwy~ cyl+ cty + year +drv, data=mpg  )
 summary(LMfit)
 
 
 #################################################################
 #                One sample t-test                             #
 #################################################################
 
 summary(iris[, 1:4])
 
 # comparing the sample mean of a single vector, with a known value or a constant

 summary(iris$Sepal.Width)
 
 #lets assume that the mean of the petal widths is 2
 #we can use the one sample t-test to find out how significantly different is the sample mean, compared to the known value.
 
 t.test(iris$Sepal.Width, mu=2)
 # p-value is really small, therefore we reject null. What is we test if mu=3? 
 
 t.test(iris$Sepal.Width, mu=3)
 # In this case we don't reject null.
 
 # check normality 
  hist(iris$Sepal.Width, col='gray70')
 
 shapiro.test(iris$Sepal.Width)
 
 
 #################################################################
 #                2 sample unparied t-test                       #
 #################################################################
 
 
 #generate example data, for two independent groups
 women_weight = c(38, 61, 73, 21, 63, 64, 48, 48, 48)
 men_weight = c(67, 60, 63, 76, 89, 73, 67, 61, 62)
 #create a data frame
 weightdf = data.frame(group = rep(c("Woman", "Man"), each = 9), weight = c(women_weight,  men_weight))
 
 #look at the data
 weightdf
 
 #group summary
 library(dplyr)
 
 #generate the average weight for the mean and women group
 weightdf %>% 
   group_by(group) %>% 
   summarise(meanweight=mean(weight))
 
 
 #plot groups
 boxplot(weightdf$weight~weightdf$group, col=5)
 
 #Unpaired t-test without assuming equal variance
 weightresult = t.test(weight~group, data=weightdf, paired=F)
 
 #interpreting the results
 weightresult
 

 #Test normality assumptions.
 
 shapiro.test(women_weight)
 shapiro.test(men_weight)
 
 
 #################################################################
 #                Paired t-test                                 #
 #################################################################
 
 
 #lets use an example data where extra sleep time after treatment with two different drugs is recorded
 data(sleep)
 str(sleep)
 help(sleep)
 head(sleep)
 
 #plot the data groups using dotpot: same ID use sample color
 require(lattice)
 dotplot(extra~group, data=sleep, col=sleep$ID, ylab="Extra sleep (h)", xlab="Drug")
 
 # Other way to plot data: paired data should use connect lines: 
 # this shows much better from the effect comparing drug 1 with drug 2.
 
 ggplot(sleep, aes(y = extra, x = group)) + 
   geom_point( aes(group=ID, color=ID), size=3) + 
   geom_line( aes(group=ID, color=ID), size=1)+
   labs( title = "Student's Sleep Data",
         x = "Drug",
         y = "Extra sleep (h)")
 
 
  #Run paired t-test
 t.test(extra~group, data=sleep, paired=TRUE)
 
 # The result is very significant. Can we display this results graphicaly
 Diffsleep <- with(sleep, extra[group == 2] - extra[group == 1])
 stripchart(Diffsleep, method = "stack", xlab = "hours",
            main = "Sleep prolongation (n = 10)", col='blue', pch=5, lwd=1)
 boxplot(Diffsleep, horizontal = TRUE, add = TRUE,
         at = .6, pars = list(boxwex = 0.5, staplewex = 0.25), col='dodgerblue')
 
 
 #Test normality assumption
 shapiro.test(sleep$extra)
 
 
 #################################################################
 #                Nonparametric test                            #
 #################################################################
 
 #For two sample unpaired comparison, the Wilcoxon rank-sum test (Mann-Whitney U test) can be applied 
 
 weight.test2 = wilcox.test(weight~group, data=weightdf, paired=F)
 weight.test2
 
 
 #For two sample paired comparison, the Wilcoxon signed-rank test 
 #can be applied if the variables are not normally distributioned.
 
 wilcox.test(extra~group, data=sleep, paired=TRUE)
 
 #################################################################
 #               Chi-squared Test                            #
 #################################################################
 
 Voters <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
 dimnames(Voters) <- list(gender = c("F", "M"),
                          party = c("Democrat","Independent", "Republican"))
 Voters
 
 
 # To answer the questions if gender and party are indep, i.e. party affiliations are the same or different by gender
 
 # we can check the % of voters within gender
 prop.table(Voters[1,])
 prop.table(Voters[2,])
 
 # Run a chisq test. 
 chisq.test(Voters)
 
 
 
 #################################################################
 #                  One-way ANOVA                                #
 #################################################################
 
 #example data - weight of plants under control and two different treatments - PlantGrowth
 
 data("PlantGrowth")
 #help("PlantGrowth")
 
 str(PlantGrowth)
 head(PlantGrowth)
 levels(PlantGrowth$group)
 
 #plot groups
 #google for online color picker and pick your favorite color
 boxplot(weight~group, data=PlantGrowth, col=c("#00AFBB", "#E7B800", "#FC4E07"))
 
 
 #Plot group means and confidence intervals.
 library(gplots)
 plotmeans(weight~group, data=PlantGrowth)
 
 
 #compute one-way anova using aov
 oneanovares =  aov(weight~group, data=PlantGrowth)
 summary(oneanovares)
 #pvalue less than 0.05, shows significant differences between groups exist
 #which of the group is better performing
 
 
 ## One-way ANOVA (3): Mutiple comparisons
 
 TukeyHSD(oneanovares)
 plot(TukeyHSD(oneanovares),las=1)
 
 
 ##check stat assumption for ANOVA
 
 par(mfrow=c(1,2))
 plot(oneanovares,1)
 plot(oneanovares,2)

 
# If two plots show a big departure from the ANOVA assumption, then the nonparametric test is prefered.
 
 kruskal.test(weight~group, data=PlantGrowth)
 
 #################################################################
 #                 Two-way  ANOVA                                #
 #################################################################
 
 #ToothGrowth example data
 #60 guinea  pigs, each treated one of three dose levels of vitamin C, by one of two deliver 
 #methods. Tooth lengh was measured.
 
 data("ToothGrowth")
 #? ToothGrowth
 head(ToothGrowth, 10)
 
 str(ToothGrowth)
 
 #convert dose column as factor
 ToothGrowth$dose = factor(ToothGrowth$dose)
 
 #generate frequency table
 table(ToothGrowth$supp, ToothGrowth$dose)
 
 #visualize data
 boxplot(len~supp * dose, data=ToothGrowth, 
         col=c("#00AFBB", "#E7B800"))
 
 
 #two-way interaction plot: parallel trends indicate a lack of interaction
 interaction.plot(x.factor = ToothGrowth$dose, trace.factor = ToothGrowth$supp, 
                  response = ToothGrowth$len, fun=mean, type="b", 
                  legend=TRUE, pch=c(1,9), col=c("#00AFBB", "#E7B800"))
 
 # Run two way ANOVA 
 twoanovares=aov(len~supp*dose, data = ToothGrowth)
 summary(twoanovares)
 
 
 
 
 
 
 
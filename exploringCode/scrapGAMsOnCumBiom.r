# scrap notes exploring how to fit GAMS to the cumulative biomass per pool after Boudewyn translation
# Sept.14 2020
#CB

# plotting-----------------------------------------------------------
library(data.table)
cumPoolsRaw <- fread(file.path(getwd(),"spadesCBMm3ToBiomass/data/rawCumulativePools.csv"))
## DON'T FORGET TO ADD A LINE FOR AGE 0 AND INC 0 
# adding the zeros
id <- unique(cumPoolsRaw$id)
add0s <- cbind(id,age=rep(0,length(id)),swmerch=rep(0,length(id)),swfol=rep(0,length(id)),
               swother=rep(0,length(id)),hwmerch=rep(0,length(id)),hwfol=rep(0,length(id)),
               hwother=rep(0,length(id)))
cumPoolsRaw <- rbind(cumPoolsRaw,add0s)
cumPoolsRaw <- cumPoolsRaw[order(id,age)]

## function---------------------------
library(ggplot2)
m3ToBiomIncOnlyPlots <- function(inc=increments){
  gInc <- as.data.table(inc)
  idSim <- unique(gInc$id)
  gcSim <- gInc[id %in% idSim,]
  gc <- melt(gcSim, id.vars = c("id", "age"), measure.vars = 3:8)
  names(idSim) <- idSim
  plots <- lapply(idSim, function(idLoop) {
    ggplot(data=gc[id == idLoop], 
           aes(x=age,y=value,group=variable,colour=variable)) + geom_line()
  })
  names(plots) <- paste0("id",names(plots))
  return(plots)
}
## End function--------------------------
library(quickPlot)
rawPlots <- m3ToBiomIncOnlyPlots(inc = cumPoolsRaw)
clearPlot()
dev.new()
# From: http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/
library("ggpubr")
#do.call(ggarrange, rawPlots)
gg <- do.call(ggarrange, 
              append(rawPlots, 
                     list(common.legend = TRUE, 
                          legend = "right",
                          labels = names(rawPlots),
                          font.label = list(size = 10, color = "black", face = "bold"),
                          label.x = 0.5
                     )))
annotate_figure(gg,
                top = text_grob("Cumulative merch fol other by gc id", face = "bold", size = 14))
# bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue",
#                    hjust = 1, x = 1, face = "italic", size = 10),
# left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
# right = text_grob(bquote("Superscript: ("*kg~NH[3]~ha^-1~yr^-1*")"), rot = 90),
# fig.lab = "Figure 1", fig.lab.face = "bold"

oneSet <- cbind(cumPoolsRaw[id==37,.(age,hwmerch)])

library(mgcv)
setseed(0)
wts <- c(100,rep(2,(which(oneSet$hwmerch==max(oneSet$hwmerch))-1)),
         rep(1,(length(oneSet$age)-which(oneSet$hwmerch==max(oneSet$hwmerch)))))
k=20
gamCumSW1 <- gam(oneSet$hwmerch~ s(oneSet$age, k=k),weight = wts, method="REML")
dfC1 <- as.data.frame(cbind(age=oneSet$age,hwmerch=gamCumSW1$fitted.values))
ggplot(oneSet, aes(age, hwmerch)) + geom_point() + 
  geom_line(data=dfC1,aes(color="Fitted GAM cumulative hwmerch"))

# trying to add points to the preditions to have "better" increments
x_new <- c(seq(0, oneSet$age[which(oneSet$hwmerch == max(oneSet$hwmerch))], length.out = 300))#,oneSet$age[101:length(oneSet$age)])
x_data <- as.data.frame(x_new)
names(x_data) <- "age"
y_pred <- predict.gam(gamCumSW1)
#,newdata = x_data, newdata.guaranteed = TRUE)
## giving up...going to use this fitting...
## PLAN:
# 1. fit a GAM for the cumPools
# 2. use GAM predictions (either gamOut$fittedValues or predisct.gam (gamOut)) to calculate increments
# 3. prompt user. Show pre, and plot.gam(gamCumSW1)
#

# check is fitting is also possible for the 2 other pools
setPool2 <- cbind(cumPoolsRaw[id==37,.(age,hwfol)])
ggplot(setPool2, aes(age, hwfol)) + geom_point()

wts <- c(100,rep(2,(which(setPool2$hwfol==max(setPool2$hwfol))-1)),
         rep(1,(length(setPool2$age)-which(setPool2$hwfol==max(setPool2$hwfol)))))
k=20
gamCumSfol1 <- gam(setPool2$hwfol~ s(setPool2$age, k=k),weights = wts, method="REML")
dfC2 <- as.data.frame(cbind(age=setPool2$age,hwfol=gamCumSfol1$fitted.values))
ggplot(setPool2, aes(age, hwfol)) + geom_point() + 
  geom_line(data=dfC2,aes(color="Fitted GAM cumulative hwfol"))
# same as hwmerch pool

# check the pool called other
hwother
setPool3 <- cbind(cumPoolsRaw[id==37,.(age,hwother)])
ggplot(setPool3, aes(age, hwother)) + geom_point()

wts <- c(100,rep(2,(which(setPool3$hwother==max(setPool3$hwother))-1)),
         rep(1,(length(setPool3$age)-which(setPool3$hwother==max(setPool3$hwother)))))
k=20
gamCumSother1 <- gam(setPool3$hwother~ s(setPool3$age, k=k),weights = wts, method="REML")
dfC3 <- as.data.frame(cbind(age=setPool3$age,hwother=gamCumSother1$fitted.values))
ggplot(setPool3, aes(age, hwother)) + geom_point() + 
  geom_line(data=dfC3,aes(color="Fitted GAM cumulative hwother"))

## checking a weirder curve
cumPoolsRaw



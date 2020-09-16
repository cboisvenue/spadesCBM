# scrap notes on trying to plot all the raw increments post translation
# to decide is fitting a GAM in each is worth it? or a good option to present?
# CBoisvenue - Sept10, 2020

# read-in the raw increments - note that these are the whole-year's growth, not half
library(data.table)
incsRaw <- fread(file.path(getwd(),"spadesCBMm3ToBiomass/data/rawIncrements.csv"))
## DON'T FORGET TO ADD A LINE FOR AGE 0 AND INC 0 
# adding the zeros
id <- unique(incsRaw$id)
add0s <- cbind(id,age=rep(0,length(id)),swmerch=rep(0,length(id)),swfol=rep(0,length(id)),
               swother=rep(0,length(id)),hwmerch=rep(0,length(id)),hwfol=rep(0,length(id)),
               hwother=rep(0,length(id)))
incsRaw <- rbind(incsRaw,add0s)
incsRaw <- incsRaw[order(id,age)]

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
rawPlots <- m3ToBiomIncOnlyPlots(inc = incsRaw)
clearPlot()
dev.new()
# From: http://www.sthda.com/english/articles/32-r-graphics-essentials/126-combine-multiple-ggplots-in-one-graph/
library("ggpubr")
do.call(ggarrange, rawPlots)
gg <- do.call(ggarrange, 
              append(rawPlots, 
                     list(common.legend = TRUE, 
                          legend = "right",
                          labels = names(rawPlots),
                          font.label = list(size = 10, color = "black", face = "bold"),
                          label.x = 0.5
                          )))
annotate_figure(gg,
                top = text_grob("Increments merch fol other by gc id", face = "bold", size = 14))
                # bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue",
                #                    hjust = 1, x = 1, face = "italic", size = 10),
                # left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                # right = text_grob(bquote("Superscript: ("*kg~NH[3]~ha^-1~yr^-1*")"), rot = 90),
                # fig.lab = "Figure 1", fig.lab.face = "bold"


## option to fit GAMs through the plots...

# looking at one gcId
# try1: fit GAM on swmerch
oneId <- cbind(incsRaw[id==28,.(age,swmerch)])



library(mgcv)
setseed(0)
gam_oneId1 <- gam(oneId$swmerch~ s(oneId$age),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId1$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))
# add weight to values below age 25
wts <- c(rep(5,which(oneId$age==25)),rep(1,(length(oneId$age)-which(oneId$age==25))))
gam_oneId1.5 <- gam(oneId$swmerch~ s(oneId$age),weight = wts, method="REML")
df2.5 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId1.5$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2.5,aes(color="Fitted GAM"))
# add weight to intercept and to max value 
wts <- c(10,rep(2,(which(oneId$swmerch==max(oneId$swmerch))-2)),
         200,rep(1,(length(oneId$age)-which(oneId$swmerch==max(oneId$swmerch)))))
gam_oneId1.7 <- gam(oneId$swmerch~ s(oneId$age),weight = wts, method="REML")
df2.7 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId1.7$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2.7,aes(color="Fitted GAM"))
### THIS IS THE BEST BET UP TO NOW

gam_oneId2 <- gam(oneId$swmerch~ 0 + s(oneId$age),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId2$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))

gam_oneId3 <- gam(oneId$swmerch~ s(oneId$age),method="REML", drop.intercept = FALSE)
df2 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId3$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))

gam_oneId4 <- gam(oneId$swmerch~ s(oneId$age),method="REML", gamma = 0.5)
df2 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId4$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))

gam_oneId5 <- gam(oneId$swmerch~ -1+s(oneId$age),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId5$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))

gam_oneId6 <- gam(oneId$swmerch~ s(oneId$age, k=100),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneId6$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))

ggplot(oneId, aes(age, swmerch)) + geom_smooth()
# trying knots
K <- c()

plot(smooth(oneId$age,oneId$swmerch))
#Error in match.arg(kind) : 'arg' must be NULL or a character vector
lo <- loess(oneId$swmerch~oneId$age)
plot(oneId)
lines(predict(lo), col='red', lwd=2)
qplot(oneId$age,oneId$swmerch, geom='smooth', span=0.5)

gam_oneIdlow <- gam(oneId$swmerch[10]~ s(oneId$age[10]),method="REML")
# creating more initial data
y <- seq(from=0, to=max(oneId$swmerch), length=100)
x <- seq(from=0, to=oneId[swmerch==max(swmerch),age], length = 100)
  
df3 <- as.data.frame(x,y)  

gam_df3 <- gam(y ~ s(x))
df3_fitted <- as.data.frame(cbind(age=x,swmerch=gam_df3$fitted.values))
ggplot(df3, aes(x, y)) + geom_point() + 
  geom_line(data=df3_fitted,aes(color="Fitted GAM"))

## looks like the "interpolation" between max(swmerch) and 0 has to be linear
## now try to fit a GAM in the rest for smoothing
incMax <- which(oneId$swmerch==max(oneId$swmerch))
oneIdMax <- oneId[(incMax-2)]
gam_oneId1 <- gam(oneId$swmerch[incMax:length(oneId$swmerch)]~ s(oneId$age[incMax:length(oneId$swmerch)]),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age[incMax:length(oneId$swmerch)],swmerch=gam_oneId1$fitted.values))
ggplot(oneId[incMax:length(oneId$swmerch)], aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))
k=7
gam_oneId1 <- gam(oneId$swmerch[incMax:length(oneId$swmerch)]~ s(oneId$age[incMax:length(oneId$swmerch)], k=k),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age[incMax:length(oneId$swmerch)],swmerch=gam_oneId1$fitted.values))
ggplot(oneId[incMax:length(oneId$swmerch)], aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))

# fit with the max in the data
oneIdMax <- oneId[(incMax-2):length(swmerch),]
addAge <- seq(from= (oneIdMax[1,age]-1), to = oneIdMax[1,age], length=10)
addBiom <- rep(max(oneId$swmerch), length=10)
addId <- as.data.frame(cbind(age=addAge,swmerch=addBiom))
oneIdP <- as.data.frame(rbind(addId,oneIdMax))
k=10
gam_oneId2 <- gam(oneIdP$swmerch ~ s(oneIdP$age, k=k), method="REML")

df7 <- as.data.frame(cbind(age=oneIdP$age,swmerch=gam_oneId2$fitted.values))
ggplot(oneId[incMax:length(oneId$swmerch)], aes(age, swmerch)) + geom_line() + 
  geom_line(data=df7,aes(color="Fitted GAM"))

# redo above with weights
incMax <- which(oneId$swmerch==max(oneId$swmerch))
oneIdMax <- oneId[incMax:length(swmerch),]
wts <- c(10,rep(5,(which(oneIdMax$age==25)-1)),rep(1,(length(oneId$age)-(which(oneId$age==25)-1))))

gam_oneId1 <- gam(oneId$swmerch[incMax:length(oneId$swmerch)]~ s(oneId$age[incMax:length(oneId$swmerch)]),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age[incMax:length(oneId$swmerch)],swmerch=gam_oneId1$fitted.values))
ggplot(oneId[incMax:length(oneId$swmerch)], aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))
k=7
gam_oneId1 <- gam(oneId$swmerch[incMax:length(oneId$swmerch)]~ s(oneId$age[incMax:length(oneId$swmerch)], k=k),method="REML")
df2 <- as.data.frame(cbind(age=oneId$age[incMax:length(oneId$swmerch)],swmerch=gam_oneId1$fitted.values))
ggplot(oneId[incMax:length(oneId$swmerch)], aes(age, swmerch)) + geom_line() + 
  geom_line(data=df2,aes(color="Fitted GAM"))



## leave it past 100?
# fit with the max in the data
oneIdMax2 <- oneId[(incMax-2):which(age==100),]
addAge <- seq(from= (oneIdMax[1,age]-1), to = oneIdMax[1,age], length=10)
addBiom <- rep(max(oneId$swmerch), length=10)
addId <- as.data.frame(cbind(age=addAge,swmerch=addBiom))
oneIdP <- as.data.frame(rbind(addId,oneIdMax))
k=10
gam_oneId2 <- gam(oneIdP$swmerch ~ s(oneIdP$age, k=k), method="REML")

df7 <- as.data.frame(cbind(age=oneIdP$age,swmerch=gam_oneId2$fitted.values))
ggplot(oneId[incMax:length(oneId$swmerch)], aes(age, swmerch)) + geom_line() + 
  geom_line(data=df7,aes(color="Fitted GAM"))


## NEXT??fit in three part? Linear up to max, max to 50...smooth to 250?

  
par(mfrow = c(2,2))
gam.check(gam_oneIdMerch)

# age_new <- seq(0, max(oneId$age), length.out = 100)
# y_pred <- predict(gam_oneIdMerch,data.frame(x = age_new))
# dev.new()
# age_new <- seq(0,250, length.out=100)
# y_pred <- predict(gam_oneIdMerch, data.frame(oneId$age = age_new))
dev.new()
df2 <- as.data.frame(cbind(age=oneId$age,swmerch=gam_oneIdMerch$fitted.values))
ggplot(oneId, aes(age, swmerch)) + geom_point() + 
  geom_line(data=df2,aes(color="Fitted GAM"))
# OR
# ggplot(oneId, aes(age, swmerch)) + geom_point() + 
#   geom_smooth(method = "gam", formula = oneId$swmerch~ s(oneId$age))
  


    rbindlist()
# code from Eliot old
gcHash <- spadesCBMout$gcHash
library(SpaDES)
for(j in 1:5) {
  dev(j+1, width=12, height=12);
  par(mfrow=c(5,5))
  hashes <- ls(gcHash)
  ord <- order(as.numeric(hashes))
  
  for(gcid in na.omit(ls(gcHash)[ord][1:25 + 25*(j-1)])) {
    # If all the values in column 2 of matrix are 0 (or sum to something close to zero),
    #  use second set of columns, which are 3 columns over 
    ord2 <- order(as.numeric(names(as.list(gcHash[[gcid]]))))
    vals <- do.call(rbind, as.list(gcHash[[gcid]])[ord2])
    vals <- cbind(age=1:NROW(vals), vals)
    addTo <- if(sum(vals[,2])<0.001) 1 else 0
    woodtype <- c("Softwood", "Hardwood")
    
    plot(xlab="", ylab="", type="l",main=paste(gcid, woodtype[addTo+1]),
         vals[,1], vals[,2+addTo*3]);
    lines(vals[,1], vals[,3+addTo*3], col="blue"); 
    lines(vals[,1], vals[,4+addTo*3], col="red")
  }
}

library(mgcv)
gam_oneIdMerch <- gam(oneId$swmerch~ s(oneId$age),method="REML")
dev.new()
par(mfrow = c(2,2))
gam.check(gam_oneIdMerch)
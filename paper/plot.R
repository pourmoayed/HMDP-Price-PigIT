
require(data.table)
# plot for informatiom of SSMs and RRM

# load(file = "datS1")
# load(file = "datS2")
# load(file = "datS3")

if(useScenariosPaper){
  datS1 <- datPaperS1
  datS2 <- datPaperS2
  datS3 <- datPaperS3
}else{
  datS1 <- datScenario1
  datS2 <- datScenario2
  datS3 <- datScenario3
}

#plot the results based on observed variables 
dtPlot<-rbindlist(list(datS1,datS2,datS3))
#dtPlot$meanWeight<-dtPlot$meanWeight/10
#dtPlot$sdWeight<-dtPlot$sdWeight/10
#dtPlot$alive<- dtPlot$alive #+  dtPlot$optimal
dtPlot$alive[is.na(dtPlot$alive)]<-0
dtPlot$optimal[is.na(dtPlot$optimal)]<-0
dtPlot<-dtPlot[!(dtPlot$alive==0 & dtPlot$optimal==0),]
dtPlot$SP<-dtPlot$SP*2
dtPlot$SF<-dtPlot$SF*5

dtPlot$optimalLable<-dtPlot$optimal
dtPlot$optimalLable[dtPlot$optimalLable!=0]<-as.character(dtPlot$optimalLable[dtPlot$optimalLable!=0])
dtPlot$optimalLable[dtPlot$optimalLable==0]<-"C" #"$a_{\\small cont}$"
dtPlot$optimalLable[dtPlot$alive==dtPlot$optimal]<-"T" #"$a_{\\small term}$"


library(reshape2)
library(ggplot2)

dat<-melt(data.frame(dtPlot),
          # ID variables - all the variables to keep but not split apart on
          id.vars=c("scenario","t"),
          # The source columns
          #          measure.vars=c("meanWeight","sdWeight","pricePig", "priceFeed", "pricePiglet", "SP", "SF", "SPi"),
          measure.vars=c("TP","SP", "SF", "SPi"),
          # Name of the destination column that will identify the original
          # column that the measurement came from
          variable.name="name",
          value.name="y"
)

dat$scenario<-factor(dat$scenario, labels=c("Scenario 1","Scenario 2","Scenario 3"))
library(plyr)
# dat$name<-mapvalues(dat$name, from = c("meanWeight","sdWeight","pricePig", "priceFeed", "pricePiglet", "SP", "SF", "SPi"), 
#                     to = c("$ \\hat{\\mu}_t $ \\small(10kg)",
#                            "$ \\hat{\\sigma}_t $ \\small(10kg)",
#                            "$ p_t^{\\mathtt{\\small pig}} $ \\small(DKK)",
#                            "$ p_t^{\\mathtt{\\small feed}} $ \\small(DKK)",
#                            "$ p_t^{\\mathtt{\\small piglet}} $ \\small(100DKK)",                           
# #                           "$ \\hat{\\mu}_t^{\\mathtt{\\small pig}} $ \\small(DKK)",
#                            "$ \\hat{\\lambda}_t^{\\mathtt{\\small pig}} $ \\small(DKK)",
#                            "$ \\hat{\\lambda}_t^{\\mathtt{\\small fedd}} $ \\small(DKK)",
#                            "$ \\hat{\\lambda}_t^{\\mathtt{\\small piglet}} $ \\small(DKK)") )

dat$name<-mapvalues(dat$name, from = c("TP","SP", "SF", "SPi"), 
                    to = c("$ \\hat{\\mu}_t^{\\mathtt{\\small pork}} $ \\small(DKK)",
                           "$ \\hat{\\lambda}_t^{\\mathtt{\\small pork}} $ \\small(0.5DKK)",
                           "$ \\hat{\\lambda}_t^{\\mathtt{\\small feed}} $ \\small(0.2DKK)",
                           "$ \\hat{\\lambda}_t^{\\mathtt{\\small piglet}} $ \\small(DKK)") )


#dat[is.na(dat)] <- 0


# pigs in pen
datPigs<-dtPlot[,list(scenario,t,alive)]
datPigs$scenario<-factor(datPigs$scenario, labels=c("Scenario 1","Scenario 2","Scenario 3"))
datPigs$y<-datPigs$alive
datPigs$name<-NA


# Marketing
datMarketing<-dtPlot[,list(scenario,t,optimalLable)]
datMarketing$scenario<-factor(datMarketing$scenario, labels=c("Scenario 1","Scenario 2","Scenario 3"))
datMarketing$y<-datMarketing$optimalLable
datMarketing$name<-NA


#Plot the data related to the simulation and the SSMs: 
library(ggplot2)
library(grid)
library(tikzDevice)

if(useScenariosPaper){
  tikz("ScenariosPaper_plot.tex", width = 10, height = 7, standAlone=T)
}else{
  tikz("Scenarios_plot.tex", width = 10, height = 7, standAlone=T)
}

plot<-ggplot(data=dat, aes(x=factor(t), y=y, group=name, shape=name, linetype=name ) ) + 
  geom_line() + scale_y_continuous(breaks=seq(-2,15,1), labels = c("","$a^*$",0:15) ) +
  #geom_point() + 
  scale_linetype_manual(values=c("solid", "dotted", "longdash", "dotdash")) +
  facet_grid(. ~ scenario) + 
  xlab("week numbers") + ylab(" ") 
g <- guide_legend("",nrow=1,byrow=TRUE, override.aes = list(fill=NA))

plot + 
  guides(shape = g, linetype=g)  + 
  geom_histogram(stat="identity", data=datPigs, alpha = 1/4, colour=NA, width=0.25, linetype = 0) + 
  #  geom_vline(aes(xintercept = w), data=vline.fm, color="gray") + 
  #  geom_vline(aes(xintercept = w), data=vline.th, color="gray", linetype="twodash") +
  geom_line() +
  geom_text(data=datMarketing, mapping=aes(x=t, y=-1, label=optimalLable), size=4) +
  #  scale_y_discrete(breaks = -2:15, labels = c("","Optimal decisions",0:15) ) +
  theme_bw() + 
  theme(legend.position="bottom", panel.background = element_blank(), 
        panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.key.width = unit(2, "cm"), legend.text.align=0.5, axis.title.x= element_text(vjust = -0.7),
        strip.background=element_rect(fill = NA))  



dev.off()



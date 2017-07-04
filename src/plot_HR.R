library(xlsx)
library(ggplot2)
library(grid)
library(gridExtra)
setwd("C:/study/Stat")

source("plot_function.R")

p1<-plot_function("PD","HR", "HR[bpm]", 40, 120)
p2<-plot_function("RD","HR", "HR[bpm]", 40, 120)
p3<-plot_function("ND","HR", "HR[bpm]", 40, 120)
p4<-plot_function("ED","HR", "HR[bpm]", 40, 120)
p5<-plot_function("CD","HR", "HR[bpm]", 40, 120)
p6<-plot_function("MD","HR", "HR[bpm]", 40, 120)
p7<-plot_function("FD","HR", "HR[bpm]", 40, 120)

plist<-list(textGrob("raw signal set"), textGrob("valid signal set"), textGrob(" "),
            p1[[1]],p1[[2]],textGrob("PD"), 
            p2[[1]],p2[[2]],textGrob("RD"),
            p3[[1]],p3[[2]],textGrob("ND"),
            p5[[1]],p5[[2]],textGrob("CD"),
            p4[[1]],p4[[2]],textGrob("ED"),
            p6[[1]],p6[[2]],textGrob("MD"),
            p7[[1]],p7[[2]],textGrob("FD"),
            textGrob("Time [s]",just = "bottom"), textGrob("Time [s]",just = "bottom"), textGrob(" "))
ggsave("HR.png",grid.arrange(grobs=plist, ncol=3, nrow=9, widths=c(10,10,1), heights=c(1,rep(4,7),1),top = "Heart Rate Dataset"))
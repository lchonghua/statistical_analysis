library(xlsx)
library(ggplot2)
library(grid)
library(gridExtra)
setwd("C:/study/Stat")

source("plot_function_RES.R")

p1<-plot_function("PD")
p2<-plot_function("RD")
p3<-plot_function("ND")
p4<-plot_function("ED")
p5<-plot_function("CD")
p6<-plot_function("MD")
p7<-plot_function("FD")

plist<-list(textGrob("Speed"), textGrob("Acceleration"), textGrob("Brake"), textGrob("Steering"), textGrob("Lane Position"), textGrob(" "),
            p1[[1]], p1[[2]], p1[[3]], p1[[4]], p1[[5]], textGrob("PD"), 
            p2[[1]], p2[[2]], p2[[3]], p2[[4]], p2[[5]], textGrob("RD"),
            p3[[1]], p3[[2]], p3[[3]], p3[[4]], p3[[5]], textGrob("ND"),
            p5[[1]], p5[[2]], p5[[3]], p5[[4]], p5[[5]], textGrob("CD"),
            p4[[1]], p4[[2]], p4[[3]], p4[[4]], p4[[5]], textGrob("ED"),
            p6[[1]], p6[[2]], p6[[3]], p6[[4]], p6[[5]], textGrob("MD"),
            p7[[1]], p7[[2]], p7[[3]], p7[[4]], p7[[5]], textGrob("FD"),
            textGrob("Time [s]"), textGrob("Time [s]"),textGrob("Time [s]"),textGrob("Time [s]"),textGrob("Time [s]"), textGrob(" "))
ggsave("RES.png",grid.arrange(grobs=plist, ncol=6, nrow=9, widths=c(rep(6,5),1), heights=c(1,rep(4,7),1)))


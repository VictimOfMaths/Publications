rm(list=ls())

library(tidyverse)
library(ggtext)
library(ragg)

sales <- data.frame(year=c(1994, 1995, 2000:2019), on=c(521, 513, 472, 471, 469, 462, 453, 439, 428, 
                                                        399, 362, 343, 326, 315, 304, 292, 286, 278, 
                                                        274, 267, 264, 260),
                    off=c(383, 393, 515, 543, 562, 578, 590, 609, 612, 630, 625, 632, 631, 623, 614,
                          610, 616, 622, 624, 630, 649, 655))

sales <- sales %>% 
  gather(channel, sales, c(2, 3))

agg_tiff("Outputs/MESASSalesxChannel.tiff", units="in", width=8, height=6, res=500)
ggplot(sales, aes(x=year, y=sales, colour=channel))+
  geom_line(show.legend=FALSE)+
  geom_point(show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(1995, 2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Units of alcohol sold per adult", limits=c(0,NA))+
  scale_colour_manual(values=c("#ff99cc", "#0099ff"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        plot.subtitle=element_markdown())+
  labs(title="Alcohol consumption has shifted away from pubs",
       subtitle="Per capita alcohol sales in England and Wales in the <span style='color:#0099ff;'>on-trade</span> and the <span style='color:#ff99cc;'>off-trade",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

inflation <- data.frame(year=c(1986:2020),
                        on=c(100, 101.7, 108.1, 115.4, 127.4, 144.1, 154.3, 162.6, 168.7, 176.2, 182.3,
                             189.4, 197.8, 205, 209.9, 215.6, 221.7, 228.3, 234.9, 242.8, 251.1, 261,
                             272.4, 281.4, 291.8, 307.8, 318.1, 325.2, 331.6, 337.6, 344.7, 354, 362.6,
                             369.2, 376.1),
                        off=c(100, 102.4, 106.5, 111, 118.6, 131.1, 137.6, 142.3, 140.4, 144, 148.7, 
                              153.1, 156.9, 159.3, 160.8, 161.6, 160.7, 157.8, 153.5, 148.3, 147.8, 148.9,
                              149, 153.6, 155.4, 163.9, 169.7, 170.9, 169.8, 163.1, 158.2, 165.7, 168.4,
                              170.9, 171.8))

inflation <- inflation %>% 
  gather(channel, inflation, c(2,3))

agg_tiff("Outputs/RPIInflationxChannel.tiff", units="in", width=8, height=6, res=500)
ggplot(inflation, aes(x=year, y=inflation, colour=channel))+
  geom_line(show.legend=FALSE)+
  geom_point(show.legend=FALSE)+
  scale_x_continuous(name="", breaks=c(1990, 1995, 2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(name="Beer price index (1987=100)", limits=c(0,400))+
  scale_colour_manual(values=c("#ff99cc", "#0099ff"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        plot.subtitle=element_markdown())+
  labs(title="Beer prices have risen much faster in pubs than shops",
       subtitle="RPI beer price indices indexed to 1987 for the <span style='color:#0099ff;'>on-trade</span> and the <span style='color:#ff99cc;'>off-trade",
       caption="Data from Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

ineq <- data.frame(IMDQ=c("Q1 - least deprived", "Q2", "Q3", "Q4", "Q5 - most deprived"),
                   deaths=c(0.3, 3.3, 1.5, 3.6, 4.9))

agg_tiff("Outputs/IASDutyDeathsxIMD.tiff", units="in", width=8, height=6, res=500)
ggplot(ineq, aes(x=IMDQ, y=deaths/100, fill=IMDQ))+
  geom_col(show.legend=FALSE)+
  geom_hline(yintercept=0.03)+
  scale_x_discrete(name="Index of Multiple Deprivation")+
  scale_y_continuous(name="Change in annual alcohol-attributable deaths\nbetween 2012 and 2019", 
                     labels=scales::label_percent(accuracy=1))+
  scale_fill_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))+
  annotate("text", x=1, y=0.036, label="Population average effect", size=3)+
  geom_curve(aes(x=1, y=0.035, xend=1.1, yend=0.0305), 
             colour="grey30", curvature=-0.15, arrow=arrow(length=unit(0.1, "cm"), type="closed"), 
             lineend="round")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Duty policies since 2012 have hurt deprived groups the most",
       subtitle="Modelled change in alcohol-attributable deaths in England between 2012 and 2019 caused by duty policies",
       caption="Analysis from the Sheffield Alcohol Policy Model | Plot by @VictimOfMaths")
dev.off()

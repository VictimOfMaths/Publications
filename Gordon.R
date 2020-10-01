library(tidyverse)
library(epiR)
library(paletteer)

#Read in data
data <- read.csv("Data/GordonData.csv")

#Convert to long
colnames(data) <- c("Clinical", "Dermoscopic", "RCM", "Hist")
data_long <- gather(data, modality, result, c(1:3))

#Calculate positives based on threshold of 2 for all modalities
data_long$pos <- factor(if_else(data_long$result>=2, "pos", "neg"), levels=c("pos", "neg"))
data_long$Hist <- factor(data_long$Hist, levels=c("1","0"))

data_clin <- subset(data_long, modality=="Clinical")
clin_table <- table(data_clin$pos, data_clin$Hist)
clin_metric <- epi.tests(clin_table, conf.level=0.95)
clin_metric$elements$sensitivity
clin_metric$elements$specificity
clin_metric$elements$pv.positive
clin_metric$elements$pv.negative
clin_metric$elements$aprev
clin_metric$elements$diag.acc
clin_metric$elements$lr.positive
clin_metric$elements$lr.negative
clin_metric$elements$nnd

data_derm <- subset(data_long, modality=="Dermoscopic")
derm_table <- table(data_derm$pos, data_derm$Hist)
derm_metric <- epi.tests(derm_table, conf.level=0.95)

data_RCM <- subset(data_long, modality=="RCM")
RCM_table <- table(data_RCM$pos, data_RCM$Hist)
RCM_metric <- epi.tests(RCM_table, conf.level=0.95)

#Extract NND values with CIs
clin_metric$elements$nnd
derm_metric$elements$nnd
RCM_metric$elements$nnd

#sensitivity, specificity, PPV, NPV & accuracy
clin_outputs <- bind_rows(as.data.frame(clin_metric$elements$sensitivity),
                          as.data.frame(clin_metric$elements$specificity),
                          as.data.frame(clin_metric$elements$pv.positive),
                          as.data.frame(clin_metric$elements$pv.negative),
                          as.data.frame(clin_metric$elements$diag.acc))

clin_outputs$modality <- "Clinical"
clin_outputs$metric <- c("Sensitivity", "Specificity", "Positive Predictive Value", 
                         "Negative Predictive Value", "Accuracy")

derm_outputs <- bind_rows(as.data.frame(derm_metric$elements$sensitivity),
                          as.data.frame(derm_metric$elements$specificity),
                          as.data.frame(derm_metric$elements$pv.positive),
                          as.data.frame(derm_metric$elements$pv.negative),
                          as.data.frame(derm_metric$elements$diag.acc))

derm_outputs$modality <- "Dermoscopic"
derm_outputs$metric <- c("Sensitivity", "Specificity", "Positive Predictive Value", 
                         "Negative Predictive Value", "Accuracy")

RCM_outputs <-bind_rows(as.data.frame(RCM_metric$elements$sensitivity),
                          as.data.frame(RCM_metric$elements$specificity),
                          as.data.frame(RCM_metric$elements$pv.positive),
                          as.data.frame(RCM_metric$elements$pv.negative),
                          as.data.frame(RCM_metric$elements$diag.acc))

RCM_outputs$modality <- "RCM"
RCM_outputs$metric <- c("Sensitivity", "Specificity", "Positive Predictive Value", 
                         "Negative Predictive Value", "Accuracy")

plotdata <- bind_rows(clin_outputs, derm_outputs, RCM_outputs)

plotdata$metric <- factor(plotdata$metric, levels=c("Sensitivity", "Specificity", "Positive Predictive Value", 
                                                    "Negative Predictive Value", "Accuracy"))

PlotForGordon <- ggplot(subset(plotdata, metric!="Accuracy"))+
  geom_col(aes(x=metric, y=est, fill=modality), position="dodge")+
  geom_errorbar(aes(x=metric, ymin=lower, ymax=upper, group=modality), width=0.3, 
                position=position_dodge(width=0.9))+
  scale_x_discrete(name="")+
  scale_y_continuous(labels=scales::percent_format(), name="")+
  scale_fill_paletteer_d("ggsci::planetexpress_futurama", name="")+
  coord_cartesian(ylim=c(0,1), expand=0)+
  theme_classic()

tiff("Outputs/PlotForGordon.tiff", units="in", width=8, height=6, res=500)
PlotForGordon
dev.off()

png("Outputs/PlotForGordon.png", units="in", width=8, height=6, res=500)
PlotForGordon
dev.off()

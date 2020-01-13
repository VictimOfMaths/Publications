rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyverse)

options(scipen=10000)

#Add source data file location here:
data <- fread("   .csv")

colnames(data) <- c("year", "age", "sex",  "imd_quintile", "cause",  "Dix", "nx", "mix")

#relabel IMDQ
data$imd_quintile<-ifelse(data$imd_quintile=="1_least_deprived", 1, data$imd_quintile)
data$imd_quintile<-ifelse(data$imd_quintile=="5_most_deprived", 5, data$imd_quintile)
data$imd_quintile<-ordered(data$imd_quintile, levels=c(5,4,3,2,1))
data$sex <- ordered(data$sex, levels=c("Male", "Female"))

#set age limits
minage <- 18
maxage <- 89

#Label causes in 3 groups: alc-specific, K73-74 and other
data$alc.ind <- case_when(
  data$cause=="LiverCirrhosis" ~ "K73K74",
  data$cause=="other_causes" ~ "Oth",
  TRUE ~ "Alc")

#collapse data into 2 datasets for main analysis and SA where K73K74 are combined with alc-specific causes
data$flag <- ifelse(data$alc.ind=="K73K74", "Oth", data$alc.ind)
setkeyv(data, c("age", "sex", "year", "imd_quintile", "flag"))
maindata<-copy(data[,.(nx=unique(nx), Dix=sum(Dix)), by=key(data)])

data$flag <- ifelse(data$alc.ind=="K73K74", "Alc", data$alc.ind)
setkeyv(data, c("age", "sex", "year", "imd_quintile", "flag"))
sadata<-copy(data[,.(nx=unique(nx), Dix=sum(Dix)), by=key(data)])


#perform all analysis on main data
#####

#Convert to wide
maindata <- spread(maindata, flag, Dix)
names(maindata)[names(maindata)=="Alc"]<-"Dx_alc"
names(maindata)[names(maindata)=="Oth"]<-"Dx_oth"
maindata[ , Dx :=Dx_alc+Dx_oth]

#Pool years
setkeyv(maindata, c("age", "sex", "imd_quintile"))
maindataallyrs<-copy(maindata[,.(nx=sum(nx), Dx=sum(Dx), Dx_alc=sum(Dx_alc), 
                                 Dx_oth=sum(Dx_oth)), by=key(maindata)])

#generate ASMRs
#read in European std. population
ESP2013 <- fread("Colin KBS Analysis/ESP2013.csv")
colnames(ESP2013)<-c("ageband", "std_pop")

#Add agebands to main data and collapse into them
maindataallyrs$ageband <- cut(maindataallyrs$age, seq(0,95,5), right=FALSE, 
                              labels=c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                       "25-29", "30-34", "35-39", "40-44", "45-49", 
                                       "50-54", "55-59", "60-64", "65-69", 
                                       "70-74", "75-79", "80-84", "85-89", "90+"))
setkeyv(maindataallyrs, c("ageband", "sex", "imd_quintile"))
agebanddata<-copy(maindataallyrs[,.(nx=sum(nx), Dx=sum(Dx), Dx_alc=sum(Dx_alc), 
                                    Dx_oth=sum(Dx_oth)), by=key(maindataallyrs)])

#bring in European Standard Population for standardisation
agebanddata<-merge(agebanddata, ESP2013, by="ageband")

#calculate age-specific rates
agebanddata[ , mx_alc := Dx_alc/nx]
agebanddata[ , mx_oth := Dx_oth/nx]
setkeyv(agebanddata, c("sex", "imd_quintile"))
ASMRs<-copy(agebanddata[ , .(mx_alc_standardised = 100000*sum(mx_alc * std_pop) / sum(std_pop), 
                             mx_oth_standardised = 100000*sum(mx_oth * std_pop) / sum(std_pop)), 
                         by = key(agebanddata)])  

#Set IMD palette for graphs
IMD_palette<-c("#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0")

n_years <- length(unique(maindata$year))

#Draw graph of age against average annual number of deaths across the whole analysis period
Fig1a <- ggplot(maindataallyrs[age<=maxage & age>=minage], 
                aes(x=age, y=Dx_alc/n_years, group=imd_quintile, colour=imd_quintile))+
  geom_line(size=0.7)+
  theme_classic()+
  facet_grid(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", 
                      labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  ylab("Anual alcohol-specific deaths")+
  xlab("")+
  theme(strip.background=element_blank(), plot.margin=unit(c(0,0,0,0.2),"cm"), 
        axis.title.y=element_text(size=9))

Fig1b <- ggplot(maindataallyrs[age<=maxage & age>=minage],
                aes(x=age, y=Dx_oth/n_years, group=imd_quintile, colour=imd_quintile))+
  geom_line(size=0.7)+
  theme_classic()+
  facet_grid(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", 
                      labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  ylab("Annual deaths from other causes")+
  xlab("Age")+
  theme(strip.text.x=element_blank(), plot.margin=unit(c(0,0,0,0.2),"cm"), 
        axis.title=element_text(size=9))

legend<-get_legend(Fig1a)
Figure1<-plot_grid(Fig1a+theme(legend.position="none"), Fig1b+theme(legend.position="none"), 
                   align="vh", ncol=1,
                   labels=c("A", "B", ""))
Figure1<-Figure1+theme(panel.border=element_blank())
Figure1<-plot_grid(Figure1, legend, rel_widths=c(1, 0.3))+theme(panel.border=element_blank())
Figure1

tiff("Figure1.tiff", units="in", width=8, height=6, res=500)
Figure1
dev.off()

#trends in rx with age/deprivation (not used in paper, but it's interesting!)
#rx = % of all deaths due to alcohol-specific causes
maindataallyrs[ , rx := Dx_alc/Dx]

tiff("ExtraFigureA.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(maindataallyrs, age>=18 & age<maxage), aes(x=age, y=rx, group=imd_quintile, colour=imd_quintile))+
  geom_line(size=0.7)+
  facet_grid(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  xlab("Age")+
  ylab("Proportion of all deaths which are alcohol-specific")+
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80,90))+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(strip.background=element_blank())
dev.off()

#####
#Go back to annual data and perform main calculations

#rx = % of all deaths due to alcohol-specific causes
maindata[ , rx := Dx_alc/Dx]
#set to 0 for age/year groups with 0 deaths from any cause
maindata$rx <- ifelse(is.na(maindata$rx), 0, maindata$rx)

#mx = death rate by cause (alc or all cause)
maindata[ , mx_alc := Dx_alc/nx]
maindata[ , mx := Dx/nx]

#qx = probability of death by cause in year 
#(assuming constant mortality rate across 1 year interval)
maindata[ , qx_alc := mx_alc/(1+mx_alc)]
maindata[ , qx := mx/(1+mx)]

#cause-deleted death probabilities _dag represents the world with no alc-specific deaths
maindata[ , qx_dag := 1-((1-qx) ^ (1-rx))]

#px = probability of survival in year
maindata[ , px := 1-qx]
maindata[ , px_dag := 1-qx_dag]

#lx = probability of survival to start of year of age
setkeyv(maindata, c("sex", "year", "imd_quintile"))
maindata[ , lx:= cumprod(c(1, px[1:(length(px) - 1)])), by = key(maindata)]
maindata[ , lx_dag:= cumprod(c(1, px_dag[1:(length(px_dag) - 1)])), by = key(maindata)]

#phix = the age distribution of death
maindata[ , phix := qx * lx]
maindata[ , phix_dag := qx_dag * lx_dag]

#Lx = the probability of survival to the mid-point of the interval
maindata[ , Lx := lx-phix/2]
maindata[ , Lx_dag := lx_dag-phix_dag/2]

#Tx = overall probability of survival to the end of age x
maindata[ , Tx := rev(cumsum(rev(Lx))), by = key(maindata)]
maindata[ , Tx_dag := rev(cumsum(rev(Lx_dag))), by = key(maindata)]

#cap calculations at max age
maindata$LxCapped <- with(maindata, ifelse(age>maxage, 0, Lx))
maindata$LxDagCapped <- with(maindata, ifelse(age>maxage, 0, Lx_dag))
maindata[ , TxCapped := rev(cumsum(rev(LxCapped))), by = key(maindata)]
maindata[ , TxDagCapped := rev(cumsum(rev(LxDagCapped))), by = key(maindata)]

#ex = life expectancy at age x
maindata[ , exCapped := TxCapped / lx]
maindata[ , exDagCapped := TxDagCapped / lx_dag]

#collapse across ages, extracting key vars - e18 and l18  
setkeyv(maindata, c("year", "sex", "imd_quintile"))
analysisdata<-copy(maindata[ , .(e18 = exCapped[age == minage],e18_dag=exDagCapped[age==minage], 
                                 l18 = lx[age==minage], l18_dag=lx_dag[age==minage]), 
                             by = key(maindata)])

#merge back into age data
maindata<-merge(maindata, analysisdata, by=key(maindata))

#####
#calculate variance/sd

#calculate the values inside the summations
maindata[ , sumcontents := lx*qx*(age-(e18+minage))^2]
maindata[ , sumcontents_dag := lx_dag*qx_dag*(age-(e18_dag+minage))^2]

#sum across ages
allcausevar<-copy(subset(maindata, age>=minage & age<=maxage)[, .(summation=sum(sumcontents), 
                                                                  summation_dag=sum(sumcontents_dag), 
                                                                  l18=unique(l18), 
                                                                  l18_dag=unique(l18_dag)), 
                                                              by=key(maindata)])
allcausevar[ , variance := summation/l18]
allcausevar[ , variance_dag := summation_dag/l18_dag]
allcausevar[ , s18 := sqrt(variance)]
allcausevar[ , s18_dag :=sqrt(variance_dag)]

#bring back into dataset with LE results
setkeyv(allcausevar, c("year", "sex", "imd_quintile"))
analysisdata<-merge(analysisdata, allcausevar, by=key(allcausevar))

#only keep the columns we actually want
analysisdata <- analysisdata[,c("year", "sex", "imd_quintile", "e18", "e18_dag",
                                "s18", "s18_dag")]

#Plot joint trends in e18 and s18
tiff("ExtraFigureB.tiff", units="in", width=8, height=6, res=500)
ggplot(analysisdata, aes(x=e18, y=s18, group=imd_quintile, colour=imd_quintile))+
  geom_path(arrow=arrow(angle=30, length=unit(0.25, "cm"), ends="last", type="closed"))+
  facet_wrap(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  xlab("Remaining life expectancy at age 18 (e(18|90))")+
  ylab("Std. deviation of remaining life expectancy at age 18 (S(18|90))")+
  theme_classic()+
  theme(strip.background=element_blank())
dev.off()

#calculate impact of alcohol on e18 and s18
analysisdata[, e18alc:=e18-e18_dag]
analysisdata[, s18alc:=s18-s18_dag]

#calculate inequality 'gap' between IMD quintiles 1 and 5
setkeyv(analysisdata, c("year", "sex"))
gradients<-copy(analysisdata[ , .(legap=e18[imd_quintile==1]-e18[imd_quintile==5], 
                                  sdgap=s18[imd_quintile==1]-s18[imd_quintile==5],  
                                  legap_dag=e18_dag[imd_quintile==1]-e18_dag[imd_quintile==5],
                                  sdgap_dag=s18_dag[imd_quintile==1]-s18_dag[imd_quintile==5]),
                              by=key(analysisdata)])

gradients[, lealc:=legap-legap_dag]
gradients[, sdalc:=sdgap-sdgap_dag]

gradients[, lerelalc:=100*lealc/legap]
gradients[, sdrelalc:=100*sdalc/sdgap]

#reverse order of IMD quintiles to match Figure
analysisdata$imd_quintile<-factor(analysisdata$imd_quintile,levels=c(1,2,3,4,5))
IMD_palette_rev<-c("#fa9fb5","#f768a1","#c51b8a","#7a0177","#49006a")

Fig2a<-ggplot(analysisdata)+
  geom_ribbon(aes(x=year, ymin=e18, ymax=e18_dag, group=imd_quintile, colour=imd_quintile, fill=imd_quintile), alpha=0.1, linetype=0)+
  geom_line(aes(x=year, y=e18, group=imd_quintile, colour=imd_quintile))+
  geom_line(aes(x=year, y=e18_dag, group=imd_quintile, colour=imd_quintile), linetype=3)+
  geom_line(aes(x=year, y=1, linetype=sex), colour="Black")+
  facet_wrap( ~sex)+
  scale_linetype_manual(values=c(3,1),name="", labels=c("Without alcohol", "Current"))+
  scale_colour_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  scale_fill_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  ylab("Remaining life expectancy \n at age 18 (years)")+
  scale_x_continuous(breaks=c(2001,2003,2005,2007,2009,2011,2013,2015))+
  xlab("")+
  scale_y_continuous(limits=c(53,67), breaks=c(54,56,58,60,62,64,66))+
  theme_classic()+
  theme(axis.title.y=element_text(size=9), plot.margin=unit(c(0,0,0,0),"cm"),
        strip.background =element_blank())

Fig2b<-ggplot(analysisdata)+
  geom_ribbon(aes(x=year, ymin=s18_dag, ymax=s18, group=imd_quintile, colour=imd_quintile, fill=imd_quintile), alpha=0.1, linetype=0)+
  geom_line(aes(x=year, y=s18, group=imd_quintile, colour=imd_quintile))+
  geom_line(aes(x=year, y=s18_dag, group=imd_quintile, colour=imd_quintile), linetype=3)+
  geom_line(aes(x=year, y=1, linetype=sex), colour="Black")+
  facet_wrap( ~sex)+
  scale_colour_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  scale_fill_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  scale_linetype_manual(values=c(3,1),name="", labels=c("Without alcohol", "Current"))+
  ylab("Std. deviation in age of death \n (years)")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(8,14))+
  scale_x_continuous(breaks=c(2001,2003,2005,2007,2009,2011,2013,2015))+
  xlab("Year")+
  theme_classic()+
  theme(strip.text.x=element_blank(),axis.title.y=element_text(size=9), plot.margin=unit(c(0,0,0,0),"cm"),
        strip.background=element_blank())

legend2<-get_legend(Fig2a)
Figure2<-plot_grid(Fig2a+theme(legend.position="none"), Fig2b+theme(legend.position="none"), align="vh", ncol=1,
                   labels=c("A", "B", ""))
Figure2<-Figure2+theme(panel.border=element_blank())
Figure2<-plot_grid(Figure2, legend2, rel_widths=c(1, 0.3))+theme(panel.border=element_blank())
Figure2

tiff("Figure2.tiff", units="in", width=10, height=7, res=300)
Figure2
dev.off()

#calculate YLLs
maindata[,YLL:=Dx_alc*exCapped]
setkeyv(maindata, c("year"))
temp <- copy(maindata[,.(YLL=sum(YLL), pop=sum(nx)), by=key(maindata)])

#generate Tables 2 & 3 for paper
#calculate figures across all IMD quintiles
setkeyv(maindata, c("age", "sex", "year"))
maindatanoimd<-copy(maindata[,.(nx=sum(nx), Dx=sum(Dx), Dx_alc=sum(Dx_alc), Dx_oth=sum(Dx_oth)), by=key(maindata)])

#rx = % of all deaths due to alcohol-specific causes
maindatanoimd[ , rx := Dx_alc/Dx]

#mx = death rate by cause (alc or notalc)
maindatanoimd[ , mx_alc := Dx_alc/nx]
maindatanoimd[ , mx := Dx/nx]

#qx = probability of death by cause in year
maindatanoimd[ , qx_alc := mx_alc/(1+mx_alc)]
maindatanoimd[ , qx := mx/(1+mx)]

#cause-deleted death probabilities _dag represents the world with no alc-specific deaths
maindatanoimd[ , qx_dag := 1-((1-qx) ^ (1-rx))]

#px = probability of survival in year
maindatanoimd[ , px := 1-qx]
maindatanoimd[ , px_dag := 1-qx_dag]

#lx = probability of survival to start of year of age
setkeyv(maindatanoimd, c("sex", "year"))
maindatanoimd[ , lx:= cumprod(c(1, px[1:(length(px) - 1)])), by = key(maindatanoimd)]
maindatanoimd[ , lx_dag:= cumprod(c(1, px_dag[1:(length(px_dag) - 1)])), by = key(maindatanoimd)]
maindatanoimd[ , phix := qx * lx]
maindatanoimd[ , phix_dag := qx_dag * lx_dag]
maindatanoimd[ , Lx := phix*1/2+(lx-phix)]
maindatanoimd[ , Lx_dag := phix_dag*1/2+(lx_dag-phix_dag)]
maindatanoimd[ , Tx := rev(cumsum(rev(Lx))), by = key(maindatanoimd)]
maindatanoimd[ , Tx_dag := rev(cumsum(rev(Lx_dag))), by = key(maindatanoimd)]
maindatanoimd[ , ex := Tx / lx]
maindatanoimd[ , ex_dag := Tx_dag / lx_dag]

#collapse for calculations, extracting key vars - e18 and l18  
setkeyv(maindatanoimd, c("year", "sex"))
analysisdatanoimd<-copy(maindatanoimd[ , .(e18 = ex[age == minage],e18_dag=ex_dag[age==minage], l18 = lx[age==minage], 
                                           l18_dag=lx_dag[age==minage]), by = key(maindatanoimd)])

#merge back into age data
maindatanoimd<-merge(maindatanoimd, analysisdatanoimd, by=key(maindatanoimd))

#calculate variance/sd
maindatanoimd[ , integral := phix*((age-(e18+minage))^2)]
maindatanoimd[ , integral_dag := phix_dag*((age-(e18_dag+minage))^2)]
allcausevarnoimd<-copy(subset(maindatanoimd, age>=minage & age<=maxage)[, .(integral=sum(integral), integral_dag=sum(integral_dag), l18=unique(l18), l18_dag=unique(l18_dag)), by=key(maindatanoimd)])
allcausevarnoimd[ , variance := integral/l18]
allcausevarnoimd[ , variance_dag := integral_dag/l18_dag]
allcausevarnoimd[ , s18 := sqrt(variance)]
allcausevarnoimd[ , s18_dag :=sqrt(variance_dag)]

#merge back into analysis data
setkeyv(allcausevarnoimd, c("year", "sex"))
analysisdatanoimd<-merge(analysisdatanoimd, allcausevarnoimd, by=key(allcausevarnoimd))
analysisdatanoimd$l18.y <-analysisdatanoimd$l18_dag.y <-NULL
names(analysisdatanoimd)[names(analysisdatanoimd)=="l18.x"]<-"l18"
names(analysisdatanoimd)[names(analysisdatanoimd)=="l18_dag.x"]<-"l18_dag"

Table2data<-analysisdatanoimd[,c("year", "sex", "e18", "e18_dag", "s18", "s18_dag")]
Table2data[ , e18alc := e18-e18_dag]
Table2data[ , s18alc := s18-s18_dag]

#Bring in data from top and bottom IMD quintiles
q1<-subset(analysisdata, imd_quintile==1)
q5<-subset(analysisdata, imd_quintile==5)

q1<-q1[,c("year", "sex", "e18", "e18_dag", "s18", "s18_dag")]
q5<-q5[,c("year", "sex", "e18", "e18_dag", "s18", "s18_dag")]

names(q1)[3]<-"e18q1"
names(q1)[4]<-"e18_dagq1"
names(q1)[5]<-"s18q1"
names(q1)[6]<-"s18_dagq1"

names(q5)[3]<-"e18q5"
names(q5)[4]<-"e18_dagq5"
names(q5)[5]<-"s18q5"
names(q5)[6]<-"s18_dagq5"

Table2data<-merge(Table2data, q1)
Table2data<-merge(Table2data, q5)
Table2data[ , e18alcq1 := e18q1-e18_dagq1]
Table2data[ , s18alcq1 := s18q1-s18_dagq1]
Table2data[ , e18alcq5 := e18q5-e18_dagq5]
Table2data[ , s18alcq5 := s18q5-s18_dagq5]

Table2data[ , legap:=e18q1-e18q5]
Table2data[ , lealcgap:=legap-(e18_dagq1-e18_dagq5)]
Table2data[ , sdgap:=s18q1-s18q5]
Table2data[ , sdalcgap:=sdgap-(s18_dagq1-s18_dagq5)]
Table2data[ , lealcgappercent:=lealcgap/legap]
Table2data[ , sdalcgappercent:=sdalcgap/sdgap]

Table2data<-Table2data[,c("sex", "year", "e18", "e18_dag", "e18alc", "e18q1", "e18_dagq1", "e18alcq1", "e18q5", "e18_dagq5", "e18alcq5", 
                          "legap", "lealcgap", "lealcgappercent","s18", "s18_dag", "s18alc", "s18q1", "s18_dagq1", "s18alcq1", "s18q5", "s18_dagq5", 
                          "s18alcq5", "sdgap", "sdalcgap", "sdalcgappercent")]

Table2data<-Table2data[order(sex, year)]

#Pull out data for Fig3
Fig3data <- gather(Table2data, measure, value, lealcgappercent, sdalcgappercent)

#Tity up formatting
Table2data$year <- as.integer(Table2data$year)
Table2data <- Table2data %>%
  mutate_at(c(3:13, 15:25), round,2)
Table2data$lealcgappercent <-  paste0(round(Table2data$lealcgappercent*100,1),"%") 
Table2data$sdalcgappercent <-  paste0(round(Table2data$sdalcgappercent*100,1),"%") 

#Calculate coefficient of variation
Table2data$CoeffVar <- paste0(round(Table2data$s18*100/Table2data$e18,2),"%")

#Split out tables by sex
Table2 <- t(subset(Table2data, sex=="Male")[,-c(1)])
Table3 <- t(subset(Table2data, sex=="Female")[,-c(1)])

#output as .csv
write.csv(Table2, "Table2data.csv")
write.csv(Table3, "Table3data.csv")

#Generate Figure3
att_palette<-c("#5ea8a7", "#ff4447")

Figure3 <- ggplot(Fig3data, aes(x=year, y=value, colour=measure))+
  geom_line()+
  facet_wrap(~sex)+
  theme_classic()+
  scale_y_continuous(labels=c("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%", "8%"), limits=c(0,0.08), breaks=seq(0,0.08,by=0.01), name="Contribution of alcohol-specific deaths to inequality gap")+
  scale_colour_manual(values=att_palette, name="Measure", labels=c("Life expectancy", "Std. deviation in age of death"))+
  scale_x_continuous(breaks=c(2001,2003,2005,2007,2009,2011,2013,2015), name="Year")+
  theme(strip.background=element_blank())

Figure3

tiff("Figure3.tiff", units="in", width=10, height=7, res=300)
Figure3
dev.off()

#repeat all analysis using alternative definition of alcohol-specific
#####

#Convert to wide
maindata <- spread(sadata, flag, Dix)
names(maindata)[names(maindata)=="Alc"]<-"Dx_alc"
names(maindata)[names(maindata)=="Oth"]<-"Dx_oth"
maindata[ , Dx :=Dx_alc+Dx_oth]

#Pool years
setkeyv(maindata, c("age", "sex", "imd_quintile"))
maindataallyrs<-copy(maindata[,.(nx=sum(nx), Dx=sum(Dx), Dx_alc=sum(Dx_alc), 
                                 Dx_oth=sum(Dx_oth)), by=key(maindata)])

#generate ASMRs
#read in European std. population
ESP2013 <- fread("Colin KBS Analysis/ESP2013.csv")
colnames(ESP2013)<-c("ageband", "std_pop")

#Add agebands to main data and collapse into them
maindataallyrs$ageband <- cut(maindataallyrs$age, seq(0,95,5), right=FALSE, 
                              labels=c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                       "25-29", "30-34", "35-39", "40-44", "45-49", 
                                       "50-54", "55-59", "60-64", "65-69", 
                                       "70-74", "75-79", "80-84", "85-89", "90+"))
setkeyv(maindataallyrs, c("ageband", "sex", "imd_quintile"))
agebanddata<-copy(maindataallyrs[,.(nx=sum(nx), Dx=sum(Dx), Dx_alc=sum(Dx_alc), 
                                    Dx_oth=sum(Dx_oth)), by=key(maindataallyrs)])

#bring in European Standard Population for standardisation
agebanddata<-merge(agebanddata, ESP2013, by="ageband")

#calculate age-specific rates
agebanddata[ , mx_alc := Dx_alc/nx]
agebanddata[ , mx_oth := Dx_oth/nx]
setkeyv(agebanddata, c("sex", "imd_quintile"))
ASMRs<-copy(agebanddata[ , .(mx_alc_standardised = 100000*sum(mx_alc * std_pop) / sum(std_pop), 
                             mx_oth_standardised = 100000*sum(mx_oth * std_pop) / sum(std_pop)), 
                         by = key(agebanddata)])  

#Set IMD palette for graphs
IMD_palette<-c("#7a0177", "#c51b8a", "#f768a1", "#fa9fb5", "#fcc5c0")

n_years <- length(unique(maindata$year))

#Draw graph of age against average annual number of deaths across the whole analysis period
Fig1a <- ggplot(maindataallyrs[age<=maxage & age>=minage], 
                aes(x=age, y=Dx_alc/n_years, group=imd_quintile, colour=imd_quintile))+
  geom_line(size=0.7)+
  theme_classic()+
  facet_grid(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", 
                      labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  ylab("Anual alcohol-specific deaths")+
  xlab("")+
  theme(strip.background=element_blank(), plot.margin=unit(c(0,0,0,0.2),"cm"), 
        axis.title.y=element_text(size=9))

Fig1b <- ggplot(maindataallyrs[age<=maxage & age>=minage],
                aes(x=age, y=Dx_oth/n_years, group=imd_quintile, colour=imd_quintile))+
  geom_line(size=0.7)+
  theme_classic()+
  facet_grid(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", 
                      labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  ylab("Annual deaths from other causes")+
  xlab("Age")+
  theme(strip.text.x=element_blank(), plot.margin=unit(c(0,0,0,0.2),"cm"), 
        axis.title=element_text(size=9))

legend<-get_legend(Fig1a)
Figure1<-plot_grid(Fig1a+theme(legend.position="none"), Fig1b+theme(legend.position="none"), 
                   align="vh", ncol=1,
                   labels=c("A", "B", ""))
Figure1<-Figure1+theme(panel.border=element_blank())
Figure1<-plot_grid(Figure1, legend, rel_widths=c(1, 0.3))+theme(panel.border=element_blank())
Figure1

tiff("FigureS1.tiff", units="in", width=8, height=6, res=500)
Figure1
dev.off()

#trends in rx with age/deprivation (not used in paper, but it's interesting!)
#rx = % of all deaths due to alcohol-specific causes
maindataallyrs[ , rx := Dx_alc/Dx]

tiff("ExtraFigureSA.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(maindataallyrs, age>=18 & age<maxage), aes(x=age, y=rx, group=imd_quintile, colour=imd_quintile))+
  geom_line(size=0.7)+
  facet_grid(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  xlab("Age")+
  ylab("Proportion of all deaths which are alcohol-specific")+
  scale_x_continuous(breaks=c(20,30,40,50,60,70,80,90))+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(strip.background=element_blank())
dev.off()

#####
#Go back to annual data and perform main calculations

#rx = % of all deaths due to alcohol-specific causes
maindata[ , rx := Dx_alc/Dx]
#set to 0 for age/year groups with 0 deaths from any cause
maindata$rx <- ifelse(is.na(maindata$rx), 0, maindata$rx)

#mx = death rate by cause (alc or all cause)
maindata[ , mx_alc := Dx_alc/nx]
maindata[ , mx := Dx/nx]

#qx = probability of death by cause in year 
#(assuming constant mortality rate across 1 year interval)
maindata[ , qx_alc := mx_alc/(1+mx_alc)]
maindata[ , qx := mx/(1+mx)]

#cause-deleted death probabilities _dag represents the world with no alc-specific deaths
maindata[ , qx_dag := 1-((1-qx) ^ (1-rx))]

#px = probability of survival in year
maindata[ , px := 1-qx]
maindata[ , px_dag := 1-qx_dag]

#lx = probability of survival to start of year of age
setkeyv(maindata, c("sex", "year", "imd_quintile"))
maindata[ , lx:= cumprod(c(1, px[1:(length(px) - 1)])), by = key(maindata)]
maindata[ , lx_dag:= cumprod(c(1, px_dag[1:(length(px_dag) - 1)])), by = key(maindata)]

#phix = the age distribution of death
maindata[ , phix := qx * lx]
maindata[ , phix_dag := qx_dag * lx_dag]

#Lx = the probability of survival to the mid-point of the interval
maindata[ , Lx := lx-phix/2]
maindata[ , Lx_dag := lx_dag-phix_dag/2]

#Tx = overall probability of survival to the end of age x
maindata[ , Tx := rev(cumsum(rev(Lx))), by = key(maindata)]
maindata[ , Tx_dag := rev(cumsum(rev(Lx_dag))), by = key(maindata)]

#cap calculations at max age
maindata$LxCapped <- with(maindata, ifelse(age>maxage, 0, Lx))
maindata$LxDagCapped <- with(maindata, ifelse(age>maxage, 0, Lx_dag))
maindata[ , TxCapped := rev(cumsum(rev(LxCapped))), by = key(maindata)]
maindata[ , TxDagCapped := rev(cumsum(rev(LxDagCapped))), by = key(maindata)]

#ex = life expectancy at age x
maindata[ , exCapped := TxCapped / lx]
maindata[ , exDagCapped := TxDagCapped / lx_dag]

#collapse across ages, extracting key vars - e18 and l18  
setkeyv(maindata, c("year", "sex", "imd_quintile"))
analysisdata<-copy(maindata[ , .(e18 = exCapped[age == 18],e18_dag=exDagCapped[age==18], 
                                 l18 = lx[age==18], l18_dag=lx_dag[age==18]), 
                             by = key(maindata)])

#merge back into age data
maindata<-merge(maindata, analysisdata, by=key(maindata))

#####
#calculate variance/sd

#calculate the values inside the summations
maindata[ , sumcontents := lx*qx*(age-(e18+minage))^2]
maindata[ , sumcontents_dag := lx_dag*qx_dag*(age-(e18_dag+18))^2]

#sum across ages
allcausevar<-copy(subset(maindata, age>=minage & age<=maxage)[, .(summation=sum(sumcontents), 
                                                                  summation_dag=sum(sumcontents_dag), 
                                                                  l18=unique(l18), 
                                                                  l18_dag=unique(l18_dag)), 
                                                              by=key(maindata)])
allcausevar[ , variance := summation/l18]
allcausevar[ , variance_dag := summation_dag/l18_dag]
allcausevar[ , s18 := sqrt(variance)]
allcausevar[ , s18_dag :=sqrt(variance_dag)]

#bring back into dataset with LE results
setkeyv(allcausevar, c("year", "sex", "imd_quintile"))
analysisdata<-merge(analysisdata, allcausevar, by=key(allcausevar))

#only keep the columns we actually want
analysisdata <- analysisdata[,c("year", "sex", "imd_quintile", "e18", "e18_dag",
                                "s18", "s18_dag")]

#Plot joint trends in e18 and s18
tiff("ExtraFigureSB.tiff", units="in", width=8, height=6, res=500)
ggplot(analysisdata, aes(x=e18, y=s18, group=imd_quintile, colour=imd_quintile))+
  geom_path(arrow=arrow(angle=30, length=unit(0.25, "cm"), ends="last", type="closed"))+
  facet_wrap(~sex)+
  scale_colour_manual(values=IMD_palette, name="IMD quintile", labels=c("5 - most deprived", "4", "3", "2", "1 - least deprived"))+
  xlab("Remaining life expectancy at age 18 (e(18|90))")+
  ylab("Std. deviation of remaining life expectancy at age 18 (S(18|90))")+
  theme_classic()+
  theme(strip.background=element_blank())
dev.off()

#calculate impact of alcohol on e18 and s18
analysisdata[, e18alc:=e18-e18_dag]
analysisdata[, s18alc:=s18-s18_dag]

#calculate inequality 'gap' between IMD quintiles 1 and 5
setkeyv(analysisdata, c("year", "sex"))
gradients<-copy(analysisdata[ , .(legap=e18[imd_quintile==1]-e18[imd_quintile==5], 
                                  sdgap=s18[imd_quintile==1]-s18[imd_quintile==5],  
                                  legap_dag=e18_dag[imd_quintile==1]-e18_dag[imd_quintile==5],
                                  sdgap_dag=s18_dag[imd_quintile==1]-s18_dag[imd_quintile==5]),
                              by=key(analysisdata)])

gradients[, lealc:=legap-legap_dag]
gradients[, sdalc:=sdgap-sdgap_dag]

gradients[, lerelalc:=100*lealc/legap]
gradients[, sdrelalc:=100*sdalc/sdgap]

#reverse order of IMD quintiles to match Figure
analysisdata$imd_quintile<-factor(analysisdata$imd_quintile,levels=c(1,2,3,4,5))
IMD_palette_rev<-c("#fa9fb5","#f768a1","#c51b8a","#7a0177","#49006a")

Fig2a<-ggplot(analysisdata)+
  geom_ribbon(aes(x=year, ymin=e18, ymax=e18_dag, group=imd_quintile, colour=imd_quintile, fill=imd_quintile), alpha=0.1, linetype=0)+
  geom_line(aes(x=year, y=e18, group=imd_quintile, colour=imd_quintile))+
  geom_line(aes(x=year, y=e18_dag, group=imd_quintile, colour=imd_quintile), linetype=3)+
  geom_line(aes(x=year, y=1, linetype=sex), colour="Black")+
  facet_wrap( ~sex)+
  scale_linetype_manual(values=c(3,1),name="", labels=c("Without alcohol", "Current"))+
  scale_colour_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  scale_fill_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  ylab("Remaining life expectancy \n at age 18 (years)")+
  scale_x_continuous(breaks=c(2001,2003,2005,2007,2009,2011,2013,2015))+
  xlab("")+
  scale_y_continuous(limits=c(53,67), breaks=c(54,56,58,60,62,64,66))+
  theme_classic()+
  theme(axis.title.y=element_text(size=9), plot.margin=unit(c(0,0,0,0),"cm"),
        strip.background =element_blank())

Fig2b<-ggplot(analysisdata)+
  geom_ribbon(aes(x=year, ymin=s18_dag, ymax=s18, group=imd_quintile, colour=imd_quintile, fill=imd_quintile), alpha=0.1, linetype=0)+
  geom_line(aes(x=year, y=s18, group=imd_quintile, colour=imd_quintile))+
  geom_line(aes(x=year, y=s18_dag, group=imd_quintile, colour=imd_quintile), linetype=3)+
  geom_line(aes(x=year, y=1, linetype=sex), colour="Black")+
  facet_wrap( ~sex)+
  scale_colour_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  scale_fill_manual(values=IMD_palette_rev, name="IMD quintile", labels=c("1 - least deprived", "2", "3", "4", "5 - most deprived"))+
  scale_linetype_manual(values=c(3,1),name="", labels=c("Without alcohol", "Current"))+
  ylab("Std. deviation in age of death \n (years)")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(8,14))+
  scale_x_continuous(breaks=c(2001,2003,2005,2007,2009,2011,2013,2015))+
  xlab("Year")+
  theme_classic()+
  theme(strip.text.x=element_blank(),axis.title.y=element_text(size=9), plot.margin=unit(c(0,0,0,0),"cm"),
        strip.background=element_blank())

legend2<-get_legend(Fig2a)
Figure2<-plot_grid(Fig2a+theme(legend.position="none"), Fig2b+theme(legend.position="none"), align="vh", ncol=1,
                   labels=c("A", "B", ""))
Figure2<-Figure2+theme(panel.border=element_blank())
Figure2<-plot_grid(Figure2, legend2, rel_widths=c(1, 0.3))+theme(panel.border=element_blank())
Figure2

tiff("FigureS2.tiff", units="in", width=10, height=7, res=300)
Figure2
dev.off()

#calculate YLLs
maindata[,YLL:=Dx_alc*exCapped]
setkeyv(maindata, c("year"))
temp <- copy(maindata[,.(YLL=sum(YLL), pop=sum(nx)), by=key(maindata)])

#generate Tables 2 & 3 for paper
#calculate figures across all IMD quintiles
setkeyv(maindata, c("age", "sex", "year"))
maindatanoimd<-copy(maindata[,.(nx=sum(nx), Dx=sum(Dx), Dx_alc=sum(Dx_alc), Dx_oth=sum(Dx_oth)), by=key(maindata)])

#rx = % of all deaths due to alcohol-specific causes
maindatanoimd[ , rx := Dx_alc/Dx]

#mx = death rate by cause (alc or notalc)
maindatanoimd[ , mx_alc := Dx_alc/nx]
maindatanoimd[ , mx := Dx/nx]

#qx = probability of death by cause in year
maindatanoimd[ , qx_alc := mx_alc/(1+mx_alc)]
maindatanoimd[ , qx := mx/(1+mx)]

#cause-deleted death probabilities _dag represents the world with no alc-specific deaths
maindatanoimd[ , qx_dag := 1-((1-qx) ^ (1-rx))]

#px = probability of survival in year
maindatanoimd[ , px := 1-qx]
maindatanoimd[ , px_dag := 1-qx_dag]

#lx = probability of survival to start of year of age
setkeyv(maindatanoimd, c("sex", "year"))
maindatanoimd[ , lx:= cumprod(c(1, px[1:(length(px) - 1)])), by = key(maindatanoimd)]
maindatanoimd[ , lx_dag:= cumprod(c(1, px_dag[1:(length(px_dag) - 1)])), by = key(maindatanoimd)]
maindatanoimd[ , phix := qx * lx]
maindatanoimd[ , phix_dag := qx_dag * lx_dag]
maindatanoimd[ , Lx := phix*1/2+(lx-phix)]
maindatanoimd[ , Lx_dag := phix_dag*1/2+(lx_dag-phix_dag)]
maindatanoimd[ , Tx := rev(cumsum(rev(Lx))), by = key(maindatanoimd)]
maindatanoimd[ , Tx_dag := rev(cumsum(rev(Lx_dag))), by = key(maindatanoimd)]
maindatanoimd[ , ex := Tx / lx]
maindatanoimd[ , ex_dag := Tx_dag / lx_dag]

#collapse for calculations, extracting key vars - e18 and l18  
setkeyv(maindatanoimd, c("year", "sex"))
analysisdatanoimd<-copy(maindatanoimd[ , .(e18 = ex[age == 18],e18_dag=ex_dag[age==18], l18 = lx[age==18], 
                                           l18_dag=lx_dag[age==18]), by = key(maindatanoimd)])

#merge back into age data
maindatanoimd<-merge(maindatanoimd, analysisdatanoimd, by=key(maindatanoimd))

#calculate variance/sd
maindatanoimd[ , integral := phix*((age-(e18+18))^2)]
maindatanoimd[ , integral_dag := phix_dag*((age-(e18_dag+18))^2)]
allcausevarnoimd<-copy(subset(maindatanoimd, age>=18 & age<=maxage)[, .(integral=sum(integral), integral_dag=sum(integral_dag), l18=unique(l18), l18_dag=unique(l18_dag)), by=key(maindatanoimd)])
allcausevarnoimd[ , variance := integral/l18]
allcausevarnoimd[ , variance_dag := integral_dag/l18_dag]
allcausevarnoimd[ , s18 := sqrt(variance)]
allcausevarnoimd[ , s18_dag :=sqrt(variance_dag)]

#merge back into analysis data
setkeyv(allcausevarnoimd, c("year", "sex"))
analysisdatanoimd<-merge(analysisdatanoimd, allcausevarnoimd, by=key(allcausevarnoimd))
analysisdatanoimd$l18.y <-analysisdatanoimd$l18_dag.y <-NULL
names(analysisdatanoimd)[names(analysisdatanoimd)=="l18.x"]<-"l18"
names(analysisdatanoimd)[names(analysisdatanoimd)=="l18_dag.x"]<-"l18_dag"

Table2data<-analysisdatanoimd[,c("year", "sex", "e18", "e18_dag", "s18", "s18_dag")]
Table2data[ , e18alc := e18-e18_dag]
Table2data[ , s18alc := s18-s18_dag]

#Bring in data from top and bottom IMD quintiles
q1<-subset(analysisdata, imd_quintile==1)
q5<-subset(analysisdata, imd_quintile==5)

q1<-q1[,c("year", "sex", "e18", "e18_dag", "s18", "s18_dag")]
q5<-q5[,c("year", "sex", "e18", "e18_dag", "s18", "s18_dag")]

names(q1)[3]<-"e18q1"
names(q1)[4]<-"e18_dagq1"
names(q1)[5]<-"s18q1"
names(q1)[6]<-"s18_dagq1"

names(q5)[3]<-"e18q5"
names(q5)[4]<-"e18_dagq5"
names(q5)[5]<-"s18q5"
names(q5)[6]<-"s18_dagq5"

Table2data<-merge(Table2data, q1)
Table2data<-merge(Table2data, q5)
Table2data[ , e18alcq1 := e18q1-e18_dagq1]
Table2data[ , s18alcq1 := s18q1-s18_dagq1]
Table2data[ , e18alcq5 := e18q5-e18_dagq5]
Table2data[ , s18alcq5 := s18q5-s18_dagq5]

Table2data[ , legap:=e18q1-e18q5]
Table2data[ , lealcgap:=legap-(e18_dagq1-e18_dagq5)]
Table2data[ , sdgap:=s18q1-s18q5]
Table2data[ , sdalcgap:=sdgap-(s18_dagq1-s18_dagq5)]
Table2data[ , lealcgappercent:=lealcgap/legap]
Table2data[ , sdalcgappercent:=sdalcgap/sdgap]

Table2data<-Table2data[,c("sex", "year", "e18", "e18_dag", "e18alc", "e18q1", "e18_dagq1", "e18alcq1", "e18q5", "e18_dagq5", "e18alcq5", 
                          "legap", "lealcgap", "lealcgappercent","s18", "s18_dag", "s18alc", "s18q1", "s18_dagq1", "s18alcq1", "s18q5", "s18_dagq5", 
                          "s18alcq5", "sdgap", "sdalcgap", "sdalcgappercent")]

Table2data<-Table2data[order(sex, year)]

#Pull out data for Fig3
Fig3data <- gather(Table2data, measure, value, lealcgappercent, sdalcgappercent)

#Tity up formatting
Table2data$year <- as.integer(Table2data$year)
Table2data <- Table2data %>%
  mutate_at(c(3:13, 15:25), round,2)
Table2data$lealcgappercent <-  paste0(round(Table2data$lealcgappercent*100,1),"%") 
Table2data$sdalcgappercent <-  paste0(round(Table2data$sdalcgappercent*100,1),"%") 
Table2data$CoeffVar <- paste0(round(Table2data$s18*100/Table2data$e18,2),"%")

#Split out tables by sex
Table2 <- t(subset(Table2data, sex=="Male")[,-c(1)])
Table3 <- t(subset(Table2data, sex=="Female")[,-c(1)])

#output as .csv
write.csv(Table2, "TableS2data.csv")
write.csv(Table3, "TableS3data.csv")

#Generate Figure3
att_palette<-c("#5ea8a7", "#ff4447")

Figure3 <- ggplot(Fig3data, aes(x=year, y=value, colour=measure))+
  geom_line()+
  facet_wrap(~sex)+
  theme_classic()+
  scale_y_continuous(labels=c("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%", "8%", "9%", "10%"), limits=c(0,0.1), breaks=seq(0,0.1,by=0.01), name="Contribution of alcohol-specific deaths to inequality gap")+
  scale_colour_manual(values=att_palette, name="Measure", labels=c("Life expectancy", "Std. deviation in age of death"))+
  scale_x_continuous(breaks=c(2001,2003,2005,2007,2009,2011,2013,2015), name="Year")+
  theme(strip.background=element_blank())

Figure3

tiff("FigureS3.tiff", units="in", width=10, height=7, res=300)
Figure3
dev.off()

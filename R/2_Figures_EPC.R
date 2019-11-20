############# Written by JMA for figures for EPC
############# 23/10/2019
rm(list=ls(all=TRUE))

library(data.table)
library(ggplot2)
library(viridis)
library(reshape2)

# set your working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/HLI-Permanyer-Kashnitsky/")

#source("1_GetHMDData.R") # Just in case you want updatedM HD Data

# Loading results
#source('R/2_HLI.R')
load('Data/Results_HLI.RData')
HLI_Decomp[,Dif.H := H2-H1]
HLI_Decomp[,Year := year.1]
HLI_Decomp[,Decade := cut(year.1,breaks = c(seq(1750,2010,10),Inf),
                         #labels = c("1900-1921","1921-1959","1960 onwards"),
                         include.lowest = T)]

HLI_Decomp.decade <- HLI_Decomp[,list(Delta.mu.decade = sum(Delta.mu),
                                      Delta.I.decade = sum(Delta.I),
                                      Dif.h.decade = sum(Dif.H),
                                      H.bar = mean(H1),
                                      Inequality.bar = mean(I1),
                                      Mu.bar = mean(mu1),
                                      year.1 =  min(year.1)
                                      ), by = list(PopName,Sex,Decade,epsilon)]

HLI_Decomp.decade[,C.mu.decade := Delta.mu.decade/Dif.h.decade]
HLI_Decomp.decade[,C.I.decade := Delta.I.decade/Dif.h.decade]
HLI_Decomp.decade[,check1 := C.mu.decade + C.I.decade]

HLI_Decomp.decade[,Delta.relative.mu.decade := Delta.mu.decade/(abs(Delta.mu.decade)+abs(Delta.I.decade))]
HLI_Decomp.decade[,Delta.relative.I.decade := Delta.I.decade/(abs(Delta.mu.decade)+abs(Delta.I.decade))]

HLI_Data$epsilon <- factor(HLI_Data$epsilon, 
                           levels=c(.5,1,2),
                           labels=c((expression(paste(epsilon, " = 0.5"))),
                                    (expression(paste(epsilon, " = 1"))),
                                    (expression(paste(epsilon, " = 2")))))

HLI_Decomp.decade$epsilon <- factor(HLI_Decomp.decade$epsilon, 
                           levels=c(.5,1,2),
                           labels=c((expression(paste(epsilon, " = 0.5"))),
                                    (expression(paste(epsilon, " = 1"))),
                                    (expression(paste(epsilon, " = 2")))))


# first plot of efficiency vs inequality for different values of epsilon
Fig1 <- ggplot(data = HLI_Data[epsilon != 0], aes(x = efficiency, y = inequality,color=Year)) +  
  geom_point(alpha=I(1/2),shape=19,show.legend = T)+
  #facet_wrap(~epsilon,scales = 'free')+
  facet_wrap(~epsilon, labeller = labeller(epsilon = label_parsed))+
  #geom_smooth(data=Results,aes(x = eo, y = h), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  scale_x_continuous(expression(e[0]))+
  scale_color_viridis(discrete=F,option = 'C',direction = 1) +
  scale_y_continuous(expression(I[epsilon]))+
  theme(legend.key.height=unit(2,"line"))+  
  #theme(legend.position = c(0.15, 0.85))+
  #ggtitle(expression(paste("Efficiency (", e[0],") vs Inequality (",I[epsilon],'), by level of ', epsilon)))+
  theme(text = element_text(size = 15))+
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())
#Fig1

png(filename = 'Figures/Fig1EPC.png', width = 800, height = 400, units = "px", pointsize = 12)
Fig1
dev.off()

Fig2A <- ggplot(data = HLI_Decomp.decade, aes(x = Delta.mu.decade, y = Delta.I.decade, color=year.1 )) +  
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+geom_abline()+
  geom_point(alpha=I(1/2),shape=19,show.legend = T)+
  facet_wrap(~epsilon, labeller = labeller(epsilon = label_parsed))+
  scale_x_continuous(expression(Delta[e[0]]),limits = c(-6,15))+
  scale_color_viridis(discrete=F,option = 'C',direction = 1, name = 'Decade') +
  scale_y_continuous(expression(Delta[I]), limits = c(-6,20))+
  theme(legend.key.height=unit(2,"line"))+  
  #ggtitle(expression(paste('A) Absolute contribution of efficiency (', e[0],") by contribution of inequality (",Delta[I],'), by level of ', epsilon)))+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())
Fig2A

png(filename = 'Figures/Fig2EPC.png', width = 800, height = 400, units = "px", pointsize = 12)
Fig2A
dev.off()

#quantify prportions
HLI_Decomp.decade[, prop.quad :=  ifelse((Delta.mu.decade < 0 & Delta.I.decade < 0), 1 , 
                                        ifelse((Delta.mu.decade >= 0 & Delta.I.decade < 0), 2 ,
                                                ifelse((Delta.mu.decade >= 0 & Delta.I.decade >= 0), 3 ,4)))]


HLI_Decomp.decade <- HLI_Decomp.decade[!is.na(HLI_Decomp.decade$prop.quad)]

summary.prop <- HLI_Decomp.decade[,length(check1), by = list(prop.quad,epsilon)]

summary.prop[,percentage := V1/sum(V1), by = list(epsilon)]

Fig3.data <- HLI_Decomp.decade[,c(1,2,3,4,5,6,7,11)]
Fig3.data <- Fig3.data[year.1 %in% 1961:2010 & !(PopName %in% 
                                                   c('TWN','KOR','HRV','CHL',
                                                     'DEUTNP','HKG','SVN','ISR','GRC',
                                                     'NZL_NM','NZL_MA','DEUTE','DEUTw'))]

#Check country data
Fig3.data[,min(year.1), by = list(PopName)][order(V1)]
Fig3.data[,max(year.1), by = list(PopName)][order(V1)]

Fig3.data[, Total.change:= sum(Dif.h.decade), by = list(PopName,Sex,epsilon)]

#Select countries
all.countries <- Fig3.data[,Total.change[1], by = list(PopName,Sex,epsilon)][order(V1)][epsilon == .5 & Sex == 'f']
all.countries

#option 
option <- 2 #for 10 best performers
ifelse(option == 1, countries <- rev(all.countries$PopName)[1:10],
       countries <- c(rev(all.countries$PopName)[1:5],all.countries$PopName[1:5]))
countries

Fig3.data <- Fig3.data[PopName %in% countries, -7]

Fig3.data <- melt.data.table(Fig3.data,
                id.vars = c('PopName','Sex','Decade','epsilon','Total.change','year.1'),
                variable.name = 'Indicator',value.name = 'Contribution')

Fig3.data$Sex <- ifelse(Fig3.data$Sex == 'f', 'Females', 'Males')

Fig3.data$PopName   <- reorder(Fig3.data$PopName,Fig3.data$Total.change)

base2 <-c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")

Fig3<-  ggplot(Fig3.data, aes(x = as.numeric(PopName), y = Contribution, fill = Decade, alpha = Indicator)) +
  geom_rect(aes(x = as.numeric(PopName), xmin = -Inf, xmax = 5.5, ymin = -Inf, ymax = Inf),
            fill = "rosybrown1", alpha = .01)+
  ggtitle('Top-5 and Bottom-5 performers in H-index from 1960 to 2010 by Efficienty and Inequality')+
  facet_grid(Sex ~ epsilon,)+
  scale_x_continuous(breaks = 1:10,labels = rev(countries))+
  scale_alpha_manual('Changes due to', values=c(0.5, 1), labels = c('Efficiency','Inequality'))+
  scale_fill_manual('Cause of death', values = base2,labels = c('1960-1970','1970-1980',
                                                                '1980-1990','1990-2000',
                                                                '2000-2010')) +
  geom_bar(stat = "identity",position = "stack", show.legend = T)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Country", y = "Total change",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'right')+
  geom_hline(yintercept = 0)+
  coord_flip()
Fig3

pdf(file="Figures/Figure_3.pdf",width = 10, height = 5, pointsize = 12,
    useDingbats = F)
Fig3
dev.off()

# Fig2 <- ggplot(data = HLI_Decomp.decade, aes(x = H.bar, y = Delta.I.decade, color=year.1 )) +  
#   geom_point(alpha=I(1/2),shape=19,show.legend = T)+
#   facet_wrap(~epsilon)+
#   scale_x_continuous(expression(H[epsilon]))+
#   scale_color_viridis(discrete=F,option = 'C',direction = 1, name = 'Decade') +
#   scale_y_continuous(expression(Delta[I]), limits = c(-6,20))+
#   theme(legend.key.height=unit(2,"line"))+  
#   ggtitle(expression(paste(H[epsilon]," by contribution of inequality (",Delta[I],'), by level of ', epsilon)))+
#   theme(text = element_text(size = 15))+
#   theme(plot.background = element_blank()
#         ,legend.key = element_blank())
# previous_theme <- theme_set(theme_bw())
# Fig2
# 
# png(filename = 'Figures/Fig2EPC.png', width = 800, height = 400, units = "px", pointsize = 12)
# Fig2
# dev.off()
# 
# 
# Fig3 <- ggplot(data = HLI_Decomp.decade, aes(x = H.bar, y = Delta.mu.decade, color=year.1 )) +  
#   geom_point(alpha=I(1/2),shape=19,show.legend = T)+
#   facet_wrap(~epsilon)+
#   scale_x_continuous(expression(H[epsilon]))+
#   scale_color_viridis(discrete=F,option = 'C',direction = 1, name = 'Decade') +
#   scale_y_continuous(expression(Delta[M]), limits = c(-6,20))+
#   theme(legend.key.height=unit(2,"line"))+  
#   ggtitle(expression(paste(H[epsilon]," by contribution of efficiency (",Delta[M],'), by level of ', epsilon)))+
#   theme(text = element_text(size = 15))+
#   theme(plot.background = element_blank()
#         ,legend.key = element_blank())
# previous_theme <- theme_set(theme_bw())
# Fig3
# 
# png(filename = 'Figures/Fig3EPC.png', width = 800, height = 400, units = "px", pointsize = 12)
# Fig3
# dev.off()
# 

############# Written by JMA
############# 23/10/2019
rm(list=ls(all=TRUE))

library(data.table)
library(ggplot2)
library(viridis)

# set your working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/HLI-Permanyer-Kashnitsky/")

#source("1_GetHMDData.R") # Just in case you want updatedM HD Data

# Loading results
#source('R/2_HLI.R')
load('Data/Results_HLI.RData')
HLI_Decomp[,Dif.H := H2-H1]
HLI_Decomp[,Year := year.1]

# first plot of efficiency vs inequality for different values of epsilon
Fig1 <- ggplot(data = HLI_Data[epsilon != 0], aes(x = efficiency, y = inequality,color=Year)) +  
  geom_point(alpha=I(1/2),shape=19,show.legend = T)+
  #facet_wrap(~epsilon,scales = 'free')+
  facet_wrap(~epsilon)+
  #geom_smooth(data=Results,aes(x = eo, y = h), method = "lm", se=FALSE,col="black",size=1,lty=2,na.rm = T) + # if I want a linear one
  scale_x_continuous(expression(mu))+
  scale_color_viridis(discrete=F,option = 'C',direction = 1) +
  scale_y_continuous(expression(I[epsilon]))+
  theme(legend.key.height=unit(2,"line"))+  
  #theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("Efficiency (", mu,") vs Inequality (",I[epsilon],'), by level of ', epsilon)))+
  theme(text = element_text(size = 15))+
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())
#Fig1

png(filename = 'Figures/EfficiencyInequality_v1.png', width = 800, height = 400, units = "px", pointsize = 12)
Fig1
dev.off()


Fig2 <- ggplot(data = HLI_Data[epsilon != 0], aes(y = H_index, x = Year,color= factor(epsilon))) +  
  geom_point(alpha=I(1/2),shape=19,show.legend = T)+
  scale_x_continuous(expression(Year))+
  #scale_color_viridis(discrete=F,option = 'C',direction = 1) +
  scale_y_continuous(expression(H[epsilon]))+
  theme(legend.key.height=unit(2,"line"))+  
  ggtitle(expression(paste(" H index (",H[epsilon],'), by level of ', epsilon)))+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())
#Fig2

png(filename = 'Figures/Hindex_v1.png', width = 800, height = 600, units = "px", pointsize = 12)
Fig2
dev.off()


Fig3 <- ggplot(data = HLI_Decomp[epsilon != 0], aes(x = Delta.mu, y = Delta.I,color=Year)) +  
  geom_point(alpha=I(1/2),shape=19,show.legend = T)+
  #facet_wrap(~epsilon,scales = 'free')+
  facet_wrap(~epsilon)+
  scale_x_continuous(expression(Delta[mu]))+
  scale_color_viridis(discrete=F,option = 'C',direction = 1) +
  scale_y_continuous(expression(Delta[I]))+
  theme(legend.key.height=unit(2,"line"))+  
  #theme(legend.position = c(0.15, 0.85))+
  #ggtitle(expression(paste("Efficiency (", mu,") vs Inequality (",I[epsilon],'), by level of ', epsilon)))+
  theme(text = element_text(size = 15))+
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,legend.key = element_blank())
previous_theme <- theme_set(theme_bw())
#Fig1

png(filename = 'Figures/DeltasMUi_v1.png', width = 800, height = 600, units = "px", pointsize = 12)
Fig3
dev.off()



#pdf(file='Figures/EfficiencyInequality_v1.pdf',width=10,height=7,pointsize=4)
#Fig1
#dev.off()



#Figure 2

gdata95<-read.csv("gdata95.csv")
gdata95$Season <- factor(gdata95$Season, levels=c("Breeding", "Summer", "Fall"))
vars2 <- c("chartreuse4", "coral3","mediumslateblue", "mediumvioletred", "royalblue4", "gold2")
ggplot(gdata95, aes(x=habitat, y=yvar, color=factor(habitat)))+
  geom_point(size=3)+geom_errorbar(aes(ymin=yvar-SE, ymax=yvar+SE), data=gdata95, width=0.2)+facet_grid(sex~Season)+
  theme(strip.background=element_rect(color="black", fill="darkslategray3"), panel.border = element_rect(colour='black', fill='NA'), 
        panel.background = element_rect(fill='gray97'), panel.grid = element_line(colour = NA), panel.spacing = unit(1, "lines"),
        legend.key = element_rect(fill='white'), legend.position="blank", legend.text=element_text(size=12),
        axis.text=element_text(size=9), axis.title.x = element_blank(),axis.text.x = element_text(size=9),
        axis.title.y = element_text(size=12, vjust=3.9, hjust=0.7), strip.text = element_text(size=10, face="bold"))+
  theme(plot.margin=unit(c(0.2,0.2,0.2,0.5), "cm"))+ylab(expression(atop("95% utilization distribution","(kilometers squared)")))+scale_color_manual(values=vars2)+labs(color="Grid Number")+ guides(colour = guide_legend(nrow = 1))

#Figure 3
gdata60<-read.csv("gdata60.csv")
gdata60$Season <- factor(gdata60$Season, levels=c("Breeding", "Summer", "Fall"))
ggplot(gdata60, aes(x=habitat, y=yvar, color=factor(habitat)))+
  geom_point(size=3)+geom_errorbar(aes(ymin=yvar-SE, ymax=yvar+SE), data=gdata60, width=0.2)+facet_grid(sex~Season)+
  theme(strip.background=element_rect(color="black", fill="darkslategray3"), panel.border = element_rect(colour='black', fill='NA'), 
        panel.background = element_rect(fill='gray97'), panel.grid = element_line(colour = NA), panel.spacing = unit(1, "lines"),
        legend.key = element_rect(fill='white'), legend.position="blank", legend.text=element_text(size=12),
        axis.text=element_text(size=9), axis.title.x = element_blank(),axis.text.x = element_text(size=9),
        axis.title.y = element_text(size=12, vjust=3.9, hjust=0.7), strip.text = element_text(size=10, face="bold"))+
  theme(plot.margin=unit(c(0.2,0.2,0.2,0.5), "cm"))+ylab(expression(atop("60% utilization distribution","(kilometers squared)")))+scale_color_manual(values=vars2)+labs(color="Grid Number")+ guides(colour = guide_legend(nrow = 1))


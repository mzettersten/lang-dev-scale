#load packages
library(here)
library(tidyverse)
library(cowplot)
library(gghalves)
library(patchwork)

# load data
d <- read_csv(here("data","mb_ma_combined_prepped_0.125.csv"))

# plot
p1 <- ggplot(d,aes(studyTypePretty,yi,color=studyTypePretty))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_point(aes(size=1/vi),position=position_jitter(width=0.05,height=0),alpha=0.2)+
  geom_half_violin(position = position_nudge(x = -.15, y = 0), width=0.5,adjust=1.5,trim = TRUE,side="l")+
  geom_half_boxplot(position = position_nudge(x = .15, y = 0), center=TRUE,width=0.5,side="r",errorbar.draw = FALSE, outlier.shape=NA)+
  scale_color_brewer(type="qual",palette="Set1",direction=-1)+
  theme_cowplot()+
  theme(legend.position="none",
        strip.text = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16))+
  ylab("Effect Size")+
  xlab("Study")

p2 <- ggplot(d,aes(mean_age/30.44,yi,color=studyTypePretty,fill=studyTypePretty))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_point(aes(size=1/vi),position=position_jitter(width=0.3,height=0),alpha=0.1)+
  geom_smooth(method="lm",size=1.5)+
  #geom_smooth(color="#d01c8b",se=FALSE,linetype="dashed")+
  scale_color_brewer(name="Study",type="qual",palette="Set1",direction=-1)+
  scale_fill_brewer(name="Study",type="qual",palette="Set1",direction=-1)+
  scale_x_continuous(breaks=seq(0,30,3))+
  theme_cowplot()+
  guides(size="none")+
  theme(legend.position=c(0.5,0.8),
        strip.text = element_text(size=16),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16))+
  ylab("Effect Size")+
  xlab("Mean Age (in months)")

plot_grid(p1,p2,align="v",rel_widths=c(1,1.5),labels = c("A","B"),label_size=24)
ggsave(here("figures","mb_ma_w_age_figure.png"),width=9,height=6,bg="white")

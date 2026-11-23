library(cowplot)
library(here)
library(tidyverse)

d_aoi <- readRDS(here("cached_intermediates", "0_d_aoi.rds"))

#summarize by subject
d_timecourse_by_dataset_age_subj_novelty <- d_aoi |>
  filter(t_norm>-1000) |> 
  #create age bins cutoffs
  mutate(
    age_bin = cut(age, breaks = c(0,12,15,18,21,24,30,36,60), 
                  labels = c("<12", "12-15","15-18","18-21","21-24", "24-30","30-36", ">36"))
  ) |>
  group_by(dataset_name,dataset_id,subject_id,age_bin,stimulus_novelty,t_norm) |> 
  #calculate proportion time spent looking at target
  summarize(
    n=n(),
    accuracy = mean(correct, na.rm=TRUE))

#by age and stimulus novelty
d_timecourse_by_dataset_age_novelty <- d_timecourse_by_dataset_age_subj_novelty |>
  filter(n>=2) |> 
  group_by(dataset_name,dataset_id,age_bin,stimulus_novelty,t_norm) |>
  summarize(
    n_subj=n(),
    proportion_target_looking = mean(accuracy,na.rm=TRUE),
    #standard error of the mean
    sem = sd(proportion_target_looking,na.rm=TRUE)/sqrt(n_subj)
  ) |> 
  ungroup() |> 
  #create joined dataset_name and age_bin column
  tidyr::unite(dataset_novelty_age_bin,dataset_name,stimulus_novelty, age_bin,remove=F)

#across dataset
d_timecourse_across_dataset_by_age_novelty_overall <- d_timecourse_by_dataset_age_novelty |>
  group_by(age_bin,stimulus_novelty,t_norm) |>
  summarize(
    N = n(),
    accuracy = mean(proportion_target_looking,na.rm=TRUE),
    #standard error of the mean
    sem = sd(proportion_target_looking,na.rm=TRUE)/sqrt(N),
    #95% CIs
    ci = qt(0.975, df=N-1)*sem
  ) 

#by age bin, not representing dataset structure
d_timecourse_by_age_novelty_overall <- d_timecourse_by_dataset_age_subj_novelty |>
  filter(n>=2) |> 
  group_by(age_bin,stimulus_novelty,t_norm) |>
  summarize(
    n_subj=n(),
    proportion_target_looking = mean(accuracy,na.rm=TRUE),
    sem = sd(accuracy,na.rm=TRUE)/sqrt(n_subj),
    #95% CIs
    ci = qt(0.975, df=n_subj-1)*sem
  ) |> 
  rename(accuracy=proportion_target_looking)

#not representing dataset structure
d_timecourse_by_age_novelty_overall |> 
  filter(age_bin!="<12"&age_bin!=">36") |> 
  filter(t_norm>-500) |> 
  ggplot(aes(x=t_norm, y=accuracy, group=stimulus_novelty, color=stimulus_novelty)) +
  geom_smooth(stat="identity", aes(y=accuracy,ymin=accuracy-sem, ymax=accuracy+sem,fill=stimulus_novelty))+
  geom_hline(yintercept=0.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="solid")+
  scale_y_continuous(breaks=seq(0.4,1,0.1),limits=c(0.4,1))+
  xlab("Time from target word onset (ms)")+
  ylab("Proportion Target Looking")+
  #scale_color_viridis_d(name="Stimulus Novelty")+
  #scale_fill_viridis_d(name="Stimulus Novelty")+
  scale_color_manual(name="Stimulus Novelty",values=c("#5D3A9B","#E66100"))+
  scale_fill_manual(name="Stimulus Novelty",values=c("#5D3A9B","#E66100"))+
  theme_cowplot()+
  facet_wrap(~age_bin)+
  theme(legend.position=c(0.1,0.85))+
  #increase facet text size
  theme(strip.text = element_text(size=16,face="bold"),axis.text = element_text(size=14),axis.title=element_text(size=18,face="bold"),legend.text=element_text(size=16),legend.title=element_text(size=18,face="bold"))
ggsave(here("figures","timecourse_by_age_novelty_overall.png"), bg="white",width=10, height=6)


#BONUS: overview plot of datasets in Peekbank
aoi_data_joined <- d_aoi
means_items <- aoi_data_joined |>
  #left_join(dataset_name_mapping) |>
  filter(age <= 60) |>
  group_by(t_norm, dataset_name, target_label) |>
  summarise(n = sum(aoi %in% c("target","distractor"), na.rm = TRUE), 
            p = sum(aoi == "target", na.rm = TRUE),
            prop_looking = p / n#, 
            #ci_lower = binom::binom.confint(p, n, method = "bayes")$lower,
            #ci_upper = binom::binom.confint(p, n, method = "bayes")$upper
            ) 

average_across_items <- means_items |>
  group_by(t_norm, dataset_name) |>
  summarize(
    N=n(),
    avg_prop_looking=mean(prop_looking,na.rm=T),
    sd = sd(prop_looking,na.rm=T),
    ci=qt(0.975, N-1)*sd/sqrt(N),
    ci_lower=avg_prop_looking-ci,
    ci_upper=avg_prop_looking+ci
  )


p <- ggplot(filter(means_items, n>=12&t_norm>=-1000&t_norm<=4000), aes(x = t_norm, y = prop_looking,color=target_label)) + 
  geom_line(alpha=0.5) + 
  # geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
  #                 fill = dataset_name), alpha = .5) +
  # geom_smooth(data=average_across_items,
  #             aes(y=avg_prop_looking,ymin = ci_lower, ymax = ci_upper),color="black",stat="identity")+
  geom_smooth(color="black",se=F,method="gam")+
  geom_hline(yintercept = .5, lty = 2) + 
  geom_vline(xintercept = 0, lty = "solid") +
  ylab("Proportion Target Looking") +
  xlab("Time (msec)") +
  theme_classic() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  #facet_wrap(~apa_cite,nrow=4)+
  facet_wrap(~dataset_name,nrow=5)+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),strip.text.x = element_text(size = 6))+
  scale_x_continuous(breaks=seq(-500,4000,500))+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1))
p

ggsave(here("figures","peekbank_item_vis_new.png"),plot=p,width=10,height=6,dpi=600)



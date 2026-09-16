library(cowplot)

d_aoi <- readRDS(here("cached_intermediates", "0_d_aoi.rds"))

#summarize by subject
d_timecourse_by_dataset_age_subj_novelty <- d_aoi |>
  filter(t_norm>-1000) |> 
  #create age bins cutoffs
  mutate(
    age_bin = cut(age, breaks = c(0,12,15,18,21,24,30,36,60), 
                  labels = c("<12", "12-15","15-18","18-21","21-24", "24-30","30-36", ">36"))
  ) |>
  #downsample t_norm to 50ms bins
  #mutate(t_norm_downsampled = round(t_norm/50)*50) |>
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
  theme(legend.position=c(0.1,0.8))
ggsave(here("figures","timecourse_by_age_novelty_overall.png"), bg="white",width=8, height=6)

d_timecourse_across_dataset_by_age_novelty_overall |> 
  filter(age_bin!="<12"&age_bin!=">36") |> 
  filter(t_norm>-500) |> 
  ggplot(aes(x=t_norm, y=accuracy, group=stimulus_novelty, color=stimulus_novelty)) +
  geom_smooth(stat="identity", aes(y=accuracy,ymin=accuracy-sem, ymax=accuracy+sem,fill=stimulus_novelty))+
  geom_hline(yintercept=0.5, linetype="dashed")+
  geom_vline(xintercept=0, linetype="solid")+
  scale_y_continuous(breaks=seq(0.4,1,0.1),limits=c(0.4,1))+
  xlab("Time from trial onset (ms)")+
  ylab("Proportion Target Looking")+
  scale_color_viridis_d(name="Stimulus Novelty")+
  scale_fill_viridis_d(name="Stimulus Novelty")+
  theme_cowplot()+
  facet_wrap(~age_bin)


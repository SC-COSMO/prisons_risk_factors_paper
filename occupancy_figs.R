#Figure 1, Figure S5, Figure S6

library(lubridate)
library(tidyverse)
library(scales)
library(ggpubr)

setwd("C:/Users/Tess/Box/Shared/Documents/COVID-19/SCCOSMO/Risk_Factors_Paper")
data <- read.csv("fig1.csv", stringsAsFactors=F)
data_excl <- read.csv("occ_no_covid.csv", stringsAsFactors=F) #version of data, but covid cases are excluded
crosswalk <- read.csv("inst_crosswalk_v3.csv", stringsAsFactors=F)

colors <- c(hue_pal()(5), "black")
names(colors) <- c(unique(crosswalk %>% arrange(division) %>% pull(division)), "All Combined")

#clean dates, merge with instids, tag outbreak insts
data <- data %>% mutate(date=lubridate::dmy(date))
data <- left_join(data, crosswalk, by="instid")
outbreaks <- c(122079210, 239192042, 241944554, 242468842, 242629354, 242654954, 242655466, 242666730, 243149546)
data <- data %>% mutate(outbreak=1*(instid %in% outbreaks))

data_excl <- data_excl %>% mutate(date=lubridate::dmy(date))
data_excl <- left_join(data_excl, crosswalk, by="instid")
data_excl <- data_excl %>% mutate(outbreak=1*(instid %in% outbreaks))

#reshapes for figure 1
data_all <- unique(data %>% select(date, month, room_occ_mean_all))
data_all <- data_all %>% mutate(division="All Combined") %>% rename(room_occ_mean=room_occ_mean_all)
data_dumbbell <- bind_rows(data, data_all)
data_dumbbell <- data_dumbbell %>% filter(date=="2020-03-01"|date=="2020-10-10") %>% group_by(instid) %>%
  mutate(pop_start=pop[date=="2020-03-01"], pop_end=pop[date=="2020-10-10"],
         occ_start=room_occ_mean[date=="2020-03-01"], occ_end=room_occ_mean[date=="2020-10-10"]) %>%
  select(instid, division, outbreak, pop_start, pop_end, occ_start, occ_end)
data_dumbbell <- unique(data_dumbbell)

#calculate % decrease
data_dumbbell <- data_dumbbell %>% 
  mutate(pop_change=100*(pop_end-pop_start)/pop_start, occ_change=100*(occ_end-occ_start)/occ_start)
data_dumbbell <- data_dumbbell %>% 
  mutate(pop_change_lab=case_when(pop_change>=1~paste0("+", as.character(round(pop_change)), "%"),
                                  pop_change<=-1~paste0(as.character(round(pop_change)), "%"),
                                  pop_change<0 & pop_change>-1~"> -1%",
                                  pop_change>0 & pop_change<1~"< +1%"),
         occ_change_lab=case_when(occ_change>=1~paste0("+", as.character(round(occ_change)), "%"),
                                  occ_change<=-1~paste0(as.character(round(occ_change)), "%"),
                                  occ_change<0 & occ_change>-1~"> -1%",
                                  occ_change>0 & occ_change<1~"< +1%"))
data_dumbbell <- data_dumbbell %>% mutate(pop_lab_pos=if_else(pop_change<0, pop_start, pop_end),
                                          occ_lab_pos=if_else(occ_change<0, occ_start, occ_end))

#position for version sorted by outbreak vs. non-outbreak and then pop
data_dumbbell <- data_dumbbell %>% mutate(y_pos=(outbreak*100000)+pop_start)
data_dumbbell$y_pos <- as.factor(data_dumbbell$y_pos)

#FIGURE 1
fig1 <- ggplot(data=data_dumbbell) +
  geom_point(aes(x=pop_start, y=y_pos, fill=division, color=division), 
             stat="identity", size=3) +
  geom_errorbar(aes(xmin=pop_start, xmax=pop_end, y=y_pos, group=y_pos, color=division), width=0) +
  geom_errorbar(aes(xmin=pop_end, xmax=pop_end, y=y_pos, group=y_pos, color=division), size=1, width=0.75) +
  geom_text(aes(x=pop_lab_pos + 400, y=y_pos, label=pop_change_lab), size=3.5) +
  geom_hline(yintercept=26.5, linetype="dashed") +
  scale_fill_manual(values=colors) + scale_color_manual(values=colors) +
  scale_y_discrete(breaks=NULL) +
  labs(x="Total Incarcerated Population by Prison", y="Prisons (sorted by population on March 1, 2020)", fill="", color="") + theme_bw() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position="top") +
  guides(color=guide_legend(nrow=2), fill=guide_legend(nrow=2))

fig2 <- ggplot(data=data_dumbbell) +
  geom_point(aes(x=occ_start-1, y=y_pos, fill=division, color=division), 
             stat="identity", size=3) +
  geom_errorbar(aes(xmin=occ_start, xmax=occ_end, y=y_pos, group=y_pos, color=division), width=0) +
  geom_errorbar(aes(xmin=occ_end, xmax=occ_end, y=y_pos, group=y_pos, color=division), size=1, width=0.75) +
  geom_text(aes(x=occ_lab_pos + 7, y=y_pos, label=occ_change_lab), size=3.5) +
  geom_hline(yintercept=26.5, linetype="dashed") +  
  geom_hline(yintercept=35.5, linetype="dashed") +
  geom_text(x=100, y=27, label="Outbreak Prisons", size=3.5, hjust=1) +
  geom_text(x=100, y=26, label="Non-Outbreak Prisons", size=3.5, hjust=1) +
  geom_text(x=100, y=36, label="All Combined", size=3.5, hjust=1) +
  scale_fill_manual(values=colors) + scale_color_manual(values=colors) +
  scale_y_discrete(breaks=NULL) +
  labs(x="Average Roommates (#) by Prison", y="", fill="", color="") + theme_bw() +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position="top") +
  guides(color=guide_legend(nrow=2), fill=guide_legend(nrow=2))

fig <- ggarrange(fig1, fig2, common.legend=T, legend="bottom")
ggsave(fig, filename="fig1.jpg", dpi=500, height=8, width=8)

#FIGURE S5#
data <- data %>% group_by(instid) %>% mutate(x_pos=room_occ_mean[date=="2020-03-01"])
data$x_pos <- as.factor(data$x_pos)
data <- data %>% mutate(room_occ25=(room_occ2+room_occ3)/2, room_occ975=(room_occ97+room_occ98)/2)
fig1 <- ggplot(data %>% filter(date=="2020-03-01"), aes(x=x_pos, y=room_occ_mean-1, color=division, fill=division)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=room_occ25, ymax=room_occ975), width=0) +
  geom_hline(aes(yintercept=room_occ_mean_all-1), linetype="dashed") +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limits=c(0, 200), expand=c(0,0)) +
  labs(x="", y="", color="", fill="") +
  ggtitle("March 1, 2020") +
  theme_bw() + theme(plot.title=element_text(size=10, face="bold"), axis.title.y=element_blank())
fig2 <- ggplot(data %>% filter(date=="2020-10-10"), aes(x=x_pos, y=room_occ_mean-1, color=division, fill=division)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=room_occ25, ymax=room_occ975), width=0) +
  geom_hline(aes(yintercept=room_occ_mean_all-1), linetype="dashed") +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limits=c(0, 200), expand=c(0,0)) +
  labs(x="Prisons (sorted by average roommates on March 1, 2020)", y="", color="", fill="") +
  ggtitle("October 10, 2020") +
  theme_bw() + theme(plot.title=element_text(size=10, face="bold"), axis.title.y=element_blank())
fig <- ggarrange(fig1, fig2, common.legend=T, legend="bottom", ncol=1)
annotate_figure(fig, left=text_grob("Average Roommates (#)", rot=90))
ggsave("fig_s5.jpg", dpi=500, height=6, width=7.2)

#FIGURE S6#
data_excl <- data_excl %>% group_by(instid) %>% mutate(x_pos=room_occ_mean[date=="2020-03-01"])
data_excl$x_pos <- as.factor(data_excl$x_pos)
data_excl <- data_excl %>% mutate(room_occ25=(room_occ2+room_occ3)/2, room_occ975=(room_occ97+room_occ98)/2)
fig1 <- ggplot(data_excl %>% filter(date=="2020-03-01"), aes(x=x_pos, y=room_occ_mean-1, color=division, fill=division)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=room_occ25, ymax=room_occ975), width=0) +
  geom_hline(aes(yintercept=room_occ_mean_all-1), linetype="dashed") +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limits=c(0, 200), expand=c(0,0)) +
  labs(x="", y="", color="", fill="") +
  ggtitle("March 1, 2020") +
  theme_bw() + theme(plot.title=element_text(size=10, face="bold"), axis.title.y=element_blank())
fig2 <- ggplot(data_excl %>% filter(date=="2020-10-10"), aes(x=x_pos, y=room_occ_mean-1, color=division, fill=division)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=room_occ25, ymax=room_occ975), width=0) +
  geom_hline(aes(yintercept=room_occ_mean_all-1), linetype="dashed") +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limits=c(0, 200), expand=c(0,0)) +
  labs(x="Prisons (sorted by average roommates on March 1, 2020)", y="", color="", fill="") +
  ggtitle("October 10, 2020") +
  theme_bw() + theme(plot.title=element_text(size=10, face="bold"), axis.title.y=element_blank())
fig <- ggarrange(fig1, fig2, common.legend=T, legend="bottom", ncol=1)
annotate_figure(fig, left=text_grob("Average Roommates (#)", rot=90))
ggsave("fig_s6.jpg", dpi=500, height=6, width=7.2)

#stats for text
data_covid_all %>% filter(date=="2020-10-10") %>% group_by(room_type) %>% summarise(room_occ=sum(room_occ))
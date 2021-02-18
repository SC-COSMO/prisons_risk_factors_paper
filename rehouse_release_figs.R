#Figure S7, Figure S10, Figure S11, Figure S12

library(lubridate)
library(tidyverse)
library(scales)
library(ggpubr)
library(RColorBrewer)
library(cowplot)

#fill colors
colors <- brewer.pal(3, "Set2")
names(colors) <- c("Released", "Cell", "Dorm")

setwd("C:/Users/Tess/Box/Shared/Documents/COVID-19/SCCOSMO/Risk_Factors_Paper")
data_covid <- read.csv("release_rehouse_risk.csv", stringsAsFactors=F)
data_age <- read.csv("release_rehouse_age.csv", stringsAsFactors=F)
data_sec <- read.csv("release_rehouse_sec.csv", stringsAsFactors=F)
crosswalk <- read.csv("inst_crosswalk_v3.csv", stringsAsFactors=F)

#clean dates, merge with instids, tag outbreak insts
outbreaks <- c(122079210, 239192042, 241944554, 242468842, 242629354, 242654954, 242655466, 242666730, 243149546)

data_covid <- data_covid %>% mutate(date=lubridate::dmy(date))
data_covid <- left_join(data_covid, crosswalk, by="instid")
data_covid <- data_covid %>% mutate(outbreak=1*(instid %in% outbreaks), 
                                    outbreak_lab=if_else(outbreak==1, "Outbreak", "Non-Outbreak"),
                                    outbreak_lab=factor(outbreak_lab, levels=c("Outbreak", "Non-Outbreak")))
data_age <- data_age %>% mutate(date=lubridate::dmy(date))
data_age <- left_join(data_age, crosswalk, by="instid")
data_age <- data_age %>% mutate(outbreak=1*(instid %in% outbreaks), 
                                outbreak_lab=if_else(outbreak==1, "Outbreak", "Non-Outbreak"),
                                outbreak_lab=factor(outbreak_lab, levels=c("Outbreak", "Non-Outbreak")))
data_sec <- data_sec %>% mutate(date=lubridate::dmy(date))
data_sec <- left_join(data_sec, crosswalk, by="instid")
data_sec <- data_sec %>% mutate(outbreak=1*(instid %in% outbreaks), 
                                outbreak_lab=if_else(outbreak==1, "Outbreak", "Non-Outbreak"),
                                outbreak_lab=factor(outbreak_lab, levels=c("Outbreak", "Non-Outbreak")))

#clean up categories
data_covid <- data_covid %>% mutate(covidriskcdcr=case_when(covidriskcdcr>=6~"Score: 6 +",
                                                            covidriskcdcr<=1~"Score: 0 - 1",
                                                            TRUE~paste0("Score: ", as.character(covidriskcdcr))))
data_age <- data_age %>% mutate(agecat=case_when(agecat=="18 - 29"|agecat=="30 - 39"~"18 - 39",
                                                 agecat=="80 - 89"|agecat=="90 plus"~"80 plus",
                                                 TRUE~agecat))

#ALL INSTITUTIONS COMBINED
#covid risk score fig: sums by date and room type
data_covid_all <- data_covid %>% group_by(date, month, covidriskcdcr, room_type) %>% summarise(room_occ=sum(room_occ))
#calculate releases as 1-dorm+cell
data_covid_release <- data_covid_all %>% group_by(date, month, covidriskcdcr) %>% summarise(pop=sum(room_occ))
data_covid_release <- data_covid_release %>% ungroup() %>% group_by(covidriskcdcr) %>% mutate(pop_start=pop[date=="2020-03-01"])
data_covid_release <- data_covid_release %>% mutate(room_occ=pop_start-pop, room_type="Released") %>% 
  select(date, month, covidriskcdcr, room_occ, room_type)
data_covid_all <- bind_rows(data_covid_all, data_covid_release)
data_covid_all$room_type <- factor(data_covid_all$room_type, levels=c("Released", "Cell", "Dorm"))
data_covid_all <- data_covid_all %>% group_by(covidriskcdcr, date, month) %>% mutate(type_prop=room_occ/sum(room_occ))
data_covid_all <- data_covid_all %>% mutate(type_prop_lab=paste0(as.character(round(100*type_prop)), "%"))
data_covid_all <- data_covid_all %>% mutate(type_prop_pos=case_when(room_type=="Dorm"~type_prop/2,
                                                                    room_type=="Cell"~type_prop/2+type_prop[room_type=="Dorm"],
                                                                    room_type=="Released"~type_prop/2+type_prop[room_type=="Cell"]+type_prop[room_type=="Dorm"]))
#age fig: sums by date and room type
data_age_all <- data_age %>% group_by(date, month, agecat, room_type) %>% summarise(room_occ=sum(room_occ))
#calculate releases as 1-dorm+cell
data_age_release <- data_age_all %>% group_by(date, month, agecat) %>% summarise(pop=sum(room_occ))
data_age_release <- data_age_release %>% ungroup() %>% group_by(agecat) %>% mutate(pop_start=pop[date=="2020-03-01"])
data_age_release <- data_age_release %>% mutate(room_occ=pop_start-pop, room_type="Released") %>% 
  select(date, month, agecat, room_occ, room_type)
data_age_all <- bind_rows(data_age_all, data_age_release)
data_age_all$room_type <- factor(data_age_all$room_type, levels=c("Released", "Cell", "Dorm"))
data_age_all <- data_age_all %>% group_by(agecat, date, month) %>% mutate(type_prop=room_occ/sum(room_occ))
data_age_all <- data_age_all %>% mutate(type_prop_lab=paste0(as.character(round(100*type_prop)), "%"))
data_age_all <- data_age_all %>% mutate(type_prop_pos=case_when(room_type=="Dorm"~type_prop/2,
                                                                    room_type=="Cell"~type_prop/2+type_prop[room_type=="Dorm"],
                                                                    room_type=="Released"~type_prop/2+type_prop[room_type=="Cell"]+type_prop[room_type=="Dorm"]))
#sec lvl score fig: sums by date and room type
data_sec_all <- data_sec %>% group_by(date, month, seclvl, room_type) %>% summarise(room_occ=sum(room_occ))
#calculate releases as 1-dorm+cell
data_sec_release <- data_sec_all %>% group_by(date, month, seclvl) %>% summarise(pop=sum(room_occ))
data_sec_release <- data_sec_release %>% ungroup() %>% group_by(seclvl) %>% mutate(pop_start=pop[date=="2020-03-01"])
data_sec_release <- data_sec_release %>% mutate(room_occ=pop_start-pop, room_type="Released") %>% 
  select(date, month, seclvl, room_occ, room_type)
data_sec_all <- bind_rows(data_sec_all, data_sec_release)
data_sec_all$room_type <- factor(data_sec_all$room_type, levels=c("Released", "Cell", "Dorm"))
data_sec_all <- data_sec_all %>% group_by(seclvl, date, month) %>% mutate(type_prop=room_occ/sum(room_occ))
data_sec_all <- data_sec_all %>% mutate(type_prop_lab=paste0(as.character(round(100*type_prop)), "%"))
data_sec_all <- data_sec_all %>% mutate(type_prop_pos=case_when(room_type=="Dorm"~type_prop/2,
                                                                    room_type=="Cell"~type_prop/2+type_prop[room_type=="Dorm"],
                                                                    room_type=="Released"~type_prop/2+type_prop[room_type=="Cell"]+type_prop[room_type=="Dorm"]))

#OUTBREAK VS. NON-OUTBREAK INSTITUTIONS
#covid risk score fig: sums by date and room type
data_covid_outbreak <- data_covid %>% group_by(date, month, covidriskcdcr, room_type, outbreak, outbreak_lab) %>% 
  summarise(room_occ=sum(room_occ))
#calculate releases as 1-dorm+cell
data_covid_release <- data_covid_outbreak %>% group_by(date, month, covidriskcdcr, outbreak, outbreak_lab) %>% 
  summarise(pop=sum(room_occ))
data_covid_release <- data_covid_release %>% ungroup() %>% group_by(covidriskcdcr, outbreak, outbreak_lab) %>% 
  mutate(pop_start=pop[date=="2020-03-01"])
data_covid_release <- data_covid_release %>% mutate(room_occ=pop_start-pop, room_type="Released") %>% 
  select(date, month, outbreak, outbreak_lab, covidriskcdcr, room_occ, room_type)
data_covid_outbreak <- bind_rows(data_covid_outbreak, data_covid_release)
data_covid_outbreak$room_type <- factor(data_covid_outbreak$room_type, levels=c("Released", "Cell", "Dorm"))
data_covid_outbreak <- data_covid_outbreak %>% group_by(covidriskcdcr, date, month, outbreak, outbreak_lab) %>% 
  mutate(type_prop=room_occ/sum(room_occ))
data_covid_outbreak <- data_covid_outbreak %>% mutate(type_prop_lab=paste0(as.character(round(100*type_prop)), "%"))
data_covid_outbreak <- data_covid_outbreak %>% mutate(type_prop_pos=case_when(room_type=="Dorm"~type_prop/2,
                                                                    room_type=="Cell"~type_prop/2+type_prop[room_type=="Dorm"],
                                                                    room_type=="Released"~type_prop/2+type_prop[room_type=="Cell"]+type_prop[room_type=="Dorm"]))
#age fig: sums by date and room type
data_age_outbreak <- data_age %>% group_by(date, month, agecat, room_type, outbreak, outbreak_lab) %>% 
  summarise(room_occ=sum(room_occ))
#calculate releases as 1-dorm+cell
data_age_release <- data_age_outbreak %>% group_by(date, month, agecat, outbreak, outbreak_lab) %>% 
  summarise(pop=sum(room_occ))
data_age_release <- data_age_release %>% ungroup() %>% group_by(agecat, outbreak, outbreak_lab) %>% 
  mutate(pop_start=pop[date=="2020-03-01"])
data_age_release <- data_age_release %>% mutate(room_occ=pop_start-pop, room_type="Released") %>% 
  select(date, month, outbreak, outbreak_lab, agecat, room_occ, room_type)
data_age_outbreak <- bind_rows(data_age_outbreak, data_age_release)
data_age_outbreak$room_type <- factor(data_age_outbreak$room_type, levels=c("Released", "Cell", "Dorm"))
data_age_outbreak <- data_age_outbreak %>% group_by(agecat, date, month, outbreak, outbreak_lab) %>% 
  mutate(type_prop=room_occ/sum(room_occ))
data_age_outbreak <- data_age_outbreak %>% mutate(type_prop_lab=paste0(as.character(round(100*type_prop)), "%"))
data_age_outbreak <- data_age_outbreak %>% mutate(type_prop_pos=case_when(room_type=="Dorm"~type_prop/2,
                                                                room_type=="Cell"~type_prop/2+type_prop[room_type=="Dorm"],
                                                                room_type=="Released"~type_prop/2+type_prop[room_type=="Cell"]+type_prop[room_type=="Dorm"]))
#sec lvl score fig: sums by date and room type
data_sec_outbreak <- data_sec %>% group_by(date, month, seclvl, room_type, outbreak, outbreak_lab) %>% 
  summarise(room_occ=sum(room_occ))
#calculate releases as 1-dorm+cell
data_sec_release <- data_sec_outbreak %>% group_by(date, month, seclvl, outbreak, outbreak_lab) %>% summarise(pop=sum(room_occ))
data_sec_release <- data_sec_release %>% ungroup() %>% group_by(seclvl, outbreak, outbreak_lab) %>% mutate(pop_start=pop[date=="2020-03-01"])
data_sec_release <- data_sec_release %>% mutate(room_occ=pop_start-pop, room_type="Released") %>% 
  select(date, month, seclvl, room_occ, room_type, outbreak, outbreak_lab)
data_sec_outbreak <- bind_rows(data_sec_outbreak, data_sec_release)
data_sec_outbreak$room_type <- factor(data_sec_outbreak$room_type, levels=c("Released", "Cell", "Dorm"))
data_sec_outbreak <- data_sec_outbreak %>% group_by(seclvl, date, month, outbreak, outbreak_lab) %>% mutate(type_prop=room_occ/sum(room_occ))
data_sec_outbreak <- data_sec_outbreak %>% mutate(type_prop_lab=paste0(as.character(round(100*type_prop)), "%"))
data_sec_outbreak <- data_sec_outbreak %>% mutate(type_prop_pos=case_when(room_type=="Dorm"~type_prop/2,
                                                                room_type=="Cell"~type_prop/2+type_prop[room_type=="Dorm"],
                                                                room_type=="Released"~type_prop/2+type_prop[room_type=="Cell"]+type_prop[room_type=="Dorm"]))


#FIGURE S7
fig1 <- ggplot(data_covid_all %>% filter(date!="2020-10-10"), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percent", fill="") +
  facet_wrap(~covidriskcdcr) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0))
fig2 <- ggplot(data_age_all %>% filter(date!="2020-10-10"), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Age Group") +
  facet_wrap(~agecat) + ggtitle("Age Group") +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))
fig3 <- ggplot(data_sec_all %>% filter(date!="2020-10-10" & !is.na(seclvl)), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Security Level") +
  facet_wrap(~seclvl) + ggtitle("Security Level") +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))
plot_grid(fig1+theme(legend.position="none"), fig3, fig2+theme(legend.position="none"), ncol=1, nrow=3, labels=c("A", "B", "C"))
ggsave(filename="figs7_orig.jpg", dpi=500, height=8, width=6)

fig1 <- ggplot(data_covid_all %>% filter(date!="2020-10-10"), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") +
  facet_wrap(~covidriskcdcr) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0))
fig2 <- ggplot(data_age_all %>% filter(date!="2020-10-10"), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + 
  facet_wrap(~agecat) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))
fig3 <- ggplot(data_sec_all %>% filter(date!="2020-10-10" & !is.na(seclvl)), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + 
  facet_wrap(~seclvl) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))
plot_grid(fig1+theme(legend.position="none"), plot_grid(fig3, NULL, ncol=2, rel_widths=c(0.855, 0.145)), ncol=1, nrow=2, labels=c("A", "B"))
ggsave(filename="figs7_updates_pt1.jpg", dpi=500, height=10, width=7)
plot_grid(fig2+theme(legend.position="bottom"), labels=c("C"))
ggsave(filename="figs7_updates_pt2.jpg", dpi=500, height=5.5, width=7)

#FIGURES S10-S12 (outbreak vs. non-outbreak)
fig1 <- ggplot(data_covid_outbreak %>% filter(date!="2020-10-10" & outbreak==1), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Covid-19 Risk Score") +
  facet_wrap(~covidriskcdcr) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0), 
                     plot.title=element_text(size=10))
fig2 <- ggplot(data_sec_outbreak %>% filter(date!="2020-10-10" & !is.na(seclvl) & outbreak==1), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Security Level") +
  facet_wrap(~seclvl) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))
fig3 <- ggplot(data_age_outbreak %>% filter(date!="2020-10-10" & outbreak==1), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Age Group") +
  facet_wrap(~agecat) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))
fig4 <- ggplot(data_covid_outbreak %>% filter(date!="2020-10-10" & outbreak==0), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Covid-19 Risk Score") +
  facet_wrap(~covidriskcdcr) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0), 
                     plot.title=element_text(size=10))
fig5 <- ggplot(data_sec_outbreak %>% filter(date!="2020-10-10" & !is.na(seclvl) & outbreak==0), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Security Level") +
  facet_wrap(~seclvl) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))
fig6 <- ggplot(data_age_outbreak %>% filter(date!="2020-10-10" & outbreak==0), aes(x=date, y=type_prop, fill=room_type)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(x=date, y=type_prop_pos, label=type_prop_lab), size=2.5) +
  scale_y_continuous(labels=percent) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%B")) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Percentage", fill="") + ggtitle("Age Group") +
  facet_wrap(~agecat) +
  theme_bw() + theme(panel.grid=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0),
                     plot.title=element_text(size=10))

fig_risk <- plot_grid(fig1+theme(legend.position="none")+ggtitle("Outbreak Prisons"), 
                    fig4+theme(legend.position="none")+ggtitle("Non-Outbreak Prisons"), 
                    ncol=1, nrow=2, align="hv")
plot_grid(fig_risk, get_legend(fig1), ncol=2, nrow=1, rel_widths=c(0.85, 0.15), align="hv")
ggsave(filename="fig_s10.jpg", dpi=500, height=8, width=8)

fig_sec <- plot_grid(fig2+theme(legend.position="none")+ggtitle("Outbreak Prisons"), 
                    fig5+theme(legend.position="none")+ggtitle("Non-Outbreak Prisons"), 
                    ncol=1, nrow=2, align="hv")
plot_grid(fig_sec, get_legend(fig1), ncol=2, nrow=1, rel_widths=c(0.8, 0.2), align="hv")
ggsave(filename="fig_s11.jpg", dpi=500, height=8, width=7)

fig_age <- plot_grid(fig3+theme(legend.position="none")+ggtitle("Outbreak Prisons"), 
                     fig6+theme(legend.position="none")+ggtitle("Non-Outbreak Prisons"), 
                     ncol=1, nrow=2, align="hv")
plot_grid(fig_age, get_legend(fig1), ncol=2, nrow=1, rel_widths=c(0.85, 0.15), align="hv")
ggsave(filename="fig_s12.jpg", dpi=500, height=8, width=8)

test <- data_covid_all %>% group_by(date, month, room_type) %>% summarise(room_occ=sum(room_occ))
test <- test %>% ungroup() %>% group_by(date, month) %>% mutate(type_prop=room_occ/sum(room_occ))

test <- data_age_all %>% group_by(date, month, room_type) %>% summarise(room_occ=sum(room_occ))
test <- test %>% ungroup() %>% group_by(date, month) %>% mutate(type_prop=room_occ/sum(room_occ))

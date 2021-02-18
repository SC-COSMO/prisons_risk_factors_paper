#Figure 2, Figure S8, Figure S9, Figure S13

library(lubridate)
library(tidyverse)
library(scales)
library(ggpubr)
library(cowplot)

setwd("C:/Users/Tess/Box/Shared/Documents/COVID-19/SCCOSMO/Risk_Factors_Paper")
data_age <- read.csv("age_activities.csv", stringsAsFactors=F)
data_covid <- read.csv("covidrisk_activities.csv", stringsAsFactors=F)
crosswalk <- read.csv("inst_crosswalk_v3.csv", stringsAsFactors=F)

#clean up dates
data_age <- data_age %>% mutate(date=lubridate::dmy(date))
data_covid <- data_covid %>% mutate(date=lubridate::dmy(date))

#colors
colors_age <- c(hue_pal()(7))
names(colors_age) <- c("80 or older", "18 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79")
colors_covid <- c(hue_pal()(7))
names(colors_covid) <- c("6+", as.character(0:5))


#merge institution info 
data_age <- left_join(data_age, crosswalk, by="instid")
data_covid <- left_join(data_covid, crosswalk, by="instid")

#tag outbreak institutions
outbreaks <- c(122079210, 239192042, 241944554, 242468842, 242629354, 242654954, 242655466, 242666730, 243149546)
data_age <- data_age %>% mutate(outbreak=1*(instid %in% outbreaks), 
                                outbreak_lab=if_else(outbreak==1, "Outbreak", "Non-Outbreak"),
                                outbreak_lab=factor(outbreak_lab, levels=c("Outbreak", "Non-Outbreak")))
data_covid <- data_covid %>% mutate(outbreak=1*(instid %in% outbreaks), 
                                    outbreak_lab=if_else(outbreak==1, "Outbreak", "Non-Outbreak"),
                                    outbreak_lab=factor(outbreak_lab, levels=c("Outbreak", "Non-Outbreak")))

#collapse age and covid risk score categories
data_age <- data_age %>% mutate(agecat=if_else(agecat=="90 plus"|agecat=="80 - 89", "80 or older", agecat))
data_age <- data_age %>% group_by(instid, name, division, type, outbreak, outbreak_lab, month, date, agecat) %>% summarise_all(sum)
data_covid <- data_covid %>% mutate(covidriskcdcr=if_else(covidriskcdcr>=6, "6+", as.character(covidriskcdcr)))
data_covid <- data_covid %>% group_by(instid, name, division, type, outbreak, outbreak_lab, month, date, covidriskcdcr) %>% summarise_all(sum)

#generate 2 week average participation in activities across all prisons
data_age_all <- data_age %>% group_by(date, agecat) %>% summarise(cum_sum_all_2wk=sum(cum_sum_all_2wk),
                                                                  cum_sum_work_2wk=sum(cum_sum_work_2wk),
                                                                  cum_sum_other_2wk=sum(cum_sum_other_2wk))
data_age_all <- data_age_all %>% mutate(prop_work_any=cum_sum_work_2wk/cum_sum_all_2wk,
                                        prop_other_any=cum_sum_other_2wk/cum_sum_all_2wk)
data_covid_all <- data_covid %>% group_by(date, covidriskcdcr) %>% summarise(cum_sum_all_2wk=sum(cum_sum_all_2wk),
                                                                  cum_sum_work_2wk=sum(cum_sum_work_2wk),
                                                                  cum_sum_other_2wk=sum(cum_sum_other_2wk))
data_covid_all <- data_covid_all %>% mutate(prop_work_any=cum_sum_work_2wk/cum_sum_all_2wk,
                                        prop_other_any=cum_sum_other_2wk/cum_sum_all_2wk)

#generate 2 week average participation in activities across all prisons with outbreaks
data_age_outbreak <- data_age %>% group_by(date, agecat, outbreak, outbreak_lab) %>% summarise(cum_sum_all_2wk=sum(cum_sum_all_2wk),
                                                                  cum_sum_work_2wk=sum(cum_sum_work_2wk),
                                                                  cum_sum_other_2wk=sum(cum_sum_other_2wk))
data_age_outbreak <- data_age_outbreak %>% mutate(prop_work_any=cum_sum_work_2wk/cum_sum_all_2wk,
                                        prop_other_any=cum_sum_other_2wk/cum_sum_all_2wk)
data_covid_outbreak <- data_covid %>% group_by(date, covidriskcdcr, outbreak, outbreak_lab) %>% summarise(cum_sum_all_2wk=sum(cum_sum_all_2wk),
                                                                             cum_sum_work_2wk=sum(cum_sum_work_2wk),
                                                                             cum_sum_other_2wk=sum(cum_sum_other_2wk))
data_covid_outbreak <- data_covid_outbreak %>% mutate(prop_work_any=cum_sum_work_2wk/cum_sum_all_2wk,
                                            prop_other_any=cum_sum_other_2wk/cum_sum_all_2wk)

#generate 2 week average participation in activities by division
data_age_div <- data_age %>% group_by(date, agecat, division) %>% summarise(cum_sum_all_2wk=sum(cum_sum_all_2wk),
                                                                  cum_sum_work_2wk=sum(cum_sum_work_2wk),
                                                                  cum_sum_other_2wk=sum(cum_sum_other_2wk))
data_age_div <- data_age_div %>% mutate(prop_work_any=cum_sum_work_2wk/cum_sum_all_2wk,
                                        prop_other_any=cum_sum_other_2wk/cum_sum_all_2wk)
data_covid_div <- data_covid %>% group_by(date, covidriskcdcr, division) %>% summarise(cum_sum_all_2wk=sum(cum_sum_all_2wk),
                                                                             cum_sum_work_2wk=sum(cum_sum_work_2wk),
                                                                             cum_sum_other_2wk=sum(cum_sum_other_2wk))
data_covid_div <- data_covid_div %>% mutate(prop_work_any=cum_sum_work_2wk/cum_sum_all_2wk,
                                            prop_other_any=cum_sum_other_2wk/cum_sum_all_2wk)

#FIGURE 2
fig1 <- ggplot(data_age_all %>% filter(date>="2020-03-01"), aes(x=date, y=prop_work_any*100, color=agecat)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_age) +
  labs(x="", y="Participation in Labor (%)", color="Age") +
  theme_bw() + theme(panel.grid=element_blank())
fig2 <- ggplot(data_age_all %>% filter(date>="2020-03-01"), aes(x=date, y=prop_other_any*100, color=agecat)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_age) +
  labs(x="", y="Participation in Other Activities (%)", color="Age") +
  theme_bw() + theme(panel.grid=element_blank())
fig3 <- ggplot(data_covid_all %>% filter(date>="2020-03-01"), aes(x=date, y=prop_work_any*100, color=covidriskcdcr)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_covid) +
  labs(x="", y="Participation in Labor (%)", color="Covid-19\nRisk Score") +
  theme_bw() + theme(panel.grid=element_blank())
fig4 <- ggplot(data_covid_all %>% filter(date>="2020-03-01"), aes(x=date, y=prop_other_any*100, color=covidriskcdcr)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_covid) +
  labs(x="", y="Participation in Other Activities (%)", color="Covid-19\nRisk Score") +
  theme_bw() + theme(panel.grid=element_blank())
plot_grid(fig1 + theme(legend.position="none"), fig2 + theme(legend.position="none"), get_legend(fig1), 
          fig3 + theme(legend.position="none"), fig4 + theme(legend.position="none"), get_legend(fig3),
          nrow=2, ncol=3, rel_widths=c(0.45, 0.45, 0.1), align="hv", labels=c("A", "B", "", "C", "D", ""))
ggsave("fig2_final.jpg", dpi=500, height=8, width=10)

#FIGURE S133: OUTBREAK VS. NON-OUTBREAK INSTITUTIONS
fig1 <- ggplot(data_age_outbreak %>% filter(date>="2020-03-01"), aes(x=date, y=prop_work_any*100, color=agecat)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~outbreak_lab) + 
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_age) +
  labs(x="", y="Participation in Labor (%)", color="Age") +
  theme_bw() + theme(panel.grid=element_blank())
fig2 <- ggplot(data_age_outbreak %>% filter(date>="2020-03-01"), aes(x=date, y=prop_other_any*100, color=agecat)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~outbreak_lab) + 
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_age) +
  labs(x="", y="Participation in Other Activities (%)", color="Age") +
  theme_bw() + theme(panel.grid=element_blank())
fig3 <- ggplot(data_covid_outbreak %>% filter(date>="2020-03-01"), aes(x=date, y=prop_work_any*100, color=covidriskcdcr)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~outbreak_lab) + 
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_covid) +
  labs(x="", y="Participation in Labor (%)", color="Covid-19\nRisk Score") +
  theme_bw() + theme(panel.grid=element_blank())
fig4 <- ggplot(data_covid_outbreak %>% filter(date>="2020-03-01"), aes(x=date, y=prop_other_any*100, color=covidriskcdcr)) +
  geom_line(size=0.9) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~outbreak_lab) + 
  scale_y_continuous(limits=c(0, 49), expand=c(0,0)) +
  scale_color_manual(values=colors_covid) +
  labs(x="", y="Participation in Other Activities (%)", color="Covid-19\nRisk Score") +
  theme_bw() + theme(panel.grid=element_blank())
fig_age <- plot_grid(fig1 + theme(legend.position="none"), fig2 + theme(legend.position="none"), nrow=2, align="hv")
fig_risk <- plot_grid(fig3 + theme(legend.position="none"), fig4 + theme(legend.position="none"), nrow=2, align="hv")
plot_grid(fig_age, get_legend(fig1), fig_risk, get_legend(fig3), 
          nrow=2, ncol=2, rel_widths=c(0.8, 0.2), align="hv", labels=c("A", "",  "B", ""))
ggsave("fig_s13.jpg", dpi=500, height=10, width=7)

#FIGURE S8 (activities by age and division)
fig1 <- ggplot(data_age_div %>% filter(date>="2020-03-01"), aes(x=date, y=prop_work_any*100, color=agecat)) +
  geom_line(size=0.8) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~division, ncol=1) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_color_manual(values=colors_age) +
  labs(x="", y="", color="Age") + ggtitle("Labor") +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.y=element_blank())
fig2 <- ggplot(data_age_div %>% filter(date>="2020-03-01"), aes(x=date, y=prop_other_any*100, color=agecat)) +
  geom_line(size=0.8) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~division, ncol=1) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_color_manual(values=colors_age) +
  labs(x="", y="", color="Age") + ggtitle("Other") +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.y=element_blank())
fig <- ggarrange(fig1, fig2, common.legend=T, legend="bottom")
annotate_figure(fig, left=text_grob("Average Participation (%)", rot=90))
ggsave("fig_s8.jpg", dpi=500, height=10, width=7)

#FIGURE S9 (activities by age and division)
fig1 <- ggplot(data_covid_div %>% filter(date>="2020-03-01"), aes(x=date, y=prop_work_any*100, color=covidriskcdcr)) +
  geom_line(size=0.8) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~division, ncol=1) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_color_manual(values=colors_covid) +
  labs(x="", y="", color="Covid-19 Risk Score") + ggtitle("Labor") +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.y=element_blank())
fig2 <- ggplot(data_covid_div %>% filter(date>="2020-03-01"), aes(x=date, y=prop_other_any*100, color=covidriskcdcr)) +
  geom_line(size=0.8) + scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  facet_wrap(~division, ncol=1) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_color_manual(values=colors_covid) +
  labs(x="", y="", color="Covid-19 Risk Score") + ggtitle("Other") +
  theme_bw() + theme(panel.grid=element_blank(), axis.title.y=element_blank())
fig <- ggarrange(fig1, fig2, common.legend=T, legend="bottom")
annotate_figure(fig, left=text_grob("Average Participation (%)", rot=90))
ggsave("fig_s9.jpg", dpi=500, height=10, width=7)
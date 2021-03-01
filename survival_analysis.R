library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(readr)
library(survminer)
library(survival)
library(stdReg)
library(scales)
library(ggpubr)
library(sjPlot)
library(zoo)
source("process_followup.R")

mods <- list()
forms <- list(
            "base" = Surv(test_day, pos_day) ~ labor + housing + name,
            "seclvl" = Surv(test_day, pos_day) ~ labor + housing + seclvl + name,
            "risk" = Surv(test_day, pos_day) ~ labor + housing + risk6 + name,
            "controls" = Surv(test_day, pos_day) ~ labor + housing + seclvl + ageinyears + sex + race + advancedliverdisease + asthma + cancer + copd + cvd + diabetes + hiv + htn + immunocompromised + name,
            "room" = Surv(test_day, pos_day) ~ labor + housing + name + cluster(room),
            "prison" = Surv(test_day, pos_day) ~ labor + housing + name + cluster(name)
)
forms_last <- list(
            "base" = Surv(last_day, pos_day) ~ labor + housing + name,
            "seclvl" = Surv(last_day, pos_day) ~ labor + housing + seclvl + name,
            "risk" = Surv(last_day, pos_day) ~ labor + housing + risk6 + name,
            "controls" = Surv(last_day, pos_day) ~ labor + housing + seclvl + ageinyears + sex + race + advancedliverdisease + asthma + cancer + copd + cvd + diabetes + hiv + htn + immunocompromised + name,
            "room" = Surv(last_day, pos_day) ~ labor + housing + name + cluster(room),
            "prison" = Surv(last_day, pos_day) ~ labor + housing + name + cluster(name)
)

for(d in c(60,90,120,150,"last")){
    print(d)
    dataN <- process_followup(data, d) %>% filter(!(name %in% c("CCC","COR","SQ")))
    print(nrow(dataN))
    if(d == 90) data90 <- dataN
    dataN <- dataN %>% filter(ntest_day > 0)
    dataN[rownames(model.matrix(forms$base, data = dataN)),] %>% select(name, room, residentid) %>% summarise_all(.funs = ~length(unique(.))) %>% print()
    
    mods[[paste0("base_", d)]] <- coxph(forms$base, data = dataN, method = "breslow")
    mods[[paste0("Lbase_", d)]] <- coxph(forms_last$base, data = dataN, method = "breslow")
    
    if(d == 90){
        for(f in names(forms)){
            print(f)
            if(f == "base"){
                dataN_testing <- dataN %>% filter(!(name %in% c("CIW")))
                
                dataN_testing[rownames(model.matrix(forms[[f]], data = dataN_testing)),] %>% select(name, room, residentid) %>% summarise_all(.funs = ~length(unique(.))) %>% print()

                mods[[paste0("testing_", d)]] <- coxph(forms$base, data = dataN_testing, method = "breslow")
                mods[[paste0("Ltesting_", d)]] <- coxph(forms_last$base, data = dataN_testing, method = "breslow")
            } else{
                dataN[rownames(model.matrix(forms[[f]], data = dataN)),] %>% select(name, room, residentid) %>% summarise_all(.funs = ~length(unique(.))) %>% print()
                mods[[paste0(f, "_", d)]] <- coxph(forms[[f]], data = dataN, method = "breslow")
                mods[[paste0("L", f, "_", d)]] <- coxph(forms_last[[f]], data = dataN, method = "breslow")
            }
        }
    }

}

# save coefficients
tab_model(mods, p.style = "stars", show.intercept = FALSE, collapse.ci = TRUE, file = "figs/risk_factors_models.html", dv.labels = names(mods))

#Plot distribution of day of outbreak of first test and first positive test
df <- data90[rownames(model.matrix(forms$base, data = data90)),] %>% filter(ntest_day > 0) %>% mutate(labor = as.factor(labor), housing = as.factor(housing), name = as.factor(name), pos_day = as.integer(pos_day)) %>% select(test_day, pos_day, labor, housing, name)
df <- as.data.frame(df[complete.cases(df), ])

events <- data90 %>% pivot_longer(c("test_day_first","outbreak_day_pos"), 
                                  names_to = "event") %>% select(event, value) %>%
                mutate(event = factor(ifelse(event == "outbreak_day_pos", "First positive test", "First test"), levels = c("First test", "First positive test")))
events <- events[complete.cases(events),]
g8 <- ggplot(events, aes(x = value, fill = event)) + 
            geom_histogram(alpha = 0.8, position = "identity", bins = 16) + 
            theme_classic(base_size = 18) + 
            guides(fill=guide_legend(title="")) + 
            labs(x = "Days since start of outbreak", y = "Persons") + 
            scale_x_continuous(breaks = seq(0,90,30), limits = c(0,90)) +
            scale_fill_manual(values = c("#8d96a3","#edae49"))
ggsave(g8, filename = "figs/FigureS14.pdf", width = 8, height = 4)

# Plot Schoenfeld residuals
fit <- coxph(formula = Surv(test_day, pos_day) ~ labor + housing + name, data = df, method = "breslow")
ftest <- cox.zph(fit)
ggexport(ggcoxzph(ftest, var = c("housing","labor")), filename = "figs/FigureS15.pdf", width = 5, height = 10)

#Plot cumulative risk
fit_labor <- stdCoxph(fit=fit, data = df, X = "labor", t = 0:89)
fit_housing <- stdCoxph(fit=fit, data = df, X = "housing", t = 0:89)

estlabor <- 1 - Reduce(f=rbind, x = summary(fit_labor, t = 0:89)$est.table) %>% as.data.frame()
estlabor <- estlabor %>% mutate(d = sort(rep(0:89,2)), group = rep(c("No room labor","Room labor"), nrow(estlabor)/2), cat = "Labor")

esthousing <- 1 - Reduce(f=rbind, x = summary(fit_housing, t = 0:89)$est.table) %>% as.data.frame()
esthousing <- esthousing %>% mutate(d = sort(rep(0:89,2)), group = rep(c("Cell","Dorm"), nrow(esthousing)/2), cat = "Housing")

test_pos90 <- expand.grid(name = unique(data90$name), outbreak_day = 0:89) %>%
                left_join(data90 %>% group_by(name) %>% 
                          summarise(n = n(), 
                                    outbreak_days = max(outbreak_day_last), 
                                    date_first = date_first[1])) %>% 
                left_join(data90 %>% group_by(name, test_day_first) %>% 
                          summarise(ntested = n()), 
                          by = c("name" = "name", "outbreak_day" = "test_day_first")) %>%
                left_join(data90 %>% group_by(name, outbreak_day_pos) %>% 
                          summarise(npos = n()), 
                          by = c("name" = "name", "outbreak_day" = "outbreak_day_pos")) %>%
                replace(is.na(.), 0) %>%
                arrange(name, outbreak_day) %>%
                group_by(name) %>%
                mutate(cumtested = cumsum(ntested),
                      cumpos = cumsum(npos),
                       ptested = 100*cumtested/n,
                       ppos = 100*cumpos/n
                      ) %>% pivot_longer(starts_with("p"), names_to = "cat", values_to = "value") %>%
                arrange(outbreak_days, name, outbreak_day) %>% 
                group_by(-outbreak_days) %>% 
                mutate(no = group_indices(), 
                       label = paste0("Prison ", no, " (", 
                                      format(date_first, format = "%b %e, %Y"), ", n=", n, ")"))

library(wesanderson)
pal <- wes_palette("FantasticFox1", 9, type = "continuous")
gA <- ggplot(test_pos90 %>% filter(cat == "ptested"), aes(x = outbreak_day, y = value, color = label)) + 
    geom_line(size = 1) + scale_x_continuous(breaks = seq(0,90,30)) + 
    theme_classic(base_size = 18) + ylab("Cumulative tested (%)") + xlab("Days since start of outbreak") + 
    theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        aspect.ratio=1
    ) + scale_color_manual(values = pal)
gAB_leg <- get_legend(gA)
gA <- gA + theme(legend.position = "none")

gB <- ggplot(test_pos90 %>% filter(cat == "ppos"), aes(x = outbreak_day, y = value, color = label)) + 
    geom_line(size = 1) + scale_x_continuous(breaks = seq(0,90,30)) + 
    theme_classic(base_size = 18) + ylab("Cumulative confirmed cases (%)") + xlab("Days since start of outbreak") + 
    theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        aspect.ratio=1,
        legend.position = "none"
    ) + scale_color_manual(values = pal)

library(scales)
colors_cd <- hue_pal()(4)

gC <- ggplot(esthousing, aes(x = d, y = Estimate, ymin = `lower 0.95`, ymax = `upper 0.95`, color = group, fill = group)) + 
    geom_ribbon(alpha = 0.5, color = NA) + geom_line() + ylim(c(0,0.8)) + scale_x_continuous(breaks = seq(0,90,30)) + 
    theme_classic(base_size = 18) + ylab("Cumulative risk") + xlab("Days since start of outbreak") + 
    theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        aspect.ratio=1
    ) + scale_color_manual(values = colors_cd[1:2]) + scale_fill_manual(values = colors_cd[1:2])

gD <- ggplot(estlabor, aes(x = d, y = Estimate, ymin = `lower 0.95`, ymax = `upper 0.95`, color = group, fill = group)) + 
    geom_ribbon(alpha = 0.5, color = NA) + geom_line() + ylim(c(0,0.8)) + scale_x_continuous(breaks = seq(0,90,30)) + 
    theme_classic(base_size = 18) + ylab("Cumulative risk") + xlab("Days since start of outbreak") + 
    theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        aspect.ratio=1
    ) + scale_color_manual(values = colors_cd[3:4]) + scale_fill_manual(values = colors_cd[3:4])

gCD_leg <- ggarrange(get_legend(gC), get_legend(gD), nrow = 2, align = "hv")

g5 <- ggarrange(gA, gB, gAB_leg, gC + theme(legend.position = "none"), gD + theme(legend.position = "none"), gCD_leg, nrow = 2, ncol = 3,
               widths = c(5,5,3.5), labels = c("A","B","","C","D",""))
ggexport(g5, filename = "figs/Figure5.pdf", width = 13.5, height = 10)

# Testing across prisons with outbreaks by room type and labor status
test_posN_housing <- expand.grid(name = unique(dataN$name), housing = c("Cell","Dorm"), outbreak_day = 0:191) %>%
                left_join(dataN %>% group_by(name, housing) %>% summarise(n = n(), outbreak_days = max(outbreak_day_last), date_first = date_first[1])) %>% 
                left_join(dataN %>% group_by(name, housing, test_day_first) %>% summarise(ntested = n()), by = c("name" = "name", "housing" = "housing", "outbreak_day" = "test_day_first")) %>%
                left_join(dataN %>% group_by(name, housing, outbreak_day_pos) %>% summarise(npos = n()), by = c("name" = "name", "housing" = "housing", "outbreak_day" = "outbreak_day_pos")) %>%
                replace(is.na(.), 0) %>%
                arrange(name, housing, outbreak_day) %>%
                group_by(name, housing) %>%
                mutate(cumtested = cumsum(ntested),
                      cumpos = cumsum(npos),
                       ptested = 100*cumtested/n,
                       ppos = 100*cumpos/n
                      ) %>% pivot_longer(starts_with("p"), names_to = "cat", values_to = "value") %>%
                arrange(outbreak_days, name, outbreak_day) %>% 
                group_by(-outbreak_days) %>% 
                mutate(no = group_indices(), label = paste0("Prison ", no, ", ", outbreak_days, " days"), date = date_first + outbreak_day)

test_posN_labor <- expand.grid(name = unique(dataN$name), labor = c("No room labor","Room labor"), outbreak_day = 0:191) %>%
                left_join(dataN %>% group_by(name, labor) %>% summarise(n = n(), outbreak_days = max(outbreak_day_last), date_first = date_first[1])) %>% 
                left_join(dataN %>% group_by(name, labor, test_day_first) %>% summarise(ntested = n()), by = c("name" = "name", "labor" = "labor", "outbreak_day" = "test_day_first")) %>%
                left_join(dataN %>% group_by(name, labor, outbreak_day_pos) %>% summarise(npos = n()), by = c("name" = "name", "labor" = "labor", "outbreak_day" = "outbreak_day_pos")) %>%
                replace(is.na(.), 0) %>%
                arrange(name, labor, outbreak_day) %>%
                group_by(name, labor) %>%
                mutate(cumtested = cumsum(ntested),
                      cumpos = cumsum(npos),
                       ptested = 100*cumtested/n,
                       ppos = 100*cumpos/n
                      ) %>% pivot_longer(starts_with("p"), names_to = "cat", values_to = "value") %>%
                arrange(outbreak_days, name, outbreak_day) %>% 
                group_by(-outbreak_days) %>% 
                mutate(no = group_indices(), label = paste0("Prison ", no, ", ", outbreak_days + 1, " days"), date = date_first + outbreak_day)

test_posN_housing <- test_posN_housing %>% mutate(label = factor(label, levels = rev(unique(test_posN_housing$label))))
test_posN_labor <- test_posN_labor %>% mutate(label = factor(label, levels = rev(unique(test_posN_labor$label))))

library(lemon)
s2A <- ggplot(test_posN_housing %>% filter(cat == "ptested", date < "2020-10-11"), 
              aes(x = date, y = value, color = housing, group = housing)) + 
    geom_line() + facet_rep_wrap(~label, repeat.tick.labels = 'left') +
    theme_classic(base_size = 18) + ylab("Cumulative tested (%)") + xlab("Date") +
    guides(color=guide_legend(title="Room type")) +
    theme(panel.border=element_blank(), axis.line=element_line(),
        strip.background = element_blank(),  aspect.ratio=1,
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_date(labels = date_format("%b")) + labs(title = "Cumulative tested",
              subtitle = "by room type") + scale_color_manual(values = colors_cd[1:2])

s2C <- ggplot(test_posN_housing %>% filter(cat == "ppos", date < "2020-10-11"), 
              aes(x = date, y = value, color = housing, group = housing)) + 
    geom_line() + facet_rep_wrap(~label, repeat.tick.labels = 'left') +
    theme_classic(base_size = 18) + ylab("Cumulative confirmed cases (%)") + xlab("Date") +
    guides(color=guide_legend(title="Room type")) +
    theme(panel.border=element_blank(), axis.line=element_line(),
        strip.background = element_blank(),  aspect.ratio=1,
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_date(labels = date_format("%b")) + labs(title = "Cumulative confirmed cases",
              subtitle = "by room labor") + scale_color_manual(values = colors_cd[1:2])

s2B <- ggplot(test_posN_labor %>% filter(cat == "ptested", date < "2020-10-11"), 
              aes(x = date, y = value, color = labor, group = labor)) + 
    geom_line() + facet_rep_wrap(~label, repeat.tick.labels = 'left') +
    theme_classic(base_size = 18) + ylab("Cumulative tested (%)") + xlab("Date") + 
    guides(color=guide_legend(title="Room labor")) +
    theme(panel.border=element_blank(), axis.line=element_line(),
        strip.background = element_blank(),  aspect.ratio=1,
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_date(labels = date_format("%b")) + labs(title = "Cumulative tested",
              subtitle = "by room type") + scale_color_manual(values = colors_cd[3:4])

s2D <- ggplot(test_posN_labor %>% filter(cat == "ppos", date < "2020-10-11"), 
                aes(x = date, y = value, color = labor, group = labor)) + 
    geom_line() + facet_rep_wrap(~label, repeat.tick.labels = 'left') +
    theme_classic(base_size = 18) + ylab("Cumulative confirmed cases (%)") + xlab("Date") + 
    guides(color=guide_legend(title="Room labor")) +
    theme(panel.border=element_blank(), axis.line=element_line(),
        strip.background = element_blank(),  aspect.ratio=1,
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_x_date(labels = date_format("%b")) + labs(title = "Cumulative confirmed cases",
              subtitle = "by room labor") + scale_color_manual(values = colors_cd[3:4])

s2 <- ggarrange(s2A, s2B, s2C, s2D, nrow = 2, ncol = 2,
               labels = c("A","B","C","D"), heights = c(12,12), 
                align = "hv", font.label = list(size = 20))
ggexport(s2, filename = "figs/FigureS4.pdf", height = 24, width = 25)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(readr)
library(ggpubr)
library(zoo)
library(lubridate)

# Load testing information
inst90 <- c(122079210, 239192042, 241944554, 242468842, 242629354, 242654954, 242655466, 242666730, 243149546)
total_counts <- read_dta("data/Total_counts_2020-12-28.dta") %>%
                    mutate(instid = as.numeric(instid)) %>% 
                    filter(instid %in% inst90) %>%
                    group_by(instid, date) %>%
                    summarise(pos = sum(new_pos, na.rm = TRUE),
                             tests = sum(newtest, na.rm = TRUE),
                             people = sum(numinmates, na.rm = TRUE)) %>%
                    mutate(date = ymd(date))
outbreak_days <- dataN %>% arrange(instid, date_outbreak) %>%
        group_by(instid) %>% 
        summarise(date_outbreak = date_outbreak[1])

outbreak_days <- outbreak_days[rep(1:nrow(outbreak_days), 90),]
outbreak_days$outbreak_day <- sort(rep(0:89, 9))
outbreak_days <- outbreak_days %>% 
                    rename(date_outbreak_first = date_outbreak) %>%
                    mutate(date_outbreak = date_outbreak_first + outbreak_day)
outbreak_days <- outbreak_days %>% 
                    left_join(total_counts, by = c("date_outbreak" = "date", "instid" = "instid"))

total_outbreak_days <- outbreak_days %>%
                        group_by(outbreak_day) %>%
                        summarise(pos = sum(pos),
                                 tests = sum(tests),
                                 people = sum(people)) %>%
                        ungroup() %>%
                        arrange(outbreak_day) %>%
                        mutate(Cases = rollmean(pos, k = 7, align = "center", fill = NA),
                              Tests = rollmean(tests, k = 7, align = "center", fill = NA),
                              Test_Pos = 100*Cases/Tests) %>%
                        pivot_longer(cols = c("Cases","Tests","Test_Pos")) %>%
                        mutate(name = factor(name, levels = c("Test_Pos", "Cases","Tests")))

s1 <- ggplot(total_outbreak_days, aes(x = outbreak_day, y = value, color = name)) +
        geom_line(size = 1.5, show.legend = FALSE)  +
     ylab(NULL) +
     scale_color_brewer(palette="Set2") +
     xlab("Days since start of outbreak") +
     scale_x_continuous(limits = c(0,90), breaks = seq(0,90,10)) +
     theme_classic(base_size = 18) +
     facet_wrap(~name, scale = "free", nrow = 3,
                strip.position = "left", 
                labeller = as_labeller(c(Test_Pos = "Test positivity (%)",
                                           Cases = "Cases", 
                                           Tests = "Tests"))) +
    theme(strip.background = element_blank(),
           strip.placement = "outside",
          strip.text.y = element_text(size = 18),
          strip.text.x = element_text(size = 18))
ggsave(s1, filename = "figs/FigureS1.pdf", width = 10, height = 8)
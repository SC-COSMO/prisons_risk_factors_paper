process_followup <- function(data, d, inst_names = inst_names){
    suffix <- d
    o <- as.numeric(d)
    if(d == "last"){
        suffix <- "_last"
        o <- 0
    }
    data$pos_day <- data[[paste0("pos_day", suffix)]]
    data$ntest_day <- data[[paste0("ntest_day", suffix)]]
    inst_cases <- data %>% group_by(instid) %>% summarise(outbreak_days = max(outbreak_day_last), pos_cases = sum(pos_day))
    inst = inst_cases %>% left_join(inst_names) %>% arrange(outbreak_days)
    instN <- inst %>% filter(pos_cases >= 50, outbreak_days >= o) %>%
                select(instid, name)
    
    dataN <- data %>% right_join(instN) %>% 
                filter(!is.na(housing)) %>% 
                mutate(last_day = ifelse(d == "last", outbreak_day_last, 
                                         pmin(o-1, outbreak_day_last)))
    dataN$test_day <- dataN[[paste0("test_day", suffix)]]
    print(nrow(dataN %>% filter(!is.na(test_day))))
    return(dataN)
}
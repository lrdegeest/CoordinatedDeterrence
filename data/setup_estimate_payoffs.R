source("utilities.r")
library(data.table)
library(dplyr)

# load raw punishment data and bind
s.pm <- read.csv("punishment_pm.csv"); s.pm$treatment = "PARTIAL"   
s.fm <- read.csv("punishment_fm.csv"); s.fm$treatment = "FULL"
sanctions <- rbind(s.pm, s.fm)
names(sanctions) <- tolower(names(sanctions))
# add sanctioning dummy 
sanctions$s_yes_no <- with(sanctions, ifelse(points > 0, 1, 0))

# summarize: one column each for sanctions assigned to insiders and outsiders
# summarize: edge count and edge weight, by treatment, group, type, period
setDT(sanctions)
sanctions_summary <- sanctions[, 
                               list("s_d" = sum(s_yes_no),
                                    "s_d_in" = sum(s_yes_no[type==1]),
                                    "s_d_out" = sum(s_yes_no[type==2]),
                                    "s_a" = sum(points),
                                    "s_a_in" = sum(points[type==1]), 
                                    "s_a_out" = sum(points[type==2])), 
                               by=.(treatment,group,period)]
sanctions_summary$treatment_n <- rep(2:3, each=60)
sanctions_summary$group <- with(sanctions_summary, 10*treatment_n + group)
sanctions_summary <- sanctions_summary[order(group,treatment,period),]
sanctions_summary$treatment_n <- NULL
# separately get active (punishing) subjects in each period, by sanctions on insiders/outsiders
active_subs <- sanctions[, 
                    list("s_d" = sum(s_yes_no),
                      "s_d_in" = sum(s_yes_no[type==1]),
                      "s_d_out" = sum(s_yes_no[type==2])), 
                    by=.(treatment,group,sender, period)]
active_subs$s_n <- with(active_subs, ifelse(s_d > 0, 1, 0))
active_subs$s_n_in <- with(active_subs, ifelse(s_d_in > 0, 1, 0))
active_subs$s_n_out <- with(active_subs, ifelse(s_d_out > 0, 1, 0))
active_subs <- active_subs[,
                           list("s_n" = sum(s_n),
                             "s_n_in" = sum(s_n_in),
                             "s_n_out" = sum(s_n_out)), 
                            by=.(treatment,group,period)]
max(active_subs$s_n) # sanity check, should not exceed 5
# merge with sanctions_summary
sanctions_summary <- cbind(sanctions_summary, active_subs[,4:6, with=F])
# load payoffs data
payoffs <- foreign::read.dta("../data/dp_data_R.dta")
# summarize: final payoffs, insider harvest and outsider poaching by treatment, group, period
setDT(payoffs)
payoffs_summary <- payoffs[, 
                           list("fp" = sum(fp[type=="Insider"]),
                                "h_in" = sum(h[type=="Insider"]),
                                "h_out" = sum(h[type=="Outsider"])),
                           by=.(treatment,group,period)]

# merge the two
full_dat <- cbind(sanctions_summary, payoffs_summary[,4:6, with=F])
# reorder levels of treatment
full_dat$treatment <- factor(tolower(full_dat$treatment), levels=c("partial", "full"))

# save as stata object
foreign::write.dta(full_dat, "estimate_payoffs.dta", convert.factors = "labels")
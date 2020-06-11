use ../data/dp_data_with_sanctions_breakdown, clear

keep if type == 1 
keep if treatment > 1
foreach var in subject{
	egen th = total(h), by(group period)
	gen n = 5
	gen in_avg = th / n
	gen other_in_avg = (th - h) / (n - 1)
}
drop th n
gen OverUnder_in = cond(h-in_avg>0,1,0)
label define OverUnder_in 0 "Under" 1 "Over" 
label values OverUnder_in OverUnder_in
replace OverUnder = 0 if OverUnder == 1
replace OverUnder = 1 if OverUnder == 2
label drop OverUnder
label define OverUnder 0 "Under" 1 "Over" 
label values OverUnder OverUnder
xtset subject period

// Sanctions on outsiders
preserve
collapse (sum) s_a_outsider = s_a_outsider, by(treatment group subject)
sort treatment group -s_a_outsider
egen rank = rank(-s_a_outsider), by(treatment group) track
gen uniquesubject = subject - 100*group
replace uniquesubject = cond(uniquesubject > 8, uniquesubject - 8, uniquesubject)
gen uniquegroup = group - 10*treatment
// average of the top punisher across treatment: compare ranks across treatments
egen total_s_a = sum(s_a_outsider), by(treatment group)
gen contribution = (s_a_outsider / total_s_a) * 100
tabstat contribution if rank == 1, by(treatment) stat(mean)
bysort treatment: tabstat contribution, by(rank) stat(mean) nototal
egen mean_top = mean(contribution) if rank == 1, by(treatment)
// plot each treatment
graph bar (sum) s_a_outsider if treatment == 2, ///
	over(rank, sort(s_a_outsider) descending) over(uniquegroup) ///
	asyvars percent nofill legend(off) ylabel(0(20)100,nogrid) ///
	bar(1, color(black)) bar(2, color(gs4)) bar(3, color(gs8)) bar(4, color(gs12)) bar(5, color(gs14)) ///
	b1title("Group") ytitle("Contribution to total sanctions" "on {bf:outsiders} (percent)") title("Partial Monitoring") ///
	name(pm_out, replace) yline(48, lcolor(red)) nodraw
graph bar (sum) s_a_outsider if treatment == 3, ///
	over(rank, sort(s_a_outsider) descending) over(uniquegroup) ///
	asyvars percent nofill legend(off) ylabel(0(20)100,nogrid) ///
	bar(1, color(black)) bar(2, color(gs4)) bar(3, color(gs8)) bar(4, color(gs12)) bar(5, color(gs14)) ///
	b1title("Group") ytitle("Contribution to total sanctions" "on {bf:outsiders}  (percent)") title("Full Monitoring") ///
	name(fm_out, replace) yline(36, lcolor(red)) nodraw
restore

// Sanctions on insiders
preserve
collapse (sum) s_a_insider = s_a_insider, by(treatment group subject)
sort treatment group -s_a_insider
egen rank = rank(-s_a_insider), by(treatment group) track
gen uniquesubject = subject - 100*group
replace uniquesubject = cond(uniquesubject > 8, uniquesubject - 8, uniquesubject)
gen uniquegroup = group - 10*treatment
// average of the top punisher across treatment: compare ranks across treatments
egen total_s_a = sum(s_a_insider), by(treatment group)
gen contribution = (s_a_insider / total_s_a) * 100
tabstat contribution if rank == 1, by(treatment) stat(mean)
bysort treatment: tabstat contribution, by(rank) stat(mean) nototal
egen mean_top = mean(contribution) if rank == 1, by(treatment)
// plot each treatment
graph bar (sum) s_a_insider if treatment == 2 & uniquegroup !=4, ///
	over(rank) over(uniquegroup) ///
	asyvars percent nofill legend(off) ylabel(0(20)100,nogrid) ///
	bar(1, color(black)) bar(2, color(gs4)) bar(3, color(gs8)) bar(4, color(gs12)) bar(5, color(gs14)) ///
	b1title("Group") ytitle("Contribution to total sanctions" "on {bf:insiders} (percent)") title("Partial Monitoring") ///
	name(pm_in, replace) yline(50, lcolor(red)) nodraw
graph bar (sum) s_a_insider if treatment == 3 & uniquegroup != 2, ///
	over(rank) over(uniquegroup) ///
	asyvars percent nofill legend(off) ylabel(0(20)100,nogrid) ///
	bar(1, color(black)) bar(2, color(gs4)) bar(3, color(gs8)) bar(4, color(gs12)) bar(5, color(gs14)) ///
	b1title("Group") ytitle("Contribution to total sanctions" "on {bf:insiders} (percent)") title("Full Monitoring") ///
	name(fm_in, replace) yline(31, lcolor(red)) nodraw
restore

// combined graph
gr combine pm_out fm_out pm_in fm_in, cols(2) ///
	note("There were no sanctions on insiders in Partial Monitoring Group 4 and Full Monitoring Group 2.")



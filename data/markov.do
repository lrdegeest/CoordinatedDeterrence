use ../data/estimate_payoffs, clear
keep if treatment > 1
keep treatment period group s_n_in s_n_out
// define states: first pass
gen greater = 1 if s_n_in > s_n_out
gen less = 1 if s_n_in < s_n_out
gen equal = 1 if s_n_in == s_n_out

// define states: second pass
// states:
//		s1: i = 0, j = 0
//		s2: i > 0, j = 0
//		s3: i = 0, j > 0
//		s4: i > 0, j > 0
gen s0 = 1 if s_n_in == 0 & s_n_out == 0
gen s1 = 1 if s_n_in > 0 & s_n_out == 0
gen s2 = 1 if s_n_in == 0 & s_n_out > 0
gen s3 = 1 if s_n_in > 0 & s_n_out > 0

// recode missing values
recode * (.=0)

// export
outsheet * using markov.csv, comma nolabel replace

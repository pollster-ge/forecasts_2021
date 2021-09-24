// This replicates nonresponse weighting method described in Valliant & Dever, "Survey Weights - A Step-By-Step Guide," https://www.stata.com/bookstore/survey-weights/
///// Wave 1: F2F omnibus survey, mid-August of 2019
clear
clear matrix
clear mata
set maxvar 10000
use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave1.dta"

/// generate party variable

gen party = p1

///change party variable to missing if there are nonsresponse codes (no party, DK, RA, undecided, etc.)

replace party = p2 if p2 != -7
lab val party p1

/// Recode auxiliary data used for nonresponse weights
/// a. Ownership index

recode l1 (1=1)(nonmissing=0), gen(language)

gen ownership=0

foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d4_`x'== 1
}

recode d2 (4/5=1) (nonmissing=0), gen(high_ed)

recode d1 (1/3=1) (nonmissing=0), gen(empl)

recode d3 (-2/-1=.)(5/6=5), gen(perceived_econ)

recode age (18/34=1)(35/54=2)(55/119=3), gen(agegroup)


/// r_pq variable measures whether a case has a nonresponse in the party variable. 1=nonresponse, 0=some valid answer

gen r_pq = 0

replace r_pq = 1 if (party>0)

/// Only leave significant predictors in the model

/// predict the propensity to nonresponse

logit r_pq  i.stratum i.agegroup i.perceived_econ language

predict predp, pr

// divide predictions into 10 percentiles

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds


list qpreds if qpreds != .

/// stop  here. Next divide pclass by the groups that will be listed after "list qpreds if qpreds!=. "

egen pclass = cut(predp), at(0, 0.2760307, 0.3283181, 0.359888, 0.3787754, 0.4097902, 0.450802, 0.5122617, 0.5261399, 0.5431589, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)

/// use survey weight instead of indwt

egen pavgwtd = wtmean(predp), weight(indwt) by(pclass)
egen RRwtd = wtmean(r_pq), weight(indwt) by(pclass)
gen adj = 1/pavgwtd

/// Code United Opposition to the UNM

replace party = . if party < 0
recode party (12=99)
replace party = 2 if p1_oth == 1
prop party[pw=adj]


/// exoprt estimates in rtf, as it produces nicely formatted table

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w1.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))


/////// Wave 2: F2F (January 2020)
clear
clear matrix
clear mata
set maxvar 10000

use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave2.dta"

gen party = pn2

replace party = pn3 if pn3 != -7

recode party (10=11) // make space for Lelo

replace party = 10 if pn2_oth == 3

replace party = 2 if pn2_oth == 5

lab def pn2 10 "Lelo" 11 "Other", modify

lab val party pn2

recode l1 (1=1)(nonmissing=0), gen(language)

gen ownership=0

foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d4_`x'== 1
}

recode d2 (4/5=1) (nonmissing=0), gen(high_ed)

recode d1 (1/3=1) (nonmissing=0), gen(empl)

recode d3 (-2/-1=.)(5/6=5), gen(perceived_econ)

recode age (18/34=1)(35/54=2)(55/119=3), gen(agegroup)

gen r_pq = 0

replace r_pq = 1 if (party>0)

/// Only leave significant predictors in the model

logit r_pq  i.stratum i.perceived_econ i.sex

predict predp, pr

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds

list qpreds if qpreds != .


egen pclass = cut(predp), at(0, 0.4640343, 0.5258915, 0.5467638, 0.5469552, 0.5470331, 0.5896299, 0.6146154, 0.6477757, 0.689846, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(indwt) by(pclass)
egen RRwtd = wtmean(r_pq), weight(indwt) by(pclass)
gen adj = 1/pavgwtd

/// Code United Opposition to the UNM

replace party = . if party < 0
prop party[pw=adj]


/// exoprt estimates in rtf, as it produces nicely formatted table

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w2.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))

///// Wave 3, RDD (April 2020)

clear
clear matrix
clear mata
set maxvar 10000
use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave3.dta"

gen party = m6
lab val party m6
replace party = 2 if m6_oth == "United opposition"

/// Recode auxiliary data used for nonresponse weights

recode d6 (3=1)(nonmissing=0), gen(ethnic)

gen ownership=0

foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d3_`x'== 1
}

recode d2 (4/5=1) (nonmissing=0), gen(high_ed)

recode d1 (1/3=1) (nonmissing=0), gen(empl)

recode m1 (1 5 6 7 8 9 =1 "Govt") (2 3 12 = 2 "Opposition") (-5 -4 =3 "None/No TV")(-2/-1=5 "DK/RA") (nonmissing=4 "Other"), gen(tv)

recode m5 (1=1) (nonmissing = 0)

gen r_pq = 0

replace r_pq = 1 if (party>0)

/// Only leave significant predictors in the model

logit r_pq i.agegroup i.stratum i.tv

predict predp, pr

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds

list qpreds if qpreds != .

egen pclass = cut(predp), at(0, 0.1263117, 0.18275, 0.2556048, 0.3585089, 0.4110084, 0.4396724, 0.4948882, 0.5044567, 0.5800728, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(weight) by(pclass)
egen RRwtd = wtmean(r_pq), weight(weight) by(pclass)
gen adj = 1/pavgwtd

/// Code United Opposition to the UNM

replace party = . if party < 0
lab val party m6
prop party[pw=adj]


/// exoprt estimates in rtf, as it produces nicely formatted table

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w3.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))

///// Wave 4, RDD (July 2020)

clear
clear matrix
clear mata
set maxvar 10000
use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave4.dta"

gen party = m5
lab val party m5
replace party = 2 if m5_oth == 9

/// Recode auxiliary data used for nonresponse weights

recode d10 (3=1)(nonmissing=0), gen(ethnic)

recode d1 (1/3=1) (nonmissing=0), gen(empl)

recode d2 (4/5=1) (nonmissing=0), gen(high_ed)

recode m4 (1=1)(nonmissing=0)

gen ownership=0

foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d4_`x'== 1
}

recode m1 (1 5 6 7 8 9 =1 "Govt") (2 3 12 = 2 "Opposition") (-5 -4 =3 "None/No TV")(-2/-1=5 "DK/RA") (nonmissing=4 "Other"), gen(tv)

gen r_pq = 0

replace r_pq = 1 if (party>0)

recode age (18/34=1)(35/54=2)(55/119=3), gen(agegroup)

/// Only leave significant predictors in the model

logit r_pq i.sex i.agegroup i.tv m4

predict predp, pr

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds

list qpreds if qpreds != .

egen pclass = cut(predp), at(0, 0.2254123, 0.2770703, 0.3307192, 0.3850066, 0.4946705, 0.5447142, 0.5920052, 0.6117548, 0.6516653, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(weight) by(pclass)
egen RRwtd = wtmean(r_pq), weight(weight) by(pclass)
gen adj = 1/pavgwtd

/// Code United Opposition to the UNM

replace party = . if party < 0
lab val party m5
prop party[pw=adj]


/// exoprt estimates in rtf, as it produces nicely formatted table

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w4.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))

///// Wave 5, RDD (October 2020)

clear
clear matrix
clear mata
set maxvar 10000
use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave5.dta"

gen ownership=0
foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d3_`x'== 1
}

recode d1 (5/6=1) (nonmissing=0), gen(empl)
recode d2 (4/5=1) (nonmissing =0), gen(high_ed)
recode d6 (3=1)(.=.)(else=0), gen(ethnic)
recode c2_1 (4/5=1)(nonmissing=0), gen(gov_trust_covid)
recode age (18/34=1)(35/54=2)(55/119=3), gen(agegroup)
recode d8 (-9/-1=4), gen(relig)
recode c1 (-9/-1=4)

recode m1 (1=1)(5=1)(6/9=1)(2/4=2)(12=2)(-5=3)(-4=4)(-2/-1=3)(.=.)(else=5),gen(tv)
recode m3 (1/4=1)(5=0)(-5/-1=0)(else=0),gen(internet)
recode m4 (1=1)(nonmissing=0)

gen party = m5
replace party = . if party < 0
lab val party m5
replace party =2 if m5_oth==9

gen r_pq = 0
replace r_pq = 1 if (m5>0)


logit r_pq i.agegroup i.tv i.c1 gov_trust_covid

predict predp, pr

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds

list qpreds if qpreds != .


egen pclass = cut(predp), at(0, 0.1341151, 0.1632999, 0.2519882, 0.337293, 0.4225165, 0.4724382, 0.5271749, 0.6172071, 0.6593611, 1)


sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(weight) by(pclass)
egen RRwtd = wtmean(r_pq), weight(weight) by(pclass)
gen adj = 1/pavgwtd

prop party[pw=adj]

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w5.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))
	 
/// Wave 6 (November 2020, post election)

clear
clear matrix
clear mata
set maxvar 10000
use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave6.dta"

gen ownership=0
foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d4_`x'== 1
}

recode d1 (5/6=1) (nonmissing=0), gen(empl)
recode d2 (4/5=1) (nonmissing =0), gen(high_ed)
recode d5 (3=1)(.=.)(else=0), gen(ethnic)
recode d8 (-9/-1=4)

recode age (18/34=1)(35/54=2)(55/119=3), gen(agegroup)
recode m1 (1=1)(5=1)(6/9=1)(2/4=2)(12=2)(-5=3)(-4=4)(-2/-1=3)(.=.)(else=5),gen(tv)
recode d7 (3=1)(.=.)(else=0), gen(relig)
recode el4 (1/2=1) (3/4=2)(nonmissing=3), gen(elections)

gen r_pq = 0
replace r_pq = 1 if (m3>0)

gen party = m3
replace party = . if party < 0
replace party = 2 if m3_oth == 7

lab val party m3

logit r_pq i.agegroup i.tv i.agegroup i.stratum i.sex i.d8 i.relig 

predict predp, pr

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds

list qpreds if qpreds != .

egen pclass = cut(predp), at(0, 0.1076506, 0.151151, 0.2048525, 0.28578, 0.369747, 0.4280462, 0.4747169, 0.5451257, 0.734018, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(weight) by(pclass)
egen RRwtd = wtmean(r_pq), weight(weight) by(pclass)
gen adj = 1/pavgwtd

prop party[pw=adj]

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w6.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))
	 
/// Wave 7 (January 2021)

clear
clear matrix
clear mata
set maxvar 10000
use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave7.dta"

gen ownership=0
foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d3_`x'== 1
}

recode d1 (5/6=1) (nonmissing=0),gen(empl)
recode d2 (4/5=1) (nonmissing = 0), gen(high_ed)
recode d6 (1=1) (nonmissing=0), gen(ethnic)
recode age (18/34=1)(35/54=2)(55/119=3), gen(agegroup)
recode m1 (-9/-1=4)

recode d7 (3=1)(.=.)(else=0), gen(relig)
recode m2 (1=1)(5=1)(6/9=1)(2/4=2)(12=2)(-5=3)(-4=4)(-2/-1=3)(.=.)(else=5),gen(tv)
recode c1_1 (4/5=1)(nonmissing=0), gen(gov_trust_covid)

// recode m3 (1/4=1)(5=0)(-5/-1=0)(else=0),gen(internet)
gen r_pq = 0
replace r_pq = 1 if (m4>0)

logit r_pq i.tv i.m1 gov_trust_covid 

predict predp, pr

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds

list qpreds if qpreds != .

egen pclass = cut(predp), at(0, 0.1079721, 0.1562306, 0.213761, 0.3239923, 0.3878944, 0.4861707, 0.4922273, 0.6760083, 0.8046541, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(weight) by(pclass)
egen RRwtd = wtmean(r_pq), weight(weight) by(pclass)
gen adj = 1/pavgwtd

gen party = m4
replace party = . if party < 0
replace party = 10 if m4_oth ==7
lab val party m4

prop party[pw=adj]

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w7.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))
	 
/// Wave 8 (April 2021)

clear
clear matrix
clear mata
set maxvar 10000
use "https://caucasusbarometer.org/downloads/omnibus/CRRC_Omnibus_Public_Wave8.dta"

recode age (18/34=1) (35/54=2) (55/199=3), gen(agegroup)

gen ownership=0
foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d3_`x'== 1
}

recode d1 (5/6=1) (nonmissing=0), gen(empl)

recode m1 (1 5 6 7 13=1 "Pro-govt") (2 3 4 12 =2 "Opposition") (-2 -1 -5=4 "No one") (-4=5 "No TV") (nonmissing=4 "Other"), gen(tv)

recode d2 (1/4=0) (5/6=1), gen(hied)

recode m3 (8/9=9)

replace m3 = 7 if m3_oth == 6

replace m3 = 2 if m3_oth == 9

recode m3 (13=14)

replace m3 = 13 if m3_oth == 4

lab def m3 13 "For Georgia" 14 "Other", modify

gen r_pq = 0

replace r_pq = 1 if (m3>0)

recode d6 (3=1)(.=.)(else=0), gen(ethnic)

recode eu2_2 (4/5=1) (-9=.)(nonmissing=0), gen(govt_trust)

logit r_pq i.agegroup i.tv govt_trust hied empl

predict predp, pr

pctile qpreds=predp, nq(10) genp(percent)

sort qpreds

list qpreds if qpreds != .
		
egen pclass = cut(predp), at(0, 0.1223831, 0.1561435, 0.2115049, 0.3142792, 0.3637728, 0.4005636, 0.4397748, 0.5207287, 0.6881685, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(weight) by(pclass)
egen RRwtd = wtmean(r_pq), weight(weight) by(pclass)
gen adj = 1/pavgwtd

// table pclass, contents(mean pavg mean pavgwtd mean pmed mean RR mean RRwtd)

gen party = m3
replace party = . if party < 0

// replace party =2 if m3_oth==2
// replace party =2 if m3_oth==8
// lab val party m3
// lab def m3 99 "Other", modify

lab val party m3

prop party[pw=adj]

quietly: estpost tabulate party[iw=adj]

quietly: local tab_label: variable label party

esttab using "estimates/omnibus_w8.rtf", append cells("b(label(Frequency)) pct(label(Proportion)fmt(2)) cumpct(label(Cumulative)fmt(2))")       ///
     varwidth(50) nonumber nomtitle noobs title(`e(depvar)': `tab_label') ///
	 varlabels(`e(labels)', blist(Total))
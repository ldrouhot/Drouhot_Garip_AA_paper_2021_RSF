capture clear
capture log close
macro drop all
set more off 
set linesize 80
version 15


set scheme plottig
grstyle init
grstyle set plain, horizontal
grstyle set color Accent: p#bar p#barline
grstyle set intensity 30: bar
grstyle set symbolsize medium
grstyle numstyle legend_cols 1



// LUCAS DROUHOT & FILIZ GARIP - AUGUST 2020

// "What’s Behind a Racial Category? Uncovering Heterogeneity Among Asian Americans Through A Data-Driven Typology"

*******************************************************

{

// III - DESCRIPTIVE ANALYSES & REGRESSION MODELS AFTER PROBABILISTIC CLASS ASSIGNMENT

cd "your path"

/// Here the dataset is saved, then multinomial assignment occurs in R. Below, we load the saved dataset 

use "Path to data/NAAS16_pooled_for_paper_after_LCA.dta", clear

encode respid, generate(respid2)

merge 1:1 respid2 using "Path to data/probabilistic_class_assignment.dta"

estimates clear
foreach i in 1 2 3 4 5 {
quietly estpost sum less_than_hs college_or_more earns_over_100k gender first_gen second_gen third_gen california other_we eastern midwest southern pacific if probab==`i' 
est store D`i'
}

esttab D* using Descriptives_lca_asian_americans.rtf, cells( "mean(fmt(2))" "sd(fmt(2) par)") mtitle("Class 1" "Class 2" "Class 3" "Class 4" "Class 5")  ///
label nonum collabels(none) replace title(Structural differentiation among Asian Americans) // TABLE 1

label define ethnicity 1 "Bengladeshi" 2 "Cambodian" 3 "Chinese" 4 "Filipino" 5 "Hmong" 6 "Indian" 7 "Japanese" 8 "Korean" 9 "Laotian" 10 "Pakistani" 11 "Vietnamese" 23 "Taiwanese" 
label values rethnic ethnicity


label define latent_class_interpretation 1 "Vulnerable" 2 "Ordinary" 3 "Hyperselected" 4 "Rooted"  5 "Achieving"
label values proba latent_class_interpretation

// Graphing weighted overall shares:

graph pie [pweight=pweight], over(proba) plabel(_all percent, size(*2)) pie(_all, explode) title("") // FIGURE 2

// Graphing ethnic distribution in each class:

label define ethnoracial 1 "Asian American" 2 "Ethnic category" 3 "(Ethnic)-American" 4 "Asian" 5 "American" 6 "Other"
replace q4_101=6 if q4_101==7
rename q4_101 ethno_racial_identity

label values ethno_racial ethnoracial

catplot ethno_racial if ethno_racial<88, over(probab, label(labsize (*0.85))) percent(probab) recast(bar) asyvar stack note("") ytitle("Percent") legend(order(7 6 5 4 3 2 1))

// Note: below the graph uses analytic weights, because catplot does not support sampling weights. The results look a bit odd. Hence in the paper with are sticking with unweighted graphs.

catplot rethnic, over(proba, label(labsize (*0.85))) percent(proba) recast(bar) asyvar stack note("") ytitle("Percent") legend(order(23 11 10 9 8 7 6 5 4 3 2 1)) // FIGURE 3 panel A

catplot probab, over(rethnic, label(alternate labsize (*0.85))) percent(rethnic) recast(bar) asyvar stack note("") b1title("") ytitle("Percent") legend(order(23 11 10 9 8 7 6 5 4 3 2 1)) // FIGURE 3 panel B

// F-test ratios class vs ethnic groups on select outcomes: racial identity / experience of discrimination / linked fate / model minority stereotype


rename q4_3 linked_fate
rename q5_1_j model_stereotype

replace model_st=. if model_st==88 | model_st==99
replace model_st=0 if model_st==2

gen strong_linked_fate=.
replace strong_linked_fate=1 if linked_fate==1 
replace strong_linked_fate=0 if linked_fate!=1 & linked_fate!=.

foreach var in asian_identity unfair linked_fate model_stereotype   { // CALCULATIONS FOR FIGURE 5
sum `var'
anova `var' rethnic 
anova `var' probabilistic_class_assignment 
}

// Graphing subgroup differences:

foreach var in strong_asian unfair strong_linked model_stereotype {
cibar `var' if `var'<10, over1(probab)
graph save `var'.gph, replace
}
grstyle numstyle legend_cols 5
grc1leg strong_asian.gph unfair.gph strong_linked.gph model_stereotype.gph, ycommon // FIGURE 4

/// Third set of analyses: health / voting / panethnic identification. Simple series of logistic regression models by latent class with predicted probabilities. 
/// Asian identification is derived from those choose answer #1 for question on preferred label: Asian-American, (ethnic) American, etc. Question is originally q4_101, renamed ethno_racial_identity above.
/// For each outcome, we estimate one model per latent class with the variables we used in the LC modeling, with experience of unfair treatment our key predictor of interest. 


gen bad_health=0 if health!=.
replace bad_health=1 if health==5

gen panethnic_identity=0 if ethno_racial<88
replace panethnic_identity=1 if ethno_racial==1

gen registered_voter=.
replace registered_voter=1 if registered==1
replace registered_voter=0 if notregistered==1


***** MODEL 1: MODELING THE PROBABILITY OF REPORTING POOR HEALTH ***** GRAPHS IN FIGURE 6 N.B.: To reproduce figure 7, change the group variable accordingly ("rethnic").

set scheme plottig
grstyle init
grstyle set plain, horizontal
grstyle set color Accent: p#bar p#barline
grstyle set intensity 30: bar
grstyle set symbolsize small
grstyle set legend 3, nobox

estimates clear

foreach i in 1 2 3 4 5 {
logit bad_health income college gender unfair_treatment  if proba==`i', robust coeflegend
margins, dydx(unfair_treatment) atmeans post
estimates store m`i'
}

logit bad_health income college gender unfair_treatment, robust coeflegend
margins, dydx(unfair_treatment) atmeans post
estimates store m0

coefplot (m1, label(1: Vulnerable)) (m2, label(2: Ordinary)) (m3, label(3: Hyperselected)) ///
(m4, label(4: Rooted)) (m5, label(5: Achieving)) (m0, label(6: All respondents)), keep(*unfair_treatment) ciopts(recast(. rcap)) byopts(xrescale) ///
coeflabel(unfair_treatment="Has experienced unfair treatment in the past", wrap(25)) title(Probability of reporting poor health,size(medl)) 

graph save discrimination_and_poor_health.gph, replace


***** MODEL 2: MODELING THE PROBABILITY OF BEING A REGISTERED VOTER ***** 

estimates clear

foreach i in 1 2 3 4 5 {
logit registered_voter income college gender unfair_treatment if proba==`i', robust coeflegend
margins, dydx(unfair_treatment) atmeans post
estimates store m`i'
}

logit registered_voter income college gender unfair_treatment, robust coeflegend
margins, dydx(unfair_treatment) atmeans post
estimates store m0

coefplot (m1, label(1: Vulnerable)) (m2, label(2: Ordinary)) (m3, label(3: Hyperselected)) ///
(m4, label(4: Rooted)) (m5, label(5: Achieving)) (m0, label(6: All respondents)), keep(*unfair_treatment) ciopts(recast(. rcap)) byopts(xrescale) ///
coeflabel(unfair_treatment="Has experienced unfair treatment in the past", wrap(25)) title(Probability of being a registered voter,size(medl)) 

graph save discrimination_and_voting_registration.gph, replace

}

***** MODEL 3: MODELING THE PROBABILITY OF CHOOSING A PANETHNIC IDENTITY *****


estimates clear

foreach i in 1 2 3 4 5 {
logit panethnic income college gender unfair_treatment if proba==`i', robust coeflegend
margins, dydx(unfair_treatment) atmeans post
estimates store m`i'
}

logit panethnic income college gender unfair_treatment, robust coeflegend
margins, dydx(unfair_treatment) atmeans post
estimates store m0

coefplot (m1, label(1: Vulnerable)) (m2, label(2: Ordinary)) (m3, label(3: Hyperselected)) ///
(m4, label(4: Rooted)) (m5, label(5: Achieving)) (m0, label(6: All respondents)), keep(*unfair_treatment) ciopts(recast(. rcap)) byopts(xrescale) ///
coeflabel(unfair_treatment="Has experienced unfair treatment in the past", wrap(25)) title(Probability of choosing a panethnic identity,size(medl)) 

graph save discrimination_and_panethnic_identity.gph, replace 



{ // SUPPLEMENTARY ANALYSES

// RECOVERING A LINEAR STORY?

***** Can we recover the pseudo-linear story obtained with the LCA classes in a regression model?

estimates clear

foreach var in strong_asian unfair strong_linked model_stereo {
logit `var' i.probabilistic c.education i.probabilistic#c.education c.income i.probabilistic#c.income i.gender i.imm_gen i.region, robust coeflegend
estimates store logit_model_`var'
}

esttab logit_model_strong_asian logit_model_unfair logit_model_strong_linked logit_model_model_stereo   ///
using "path/logit_models_race_full_interactions.rtf", replace se r2 label mtitles starlevels(* 0.05 ** 0.01 *** 0.001)

// This is the general, pooled model across four outcomes of interest. Pretty much linear for strength of racial identity, not at all for xp of unfair treatment. For model minority stereotype, strong 
// interaction for the effect of income. Somewhat more linear for sense of linked fate. 
// This is probably better than a series of separate models with Paternoster tests for equality of coefficients because it's important to see the effect of dummies for Latent Classes.

// Separate effect size graphs for each outcome:

coefplot logit_model_strong_asian, levels(95) ciopts(recast(. rcap)) title(Strong Asian identity, size(medlarge)) ///
coeflabel(income= "Income" education="Education" 1.gender="Gender (ref: Male)" ///
2.region="Eastern US (ref: California)" ///
3.region="Midwestern US" ///
4.region="Southern US" ///
5.region="Pacific US" ///
6.region="Other Western US" ///
2.imm_gen="2nd generation (ref: 1st generation)" ///
3.imm_gen="3rd generation (ref: 1st generation)" ///
2.probabilistic_class_assignment#c.education="Ordinary * Education" ///
3.probabilistic_class_assignment#c.education="Hyperselected * Education" ///
4.probabilistic_class_assignment#c.education="Rooted * Education" ///
5.probabilistic_class_assignment#c.education="Achieving * Education" ///
2.probabilistic_class_assignment#c.income="Ordinary * Income" ///
3.probabilistic_class_assignment#c.income="Hyperselected * Income" ///
4.probabilistic_class_assignment#c.income="Rooted * Income" ///
5.probabilistic_class_assignment#c.income="Achieving * Income") ///
headings(income="{bf: Income: main effect and interactions (ref: Vulnerable)}" education="{bf: Education: main effect and interactions (ref: Vulnerable)}" ///
2.probabilistic_class_assignment="{bf: Latent class membership: main effect (ref: Vulnerable)}" ///
1.gender="{bf: Other controls: Gender, generation, U.S. region}", labcolor(blue)) ///
drop(_cons) xline(0) note("Number of observations: 4740" "Pseudo R-squared: 0.0343")  
graph save logit_model_strong_asian.gph, replace

coefplot logit_model_unfair, levels(95) ciopts(recast(. rcap)) title(Unfair treatment, size(medlarge)) ///
coeflabel(income= "Income" education="Education" 1.gender="Gender (ref: Male)" ///
2.region="Eastern US (ref: California)" ///
3.region="Midwestern US" ///
4.region="Southern US" ///
5.region="Pacific US" ///
6.region="Other Western US" ///
2.imm_gen="2nd generation (ref: 1st generation)" ///
3.imm_gen="3rd generation (ref: 1st generation)" ///
2.probabilistic_class_assignment#c.education="Ordinary * Education" ///
3.probabilistic_class_assignment#c.education="Hyperselected * Education" ///
4.probabilistic_class_assignment#c.education="Rooted * Education" ///
5.probabilistic_class_assignment#c.education="Achieving * Education" ///
2.probabilistic_class_assignment#c.income="Ordinary * Income" ///
3.probabilistic_class_assignment#c.income="Hyperselected * Income" ///
4.probabilistic_class_assignment#c.income="Rooted * Income" ///
5.probabilistic_class_assignment#c.income="Achieving * Income") ///
headings(income="{bf: Income: main effect and interactions (ref: Vulnerable)}" education="{bf: Education: main effect and interactions (ref: Vulnerable)}" ///
2.probabilistic_class_assignment="{bf: Latent class membership: main effect (ref: Vulnerable)}" ///
1.gender="{bf: Other controls: Gender, generation, U.S. region}", labcolor(blue)) ///
drop(_cons) xline(0) note("Number of observations: 3610" "Pseudo R-squared: 0.0466") 
graph save logit_model_unfair.gph, replace

coefplot logit_model_strong_linked, levels(95) ciopts(recast(. rcap)) title(Strong racially linked fate, size(medlarge)) ///
coeflabel(income= "Income" education="Education" 1.gender="Gender (ref: Male)" ///
2.region="Eastern US (ref: California)" ///
3.region="Midwestern US" ///
4.region="Southern US" ///
5.region="Pacific US" ///
6.region="Other Western US" ///
2.imm_gen="2nd generation (ref: 1st generation)" ///
3.imm_gen="3rd generation (ref: 1st generation)" ///
2.probabilistic_class_assignment#c.education="Ordinary * Education" ///
3.probabilistic_class_assignment#c.education="Hyperselected * Education" ///
4.probabilistic_class_assignment#c.education="Rooted * Education" ///
5.probabilistic_class_assignment#c.education="Achieving * Education" ///
2.probabilistic_class_assignment#c.income="Ordinary * Income" ///
3.probabilistic_class_assignment#c.income="Hyperselected * Income" ///
4.probabilistic_class_assignment#c.income="Rooted * Income" ///
5.probabilistic_class_assignment#c.income="Achieving * Income") ///
headings(income="{bf: Income: main effect and interactions (ref: Vulnerable)}" education="{bf: Education: main effect and interactions (ref: Vulnerable)}" ///
2.probabilistic_class_assignment="{bf: Latent class membership: main effect (ref: Vulnerable)}" ///
1.gender="{bf: Other controls: Gender, generation, U.S. region}", labcolor(blue)) ///
drop(_cons) xline(0) note("Number of observations: 3610" "Pseudo R-squared: 0.0224") 
graph save logit_model_strong_linked.gph, replace

coefplot logit_model_model_stereo, levels(95) ciopts(recast(. rcap)) title(Model minority stereotype, size(medlarge)) ///
coeflabel(income= "Income" education="Education" 1.gender="Gender (ref: Male)" ///
2.region="Eastern US (ref: California)" ///
3.region="Midwestern US" ///
4.region="Southern US" ///
5.region="Pacific US" ///
6.region="Other Western US" ///
2.imm_gen="2nd generation (ref: 1st generation)" ///
3.imm_gen="3rd generation (ref: 1st generation)" ///
2.probabilistic_class_assignment#c.education="Ordinary * Education" ///
3.probabilistic_class_assignment#c.education="Hyperselected * Education" ///
4.probabilistic_class_assignment#c.education="Rooted * Education" ///
5.probabilistic_class_assignment#c.education="Achieving * Education" ///
2.probabilistic_class_assignment#c.income="Ordinary * Income" ///
3.probabilistic_class_assignment#c.income="Hyperselected * Income" ///
4.probabilistic_class_assignment#c.income="Rooted * Income" ///
5.probabilistic_class_assignment#c.income="Achieving * Income") ///
headings(income="{bf: Income: main effect and interactions (ref: Vulnerable)}" education="{bf: Education: main effect and interactions (ref: Vulnerable)}" ///
2.probabilistic_class_assignment="{bf: Latent class membership: main effect (ref: Vulnerable)}" ///
1.gender="{bf: Other controls: Gender, generation, U.S. region}", labcolor(blue)) ///
drop(_cons) xline(0) note("Number of observations: 3322" "Pseudo R-squared: 0.1711") 
graph save logit_model_stereo.gph, replace

grc1leg logit_model_strong_asian.gph logit_model_unfair.gph logit_model_strong_linked.gph logit_model_stereo.gph // FIGURE 8

// Better yet: marginal effect graphs with interactions...We include both since we need to see the significant dummies for differences across latent classes from the graph above.

// Re-labelling income and education for purposes of clarity in the graph.

label define new_income 1 "0-20k" 2 "20-50k" 3 "50-75k" 4 "75-100k" 5 "100-125k" 6 "125-250k" 7 "250k+"
label values income new_income

label define new_educ 1 "No degree" 2 "Less than HS" 3 "HS" 4 "Some college" 5 "College" 6 "GS"
label values education new_educ

estimates clear

foreach var in strong_asian unfair strong_linked model_stereo {
logit `var' i.probabilistic c.education i.probabilistic#c.education c.income i.probabilistic#c.income i.gender i.imm_gen i.region, robust coeflegend  // FIGURE 9
margins,  at(education=(1(1)6)) atmeans over(probabilistic_class_assignment) 
qui marginsplot, scheme(vg_s1c)  ymtick(##10) xlabel(,labs(small)) ylabel(,labs(small)) xtitle("Education", size(medsmall)) ytitle("") title("",size(zero)) ///
legend(region(lwidth(none)) bmargin(vsmall) size(vsmall) rows(1) all)   ///
recast(line) ciopts(color(gs14)) ///
plot1opts(lcolor(blue) msymbol("circle")) plot2opts(lcolor(midgreen) ///
msymbol(triangle)) plot3opts(lcolor(red) msymbol(diamond)) plot4opts(lcolor(black) msymbol(smsquare_hollow)) ///
plot5opts(lcolor(purple) msymbol(circle_hollow)) graphregion(color(white)) bgcolor(white)
graph save margins_educ_`var'.gph, replace

margins,  at(income=(1(1)7)) atmeans over(probabilistic_class_assignment) // FIGURE 10
qui marginsplot, scheme(vg_s1c)  ymtick(##10) xlabel(,labs(small)) ylabel(,labs(small)) xtitle("Income", size(medsmall)) ytitle("") title("",size(zero)) ///
legend(region(lwidth(none)) bmargin(vsmall) size(vsmall) rows(1) all)   ///
recast(line) ciopts(color(gs14)) ///
plot1opts(lcolor(blue) msymbol("circle")) plot2opts(lcolor(midgreen) ///
msymbol(triangle)) plot3opts(lcolor(red) msymbol(diamond)) plot4opts(lcolor(black) msymbol(smsquare_hollow)) ///
plot5opts(lcolor(purple) msymbol(circle_hollow)) graphregion(color(white)) bgcolor(white)
graph save margins_income_`var'.gph, replace
}

grc1leg margins_educ_strong_asian.gph margins_educ_unfair.gph margins_educ_strong_linked.gph margins_educ_model_stereo.gph, ycommon

grc1leg margins_income_strong_asian.gph margins_income_unfair.gph margins_income_strong_linked.gph margins_income_model_stereo.gph








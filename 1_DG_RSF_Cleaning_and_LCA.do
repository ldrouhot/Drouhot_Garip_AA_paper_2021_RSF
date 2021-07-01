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
grstyle set symbolsize small
grstyle numstyle legend_cols 1


// LUCAS G. DROUHOT & FILIZ GARIP

// "What’s Behind a Racial Category? Uncovering Heterogeneity Among Asian Americans Through A Data-Driven Typology"

// August 2020

// NB: All data analyses including the post-election 2016 data were carried out using the pre-final version of the dataset before it was formally approved and published at ISPSR.

******************************************************************************************************************************************************************************************************

// I - CLEANING AND LATENT CLASS MODELING

cd "your directory path"

// Loading data from the NAAS:

use "path to data /Data/NAAS16-pre-election-ICPSRpending.dta", clear
append using "path to data/naas16post-ICPSR-prep.dta", force generate(post_election)

{ // Cleaning & recoding

keep if race==1 // We only keep individuals self-identifying as Asian-Americans. The paper focuses on heterogeneity within a self-identified category, hence attrition due to identificational change is ignorable.

egen state=group(rstate) // this creates a categorical variable for state of residence. 

rename region californian_region

gen region=.

label define state 1 "California" 2 "Eastern US"  3 "Midwestern US" 4 "Southern US" 5 "Pacific US" 6 "Other western US"

replace region=1 if state==5
replace region=2 if state==7 | state==8 | state==9 | state==22 | state==21 | state==20 | state==31 | state==32 | state==35 | state==39 | state==40 | state==47 | state==31
replace region=3 if state==13 | state==15 | state==16 | state==17 | state==23 | state==24 | state==36 | state==42 | state==49 | state==50 | state==30  | state==14 | state==29 
replace region=4 if state==1 | state==3 | state==10  | state==18 | state==19 | state==21 | state==25 | state==26 | state==43 | state==41 | state==46 | state==2 | state==11 | state==28 | state==37 | state==44 
replace region=5 if state==1 | state==12 
replace region=6 if region==.

label value region state

rename s7 gender
label var gender "Prop. female"

rename s8 education

rename s9 born_in_USA

rename q8_15 income

replace educ3=0 if q10_2b==1 | q10_2b==2
replace educ3=1 if q10_2b==3
replace educ3=2 if q10_2b==5 | q10_2b==6

replace income=8 if income==88 | income==99
replace q10_15=8 if q10_15==88 | q10_15==99

replace income=1 if  q10_15==1 & post_election==1
replace income=2 if  q10_15==2 & post_election==1
replace income=3 if  q10_15==3 & post_election==1
replace income=4 if  q10_15==4 & post_election==1
replace income=5 if  q10_15==5 & post_election==1
replace income=6 if  q10_15==6 & post_election==1
replace income=7 if  q10_15==7 & post_election==1
replace income=8 if  q10_15==8 & post_election==1

gen income_missing=0
replace income_missing=1 if income==8 
replace income_missing=1 if q10_15==8 & post_election==1


gen earns_over_100k=0
replace earns_over_100k=1 if income==5 | income==6 | income==7 |  q10_15==5 |  q10_15==6 |  q10_15==7
label var earns_over_100k "Proportion earning more than 100k"

drop if income==8

** drop if wave2part==1 // It is not entirely clear from the documentation what this variable capture; it would suggest respondent who already took the survey, but all respondents' ID are unique. 
                        // All analyses were repeated dropping these observations and the results are indistinguishible.

gen imm_gen=.

replace imm_gen=1 if forborn==1
replace imm_gen=2 if forborn==0 & q1_2==2
replace imm_gen=3 if forborn==0 & q1_2==1

gen first_gen=0
replace first_gen=1 if imm_gen==1
label var first_gen "Prop. 1st generation"
gen second_gen=0
replace second_gen=1 if imm_gen==2
label var second_gen "Prop. 2nd generation"
gen third_gen=0
replace third_gen=1 if imm_gen==3
label var third_gen "Prop. 3rd generation +"

gen cambodian=0
replace cambodian=1 if rethnic==2
label var cambodian "Prop. Cambodian"
gen chinese=0
replace chinese=1 if rethnic==3
label var chinese "Prop. Chinese"
gen filipino=0
replace filipino=1 if rethnic==4
label var filipino "Prop. Filipino"
gen hmong=0
replace hmong=1 if rethnic==5
label var hmong "Prop. Hmong"
gen indian=0
replace indian=1 if rethnic==6
label var indian "Prop. Indian"
gen japanese=0
replace japanese=1 if rethnic==7
label var japanese "Prop. Japanese"
gen korean=0
replace korean=1 if rethnic==8
label var korean "Prop. Korean"
gen laotian=0
replace laotian=1 if rethnic==9
label var laotian "Prop. Laotian"
gen vietnamese=0
replace vietnamese=1 if rethnic==11
label var vietnamese "Prop. Vietnamese"
gen taiwanese=0
replace taiwanese=1 if rethnic==23
label var taiwanese "Prop. Taiwanese"
gen pakistani=0
replace pakistani=1 if rethnic==10
label var pakistani "Prop. Pakistani"
gen bangladeshi=0
replace bangladeshi=1 if rethnic==1
label var bangladeshi "Prop. Bangladeshi"

gen less_than_hs=0
replace less_than_hs=1 if educ3==0
label var less_than_hs "Proportion with less than a high school diploma"
gen college_or_more=0
replace college_or_more=1 if educ3==2
label var college_or_more "Proportion with a college degree or more"

replace gender=gender-1
drop if gender>1

label define gender 0 "0. Male" 1 "1. Female"
label values gender gender

gen health=.
replace health=1 if q8_8==1 | q10_6==1
replace health=2 if q8_8==2 | q10_6==2
replace health=3 if q8_8==3 | q10_6==3
replace health=4 if q8_8==4 | q10_6==4
replace health=5 if q8_8==5 | q10_6==5

label define health 1 "Excellent" 2 "Very good" 3 "Good" 4 "Fair" 5 "Poor"
label values health health
label var health "Self reported health"

gen intermarriage=.
replace intermarriage=1 if q8_701!=1 & q8_701!=. & q8_701<88
replace intermarriage=1 if q10_501!=1 & q10_501!=. & q8_701<88
replace intermarriage=0 if q10_501==1 | q8_701==1
label var intermarriage "Prop. in interracial relationship"

rename q4_3 linked_fate

gen asian_linked_fate=.
replace asian_linked_fate=1 if linked_fate==1
replace asian_linked_fate=0 if linked_fate==2

// For racial identity, the frequencies are widely different across survey waves. Second, for the post-election surveys, there are a lot of unlabelled value 5. Likely, they belong
// in value 4 "Extremely important" judging by the frequencies above but can't be sure. 

// N.B. "don't knows" are coded as "not strong".

gen strong_asian_identity=.
replace strong_asian_identity=0 if q2_2a==1 | q2_2a==2 | q2_2a==88 | q4_2a==1 | q4_2a==2 | q4_2a==88
replace strong_asian_identity=1 if q2_2a==3 | q2_2a==4 | q4_2a==3 | q4_2a==4
label var strong_asian_identity "Prop. reporting being Asian is very or extremely important"

gen asian_identity=.
replace asian_identity=1 if q2_2a==1 | q2_4a==1
replace asian_identity=2 if q2_2a==2 | q2_4a==2
replace asian_identity=3 if q2_2a==3 | q2_4a==3
replace asian_identity=4 if q2_2a==4 | q2_4a==4 | q2_2a==5

label define identity 1 "Not at all Important" 2 "Somewhat Important" 3 "Very Important" 4 "Extremely Important"
label values asian_identity identity

rename q2_1a successful
gen emphasizes_professional_success=.
replace emphasizes=0 if successful==1 | successful==2
replace emphasizes=1 if successful==3 | successful==4
label var emphasizes "Prop. strongly valuing having high-paying career"

gen racial_inequality=.
replace racial_inequality=1 if q6_1a==9 | q6_1b==9 | q6_1c==9
replace racial_inequality=0 if q6_1a!=9 & q6_1a<88 & q6_1a!=.
label var racial_inequality "Prop. mentioning racial inequality as key problem in US society"

gen unfair_treatment=.
replace unfair_treatment=0 if q5_2_a==2 | q5_2_b==2 | q5_2_c==2 | q5_2_d==2 | q5_2_e==2 | q5_2_a==88 | q5_2_b==88 | q5_2_c==88 | q5_2_d==88 | q5_2_e==88
replace unfair_treatment=1 if q5_2_a==1 | q5_2_b==1 | q5_2_c==1 | q5_2_d==1 | q5_2_e==1
label var unfair "Prop. who report having received unfair treatment before"

gen california=0 if region!=.
gen eastern_us=0 if region!=.
gen midwestern_us=0 if region!=.
gen southern_us=0 if region!=.
gen pacific_us=0 if region!=.
gen other_western_us=0 if region!=.

replace california=1 if region==1
label var california "Prop. in California"
replace eastern_us=1 if region==2
label var eastern_us "Prop. in Eastern US"
replace midwestern_us=1 if region==3
label var midwestern "Prop. in Midwestern US"
replace southern_us=1 if region==4
label var southern "Prop. in Southern US"
replace pacific_us=1 if region==5
label var pacific "Prop. in Pacific US"
replace other_western_us=1 if region==6
label var other_wester "Prop. Western US other than California"

}

{  // Latent class modelling

*** We find the 5-class solution is best in terms of interpretability and parsimony, all while preserving within-class sample size for further analyses.
*** Additionally, some of the higher number classes - 6 and above - have convergence problems. In some LCA runs, the results converge on unefficient solutions with one group
*** dominating all others in size. Interested users can change the number of classes and see how outputs given by "lcgof" indicate varying levels of fit. 

	   
gsem (educ3 <-,  ologit) /// N.B. Educ3 has more missing variables than "education". Results with "education" are substantively identical in terms of latent classes. 
(income <-, ologit) ///
       (gender   <-, logit) ///
       (region  <-, mlogit) ///
       (imm_gen  <-, ologit), ///
       lclass(C 5) startvalues(randomid, draws(15) seed(2489)) iter(30) nonrtolerance 
	   
	   estat lcgof
	   
predict cpost*, classposteriorpr

egen max = rowmax(cpost*)

generate predclass = 1 if cpost1==max
replace predclass = 2 if cpost2==max
replace predclass = 3 if cpost3==max
replace predclass = 4 if cpost4==max
replace predclass = 5 if cpost5==max

tab predclass

/* After that step, the dataset is manually saved under the name "NAAS16_pooled_for_paper_after_LCA.dta". We save manually to avoid accidental replacement.




}

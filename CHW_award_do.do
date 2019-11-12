***
** How workers respond to social rewards: Evidence from community health workers in Uganda
**Reajul Chowdhury, Kevin McKague, Heather Krause

clear all
cd "P:\BRAC CHP Program Data"
use "merged_CHP_performance_branch_sales.dta", clear

******************************************************************************************
****************** Variable Generation ***************************************************
 /// Generate branch_size
drop branch_n_id
egen branch_n_id = group(branch_uuid)
drop if branch_n_id==.
 
bysort branch_n_id: egen brnch_size_1= count(chw_uuid) if month==12 //this is a balanced pnael -- each month having 4103 observations ..so month = does not mater..we just want to make sure that it does for each month
bysort branch_n_id: egen brnch_size_2=max(brnch_size_1)
rename brnch_size_2 branch_size
drop brnch_size_1


// If there is a winner CHW in the branch how it affects the performance of the other CHWs in the same branch excluding the the winners from the sample
//First I need to generate a variable for each CHW in each month if there is a winner from the same branch in that particular year
gen a_winner_sm_br_inyear = 1 if any_award_15==1 & month < 13
recode a_winner_sm_br_inyear . = 1 if any_award_16==1 & month >= 13 & month <25
recode a_winner_sm_br_inyear . = 1 if any_award_17==1 & month >24 
recode a_winner_sm_br_inyear . = 0

//If they are neighbors to a winning branch in that particular Year only ... 
//First I need to generate a variable for each CHW in each month if there is a winner within the nearest 3 branches in that particular year
gen a_winner_nr_3_br_inyear = 1 if within_3_treat_2016==1 & month < 13
recode a_winner_nr_3_br_inyear . = 1 if within_3_treat_2017==1 & month >= 13 & month <25
recode a_winner_nr_3_br_inyear . = 1 if within_3_treat_2018==1 & month >24 
recode a_winner_nr_3_br_inyear . = 0

// Number of months the CHWs were active
bysort chw_uuid: egen total_active_months= total(active_chws_in_period)
gen total_active_months_prcnt = (total_active_months/21)
graph hbar (mean) total_active_months_prcnt, over(award_rcvd_never_brnch) vertical nofill ytitle("Num. of Months CHWs Were Active (out of 21 Months)") title("Performance of Award Winners vs Non-winners CHWs")  

/////Event Study -- only focusing on those for which we have at least 1 year prior the treatment 
gen event_month_for16 =13 // The awrd was given in Dec, 2016... Assume that it akes 1 month to spread the message
gen t_for16_month=(month-event_month_for16)
twoway (scatter on_time_follow_up_percent t_for16_month) (lfit on_time_follow_up_percent t_for16_month) if within_3_treat_2017==1 & within_3_treat_2016==0 & on_time_follow_up_percent<3
gen event_month_for17 = 25 // The awrd was given in Dec, 2017... Assume that it takes 1 month to spread the message
gen t_for17_month=(month-event_month_for17)
twoway (scatter on_time_follow_up_percent t_for17_month) (lfit on_time_follow_up_percent t_for17_month) if within_3_treat_2018==1 & within_3_treat_2016==0 & within_3_treat_2017==0 & on_time_follow_up_percent<3





********************************************************************************************
**//////////////////////////////////////////////////////////////////////////////////////////

*** FACTOR ANALYSIS: PCA
////////////////////////
sum CHP_Profit_aggr families_registered pregnancies_registered total_healthy_pnc_visits on_time_pnc_visits /// 
 assess_u1 assess_u5 treatments_u1 treatments_u5 percent_mrdt on_time_follow_up_percent families_registered_all_time ///
 malaria_all_ages family_surveys total_active_months total_active_months_prcnt

foreach var of varlist families_registered pregnancies_registered total_healthy_pnc_visits on_time_pnc_visits ///
 assess_u1 assess_u5 treatments_u1 treatments_u5 on_time_follow_ups malaria_all_ages ///
 family_surveys total_active_months {
	gen ln_`var'=ln(`var'+0.0001)
	}
	
sum ln_families_registered - ln_total_active_months

* Narrowing down variables to strongest predictors
pwcorr ln_families_registered - ln_total_active_months, star(.05)
// Decided to lg_CHP_Profit_aggr as it is correlated with only 1 variable

* Running factor analysis with selected variables
factor ln_families_registered - ln_total_active_months, pcf
	

*rotate, varimax

predict factor
sum factor
estat kmo // Get Kaiser-Meyer-Olkin measure of sampling adequacy
pwcorr factor ln_families_registered - ln_total_active_months, star(.05)

histogram factor, bin(20) percent fcolor(gray) lcolor(gs5) lwidth(vvvthin) kdensity kdenopts(lcolor(black) lwidth(medthick)) ytitle(Percentage of observations) ytitle(, size(medsmall)) ylabel(, labsize(small) ticks tposition(inside)) xtitle(Performance Index) xtitle(, size(medsmall)) xlabel(-2(0.5)1.5, labsize(small)) scheme(s2mono) name(hist, replace)

label define nvr_win_br 0"Branch with Award Winner CHW" 1"Branch with No Award Winner CHW"
label values award_rcvd_never_brnch nvr_win_br

graph box factor, over(award_rcvd_never_brnch) box(1, fcolor(gray) lcolor(black)) marker(1, mcolor(black)) ytitle(Performance index) ytitle(, size(medsmall)) scheme(s2mono) name(boxplot, replace)
rename factor performance_index


// Performance bracket 
ssc install egenmore
egen quant_performance_index=xtile(performance_index), n(10) by(month)
tab quant_performance_index, gen (perfor_bracket_)
gen perfor_bracket_last80=1 if perfor_bracket_1 ==1 | perfor_bracket_2 ==1 |  perfor_bracket_3 ==1 |  perfor_bracket_4 ==1 |  perfor_bracket_5 ==1 |  perfor_bracket_6 ==1 |  perfor_bracket_7 ==1 |  perfor_bracket_8 ==1 
recode perfor_bracket_last80 .=0

gen perfor_bracket_top20=1 if perfor_bracket_9 ==1 | perfor_bracket_10 ==1  
recode perfor_bracket_top20 .=0

gen perfor_bracket_top10=1 if perfor_bracket_10 ==1  
recode perfor_bracket_top10 .=0

gen perfor_bracket_last50 =1 if perfor_bracket_1 ==1 | perfor_bracket_2 ==1 |  perfor_bracket_3 ==1 |  perfor_bracket_4 ==1 |  perfor_bracket_5 ==1
recode perfor_bracket_last50 .=0

gen perfor_bracket_last20 =1 if perfor_bracket_1 ==1 | perfor_bracket_2 ==1 
recode perfor_bracket_last20 .=0

gen perfor_bracket_60_80 =1 if perfor_bracket_6 ==1 | perfor_bracket_7 ==1 | perfor_bracket_8 ==1 
recode perfor_bracket_60_80 .=0

gen perfor_bracket_40_60 =1 if perfor_bracket_4 ==1 | perfor_bracket_5 ==1 | perfor_bracket_6 ==1 
recode perfor_bracket_40_60 .=0

gen perfor_bracket_50_70 =1 if perfor_bracket_5 ==1 | perfor_bracket_6 ==1 | perfor_bracket_7 ==1 
recode perfor_bracket_50_70 .=0

gen perfor_bracket_top50=1 if perfor_bracket_6 ==1 | perfor_bracket_7 ==1 | perfor_bracket_8 ==1 | perfor_bracket_9 ==1 | perfor_bracket_10 ==1 
recode perfor_bracket_top50 .=0
// lag outcome variable
sort chw_uuid month
by chw_uuid: gen lag_performance_index = performance_index[_n-1]

*****Reporducing tables with factor as outcome
*******************************************************************************
*********
*********Table 2
//reg performance_index a_winner_sm_br_inyear branch_size i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
//outreg using "factor_table_1", se summstat(F \ r2) starlevels(10 5 1) replace  //not reported
reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size i.month if award_rcvd_never_CHW==1, cluster(branch_uuid) //Reported in the paper 
outreg using "factor_table_1", se summstat(F \ r2) starlevels(10 5 1) replace //Reported
//reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size i.month a_winner_sm_br_inyear##i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
//outreg using "factor_table_1", se summstat(F \ r2) starlevels(10 5 1) merge addtable //not Reported

*******************************************************************************
*********Table 3
reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size perfor_bracket_last80 a_winner_sm_br_inyear##perfor_bracket_last80 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "factor_table_2", se summstat(F \ r2) starlevels(10 5 1) replace
reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size perfor_bracket_last50 a_winner_sm_br_inyear##perfor_bracket_last50 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "factor_table_2", se summstat(F \ r2) starlevels(10 5 1) merge addtable
reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size perfor_bracket_last20 a_winner_sm_br_inyear##perfor_bracket_last20 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "factor_table_2", se summstat(F \ r2) starlevels(10 5 1) merge addtable
reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size perfor_bracket_top20 a_winner_sm_br_inyear##perfor_bracket_top20 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "factor_table_2", se summstat(F \ r2) starlevels(10 5 1) merge addtable
reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size perfor_bracket_top10 a_winner_sm_br_inyear##perfor_bracket_top10 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "factor_table_2", se summstat(F \ r2) starlevels(10 5 1) merge addtable

reg performance_index lag_performance_index a_winner_sm_br_inyear branch_size perfor_bracket_top50 a_winner_sm_br_inyear##perfor_bracket_top50 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "factor_table_2", se summstat(F \ r2) starlevels(10 5 1) merge addtable
*******************************************************************************
*********Table 4
//reg performance_index  a_winner_nr_3_br_inyear branch_size i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
//outreg using "factor_table_3", se summstat(F \ r2) starlevels(10 5 1) replace
reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_3", se summstat(F \ r2) starlevels(10 5 1) replace //reported
//reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size i.month a_winner_nr_3_br_inyear##i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
//outreg using "factor_table_3", se summstat(F \ r2) starlevels(10 5 1) merge addtable

*******************************************************************************
*********Table 5
reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size perfor_bracket_last80 a_winner_nr_3_br_inyear##perfor_bracket_last80 i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) replace
reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size perfor_bracket_last50 a_winner_nr_3_br_inyear##perfor_bracket_last50 i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) merge addtable
reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size perfor_bracket_last20 a_winner_nr_3_br_inyear##perfor_bracket_last20 i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) merge addtable
reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size perfor_bracket_top20 a_winner_nr_3_br_inyear##perfor_bracket_top20 i.month ///
 if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) merge addtable
reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size perfor_bracket_top10 a_winner_nr_3_br_inyear##perfor_bracket_top10 i.month ///
 if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) merge addtable
reg performance_index lag_performance_index a_winner_nr_3_br_inyear branch_size perfor_bracket_top50 a_winner_nr_3_br_inyear##perfor_bracket_top50 i.month ///
 if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) merge addtable


****************************************************************************************
****************** Multi level Modelling 
****************************************************************************************
gen NOT_a_winner_sm_br_inyear=!a_winner_sm_br_inyear
gen NOT_sm_bry_perfor_80 = NOT_a_winner_sm_br_inyear*perfor_bracket_last80
gen NOT_sm_bry_perfor_50 = NOT_a_winner_sm_br_inyear*perfor_bracket_last50
gen NOT_sm_bry_perfor_LAST20 = NOT_a_winner_sm_br_inyear*perfor_bracket_last20
gen NOT_sm_bry_perfor_TOP20 = NOT_a_winner_sm_br_inyear*perfor_bracket_top20
gen sm_bry_perfor_80 = a_winner_sm_br_inyear*perfor_bracket_last80
gen sm_bry_perfor_50 = a_winner_sm_br_inyear*perfor_bracket_last50
gen sm_bry_perfor_LAST20 = a_winner_sm_br_inyear*perfor_bracket_last20
gen sm_bry_perfor_TOP20 = a_winner_sm_br_inyear*perfor_bracket_top20
gen sm_bry_perfor_TOP50 = a_winner_sm_br_inyear*perfor_bracket_top50

//gen sm_bry_pefor_quant=a_winner_sm_br_inyear*quant_performance_index

gen NOT_a_winner_nr_3_br_inyear=!a_winner_nr_3_br_inyear
gen niegh_3_perfor_80= a_winner_nr_3_br_inyear*perfor_bracket_last80
gen niegh_3_perfor_50= a_winner_nr_3_br_inyear*perfor_bracket_last50
gen niegh_3_perfor_LAST20= a_winner_nr_3_br_inyear*perfor_bracket_last20
gen niegh_3_perfor_TOP20= a_winner_nr_3_br_inyear*perfor_bracket_top20
gen niegh_3_perfor_40_60= a_winner_nr_3_br_inyear*perfor_bracket_40_60
gen niegh_3_perfor_50_70= a_winner_nr_3_br_inyear*perfor_bracket_50_70
gen niegh_3_perfor_top50= a_winner_nr_3_br_inyear*perfor_bracket_top50


gen NOT_neigh_3_per_80 = NOT_a_winner_nr_3_br_inyear*perfor_bracket_last80
gen NOT_neigh_3_per_50 = NOT_a_winner_nr_3_br_inyear*perfor_bracket_last50
gen NOT_neigh_3_per_LAST20 = NOT_a_winner_nr_3_br_inyear*perfor_bracket_last20
gen NOT_neigh_3_per_TOP20 = NOT_a_winner_nr_3_br_inyear*perfor_bracket_top20
gen NOT_neigh_3_per_40_60 = NOT_a_winner_nr_3_br_inyear*perfor_bracket_40_60



gen niegh_3_perfor_60_80= a_winner_nr_3_br_inyear*perfor_bracket_60_80




****** *_______________________Generating Variable for Clusters___________________________________**
egen pickone_branch = tag(branch_uuid)
egen pickone_chw = tag(chw_uuid)
**Number of CHWs within each branch
bysort branch_uuid: egen branch_size_new = total(pickone_chw)
drop if branch_size_new==0
sum performance_index 
histogram performance_index, normal
** To normalize the Performance_index
egen normalized_per_index = std(performance_index)
sum normalized_per_index
histogram normalized_per_index, normal
**** As we see the performance index was already normalized..so we do not need to normalize it again
drop normalized_per_index


/// Estimates without lagged performance index

**Table 2 - Pooled OLS
reg performance_index a_winner_sm_br_inyear branch_size i.month if award_rcvd_never_CHW==1, cluster(branch_uuid) 
//Reported in the paper 
outreg using "without_lag_1", se summstat(F \ r2) starlevels(10 5 1) replace

** Table 2- MLM RE
xtmixed performance_index a_winner_sm_br_inyear  branch_size || branch_n_id: || chw_n_id:, mle var
estimate store ran_int_both // the number of observations is now 85050 because there is noe laged perform index and we do not need to drop the first month observatiosns  

xtmixed performance_index a_winner_sm_br_inyear  branch_size || branch_n_id: , mle var
estimate store ran_int_branch

xtmixed performance_index a_winner_sm_br_inyear branch_size || chw_n_id: , mle var
estimate store ran_int_chw

lrtest  ran_int_both  ran_int_branch // chi= -0.00, prob=1.00 - The 3 lvel model is not prefereed over the 2-level observations-within-Branch model. Implies that there is no significant difference between the CHW level observations 
lrtest ran_int_both ran_int_chw // chi=801.93 prob=0.000. Implies that the monthly observations within a CHW are heterogenous and similar across different CHWs. 
//So, the performance index vary significantly at branch level but not at the CHW level. Therefoe, we will be beteer off if we just use the 2 level model observations within branch model.. 

xtmixed performance_index a_winner_sm_br_inyear branch_size || branch_n_id:  a_winner_sm_br_inyear, mle var
estimate store ran_int_slope_branch

xtmixed performance_index a_winner_sm_br_inyear branch_size || chw_n_id:  a_winner_sm_br_inyear, mle var
estimate store ran_int_slope_chw

**Random intercept and slope at both level
xtmixed performance_index a_winner_sm_br_inyear branch_size || branch_n_id:  a_winner_sm_br_inyear || chw_n_id:  a_winner_sm_br_inyear, mle var
estimate store ran_int_slope_both

xtmixed performance_index a_winner_sm_br_inyear branch_size || branch_n_id:  a_winner_sm_br_inyear || chw_n_id: , mle var
estimate store ran_all_br_randint_chw

lrtest ran_int_slope_both ran_all_br_randint_chw


**** Table 3 - Effects on Different Quality from Same branch
** Pooled OLS
reg performance_index a_winner_sm_br_inyear branch_size perfor_bracket_last50 a_winner_sm_br_inyear##perfor_bracket_last50 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "withoutlag_table_3", se summstat(F \ r2) starlevels(10 5 1) replace 

reg performance_index a_winner_sm_br_inyear branch_size perfor_bracket_last20 a_winner_sm_br_inyear##perfor_bracket_last20 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "withoutlag_table_3", se summstat(F \ r2) starlevels(10 5 1) merge addtable


reg performance_index a_winner_sm_br_inyear branch_size perfor_bracket_top50 a_winner_sm_br_inyear##perfor_bracket_top50 i.month if award_rcvd_never_CHW==1, cluster(branch_uuid)
outreg using "withoutlag_table_3", se summstat(F \ r2) starlevels(10 5 1) merge addtable


** MLM RE
xtmixed performance_index a_winner_sm_br_inyear  branch_size perfor_bracket_top50  sm_bry_perfor_TOP50 || branch_n_id: sm_bry_perfor_TOP50 || chw_n_id: , mle var
estimate store rand_int_slope_brnch_TOP50

xtmixed performance_index a_winner_sm_br_inyear branch_size perfor_bracket_last50 sm_bry_perfor_50 || branch_n_id: sm_bry_perfor_50 || chw_n_id: , mle var
estimate store rand_int_slope_both_per50

xtmixed performance_index a_winner_sm_br_inyear  branch_size perfor_bracket_last20 sm_bry_perfor_LAST20 || branch_n_id: sm_bry_perfor_LAST20  || chw_n_id:  , mle var
estimate store rand_int_slope_both_last20





**** Table 4  
** Pooled OLS
reg performance_index a_winner_nr_3_br_inyear branch_size i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_3", se summstat(F \ r2) starlevels(10 5 1) replace //reported

*** MLM RE
xtmixed performance_index a_winner_nr_3_br_inyear branch_size if  award_rcvd_never_brnch==1 || branch_n_id: a_winner_nr_3_br_inyear || chw_n_id: , mle var 
estimate store ran_int_slope_nb


**** Table 5
** Pooled OLS

reg performance_index  a_winner_nr_3_br_inyear branch_size perfor_bracket_last50 a_winner_nr_3_br_inyear##perfor_bracket_last50 i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) replace

reg performance_index  a_winner_nr_3_br_inyear branch_size perfor_bracket_last20 a_winner_nr_3_br_inyear##perfor_bracket_last20 i.month if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) merge addtable


reg performance_index  a_winner_nr_3_br_inyear branch_size perfor_bracket_top50 a_winner_nr_3_br_inyear##perfor_bracket_top50 i.month ///
 if  award_rcvd_never_brnch==1, cluster(branch_uuid) //Excluding the winner branches// We are focusing on branches which never received Award
outreg using "factor_table_4", se summstat(F \ r2) starlevels(10 5 1) merge addtable

*** MLM RE
xtmixed performance_index a_winner_nr_3_br_inyear  branch_size perfor_bracket_top50 niegh_3_perfor_top50 if  award_rcvd_never_brnch==1 || branch_n_id: niegh_3_perfor_top50 || chw_n_id: , mle var

xtmixed performance_index a_winner_nr_3_br_inyear branch_size perfor_bracket_last50 niegh_3_perfor_50 if  award_rcvd_never_brnch==1 || branch_n_id: niegh_3_perfor_50 || chw_n_id: , mle var

xtmixed performance_index a_winner_nr_3_br_inyear  branch_size perfor_bracket_last20 niegh_3_perfor_LAST20 if  award_rcvd_never_brnch==1 || branch_n_id: niegh_3_perfor_LAST20 || chw_n_id: , mle var


export delimited using data_for_spatial_final.csv, replace
save "DATA\merged_CHP_performance_branch_sales", replace 

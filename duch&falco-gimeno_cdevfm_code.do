***********************************************************************************************************************
*This is the code to replicate the results of the article "Coalition-Directed Economic Voting and the Finance Minister"
*To be run on duch&falco-gimeno_cdevfm_data.dta
*July 2, 2018
***********************************************************************************************************************

*load data
use duch&falco-gimeno_cdevfm_data.dta, clear

*install required packages
ssc install combomarginsplot
ssc install gr0034
ssc install blindschemes

*set scheme
set scheme plottig

*label variables and values
label var ev100 "Party Economic Vote * 100 (PEV100)"
label var nopm_nofm "NO PM - NO FM"
label var pm_nofm "YES PM - NO FM"
label var nopm_fm "NO PM - YES FM"
label var pm_fm "YES PM - YES FM"
label var compartment "Compartmentalization"
label var seatshare "Party Size (% Seats)"
label var economy_share "Economic Profile"
label var cabinet_party "Incumbent Party"
label var coalition "Coalition Government"
label var pm_sw "PM Party"
label var fm_sw "FM Party"
label var data "Election Study (Survey)"
label var ccode "Country Code"
label var year_x "Year"

label def incumbent 0 "Opposition Party" 1 "Incumbent Party"
label def coalition 0 "Single Party Gov." 1 "Coalition Gov." 
label def pm 0 "No PM" 1 "PM"
label def fm 0 "No FM" 1 "FM" 

label values cabinet_party incumbent
label values coalition coalition
label values pm_sw pm
label values fm_sw fm


**************************************************
*FIGURE 1: IDENTIFICATION OF THE FINANCE MINISTER*
**************************************************

/*different database: CSES data*/



*********************************
*TABLE 1: DESCRIPTIVE STATISTICS*
*********************************

/*just to generate the main e(sample) from which we show the descriptive statistics*/ quietly: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare , cluster(data)
/*full sample*/ tabstat ev100 cabinet_party coalition seatshare economy_share if e(sample), s(n mean sd med) c(stat) format(%9.3f)

/*just to generate the main e(sample) from which we show the descriptive statistics*/ quietly: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare , cluster(data)
/*coalition parties*/ tabstat ev100 nopm_nofm pm_nofm nopm_fm pm_fm compartment seatshare economy_share if e(sample) & coalition==1 & cabinet_party==1, s(n mean sd med) c(stat) format(%9.3f)



***************************************************************************************
*TABLE 2: BASELINE MODELS: MARGINAL EFFECTS OF PARTICIPATION IN GOVERNMENT, PM, AND FM*
***************************************************************************************

/*column 1*/
quietly: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare , cluster(data)
margins , dydx(cabinet_party) at(coalition=(0 1))

/*column 2*/
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , dydx(cabinet_party) at(coalition=(0 1))

/*column 3*/
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare , cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1)

/*column 4*/
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1)



******************************************************************************************************
*FIGURE 2: MARGINAL EFFECTS OF CONTROLLING THE FM AND PM FOR DIFFERENT VALUES OF COMPARTMENTALIZATION*
******************************************************************************************************

/*left panel*/
quietly: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , dydx(fm_sw) at(coalition=1 cabinet_party==1 compartment=(0(.5)9))
marginsplot, yline(0, lp(dash) lc(plr1)) title("FM effect") xtitle("Level of Compartmentalization", size(small)) ytitle("") xlabel(#10) ylabel(#10) ///
	addplot(histogram compartment if e(sample) & coalition==1, start(-.25) width(.5) yaxis(2) yscale(off axis(2)) legend(off) below fcolor(none) lcolor(gs11)) ///
	saving(g1, replace)

/*right panel*/
quietly: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , dydx(pm_sw) at(coalition=1 cabinet_party==1 compartment=(0(.5)9))
marginsplot, yline(0, lp(dash) lc(plr1)) title("PM effect") xtitle("Level of Compartmentalization", size(small)) ytitle("") xlabel(#10) ylabel(#10) ///
	addplot(histogram compartment if e(sample) & coalition==1, start(-.25) width(.5) yaxis(2) yscale(off axis(2)) legend(off) below fcolor(none) lcolor(gs11)) ///
	saving(g2, replace)

/*combination of the two panels*/
gr combine g1.gph g2.gph , xcommon ycommon col(2) imargin(zero)


****************************************************************************************
*FIGURE 3: COMPARISON OR PREDICTIVE MARGINS AT DIFFERENT VALUES OF COMPARTMENTALIZATION*
****************************************************************************************

/*panel "general"*/
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=0 cabinet_party=0) post saving(preda_spop, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=0) post saving(preda_cop, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=0 cabinet_party=1) post saving(preda_spin, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1) post saving(preda_cin, replace)
marginsplot

combomarginsplot preda_spop preda_cop preda_spin preda_cin , yline(0, lp(dash) lc(plr1)) recast(scatter) ///
	title("") ///
	xlabel(1 "      Opp-SP" 2 "Opp-Coal" 3 "Gov-SP" 4 "Gov-Coal", angle(vertical) labsize(medium)) xscale(range(0.75 4.24))  ///
	ylabel(-6(1)2, labsize(medium)) ///
	xtitle("General", size(large)) ytitle("") ///
	saving(pred_all, replace)


/*panel "parties in coalitions"*/
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 pm_sw=0 fm_sw=0) post saving(predc_npnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 pm_sw=0 fm_sw=1) post saving(predc_npyf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 pm_sw=1 fm_sw=0) post saving(predc_ypnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party i.coalition##i.pm_sw i.coalition##i.fm_sw i.pm_sw##i.fm_sw seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 pm_sw=1 fm_sw=1) post saving(predc_ypyf, replace)
marginsplot

combomarginsplot predc_npnf predc_npyf predc_ypnf predc_ypyf , yline(0, lp(dash) lc(plr1)) recast(scatter) ///
	title("") ///
	xlabel(1 "noPM-noFM" 2 "noPM-FM" 3 "PM-noFM" 4 "PM-FM", angle(vertical) labsize(medium)) xscale(range(0.75 4.24))  ///
	ylabel(-6(1)2, labsize(medium)) ///
	xtitle("Parties in Coalitions", size(large)) ytitle("") ///
	saving(pred_govcoal, replace)


/*combination of the two upper panels*/
gr combine pred_all.gph pred_govcoal.gph , xcommon ycommon col(5) imargin(medium) xsize(10)



/*panel "very low compartmentalization"*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(0.29) pm_sw=0 fm_sw=0) post saving(predvl_npnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(0.29) pm_sw=0 fm_sw=1) post saving(predvl_npyf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(0.29) pm_sw=1 fm_sw=0) post saving(predvl_ypnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(0.29) pm_sw=1 fm_sw=1) post saving(predvl_ypyf, replace)
marginsplot

combomarginsplot predvl_npnf predvl_npyf predvl_ypnf predvl_ypyf , yline(0, lp(dash) lc(plr1)) recast(scatter) ///
	title("") ///
	xlabel(1 "noPM-noFM" 2 "noPM-FM" 3 "PM-noFM" 4 "PM-FM", angle(vertical) labsize(medium)) xscale(range(0.75 4.24)) ///
	ylabel(-6(1)2, labsize(medium)) ///
	xtitle("Very Low Compartment.", size(large)) ytitle("") ///
	saving(predvl, replace)



/*panel "low compartmentalization"*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(1.74) pm_sw=0 fm_sw=0) post saving(predl_npnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(1.74) pm_sw=0 fm_sw=1) post saving(predl_npyf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(1.74) pm_sw=1 fm_sw=0) post saving(predl_ypnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(1.74) pm_sw=1 fm_sw=1) post saving(predl_ypyf, replace)
marginsplot

combomarginsplot predl_npnf predl_npyf predl_ypnf predl_ypyf , yline(0, lp(dash) lc(plr1)) recast(scatter) ///
	title("") ///
	xlabel(1 "noPM-noFM" 2 "noPM-FM" 3 "PM-noFM" 4 "PM-FM", angle(vertical) labsize(medium)) xscale(range(0.75 4.24))  ///
	ylabel(-6(1)2, labsize(medium)) ///
	xtitle("Low Compartment.", size(large)) ytitle("") ///
	saving(predl, replace)


/*panel "median compartmentalization"*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(3.19) pm_sw=0 fm_sw=0) post saving(predm_npnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(3.19) pm_sw=0 fm_sw=1) post saving(predm_npyf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(3.19) pm_sw=1 fm_sw=0) post saving(predm_ypnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(3.19) pm_sw=1 fm_sw=1) post saving(predm_ypyf, replace)
marginsplot

combomarginsplot predm_npnf predm_npyf predm_ypnf predm_ypyf , yline(0, lp(dash) lc(plr1)) recast(scatter) ///
	title("") ///
	xlabel(1 "noPM-noFM" 2 "noPM-FM" 3 "PM-noFM" 4 "PM-FM", angle(vertical) labsize(medium)) xscale(range(0.75 4.24))  ///
	ylabel(-6(1)2, labsize(medium)) ///
	xtitle("Median Compartment.", size(large)) ytitle("") ///
	saving(predm, replace)


/*panel "high compartmentalization"*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(4.64) pm_sw=0 fm_sw=0) post saving(predh_npnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(4.64) pm_sw=0 fm_sw=1) post saving(predh_npyf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(4.64) pm_sw=1 fm_sw=0) post saving(predh_ypnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(4.64) pm_sw=1 fm_sw=1) post saving(predh_ypyf, replace)
marginsplot

combomarginsplot predh_npnf predh_npyf predh_ypnf predh_ypyf , yline(0, lp(dash) lc(plr1)) recast(scatter) ///
	title("") ///
	xlabel(1 "noPM-noFM" 2 "noPM-FM" 3 "PM-noFM" 4 "PM-FM", angle(vertical) labsize(medium)) xscale(range(0.75 4.24))  ///
	ylabel(-6(1)2, labsize(medium)) ///
	xtitle("High Compartment.", size(large)) ytitle("") ///
	saving(predh, replace)


/*panel "very high compartmentalization"*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(6.09) pm_sw=0 fm_sw=0) post saving(predvh_npnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(6.09) pm_sw=0 fm_sw=1) post saving(predvh_npyf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(6.09) pm_sw=1 fm_sw=0) post saving(predvh_ypnf, replace)
marginsplot
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share , cluster(data)
margins , at(coalition=1 cabinet_party=1 compartment=(6.09) pm_sw=1 fm_sw=1) post saving(predvh_ypyf, replace)
marginsplot

combomarginsplot predvh_npnf predvh_npyf predvh_ypnf predvh_ypyf , yline(0, lp(dash) lc(plr1)) recast(scatter) ///
	title("") ///
	xlabel(1 "noPM-noFM" 2 "noPM-FM" 3 "PM-noFM" 4 "PM-FM", angle(vertical) labsize(medium)) xscale(range(0.75 4.24))  ///
	ylabel(-6(1)2, labsize(medium)) ///
	xtitle("Very High Compartment.", size(large)) ytitle("") ///
	saving(predvh, replace)


/*combination of the five bottom panels*/
gr combine predvl.gph predl.gph predm.gph predh.gph predvh.gph , xcommon ycommon col(5) imargin(zero) xsize(10)



*******************************************************************************
*TABLE 3: ROBUSTNESS CHECKS: ADDRESSING CROSS-COUNTRY AND CROSS-TIME VARIATION*
*******************************************************************************

/*column 1*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(1.74))
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(4.64))

/*column 2*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share c.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(1.74))
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share c.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(4.64))

/*column 3*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.ccode#c.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(1.74))
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.ccode#c.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(4.64))

/*column 4*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(1.74))
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(4.64))

/*column 5*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.ccode , cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(1.74))
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.ccode , cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(4.64))

/*column 6*/
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.ccode i.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(1.74))
qui: reg ev100 i.coalition##i.cabinet_party##i.pm_sw##i.fm_sw##c.compartment seatshare economy_share i.ccode i.year_x, cluster(data)
margins , dydx(pm_sw fm_sw) at(coalition=1 cabinet_party=1 compartment=(4.64))



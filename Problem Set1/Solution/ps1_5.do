cd U:/classes/gu_macrometrics
clear
set more off
import excel "U:\classes\gu_macrometrics\data\ps1_p5_data.xls", sheet("FRED Graph") cellrange(A12:C289) firstrow

gen q = quarter(observation)
gen y = year(observation)
sort y q

gen unemp = UNRATE
gen infl = 100*((GDPDEF/GDPDEF[_n-1])^4-1)

drop if y<1948 | y>2007
gen t = _n
order t observation y q
tsset t

* a) Pure AR
foreach i of numlist 1/10 {
reg infl L(1/`i').infl if t>10
est store fit_ar`i'
}
esttab fit_ar*, se stats(N aic bic)
varsoc infl, maxlag(10)
//--> the AIC suggests an optimal lag of 8; varsoc agrees

//but let's just restrict attention to the first 5 lags
foreach i of numlist 1/5 {
reg infl L(1/`i').infl if t>5
est store fit_ar`i'
}
esttab fit_ar*, se stats(N aic bic)
varsoc infl, maxlag(5)
//---> optimal lag is 2; again, varsoc agrees

* b) SB
cap drop period2 *Xper2
gen period2 = y>=1984
gen l1inflXper2 = L1.infl * period2
gen l2inflXper2 = L2.infl * period2
reg infl L(1/2).infl period2 l1inflXper2 l2inflXper2
test period2 l1inflXper2 l2inflXper2
//---> fail to reject --> no evidence for break

* c) Phillips Model
foreach i of numlist 1/10{
qui reg infl L(1/`i').infl L(1/`i').unemp if t>10
est store fit_phil`i'
}
esttab fit_phil? fit_phil10, se stats(N aic bic)
//--->optimal lags are 8

* d) Phillips break
reg infl L(1/8).infl L(1/8).unemp if y<1990
est store fit_phil_per1
test L1.unemp + L2.unemp + L3.unemp + L4.unemp + L5.unemp + L6.unemp + L7.unemp + L8.unemp= 0 //fail to reject 
reg infl L(1/8).infl L(1/8).unemp if y>=1990
est store fit_phil_per2
test L1.unemp + L2.unemp + L3.unemp + L4.unemp + L5.unemp + L6.unemp + L7.unemp + L8.unemp = 0 //fail to reject 



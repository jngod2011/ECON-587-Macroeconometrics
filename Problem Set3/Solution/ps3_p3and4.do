cd "C:\Users\dburk\Dropbox\classes\gu_macrometrics\ps3\"

import excel "ps3data.xls", sheet("Monthly") firstrow clear


** Problem 3
gen year = year(DATE)
gen quarter = quarter(DATE)
collapse UNRATE* CPI*, by(year quarter)
save unrate_CPI_q, replace

import excel "ps3data.xls", sheet("Quarterly") firstrow clear
gen year = year(DATE)
gen quarter = quarter(DATE)
merge 1:1 year quarter using unrate_CPI_q

sort year quarter
gen t = _n
gen t_q = qofd(DATE)
format t_q %tq
tsset t_q

cap drop gdp pgdp cpi unemp pi gap
gen gdp = GDP
gen pgdp = GDPPOT
gen cpi = CPILFESL
gen unemp = UNRATE
gen pi = ((cpi/L.cpi)^4-1)*100

gen gap = 100*(log(gdp)-log(pgdp))

reg pi L(1/4).pi L(1/4).gap if y>1957 & y<2007

foreach b of numlist 60/200{
qui cap drop d
qui gen d = t>`b'
qui reg pi L(1/4).pi c.d##c.L(1/4).gap if y>1957 & y<2007
qui testparm c.d#c.L(1/4).gap
di "time = " `b' "; p value = " r(p) "; F stat = " r(F)
}
//largest F stat is around t=139, which is 1981q3
// F stat also peaks around 1974



foreach b of numlist 60/200{
qui cap drop d
qui gen d = t>`b'
qui reg pi L(1/4).pi c.d##c.L(1/4).gap if y>1957 & y<2007
qui test c.d#cL.gap = -c.d#cL2.gap - c.d#cL3.gap - c.d#cL4.gap  //testing whether sum changes
di "time = " `b' "; p value = " r(p) "; F stat = " r(F)
}


///1981q3 break
 reg pi L(1/4).pi c.L(1/4).gap if t_q>=tq(1957q2) & t_q<tq(1981q4)
est store mod1
reg pi L(1/4).pi c.L(1/4).gap if t_q>=tq(1981q4)
est store mod2
esttab mod1 mod2, se stats(N aic bic)
suest mod1 mod2
test ([mod1_mean]L1.gap = [mod2_mean]L1.gap) ([mod1_mean]L2.gap = [mod2_mean]L2.gap) ///
     ([mod1_mean]L3.gap = [mod2_mean]L3.gap) ([mod1_mean]L4.gap = [mod2_mean]L4.gap) 
test [mod1_mean]L1.gap + [mod1_mean]L2.gap + [mod1_mean]L3.gap + [mod1_mean]L4.gap = ///
	[mod2_mean]L1.gap + [mod2_mean]L2.gap + [mod2_mean]L3.gap + [mod2_mean]L4.gap


 
 ///1974 break?
 reg pi L(1/4).pi c.L(1/4).gap if t_q>=tq(1957q2) & t_q<tq(1974q4)
est store mod1
reg pi L(1/4).pi c.L(1/4).gap if t_q>=tq(1974q4)
est store mod2
esttab mod1 mod2, se stats(N aic bic)
suest mod1 mod2
test ([mod1_mean]L1.gap = [mod2_mean]L1.gap) ([mod1_mean]L2.gap = [mod2_mean]L2.gap) ///
     ([mod1_mean]L3.gap = [mod2_mean]L3.gap) ([mod1_mean]L4.gap = [mod2_mean]L4.gap) 
test [mod1_mean]L1.gap + [mod1_mean]L2.gap + [mod1_mean]L3.gap + [mod1_mean]L4.gap = ///
	[mod2_mean]L1.gap + [mod2_mean]L2.gap + [mod2_mean]L3.gap + [mod2_mean]L4.gap

//stable within periods?
foreach b of numlist 60/111{
qui cap drop d
qui gen d = t>`b'
qui reg pi L(1/4).pi c.d##c.L(1/4).gap if y>1957 & y<1974
qui test c.d#cL.gap = -c.d#cL2.gap - c.d#cL3.gap - c.d#cL4.gap  //testing whether sum changes
di "time = " `b' "; p value = " r(p) "; F stat = " r(F)
}	

* SUR
reg pi L(1/4).pi c.L(1/4).gap if t_q>=tq(1957q2) & t_q<tq(1970q1)
est store mod0
reg pi L(1/4).pi c.L(1/4).gap if t_q>=tq(1970q1) & t_q<tq(1974q4)
est store mod1
reg pi L(1/4).pi c.L(1/4).gap if t_q>=tq(1974q4)
est store mod2
esttab mod1 mod2, se stats(N aic bic)
suest mod0 mod1 mod2
test ([mod1_mean]L1.gap = [mod2_mean]L1.gap) ([mod1_mean]L2.gap = [mod2_mean]L2.gap) ///
     ([mod1_mean]L3.gap = [mod2_mean]L3.gap) ([mod1_mean]L4.gap = [mod2_mean]L4.gap) 
test [mod1_mean]L1.gap + [mod1_mean]L2.gap + [mod1_mean]L3.gap + [mod1_mean]L4.gap = ///
	[mod2_mean]L1.gap + [mod2_mean]L2.gap + [mod2_mean]L3.gap + [mod2_mean]L4.gap


*** Prob. 4
var pi unemp, lags(1/4) 
esttab, se stats(N aic bic)
vargranger

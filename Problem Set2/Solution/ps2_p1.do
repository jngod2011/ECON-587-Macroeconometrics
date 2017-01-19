*cd U:\classes\gu_macrometrics\psets\ps2\
cd C:\Users\dburk\Dropbox\classes\gu_macrometrics\psets\ps2

clear
import excel "ps2data.xlsx", firstrow

gen q = quarter(observation)
gen y = year(observation)
sort y q
gen t = _n
gen date = observation
gen t_q = qofd(date)

tsset t_q
keep if inrange(t_q, quarterly("1948q1","YQ"), quarterly("2007q4","YQ"))

gen du = D.UNRATE * 4 //annualized level difference
gen g = ((GDPC1/L.GDPC1)^4 - 1)*100 //annualized growth rate in percentage

* Problem 1
* part i.
reg du L(1/2).du L(0/2).g 
esttab
* part ii.
reg du L(1/2).du L(0/2).g if inrange(t_q, quarterly("1949q1","YQ"), quarterly("2007q4","YQ"))
est store mod_b
reg du L(1/1).du L(0/1).g if inrange(t_q, quarterly("1949q1","YQ"), quarterly("2007q4","YQ"))
est store mod_a
reg du L(1/3).du L(0/3).g if inrange(t_q, quarterly("1948q4","YQ"), quarterly("2007q4","YQ"))
est store mod_c
esttab mod*, se stats(N aic bic)  //aic is minimized at 2 lags

varsoc du g  // Stata's canned command confirms this

* part iii. test for break

//first Chow test baby model (to make clear the logic of the test)
gen per2 = y>=1984
xi: reg du i.per2*g
test _Iper2 _IperXg_1 // reject that there is no break

reg du g
estat sbknown, break(145) 	//this is Stata's canned command (new to Stata 14!)

//now test full model
//using canned command
reg du L(1/2).du L(0/2).g
estat sbknown, break(145)

//or do it "manually"
gen LduXp2 = L.du * per2
gen L2duXp2 = L2.du * per2
gen gXp2 = g * per2
gen LgXp2 = L.g * per2
gen L2gXp2 = L2.g * per2
reg du L(1/2).du L(0/2).g per2 LduXp2 - L2gXp2
test per2 LduXp2 L2duXp2 gXp2 LgXp2 L2gXp2 // --> reject null of no break!

* part iv. GDP growth rate consistent with stable unemp (see p. 75 of Knotek paper)
reg du g if !per2
di -_b[_cons]/_b[g] // --> 4.2

reg du g if per2
di -_b[_cons]/_b[g] // --> 2.6

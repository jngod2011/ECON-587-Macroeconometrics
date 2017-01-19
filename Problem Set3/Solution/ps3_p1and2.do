*cd cd "C:\Users\dburk\Dropbox\classes\gu_macrometrics\ps3"
cd "C:\Users\dburk\Dropbox\classes\gu_macrometrics\ps3"
import excel "mystery data.xls", sheet("Sheet1") firstrow clear
gen t = _n
tsset t

*** MS1
tsline ms1
dfuller ms1, trend regress
reg D.ms1 L.ms1 t // unit root!
corrgram D.ms1, lags(12)
qui{
arima ms1, arima(2,1,0)
est store mod1
arima ms1, arima(2,1,1)
est store mod2
arima ms1, arima(1,1,1)
est store mod3
arima ms1, arima(1,1,0)
est store mod4
}
esttab mod1 mod2 mod3 mod4, se stats(N aic bic)

*** MS2
tsline ms2
dfuller ms2, regress
reg D.ms2 L.ms2 t // unit root!
corrgram D.ms2
qui{
arima ms2, arima(1,1,0)
est store mod1
arima ms2, arima(1,1,1)
est store mod2
arima ms2, arima(2,1,0)
est store mod3
}
esttab mod1 mod2 mod3, se stats(N aic bic)

arima ms2, arima(1,1,0)
cap drop r
predict r, resid
corrgram r

*** MS3
tsline ms3
dfuller ms3, trend regress
reg D.ms3 L.ms3 t // unit root!
reg D.ms3 L.ms3 // unit root!

corrgram D.ms3
qui{
arima ms3, arima(1,1,0)
est store mod1
arima ms3, arima(0,1,1)
est store mod2
arima ms3, arima(1,1,1)
est store mod3
}
esttab mod1 mod2 mod3, se stats(N aic bic)

arima ms3, arima(0,1,1)
cap drop r
predict r, resid
corrgram r


*** MS4
tsline ms4
dfuller ms4, trend regress
reg D.ms4 L.ms4 t // unit root!
reg D.ms4 L.ms4 // unit root!

corrgram D.ms4, lags(12)
qui{
arima ms4, arima(1,1,0)
est store mod1
arima ms4, arima(0,1,1)
est store mod2
arima ms4, arima(1,1,1)
est store mod3
arima ms4, arima(2,1,0)
est store mod4
}
esttab mod1 mod2 mod3 mod4, se stats(N aic bic)

arima ms4, arima(1,1,1)
cap drop r
predict r, resid
corrgram r
wntestq r



**** Problem 2

foreach p of numlist 1/217 {
cap drop per2
gen per2 = t>=(217-`p')
qui reg ms5 per2
qui test per2
di "break at " 217 - `p' "; F stat: " e(F)
}

//max F stat at t=65
replace per2 = t>=65
reg ms5 per2
esttab, se stats(N aic bic)



//ms6 and ms7
foreach p of numlist 1/217 {
cap drop per2 
gen per2 = t>=(217-`p')
cap drop ms7Xper2
qui gen ms7Xper2 = ms7 * per2
qui reg ms6 ms7 per2 ms7Xper2 
qui test per2 ms7Xper2
di "break at " 217 - `p' "; F stat: " e(F)
}

//largest f stat at t = 105 (1985.1)
foreach p of numlist 1/217 {
cap drop per2 
gen per2 = t>=(217-`p')
cap drop ms7Xper2
qui gen ms7Xper2 = ms7 * per2
qui reg ms6 ms7 per2 ms7Xper2
qui test per2 ms7Xper2
di "break at " 217 - `p' "; F stat: " e(F)
}


cap drop per2 
gen per2 = t>=105
cap drop ms7Xper2
qui gen ms7Xper2 = ms7 * per2
reg ms6 ms7 per2 ms7Xper2
esttab, se stats(N aic bic)

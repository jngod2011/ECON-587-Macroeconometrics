import delimited C:\Users\wang_\Desktop\GDPDEF.csv
replace v1 = 0 in 1
rename v1 t
tsset t
//a
dfuller inflation //make sure it is a stationary process
corrgram inflation
ac inflation
pac inflation //notice that there is a peak in lag(8)
qui arima inflation,ar(1) noconstant nolog
estat ic
qui arima inflation,ar(1/2) noconstant nolog
estat ic
qui arima inflation,ar(1/3) noconstant nolog
estat ic
qui arima inflation,ar(1/4) noconstant nolog
estat ic
qui arima inflation,ar(1/5) noconstant nolog
estat ic
qui arima inflation,ar(1/6) noconstant nolog
estat ic
qui arima inflation,ar(1/7) noconstant nolog
estat ic
qui arima inflation,ar(1/8) noconstant nolog
estat ic
qui arima inflation,ar(1/9) noconstant nolog
estat ic
qui arima inflation,ar(1 2 8) noconstant nolog
estat ic
qui arima inflation,ar(1 2 7 8) noconstant nolog //this has the lowest AIC
estat ic
arima inflation,ar(1 2 7 8) noconstant nolog
predict e1,res
wntestq e1 //Box Q-statistic
corrgram e1
//b
tsline inflation
qui arima inflation,ar(1 2 7 8) noconstant nolog
est store ALL
qui arima inflation if t<144,ar(1 2 7 8) noconstant nolog
est store A
qui arima inflation if t>=144,ar(1 2 7 8) noconstant nolog
est store B
lrtest (ALL)(A B),stats //chow test
//c
qui arima inflation L.unrate if t>5,ar(1) nolog
estat ic
qui arima inflation L.unrate L2.unrate if t>5,ar(1/2) nolog
estat ic
qui arima inflation L.unrate L2.unrate L3.unrate if t>5,ar(1/3) nolog //this has the lowest AIC
estat ic
qui arima inflation L.unrate L2.unrate L3.unrate L4.unrate if t>5,ar(1/4) nolog
estat ic
qui arima inflation L.unrate L2.unrate L3.unrate L4.unrate L5.unrate,ar(1/5) nolog
estat ic
arima inflation unrate L.unrate L2.unrate L3.unrate,ar(1/3) nolog
predict e2,res
wntestq e2 //Box Q-statistic
corrgram e2
//d
arima inflation L.unrate L2.unrate L3.unrate if t<=167,ar(1/3) nolog
test [inflation]L1.+[inflation]L2.+[inflation]L3.=0
arima inflation L.unrate L2.unrate L3.unrate if t>167,ar(1/3) nolog
test [inflation]L1.+[inflation]L2.+[inflation]L3.=0
sureg (inflation L.unrate L2.unrate L3.unrate L.inflation L2.inflation L3.inflation if t<=167) (inflation L.unrate L2.unrate L3.unrate L.inflation L2.inflation L3.inflation if t>167)

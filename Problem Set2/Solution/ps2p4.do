*cd "U:\classes\gu_macrometrics\psets\ps2\"
cd C:\Users\dburk\Dropbox\classes\gu_macrometrics\psets\ps2

import excel "ps2_p4data.xls", sheet("Quarterly") firstrow clear

gen t_q = qofd(DATE)

tsset t_q

* GDP GROWTH RATE
gen g = 100*((GDPC1/L.GDPC1)^4-1)
replace g =. if !inrange(t_q,tq(1984q1),tq(2009q4))
corrgram g
ac g
pac g  // ARMA(2,2) maybe?

//checking AIC of various models
qui{
arima g, arima(0,0,2)
est store mod1
arima g, arima(1,0,2)
est store mod2
arima g, arima(2,0,2)
est store mod3
arima g, arima(2,0,1)
est store mod4
arima g, arima(1,0,1)
est store mod5
}
esttab mod1 mod2 mod3 mod4 mod5, se stats(N aic bic) //AIC chose ARMA(2,1), BIC chose ARMA(1,1)


* LABOR PRODUCTIVITY
gen lp = OPHNFB
replace lp = . if !inrange(t_q,tq(1984q1),tq(2009q3))
ac lp //gradual decay consistent with nonstationary process
pac lp  //large spike at first lag consistent with AR process; but spike is about 1.0 so concern that this is unit root

corrgram lp
qui{
	arima lp, arima(0,0,2)
	est store mod1
	arima lp, arima(1,0,2)
	est store mod2
	arima lp, arima(1,0,0)
	est store mod3
	*arima lp, arima(2,0,2) //no convergence
	*arima lp, arima(2,0,1)  //no convergence
}
esttab mod1 mod2 mod3, se stats(N aic bic) //AIC chose ARMA(1,2)

// now let's check the difference:
corrgram D.lp // no clear pattern of spikes make me thing this may be decently approximated as white noise
qui{
	arima D.lp, arima(0,0,0)
	est store mod1
	arima D.lp, arima(1,0,2)
	est store mod2
	arima D.lp, arima(2,0,2)
	est store mod3
	arima D.lp, arima(2,0,1)
	est store mod4
}
	esttab mod1 mod2 mod3 mod4, se stats(N aic bic) //AIC chose ARMA(2,2), BIC chose MA(2)

	
* T BILL
import excel "ps2_p4data.xls", sheet("Monthly") firstrow clear
gen t_m = mofd(DATE)
format t_m %tm
tsset t_m
gen trate = tbill
replace trate =. if !inrange(t_m,tm(1990m1),tm(2009m4))

arima trate, arima(2,0,0)
est store mod1
arima trate, arima(2,0,1)
est store mod2
arima trate, arima(1,0,1)
est store mod3
arima trate, arima(1,0,2)
est store mod4
arima trate, arima(2,0,2)
est store mod5
arima trate, arima(2,0,3)
est store mod6

esttab mod1 mod3 mod5 mod6, se stats(N aic bic) //AIC chose ARMA(2,2), BIC chose MA(2)

est replay mod5
predict uhat, resid
corrgram uhat





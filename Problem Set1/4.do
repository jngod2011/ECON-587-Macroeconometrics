import delimited C:\Users\wang_\Desktop\medicare.csv
tsset fiscalyear
dfuller netmedicare
g d_netmedicare=d.netmedicare
dfuller d_netmedicare
line d_netmedicare fiscalyear, title("CBO's Projection of Net Medicare Spending(differenced)")
corrgram d_netmedicare
ac d_netmedicare,lag(10)
pac d_netmedicare,lag(10)
qui arima d_netmedicare,ar(2) ma(2) nolog
estat ic
qui arima d_netmedicare,ar(1 2) nolog
estat ic
qui arima d_netmedicare,ar(2) nolog
estat ic
qui arima d_netmedicare,ma(1 2) nolog
estat ic
qui arima d_netmedicare,ma(2) nolog
estat ic
predict e1,res
wntestq e1
corrgram e1
//MA(2) without first lag

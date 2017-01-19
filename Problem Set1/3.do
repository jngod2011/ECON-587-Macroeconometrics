import delimited D:\360安全浏览器下载\sometimeseriesdata.csv
tsset t
corrgram rebekah
ac rebekah,lag(10)
pac rebekah,lag(10)
//MA(1),beta<0
corrgram daniel
ac daniel,lag(10)
pac daniel,lag(10)
//ARMA(1,1),a1>0
corrgram zhuoxiansheng
ac zhuoxiansheng,lag(10)
pac zhuoxiansheng, lag(10)
//AR(1),a1>0
corrgram sylvia
ac sylvia,lag(10)
pac sylvia,lag(10)
//AR(1),a1<0
corrgram zhirui
ac zhirui,lag(10)
pac zhirui,lag(10)
//MA(1),beta>0
corrgram hao
ac hao,lag(10)
pac hao,lag(10)
//AR(1),a1>0
corrgram joanne
ac joanne,lag(10)
pac joanne,lag(10)
//white noise
corrgram sebastian
ac sebastian,lag(10)
pac sebastian,lag(10)
//ARMA(1,1),a1>0

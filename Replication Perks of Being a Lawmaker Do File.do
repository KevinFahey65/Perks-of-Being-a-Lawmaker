** Do-File 2-22-2016 **

* generate tenure variable *

g tenure = year - firstelect


* Revalue to 2001 dollars *
* I chose 2001 dollars when my data went back to 2001. I stuck with 2001 dollars after adding 1995-2000 *

g networth01 =.
 replace networth01 = networth*1.1621 if year == 1995
 replace networth01 = networth*1.1287 if year == 1996
 replace networth01 = networth*1.1034 if year == 1997
 replace networth01 = networth*1.0865 if year == 1998
 replace networth01 = networth*1.0630 if year == 1999
 replace networth01 = networth*1.0285 if year == 2000
 replace networth01 = networth if year == 2001
 replace networth01 = networth*0.9844 if year == 2002
 replace networth01 = networth*0.9625 if year == 2003
 replace networth01 = networth*0.9375 if year == 2004
 replace networth01 = networth*0.9068 if year == 2005
 replace networth01 = networth*0.8785 if year == 2006
 replace networth01 = networth*0.8541 if year == 2007
 replace networth01 = networth*0.8226 if year == 2008
 replace networth01 = networth*0.8255 if year == 2009
 replace networth01 = networth*0.8122 if year == 2010
 replace networth01 = networth*0.7873 if year == 2011
 replace networth01 = networth*0.7714 if year == 2012
 replace networth01 = networth*0.7602 if year == 2013
 replace networth01 = networth*0.7481 if year == 2014
 
 g income01 =.
 replace income01 = income*1.1621 if year == 1995
 replace income01 = income*1.1287 if year == 1996
 replace income01 = income*1.1034 if year == 1997
 replace income01 = income*1.0865 if year == 1998
 replace income01 = income*1.0630 if year == 1999
 replace income01 = income*1.0285 if year == 2000
 replace income01 = income if year == 2001
 replace income01 = income*0.9844 if year == 2002
 replace income01 = income*0.9625 if year == 2003
 replace income01 = income*0.9375 if year == 2004
 replace income01 = income*0.9068 if year == 2005
 replace income01 = income*0.8785 if year == 2006
 replace income01 = income*0.8541 if year == 2007
 replace income01 = income*0.8226 if year == 2008
 replace income01 = income*0.8255 if year == 2009
 replace income01 = income*0.8122 if year == 2010
 replace income01 = income*0.7873 if year == 2011
 replace income01 = income*0.7714 if year == 2012
 replace income01 = income*0.7602 if year == 2013
 replace income01 = income*0.7481 if year == 2014
 
 
* generate combined business and management careers variable
  
g bizman = 0
 replace bizman = 1 if business == 1 | management == 1

 
 * Generate age variable

g age = year - birth


* generate lagged networth and income variables

sort memberid year
by memberid: g netlag = networth01[_n-1]
by memberid: g inclag = income01[_n-1]

* and logged lags

g loginclag = log(inclag)
g lognetlag = log(netlag)


* generate differenced income and net worth variables

 g difnet = (networth01 - networth01[_n-1])
 
 g difinc = (income01 - income01[_n-1])


* generate GDP growth for the state of Florida by year

g gdp =.
replace gdp = 346951 if year == 1995
replace gdp = 370912 if year == 1996
replace gdp = 403677 if year == 1997
replace gdp = 431374 if year == 1998
replace gdp = 459431 if year == 1999
replace gdp = 490538 if year == 2000
replace gdp = 519024 if year == 2001
replace gdp = 550486 if year == 2002
replace gdp = 585729 if year == 2003
replace gdp = 637113 if year == 2004
replace gdp = 700222 if year == 2005
replace gdp = 748021 if year == 2006
replace gdp = 773992 if year == 2007
replace gdp = 754780 if year == 2008
replace gdp = 722825 if year == 2009
replace gdp = 730896 if year == 2010
replace gdp = 736890 if year == 2011
replace gdp = 766259 if year == 2012
replace gdp = 800679 if year == 2013
replace gdp = 839944 if year == 2014


g gdp01 =.
replace gdp01 = 550486*1.1621 if year == 1995
replace gdp01 = 585729*1.1287 if year == 1996
replace gdp01 = 637113*1.1034 if year == 1997
replace gdp01 = 700222*1.0865 if year == 1998
replace gdp01 = 748021*1.0630 if year == 1999
replace gdp01 = 773992*1.0285 if year == 2000
replace gdp01 = 550486*0.9844 if year == 2002
replace gdp01 = 585729*0.9625 if year == 2003
replace gdp01 = 637113*0.9375 if year == 2004
replace gdp01 = 700222*0.9068 if year == 2005
replace gdp01 = 748021*0.8785 if year == 2006
replace gdp01 = 773992*0.8541 if year == 2007
replace gdp01 = 754780*0.8226 if year == 2008
replace gdp01 = 722825*0.8255 if year == 2009
replace gdp01 = 730896*0.8122 if year == 2010
replace gdp01 = 736890*0.7873 if year == 2011
replace gdp01 = 766259*0.7714 if year == 2012
replace gdp01 = 800679*0.7602 if year == 2013
replace gdp01 = 839944*0.7481 if year == 2014

* generate logged networth and income

g lognet = log(networth01+577501)
g loginc = log(income01+15771)


* generate count of time in office

by memberid: g memcount = [_n]


** generate differenced independent variables **

by memberid: g electdif = votemarg - votelag
by memberid: g apropdif = approp - approp[_n-1]
by memberid: g fintaxdif = fintax - fintax[_n-1]
by memberid: g rulesdif = rules - rules[_n-1]
by memberid: g gdpdif = gdp01 - gdp01[_n-1]
by memberid: g agdif = agriculture - agriculture[_n-1]
by memberid: g juddif = judiciary - judiciary[_n-1]
by memberid: g edudif = education - education[_n-1]
by memberid: g healthdif = health - health[_n-1]
by memberid: g votedif = votemarg - votelag
by memberid: g ethicsdif = ethicscmte - ethicscmte[_n-1]
by memberid: g majptydif = majpty - majpty[_n-1]


* generate prelect + prapp variable *

g prexp =.

replace prexp = 0 if prelect == 0 | prapp == 0
replace prexp = 1 if prelect == 1 
replace prexp = 1 if prapp == 1


* Off to get majority-party and minority-party leadership data
* First the speaker of the house:

g speaker = 0
replace speaker = 1 if memberid == 4399 & year == 2014
replace speaker = 1 if memberid == 4399 & year == 2013
replace speaker = 1 if memberid == 4340 & year == 2012
replace speaker = 1 if memberid == 4340 & year == 2011
replace speaker = 1 if memberid == 4264 & year == 2010
replace speaker = 1 if memberid == 4264 & year == 2009
replace speaker = 1 if memberid == 4180 & year == 2008
replace speaker = 1 if memberid == 4180 & year == 2007
replace speaker = 1 if memberid == 4157 & year == 2006
replace speaker = 1 if memberid == 4157 & year == 2005
replace speaker = 1 if memberid == 4117 & year == 2004
replace speaker = 1 if memberid == 4117 & year == 2003 
replace speaker = 1 if memberid == 3084 & year == 2002
replace speaker = 1 if memberid == 3084 & year == 2001
replace speaker = 1 if memberid == 3988 & year == 2000
replace speaker = 1 if memberid == 3988 & year == 1999
replace speaker = 1 if memberid == 2966 & year == 1998
replace speaker = 1 if memberid == 2966 & year == 1997
replace speaker = 1 if memberid == 3007 & year == 1996
replace speaker = 1 if memberid == 3007 & year == 1995

* now the majority leader

g majld = 0
replace majld = 1 if memberid == 4507 & year == 2014
replace majld = 1 if memberid == 4507 & year == 2013
replace majld = 1 if memberid == 4357 & year == 2012
replace majld = 1 if memberid == 4357 & year == 2011
replace majld = 1 if memberid == 4280 & year == 2010
replace majld = 1 if memberid == 4280 & year == 2009
replace majld = 1 if memberid == 4280 & year == 2008
replace majld = 1 if memberid == 4226 & year == 2007
replace majld = 1 if memberid == 4209 & year == 2006
replace majld = 1 if memberid == 4209 & year == 2005
replace majld = 1 if memberid == 4180 & year == 2004
replace majld = 1 if memberid == 4180 & year == 2003
replace majld = 1 if memberid == 4067 & year == 2002
replace majld = 1 if memberid == 4058 & year == 2001
replace majld = 1 if memberid == 4067 & year == 2000
replace majld = 1 if memberid == 4067 & year == 1999
replace majld = 1 if memberid == 3033 & year == 1998
replace majld = 1 if memberid == 3033 & year == 1997
replace majld = 1 if memberid == 3061 & year == 1996
replace majld = 1 if memberid == 3061 & year == 1995

* Now the minority party leader

g minld = 0
replace minld = 1 if memberid == 4384 & year == 2014
replace minld = 1 if memberid == 4384 & year == 2013
replace minld = 1 if memberid == 3041 & year == 2012
replace minld = 1 if memberid == 3041 & year == 2011
replace minld = 1 if memberid == 4354 & year == 2010
replace minld = 1 if memberid == 4354 & year == 2009
replace minld = 1 if memberid == 4225 & year == 2008
replace minld = 1 if memberid == 4225 & year == 2007
replace minld = 1 if memberid == 4168 & year == 2006
replace minld = 1 if memberid == 4168 & year == 2005
replace minld = 1 if memberid == 4125 & year == 2004
replace minld = 1 if memberid == 4125 & year == 2003
replace minld = 1 if memberid == 3046 & year == 2002
replace minld = 1 if memberid == 3046 & year == 2001
replace minld = 1 if memberid == 4008 & year == 2000
replace minld = 1 if memberid == 4008 & year == 1999
replace minld = 1 if memberid == 3053 & year == 1998
replace minld = 1 if memberid == 3053 & year == 1997
replace minld = 1 if memberid == 2966 & year == 1996
replace minld = 1 if memberid == 2966 & year == 1995

* Now generate leadership variable

g leadership = 0
replace leadership = 1 if speaker == 1
replace leadership = 1 if majld == 1
replace leadership = 1 if minld == 1

g leaddif = leadership - leadership[_n-1]

** Generate Majority-Party Variable now that Dem and Minority Party are not synonymous **

g majpty = 0
replace majpty = 1 if dem == 1 & year <= 1996 
replace majpty = 1 if dem == 0 & year >= 1997

** Generate lagged covariates for matching **

sort memberid year
by memberid: g agelag = age[_n-1]
by memberid: g tenurelag = tenure[_n-1]
by memberid: g ruleslag = rules[_n-1]
by memberid: g fintaxlag = fintax[_n-1]
by memberid: g approplag = approp[_n-1]
by memberid: g leaderlag = leadership[_n-1]
by memberid: g agrilag = agriculture[_n-1]
by memberid: g edulag = education[_n-1]
by memberid: g healthlag = health[_n-1]
by memberid: g judiclag = judiciary[_n-1]
by memberid: g difloginc = loginc - loginc[_n-1]
by memberid: g lagloginc = loginc[_n-1]
by memberid: g majptylag = majpty[_n-1]

** Second lag for leadership and committees of interest **

by memberid: g aproplag2 = approp[_n-2]
by memberid: g fintaxlag2 = fintax[_n-2]
by memberid: g ruleslag2 = rules[_n-2]
by memberid: g leaderlag2 = leadership[_n-2]

** Now an Error Correction Model, Bewley Transformation **

xtset memberid year
xtreg D.loginc l.loginc majpty D.majpty votemarg D.votemarg rules D.rules fintax D.fintax approp D.approp leadership D.leadership ethicscmte D.ethicscmte ethicsviol D.ethicsviol agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01
predict deltayhatfull
xtreg loginc deltayhatfull majpty votemarg rules fintax approp leadership tenure ethicscmte ethicsviol agriculture education health judiciary highpost lowpost age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.rules D.fintax D.approp D.leadership D.ethicscmte D.ethicsviol D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)

estimates store m1, title(Model 1: Income Full Model)

xtset memberid year
xtreg D.loginc l.loginc votemarg D.votemarg tenure D.tenure agriculture D.agriculture judiciary D.judiciary education D.education health D.health highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01
predict deltayhatvote
xtreg loginc deltayhatvote votemarg tenure agriculture education health judiciary highpost lowpost age postgrad legal bizman female white gdp01 D.tenure D.votemarg D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)

estimates store m2, title(Model 2: Income Electoral Hypothesis Only)

xtset memberid year
xtreg D.loginc l.loginc majpty D.majpty leadership D.leadership rules D.rules fintax D.fintax approp D.approp leadership D.leadership tenure D.tenure agriculture D.agriculture judiciary D.judiciary education D.education health D.health highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01
predict deltayhataccess
xtreg loginc deltayhataccess majpty leadership rules fintax approp agriculture education health judiciary highpost lowpost age postgrad legal bizman female white gdp01 D.majpty D.leadership tenure D.tenure D.rules D.fintax D.approp D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)

estimates store m3, title(Model 3: Income Access Hypothesis Only)

estout m1 m2 m3, style(tex) cells(b (star fmt(3)) se(par fmt(3))) stats(N)



** Now ECM one-stage, No Bewley Transformation **

xtset memberid year
xtreg D.loginc l.loginc D.votemarg l.votemarg D.leadership l.leadership D.majpty l.majpty D.rules l.rules D.fintax l.fintax D.approp l.approp D.highpost l.highpost D.lowpost l.lowpost D.age l.age D.tenure l.tenure D.postgrad l.postgrad D.legal l.legal D.bizman l.bizman D.female l.female D.white l.white D.gdp01 l.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)
  
estimates store m5, title(Model 5: Income Not Bewley)

xtset memberid year
xtreg D.loginc l.loginc D.votemarg l.votemarg D.leadership l.leadership D.majpty l.majpty D.rules l.rules D.fintax l.fintax D.approp l.approp D.agriculture l.agriculture D.judiciary l.judiciary D.education l.education D.health l.health D.highpost l.highpost D.lowpost l.lowpost D.age l.age D.tenure l.tenure D.postgrad l.postgrad D.legal l.legal D.bizman l.bizman D.female l.female D.white l.white D.gdp01 l.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)
  
estimates store m6, title(Model 6: Model Not Bewley Policy Committees)

estout m5 m6, style(tex) cells(b (star fmt(3)) se(par fmt(3))) stats(N)

** Wow nothing is statistically significant **




** Adding in Schneer Corporate Board Data **

gen board =0
replace board = 5 if memberid == 2943
replace board = 7 if memberid == 2966
replace board = 8 if memberid == 2975
replace board = 9 if memberid == 3033
replace board = 10 if memberid == 3046
replace board = 12 if memberid == 3050
replace board = 9 if memberid == 3069
replace board = 10 if memberid == 3075
replace board = 10 if memberid == 4011
replace board = 20 if memberid == 4046
replace board = 12 if memberid == 4078
replace board = 5 if memberid == 4120
replace board = 6 if memberid == 4121
replace board = 10 if memberid == 4127
replace board = 6 if memberid == 4145
replace board = 10 if memberid == 4152
replace board = 10 if memberid == 4166
replace board = 8 if memberid == 4168
replace board = 21 if memberid == 4169
replace board = 12 if memberid == 4180
replace board = 10 if memberid == 4187
replace board = 2 if memberid == 4191
replace board = 17 if memberid == 4220
replace board = 7 if memberid == 4223
replace board = 18 if memberid == 4229
replace board = 11 if memberid == 4259
replace board = 5 if memberid == 4267
replace board = 21 if memberid == 4271
replace board = 12 if memberid == 4284
replace board = 7 if memberid == 4286
replace board = 5 if memberid == 4362
replace board = 21 if memberid == 4375
replace board = 10 if memberid == 4397
replace board = 9 if memberid == 4459
replace board = 7 if memberid == 4502


gen binboard = 0
replace binboard = 1 if board >= 1



** Committee Chair ** 

* Combine all chairs of all three committees for all years into one dataset *

g chair = 0 

replace chair = 1 if memberid == 3053 & year == 1995
replace chair = 1 if memberid == 3053 & year == 1996
replace chair = 1 if memberid == 2991 & year == 1995
replace chair = 1 if memberid == 2991 & year == 1996
replace chair = 1 if memberid == 2992 & year == 1995
replace chair = 1 if memberid == 2992 & year == 1996
replace chair = 1 if memberid == 3071 & year == 1995
replace chair = 1 if memberid == 3071 & year == 1996
replace chair = 1 if memberid == 3042 & year == 1997
replace chair = 1 if memberid == 3042 & year == 1998
replace chair = 1 if memberid == 3988 & year == 1997
replace chair = 1 if memberid == 3988 & year == 1998
replace chair = 1 if memberid == 3078 & year == 1997
replace chair = 1 if memberid == 3078 & year == 1998
replace chair = 1 if memberid == 3057 & year == 1999
replace chair = 1 if memberid == 3057 & year == 2000
replace chair = 1 if memberid == 3033 & year == 1999
replace chair = 1 if memberid == 3078 & year == 2000
replace chair = 1 if memberid == 3054 & year == 1999
replace chair = 1 if memberid == 3054 & year == 2000
replace chair = 1 if memberid == 4066 & year == 2001
replace chair = 1 if memberid == 4066 & year == 2002
replace chair = 1 if memberid == 4129 & year == 2001
replace chair = 1 if memberid == 4129 & year == 2002
replace chair = 1 if memberid == 4161 & year == 2003
replace chair = 1 if memberid == 4161 & year == 2004
replace chair = 1 if memberid == 4167 & year == 2003
replace chair = 1 if memberid == 4167 & year == 2004
replace chair = 1 if memberid == 4121 & year == 2003
replace chair = 1 if memberid == 4121 & year == 2004
replace chair = 1 if memberid == 4223 & year == 2003
replace chair = 1 if memberid == 4223 & year == 2004
replace chair = 1 if memberid == 4212 & year == 2005
replace chair = 1 if memberid == 4212 & year == 2006
replace chair = 1 if memberid == 4137 & year == 2005
replace chair = 1 if memberid == 4137 & year == 2006
replace chair = 1 if memberid == 4144 & year == 2005
replace chair = 1 if memberid == 4144 & year == 2006
replace chair = 1 if memberid == 4284 & year == 2007
replace chair = 1 if memberid == 4284 & year == 2008
replace chair = 1 if memberid == 4262 & year == 2007
replace chair = 1 if memberid == 4262 & year == 2008
replace chair = 1 if memberid == 4277 & year == 2007
replace chair = 1 if memberid == 4277 & year == 2008
replace chair = 1 if memberid == 4336 & year == 2010
replace chair = 1 if memberid == 4340 & year == 2009
replace chair = 1 if memberid == 4278 & year == 2009
replace chair = 1 if memberid == 4278 & year == 2010
replace chair = 1 if memberid == 4286 & year == 2009
replace chair = 1 if memberid == 4284 & year == 2010
replace chair = 1 if memberid == 4345 & year == 2011
replace chair = 1 if memberid == 4345 & year == 2012
replace chair = 1 if memberid == 4384 & year == 2011
replace chair = 1 if memberid == 4384 & year == 2012
replace chair = 1 if memberid == 4363 & year == 2011
replace chair = 1 if memberid == 4363 & year == 2012
replace chair = 1 if memberid == 4368 & year == 2013
replace chair = 1 if memberid == 4368 & year == 2014
replace chair = 1 if memberid == 4446 & year == 2013
replace chair = 1 if memberid == 4446 & year == 2014
replace chair = 1 if memberid == 4386 & year == 2013
replace chair = 1 if memberid == 4386 & year == 2014

sort memberid year

by memberid: g chairlag = chair[_n-1]
by memberid: g chairlag2 = chair[_n-2]

by memberid: g chairdif = chair - chair[_n-1]


** Now an Error Correction Model, Bewley Transformation **

xtset memberid year
xtreg D.loginc l.loginc majpty D.majpty votemarg D.votemarg rules D.rules fintax D.fintax approp D.approp leadership D.leadership chair D.chair agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01
predict deltayhatfull
xtreg loginc deltayhatfull majpty votemarg rules fintax approp leadership tenure chair agriculture education health judiciary highpost lowpost age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.rules D.fintax D.approp D.leadership D.chair D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)

estimates store mc, title(Model Cmte: Income Full Model)

* Same ECM with just Ethics Committee Variable *

xtset memberid year
xtreg D.loginc l.loginc majpty D.majpty votemarg D.votemarg rules D.rules fintax D.fintax approp D.approp leadership D.leadership ethicscmte D.ethicscmte agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01
predict deltayhatfulla
xtreg loginc deltayhatfulla majpty votemarg rules fintax approp leadership tenure ethicscmte agriculture education health judiciary highpost lowpost age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.rules D.fintax D.approp D.leadership D.ethicscmte D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)

estimates store mc2, title(Model Cmte: Income No Ethics)

estout mc mc2, style(tex) cells(b (star fmt(3)) se(par fmt(3))) stats(N)

** Include second and third lagged income variables **

sort memberid year

by memberid: g loginclag2 = loginc[_n-2]
by memberid: g loginclag3 = loginc[_n-3]



* Create Time Series Models *



* ADL model *
xtset memberid year
xtreg loginc loginclag votemarg leadership majpty chair rules fintax approp agriculture education health judiciary highpost lowpost age postgrad legal bizman female white gdp01 l.votemarg l.leadership l.majpty l.chair l.rules l.fintax l.approp l.agriculture l.education l.health l.judiciary l.highpost l.lowpost l.age l.postgrad l.legal l.bizman l.female l.white l.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)
estimates store adl, title(Model: General ADL Model)
estout adl, style(tex) cells(b (star fmt(3)) se(par fmt(3))) stats(N)


* ECM * 

xtset memberid year
xtreg D.loginc l.loginc majpty D.majpty votemarg D.votemarg rules D.rules fintax D.fintax approp D.approp leadership D.leadership chair D.chair agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01
predict deltayhatfull
xtreg loginc deltayhatfull votemarg leadership majpty chair rules fintax approp agriculture education health judiciary highpost lowpost tenure age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.rules D.fintax D.approp D.leadership D.chair D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)
estimates store ecm, title(Model: General Error Correction Model)
estout ecm, style(tex) cells(b (star fmt(3)) se(par fmt(3))) stats(N)


* Differences Model *

xtset memberid year
xtreg D.loginc D.votemarg D.leadership D.chair D.majpty D.rules D.fintax D.approp D.agriculture D.education D.health D.judiciary D.age D.tenure D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)
estimates store dif, title(Model: Differences Model)
estout dif, style(tex) cells(b (star fmt(3)) se(par fmt(3))) stats(N) 

g altdifinc = loginc - lagloginc

xtset memberid year 
xtreg altdifinc D.votemarg D.leadership D.chair D.majpty D.rules D.fintax D.approp D.agriculture D.education D.health D.judiciary, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)
estimates store dif, title(Model: Differences Model)
estout dif, style(tex) cells(b (star fmt(3)) se(par fmt(3))) stats(N) 



* Dummy for Unopposed *

g unopposed =.
replace unopposed = 1 if votemarg ==1
replace unopposed = 0 if votemarg < 1

* Dummy for Competitive *

g competitive =.
replace competitive = 1 if votemarg <= 0.55
replace competitive = 0 if votemarg > 0.55

g uncompetitive =.
replace uncompetitive = 1 if votemarg > 0.55 & votemarg < 1
replace uncompetitive = 0 if votemarg <= 0.55 | votemarg == 1

* Break up uncompetitive into two groups *

g posscompetitive =.
replace posscompetitive = 1 if votemarg > 0.55 & votemarg < 0.75
replace posscompetitive = 0 if votemarg <= 0.55 | votemarg >= 0.75

g safeuncompetitive =.
replace safeuncompetitive = 1 if votemarg >= 0.75 & votemarg < 1
replace safeuncompetitive = 1 if votemarg < 0.75 | votemarg == 1





* Dummy for GDP growth * 

xtset memberid year
gen gdpgrow = (gdp01[_n]-gdp01[_n-1])/gdp01[_n-1]



* ECM When Removing Redistricting Years *

drop if year == 2010 | year == 2011 | year == 2001 | year == 2002

** Save as 01-24-2017 redist **

xtset memberid year

xtreg D.loginc l.loginc majpty D.majpty votemarg D.votemarg rules D.rules fintax D.fintax approp D.approp leadership D.leadership chair D.chair agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01

predict deltayhatfull

xtreg loginc deltayhatfull votemarg leadership majpty chair rules fintax approp agriculture education health judiciary highpost lowpost tenure age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.rules D.fintax D.approp D.leadership D.chair D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)


* ECM, Electoral Safety Hypothesis Only Redistricting Years*

xtset memberid year

xtreg D. loginc l.loginc votemarg D.votemarg agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01

predict deltayhatfulles

xtreg loginc deltayhatfulles votemarg agriculture education health judiciary highpost lowpost tenure age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.rules D.fintax D.approp D.leadership D.chair D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)


* ECM, When Examining Only Rules$t-1$ Observations *

keep if ruleslag == 1

xtset memberid year

xtreg D.loginc l.loginc majpty D.majpty votemarg D.votemarg rules D.rules fintax D.fintax approp D.approp leadership D.leadership chair D.chair agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01

predict deltayhatfullrl

xtreg loginc deltayhatfullrl votemarg leadership majpty chair rules fintax approp agriculture education health judiciary highpost lowpost tenure age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.rules D.fintax D.approp D.leadership D.chair D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(year)


** ECM With Tax Return Data **

xtset memberid year
xtreg D.loginc l.loginc majpty D.majpty votemarg D.votemarg taxreturn D.taxreturn rules D.rules fintax D.fintax approp D.approp leadership D.leadership chair D.chair agriculture D.agriculture judiciary D.judiciary education D.education health D.health tenure D.tenure highpost D.highpost lowpost D.lowpost age D.age postgrad D.postgrad legal D.legal bizman D.bizman female D.female white D.white gdp01 D.gdp01
predict deltayhatfull
xtreg loginc deltayhatfull votemarg taxreturn leadership majpty chair rules fintax approp agriculture education health judiciary highpost lowpost tenure age postgrad legal bizman female white gdp01 D.tenure D.majpty D.votemarg D.taxreturn D.rules D.fintax D.approp D.leadership D.chair D.agriculture D.education D.health D.judiciary D.highpost D.lowpost D.age D.postgrad D.legal D.bizman D.female D.white D.gdp01, vce(bootstrap, reps(1000) seed(1)) cluster(memberid)
estimates store ecm, title(Model: Tax Return Error Correction Model)


** Create Lagged/Differenced Measure of Tax Return Variable **

xtset memberid year

gen taxreturnlag = taxreturn[_n-1]
gen taxreturndif = (taxreturn - taxreturnlag)/taxreturnlag

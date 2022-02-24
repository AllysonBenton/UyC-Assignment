***************
*Allyson L. Benton, PhD
*www.allysonbenton.com
*allyson.benton@gmail.com

***************
*Replication Files for:
*Benton,Allyson Lucinda (2017) Configuring authority over electoral manipulation in electoral authoritarian regimes: evidence from Mexico, *Democratization, 24:3, 521-543, DOI: 10.1080/13510347.2016.1236789
***************

**************
**Beginning Procedures**

clear

*cd "TO BE DETERMINED/Benton_2017_ReplicationFiles"

use "Benton2017_UyC_Replication.dta", clear


***************
**SUMMARY STATISTICS**

total uyc95, over(uyc92)
total uyc92, over(uyc95)

total priw92m1, over(priw95sd)
total priw95sd, over(priw92m1)

total priw86m
total priw89m
total priw92m1
total priw95sd

*sum pri92m, over(priw92m1)

sum pri95sd cpri95sd pri92m cpri92m pri94d cpri94d

*******Descriptive Statistics / appendix*******

sum pri95sd cpri95sd ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 pop_90 if priw92m1==1

total uyc95 conf89 conf92 conf95 if priw92m1==1

sum pri95sd cpri95sd ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 pop_90 if priw92m1==0

total uyc95 conf89 conf92 conf95 if priw92m1==0

total uyc95 if ind5_90 < 1
total uyc95 if ind5_90 < .75
total uyc95 if ind5_90 < .50
total uyc95 if ind5_90 < .25



*************************************
**assessing the conventional wisdom**
*************************************
**Appendix**

**UyC pre-95 are places with 100% PRI support**

**if not indigenous matters on its own, then top-down imposition**
**if indigenous matters only with herfindahl concentration, then bottom-up organization/demand**


gen uyc89 = pri89m
replace uyc89 = 0 if pri89m<1

logit uyc86 c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc89 c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc92 c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc95 c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc98 c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust

margins, at(ind5_90=(0(.1)1))
marginsplot, ylabel(0(.1)1) xlabel(0(.1)1) recast(line) recastci(rarea) scheme(s1mono)

logit uyc86 c.ind5_90##c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc89 c.ind5_90##c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc92 c.ind5_90##c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc95 c.ind5_90##c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust
logit uyc98 c.ind5_90##c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 c.migrindex00 c.pop_90, robust

margins, at(ind5_90=(0(.1)1) herfin90=(0(.5)1))
marginsplot, ylabel(0(.1)1) xlabel(0(.1)1) recast(line) recastci(rarea) scheme(s1mono)



***********************************************
*****the conventional wisdom*******************
***********************************************

*muni*
regress pri86m c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop c.loc2_90 c.migrindex00 c.lpop_90
regress pri89m c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop c.loc2_90 c.migrindex00 c.lpop_90
regress pri92m c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop c.loc2_90 c.migrindex00 c.lpop_90

*state*
regress pri92sd c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop c.loc2_90 c.migrindex00 c.lpop_90
regress pri95sd c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop c.loc2_90 c.migrindex00 c.lpop_90

*federal*
regress pri91d c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop c.loc2_90 c.migrindex00 c.lpop_90
regress pri94d c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop c.loc2_90 c.migrindex00 c.lpop_90



**************
***Figure 1***
**************
*Figure 1a*
regress pri92sd ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 lpop_90
fitstat
collin ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 lpop_90

coefplot, scheme(s1mono) l(90) ci(90) xline(0) drop(_cons) coeflabel(pri92m = "Mun. PRI 1992" cpri92m = "Chg. Mun. PRI 1989-92" cpri92m2 = "Chg. Mun. PRI 1989-92" pri95sd = "State PRI 1995" cpri95sd = "Chg. State PRI 1992-95" pri94d = "Federal PRI 1994" cpri94d = "Chg. Federal PRI 1991-94" c.pri92m#c.cpri92m = "Muni PRI * Chg. Muni PRI" c.pri92m#c.cpri92m2 = "Muni PRI * Chg. Muni PRI" c.pri95sd#c.cpri95sd = "State PRI * Chg. State PRI" c.pri94d#c.cpri94d = "Fed. PRI * Chg. Fed. PRI" 1.conf89 = "Conflicts 1989" 1.conf92 = "Conflicts 1992" 1.conf95 = "Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop." lpop_90 = "Total Pop. (log)")

*Figure 1b*
regress pri95sd ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 lpop_90
fitstat
collin ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 lpop_90

coefplot, scheme(s1mono) l(90) ci(90) xline(0) drop(_cons) coeflabel(pri92m = "Mun. PRI 1992" cpri92m = "Chg. Mun. PRI 1989-92" cpri92m2 = "Chg. Mun. PRI 1989-92" pri95sd = "State PRI 1995" cpri95sd = "Chg. State PRI 1992-95" pri94d = "Federal PRI 1994" cpri94d = "Chg. Federal PRI 1991-94" c.pri92m#c.cpri92m = "Muni PRI * Chg. Muni PRI" c.pri92m#c.cpri92m2 = "Muni PRI * Chg. Muni PRI" c.pri95sd#c.cpri95sd = "State PRI * Chg. State PRI" c.pri94d#c.cpri94d = "Fed. PRI * Chg. Fed. PRI" 1.conf89 = "Conflicts 1989" 1.conf92 = "Conflicts 1992" 1.conf95 = "Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop." lpop_90 = "Total Pop. (log)")



***********************************************
**Models for PRI-Run municipalities************
***********************************************

****
*assessing collinearity*
****
logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1

logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1, or
lfit, group(10)

collin uyc95 pri95sd cpri95sd  conf89  conf92  conf95  ind5_90 herfin90  im_90  cat5_90  ej91ppop loc2_90 migrindex00  pop_90 if priw92m1==1
*colin seems not to be a problem*

regress pri95sd cpri95sd  conf89  conf92  conf95  ind5_90 herfin90  im_90  cat5_90  ej91ppop loc2_90 migrindex00  pop_90 if priw92m1==1
*R2 = 0.4768, Tolerance = 1 - 0.4768 = .5232, The VIF is 1/.5232 =  1.91; A Tolerance of 0.1 or less (equivalently VIF of 10 or greater) is a cause for concern; so I am not concerned here.*
* I conclude that colin is not a prblem*


****
*assessing goodness of fit**
****

logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1

estat gof, group(20) table
estat gof, group(10) table
estat gof, group(5) table
*pr stays insig, so gof is ok*


****
*assessing model specification, misspecification**
****

logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1
linktest, nolog
*check to see _hatsq is not significant (linktest is not significant). If insig, then model is probably adequately specified*

****
*assessing the presence of outlying observations*
****

*Pearson residual, deviance residual and Pregibon leverage are considered to be the three basic building blocks for logistic regression diagnostics. 
*A good way of looking at them is to graph them against either the predicted probabilities or simply case numbers.

*Pearson residual*
*Pearson residuals are defined to be the standardized difference between the observed frequency and the predicted frequency. 
*They measure the relative deviations between the observed and fitted values. 

*Deviance residual*
*Deviance residual is another type of residual. It measures the disagreement between the maxima of the observed and the fitted log likelihood functions. 
*Since logistic regression uses the maximal likelihood principle, the goal in logistic regression is to minimize the sum of the deviance residuals. 
*Therefore, this residual is parallel to the raw residual in OLS regression, where the goal is to minimize the sum of squared residuals.

*Pregibon leverage*
*The hat diagonal since technically it is the diagonal of the hat matrix, measures the leverage of an observation.

*get predicted probability*
predict pprob

summarize pprob
histogram pprob

*get Pearson residual*
predict stdres, rstand

*get Deviance residual*
predict dv, dev

*get Pregibon leverage or hat diagonal*
predict h, hat

scatter stdres pprob, mlab(inegi) yline(0)
scatter stdres inegi, mlab(inegi) yline(0)

scatter dv pprob, mlab(inegi) yline(0)
scatter dv inegi, mlab(inegi) 

scatter h pprob, mlab(inegi)  yline(0)
scatter h inegi, mlab(inegi)

clist if inegi==20059
clist if inegi==20204

logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1
lfit

logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1 & inegi!=20059 & inegi!=20204
lfit

*these two observations (above) run contrary to the hypotheses, but they are not influential observations, so I keep them in the analysis below*

****
*assessing influential cases*
****

* We have seen quite a few logistic regression diagnostic statistics. Now how large does each one have to be, to be considered influential? First of all, we always have to make our judgment based on our theory and our analysis. Secondly, there are some rule-of-thumb cutoffs when the sample size is large. These are shown below. When the sample size is large, the asymptotic distribution of some of the measures would follow some standard distribution. That is why we have these cutoff values, and why they only apply when the sample size is large enough. Usually, we would look at the relative magnitude of a statistic an observation has compared to others. That is, we look for data points that are farther away from most of the data points.
* dx2 stands for the difference of chi-squares and dd stands for the difference of deviances*
* high/outlying dx2 means that when this observation is excluded from the analysis, the Pearson chi-square fit statistic will decrease by the amount shown*
* high dd means that when this observation is included, it will increase the deviance about the amount shown
* Run analyses with and without any extreme cases and compare their Pearson chi-squares to see if this is the case*

predict dx2, dx2
predict dd, dd
predict db, dbeta

scatter dx2 inegi, mlab(inegi)
scatter dd inegi, mlab(inegi)

scatter dd pprob if uyc95==1 [w=db], msym(Oh) jitter(2) mlabel(inegi)
scatter dd pprob if uyc95==0 [w=db], msym(Oh) jitter(2) mlabel(inegi)

twoway (scatter dd pprob if uyc95==1, msym(Oh) jitter(2) mlabel(inegi)) (scatter dd pprob if uyc95==0, msym(Oh) jitter(2) mlabel(inegi))      

scatter dx2 pprob if uyc95==1 [w=db], msym(Oh) jitter(2) mlabel(inegi)
scatter dx2 pprob if uyc95==0 [w=db], msym(Oh) jitter(2) mlabel(inegi)

twoway (scatter dx2 pprob if uyc95==1, msym(Oh) jitter(2) mlabel(inegi)) (scatter dx2 pprob if uyc95==0, msym(Oh) jitter(2) mlabel(inegi))      


leastlikely uyc95 pri95sd cpri95sd  conf89  conf92  conf95  ind5_90 herfin90  im_90  cat5_90  ej91ppop loc2_90 migrindex00  pop_90 if priw92m1==1


************
***MODELS***
************

*pri munis*
*state elections*

*Figure 2*
logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1
*Figure 2a*
coefplot, scheme(s1mono) l(90) ci(90) xline(0) drop(_cons ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 pop_90 ) coeflabel(pri92m = "Mun. PRI 1992" cpri92m = "Chg. Mun. PRI 1989-92" cpri92m2 = "Chg. Mun. PRI 1989-92" pri95sd = "State PRI 1995" cpri95sd = "Chg. State PRI 1992-95" pri94d = "Federal PRI 1994" cpri94d = "Chg. Federal PRI 1991-94" c.pri92m#c.cpri92m = "Muni PRI * Chg. Muni PRI" c.pri92m#c.cpri92m2 = "Muni PRI * Chg. Muni PRI" c.pri95sd#c.cpri95sd = "State PRI * Chg. State PRI" c.pri94d#c.cpri94d = "Fed. PRI * Chg. Fed. PRI" 1.conf89 = "Conflicts 1989" 1.conf92 = "Conflicts 1992" 1.conf95 = "Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")
*Figure 2b*
coefplot, scheme(s1mono) l(90) ci(90) xline(0) drop(_cons pri95sd cpri95sd c.pri95sd#c.cpri95sd 1.conf89 1.conf92 1.conf95 ) coeflabel(pri92m = "Mun. PRI 1992" cpri92m = "Chg. Mun. PRI 1989-92" cpri92m2 = "Chg. Mun. PRI 1989-92" pri95sd = "State PRI 1995" cpri95sd = "Chg. State PRI 1992-95" pri94d = "Federal PRI 1994" cpri94d = "Chg. Federal PRI 1991-94" c.pri92m#c.cpri92m = "Muni PRI * Chg. Muni PRI" c.pri92m#c.cpri92m2 = "Muni PRI * Chg. Muni PRI" c.pri95sd#c.cpri95sd = "State PRI * Chg. State PRI" c.pri94d#c.cpri94d = "Fed. PRI * Chg. Fed. PRI" 1.conf89 = "Conflicts 1989" 1.conf92 = "Conflicts 1992" 1.conf95 = "Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")

*Figure 3a*
margins, at(pri95sd=(0(.1)1)) l(90)
marginsplot, l(90) ylabel(0(.1)1) xlabel(0(.1)1) recast(line) recastci(rarea) scheme(s1mono)

*Figure 3b*
margins, at(pri95sd=(0(.1).6) cpri95sd=(-.40 0)) l(90)
marginsplot, l(90) ylabel(0(.1)1) xlabel(0(.1)1) recast(line) recastci(rarea) scheme(s1mono)

*Figure 4a*
margins, at(pri95sd=(0(.1)1) conf92=(0 1)) l(90)
marginsplot, l(90) ylabel(0(.1)1) xlabel(0(.1)1) recast(line) recastci(rarea) scheme(s1mono)

*Figure 4b*
margins, at(pri95sd=(0(.1)1) cpri95sd=(-.40 0) conf92=(0 1)) l(90)
marginsplot, l(90) ylabel(0(.1)1) xlabel(0(.1)1) recast(line) recastci(rarea) scheme(s1mono)


*Figure 5*

*pri munis*
*showing federal doesn't matter*
logit uyc95 c.pri95sd##c.cpri95sd c.pri94d##c.cpri94d i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1 
margins, at(pri94d=(0(.1)1) cpri94d=(-.40 0))
marginsplot, ylabel(0(.1)1) xlabel(0(.1)1) recast(line) recastci(rarea) scheme(s1mono)
coefplot, scheme(s1mono) l(90) ci(90) xline(0) drop(_cons) coeflabel(pri92m = "Mun. PRI 1992" cpri92m = "Chg. Mun. PRI 1989-92" cpri92m2 = "Chg. Mun. PRI 1989-92" pri95sd = "State PRI 1995" cpri95sd = "Chg. State PRI 1992-95" pri94d = "Federal PRI 1994" cpri94d = "Chg. Federal PRI 1991-94" c.pri92m#c.cpri92m = "Muni PRI * Chg. Muni PRI" c.pri92m#c.cpri92m2 = "Muni PRI * Chg. Muni PRI" c.pri95sd#c.cpri95sd = "State PRI * Chg. State PRI" c.pri94d#c.cpri94d = "Fed. PRI * Chg. Fed. PRI" 1.conf89 = "Conflicts 1989" 1.conf92 = "Conflicts 1992" 1.conf95 = "Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")


**************************************
****Models for Opposition-Run Munis***
**************************************

****
*assessing influential observations*
****

*Pearson residual, deviance residual and Pregibon leverage are considered to be the three basic building blocks for logistic regression diagnostics. 
*A good way of looking at them is to graph them against either the predicted probabilities or simply case numbers.

*Pearson residual*
*Pearson residuals are defined to be the standardized difference between the observed frequency and the predicted frequency. 
*They measure the relative deviations between the observed and fitted values. 

*Deviance residual*
*Deviance residual is another type of residual. It measures the disagreement between the maxima of the observed and the fitted log likelihood functions. 
*Since logistic regression uses the maximal likelihood principle, the goal in logistic regression is to minimize the sum of the deviance residuals. 
*Therefore, this residual is parallel to the raw residual in OLS regression, where the goal is to minimize the sum of squared residuals.

*Pregibon leverage*
*The hat diagonal since technically it is the diagonal of the hat matrix, measures the leverage of an observation.

*get predicted probability*
predict pprob2

summarize pprob2
histogram pprob2

*get Pearson residual*
predict stdres2, rstand

*get Deviance residual*
predict dv2, dev

*get Pregibon leverage or hat diagonal*
predict h2, hat

scatter stdres2 pprob2, mlab(inegi) yline(0)
scatter stdres2 inegi, mlab(inegi) yline(0)

scatter dv2 pprob2, mlab(inegi) yline(0)
*scatter dv2 inegi2, mlab(inegi) 

scatter h2 pprob2, mlab(inegi)  yline(0)
scatter h2 inegi, mlab(inegi)

clist if inegi==20059

logit uyc95 oppw92m1##c.pri95sd##c.cpri95sd oppw92m1##i.conf89 oppw92m1##i.conf92 oppw92m1##i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if inegi!=20059 & inegi!=20469
lfit

logit uyc95 oppw92m1##c.pri95sd##c.cpri95sd oppw92m1##i.conf89 oppw92m1##i.conf92 oppw92m1##i.conf95 c.ind5_9 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if inegi!=20059 & inegi!=20469
lfit

*inegi=20059 changes the indigenous variable to significant*

predict dx2, dx2
predict dd, dd

scatter dx2 inegi, mlab(inegi)
scatter dd inegi, mlab(inegi)

clist if inegi==20059

logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1
lfit

logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1 & inegi!=20059
lfit

di 530.97 - 487.40

scatter dx2 inegi if uyc95==1 [w=db], msym(Oh) jitter(2) mlabel(inegi)
scatter dx2 inegi if uyc95==0 [w=db], msym(Oh) jitter(2) mlabel(inegi)

scatter dd inegi if uyc95==1 [w=db], msym(Oh) jitter(2) mlabel(inegi)
scatter dd inegi if uyc95==0 [w=db], msym(Oh) jitter(2) mlabel(inegi)

twoway (scatter dx2 pprob if uyc95==1, msym(Oh) jitter(2) mlabel(inegi)) (scatter dx2 pprob if uyc95==0, msym(Oh) jitter(2) mlabel(inegi))      

scatter dx2 pprob if uyc95==1 [w=db], msym(Oh) jitter(2) mlabel(inegi)
scatter dx2 pprob if uyc95==0 [w=db], msym(Oh) jitter(2) mlabel(inegi)

leastlikely uyc95 pri95sd cpri95sd  conf89  conf92  conf95  ind5_90 herfin90  im_90  cat5_90  ej91ppop loc2_90 migrindex00  pop_90 if priw92m1==1



***********
*OPP munis*
***********

*opp munis*
*state elections*

*Figure 6*
logit uyc95 oppw92m1##c.pri95sd##c.cpri95sd oppw92m1##i.conf89 oppw92m1##i.conf92 oppw92m1##i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90
coefplot, scheme(s1mono) l(90) xline(0) drop(_cons 1.oppw92m1 1.oppw92m1#c.pri95sd 1.oppw92m1#c.pri95sd#c.cpri95sd 1.oppw92m1#c.cpri95sd 1.oppw92m1#1.conf89 1.oppw92m1#1.conf92 1.oppw92m1#1.conf95) coeflabel(pri92m = "PRI: Mun. PRI 1992" cpri92m = "PRI: Chg. Mun. PRI 1989-92" cpri92m2 = "PRI: Chg. Mun. PRI 1989-92" pri92m = "PRI: Mun. PRI 1992" cpri92m = "PRI: Chg. Mun. PRI 1989-92" cpri92m2 = "PRI: Chg. Mun. PRI 1989-92" pri95sd = "PRI: State PRI 1995" cpri95sd = "PRI: Chg. State PRI 1992-95" pri94d = "PRI: Federal PRI 1994" cpri94d = "PRI: Chg. Federal PRI 1991-94" c.pri92m#c.cpri92m = "PRI: Muni PRI * Chg. Muni PRI" c.pri92m#c.cpri92m2 = "PRI: Muni PRI * Chg. Muni PRI" c.pri95sd#c.cpri95sd = "PRI: State PRI * Chg. State PRI" c.pri94d#c.cpri94d = "PRI: Fed. PRI * Chg. Fed. PRI" 1.conf89 = "PRI: Conflicts 1989" 1.conf92 = "PRI: Conflicts 1992" 1.conf95 = "PRI: Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")
coefplot, scheme(s1mono) l(90) xline(0) drop(_cons 1.oppw92m1 pri95sd cpri95sd c.pri95sd#c.cpri95sd 1.conf89 1.conf92 1.conf95) coeflabel(1.oppw92m1#c.pri92m = "OPP: Mun. PRI 1992" 1.oppw92m1#c.cpri92m = "OPP: Chg. Mun. PRI 1989-92" 1.oppw92m1#c.cpri92m2 = "OPP: Chg. Mun. PRI 1989-92" 1.oppw92m1#c.pri95sd = "OPP: State PRI 1995" 1.oppw92m1#c.cpri95sd = "OPP: Chg. State PRI 1992-95" 1.oppw92m1#c.pri94d = "OPP: Federal PRI 1994" 1.oppw92m1#c.cpri94d = "OPP: Chg. Federal PRI 1991-94" 1.oppw92m1#c.pri92m#c.cpri92m = "OPP: Muni PRI * Chg. Muni PRI" 1.oppw92m1#c.pri92m#c.cpri92m2 = "OPP: Muni PRI * Chg. Muni PRI" 1.oppw92m1#c.pri95sd#c.cpri95sd = "OPP: State PRI * Chg. State PRI" 1.oppw92m1#c.pri94d#c.cpri94d = "OPP: Fed. PRI * Chg. Fed. PRI" 1.oppw92m1#1.conf89 = "OPP: Conflicts 1989" 1.oppw92m1#1.conf92 = "OPP: Conflicts 1992" 1.oppw92m1#1.conf95 = "OPP: Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")


gen oppw92m1_pri95 = oppw92m1*pri95sd
gen oppw92m1_cpri95 = oppw92m1*cpri95sd
gen oppw92m1_conf89 = oppw92m1*conf89
gen oppw92m1_conf92 = oppw92m1*conf92
gen oppw92m1_conf95 = oppw92m1*conf95

collin pri95sd oppw92m1_pri95 cpri95sd  oppw92m1_cpri95 conf89  conf92  conf95  oppw92m1_conf89 oppw92m1_conf92 oppw92m1_conf95 ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 pop_90


*Figure 6a*
coefplot, scheme(s1mono) l(90) xline(0) drop(_cons 1.oppw92m1 1.oppw92m1#c.pri95sd 1.oppw92m1#c.pri95sd#c.cpri95sd 1.oppw92m1#c.cpri95sd 1.oppw92m1#1.conf89 1.oppw92m1#1.conf92 1.oppw92m1#1.conf95 ind5_90 herfin90 c.ind5_90#c.herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 pop_90) coeflabel(pri92m = "PRI: Mun. PRI 1992" cpri92m = "PRI: Chg. Mun. PRI 1989-92" cpri92m2 = "PRI: Chg. Mun. PRI 1989-92" pri92m = "PRI: Mun. PRI 1992" cpri92m = "PRI: Chg. Mun. PRI 1989-92" cpri92m2 = "PRI: Chg. Mun. PRI 1989-92" pri95sd = "PRI: State PRI 1995" cpri95sd = "PRI: Chg. State PRI 1992-95" pri94d = "PRI: Federal PRI 1994" cpri94d = "PRI: Chg. Federal PRI 1991-94" c.pri92m#c.cpri92m = "PRI: Muni PRI * Chg. Muni PRI" c.pri92m#c.cpri92m2 = "PRI: Muni PRI * Chg. Muni PRI" c.pri95sd#c.cpri95sd = "PRI: State PRI * Chg. State PRI" c.pri94d#c.cpri94d = "PRI: Fed. PRI * Chg. Fed. PRI" 1.conf89 = "PRI: Conflicts 1989" 1.conf92 = "PRI: Conflicts 1992" 1.conf95 = "PRI: Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")

*Figure 6b*
coefplot, scheme(s1mono) l(90) xline(0) drop(_cons 1.oppw92m1 pri95sd cpri95sd c.pri95sd#c.cpri95sd 1.conf89 1.conf92 1.conf95 ind5_90 herfin90 c.ind5_90#c.herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 pop_90) coeflabel(1.oppw92m1#c.pri92m = "OPP: Mun. PRI 1992" 1.oppw92m1#c.cpri92m = "OPP: Chg. Mun. PRI 1989-92" 1.oppw92m1#c.cpri92m2 = "OPP: Chg. Mun. PRI 1989-92" 1.oppw92m1#c.pri95sd = "OPP: State PRI 1995" 1.oppw92m1#c.cpri95sd = "OPP: Chg. State PRI 1992-95" 1.oppw92m1#c.pri94d = "OPP: Federal PRI 1994" 1.oppw92m1#c.cpri94d = "OPP: Chg. Federal PRI 1991-94" 1.oppw92m1#c.pri92m#c.cpri92m = "OPP: Muni PRI * Chg. Muni PRI" 1.oppw92m1#c.pri92m#c.cpri92m2 = "OPP: Muni PRI * Chg. Muni PRI" 1.oppw92m1#c.pri95sd#c.cpri95sd = "OPP: State PRI * Chg. State PRI" 1.oppw92m1#c.pri94d#c.cpri94d = "OPP: Fed. PRI * Chg. Fed. PRI" 1.oppw92m1#1.conf89 = "OPP: Conflicts 1989" 1.oppw92m1#1.conf92 = "OPP: Conflicts 1992" 1.oppw92m1#1.conf95 = "OPP: Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")

*Figure 6c*
coefplot, scheme(s1mono) l(90) xline(0) drop(_cons 1.oppw92m1 pri95sd cpri95sd c.pri95sd#c.cpri95sd 1.conf89 1.conf92 1.conf95 1.oppw92m1#c.pri95sd 1.oppw92m1#c.pri95sd#c.cpri95sd 1.oppw92m1#c.cpri95sd 1.oppw92m1#1.conf89 1.oppw92m1#1.conf92 1.oppw92m1#1.conf95) coeflabel(1.oppw92m1#c.pri92m = "OPP: Mun. PRI 1992" 1.oppw92m1#c.cpri92m = "OPP: Chg. Mun. PRI 1989-92" 1.oppw92m1#c.cpri92m2 = "OPP: Chg. Mun. PRI 1989-92" 1.oppw92m1#c.pri95sd = "OPP: State PRI 1995" 1.oppw92m1#c.cpri95sd = "OPP: Chg. State PRI 1992-95" 1.oppw92m1#c.pri94d = "OPP: Federal PRI 1994" 1.oppw92m1#c.cpri94d = "OPP: Chg. Federal PRI 1991-94" 1.oppw92m1#c.pri92m#c.cpri92m = "OPP: Muni PRI * Chg. Muni PRI" 1.oppw92m1#c.pri92m#c.cpri92m2 = "OPP: Muni PRI * Chg. Muni PRI" 1.oppw92m1#c.pri95sd#c.cpri95sd = "OPP: State PRI * Chg. State PRI" 1.oppw92m1#c.pri94d#c.cpri94d = "OPP: Fed. PRI * Chg. Fed. PRI" 1.oppw92m1#1.conf89 = "OPP: Conflicts 1989" 1.oppw92m1#1.conf92 = "OPP: Conflicts 1992" 1.oppw92m1#1.conf95 = "OPP: Conflicts 1995" cat5_90 = "Catholic Pop." loc2_90 = "Rural Pop." ej91ppop = "Communal Pop." ind5_90 = "Indigenous Pop." herfin90 = "Indig. Herf. Index" c.ind5_90#c.herfin90 = "Indig. * Herf. Index" i_egr95 = "Fiscal Resources" im_90 = "Poverty Rate" pop_90 = "Total Pop.")


*Figure 7a*
margins if oppw92m1==1, at(pri95sd=(.2(.1).9) cpri95sd=(0)) l(90)
marginsplot, l(90) ylabel(0(.1)1) xlabel(.2(.1).9) recast(line) recastci(rarea) scheme(s1mono)




**************************************************************
**************************************************************
**MAKING Appendix**********************************************
**************************************************************
**************************************************************


**Appendix 1**

eststo clear
quietly regress pri92sd ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 lpop_90
eststo
quietly regress pri95sd ind5_90 herfin90 im_90 cat5_90 ej91ppop loc2_90 migrindex00 lpop_90
eststo
esttab using excel3.csv, se star(* 0.10 ** 0.05 *** 0.01) pr2 bic scalars("ll Log-Likehd." "chi2 Chi-Squ.") compress




**Appendix 2**

eststo clear
**Model 1 above**
quietly logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1
eststo
**Model 2 above**
quietly logit uyc95 oppw92m1##c.pri95sd##c.cpri95sd oppw92m1##i.conf89 oppw92m1##i.conf92 oppw92m1##i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90
eststo
**Model 3 above**
quietly logit uyc95 c.pri95sd##c.cpri95sd c.pri94d##c.cpri94d i.conf89 i.conf92 i.conf95 c.ind5_90 c.herfin90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1
eststo
**Model 4 without herfin*
quietly logit uyc95 c.pri95sd##c.cpri95sd i.conf89 i.conf92 i.conf95 c.ind5_90 c.im_90 c.cat5_90 c.ej91ppop loc2_90 migrindex00 c.pop_90 if priw92m1==1
eststo

esttab using excel3.csv, se star(* 0.10 ** 0.05 *** 0.01) pr2 bic scalars("ll Log-Likehd." "chi2 Chi-Squ.") compress


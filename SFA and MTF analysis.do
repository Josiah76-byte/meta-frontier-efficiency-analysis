//// Meta Frontier Efficiency Analysis/////
clear all
set more off

***Section 1. Data preparation

use camel_milk_data.dta, clear

generate lnproft = ln(profit)  
generate lnlab = ln(labor)
generate lnbuy = ln(buyprice)
generate lncap = ln(capital)
**POOLED STOCHASTIC FRONTIER
// stochstic frontier pooled sample
sfcross lnproft lnlab lnbuy lncap  lnlab2 lnbuy2  lncap2 lnlabbuy lnbuycap lnlabcap  , model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PEp, jlms
***COUNTY STOCHASTIC FRONTIER
// stochastic for garissa
sfcross lnproft  lnlab  lnbuy lncap lnlab2  lnbuy2 lncap2 lnlabbuy lnbuycap lnlabcap if county==1, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PEg if county==1, jlms
// stochastic for marisabit
sfcross lnproft  lnlab  lnbuy lncap lnlab2 lnbuy2 lncap2 lnlabbuy lnbuycap lnlabcap if county==2, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PEm if county==2, jlms
// stochastic for Isiolo
sfcross lnproft lnlab lnbuy lncap  lnlab2 lnbuy2  lncap2 lnlabbuy lnbuycap lnlabcap if county==3, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PEi if county==3, jlms
// stochastic for turkana
sfcross lnproft lnlab  lnbuy lncap lnlab2 lnbuy2  lncap2 lnlabbuy lnbuycap lnlabcap if county==4, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PEt if county==4, jlms
// stochastic for Wajir
sfcross lnproft lnlab lnbuy lncap lnlab2 lnbuy2  lncap2 lnlabbuy lnbuycap lnlabcap if county==5, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PEw if county==5, jlms

//Stochastic meta-frontier analysis//
// stochstic frontier pooled sample
sfcross lnproft  lnlab lnbuy lncap  lnlab2 lnbuy2  lncap2 lnlabbuy lnbuycap lnlabcap  , model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PE, jlms

 /*SFA for garissa sample*/
 sfcross lnproft  lnlab  lnbuy lncap lnlab2  lnbuy2 lncap2 lnlabbuy lnbuycap lnlabcap if county==1, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PE1 if county==1, jlms

/*Generating frontier for garissa*/
gen glab=_b[lnlab]*lnlab if county==1
gen gbuy=_b[lnbuy]*lnbuy if county==1
gen gcap=_b[lncap]*lncap if county==1
gen glab2=_b[lnlab2]*lnlab2 if county==1
gen gbuy2=_b[lnbuy2]*lnbuy2 if county==1
gen gcap2=_b[lncap2]*lncap2 if county==1
gen glabnbuy=_b[lnlabbuy]*lnlabbuy if county==1
gen gbuyncap=_b[lnbuycap]*lnbuycap if county==1
gen glabncap=_b[lnlabcap]*lnlabcap if county==1
gen gY= 11.33998+glab+gbuy+gcap+glab2+gbuy2+gcap2+glabnbuy+gbuyncap+glabncap

 /*SFA for marisabit sample*/
 sfcross lnproft  lnlab  lnbuy lncap lnlab2  lnbuy2 lncap2 lnlabbuy lnbuycap lnlabcap if county==2, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PE2 if county==2, jlms

/*Generating frontier for marsabit*/
gen mlab=_b[lnlab]*lnlab if county==2
gen mbuy=_b[lnbuy]*lnbuy if county==2
gen mcap=_b[lncap]*lncap if county==2
gen mlab2=_b[lnlab2]*lnlab2 if county==2
gen mbuy2=_b[lnbuy2]*lnbuy2 if county==2
gen mcap2=_b[lncap2]*lncap2 if county==2
gen mlabnbuy=_b[lnlabbuy]*lnlabbuy if county==2
gen mbuyncap=_b[lnbuycap]*lnbuycap if county==2
gen mlabncap=_b[lnlabcap]*lnlabcap if county==2
gen mY=  9.672129+mlab+mbuy+mcap+glab2+mbuy2+gcap2+mlabnbuy+mbuyncap+mlabncap

/*SFA for isiolo sample*/
 sfcross lnproft  lnlab  lnbuy lncap lnlab2  lnbuy2 lncap2 lnlabbuy lnbuycap lnlabcap if county==3, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PE3 if county==3, jlms

/*Generating frontier for isiolo*/
gen ilab=_b[lnlab]*lnlab if county==3
gen ibuy=_b[lnbuy]*lnbuy if county==3
gen icap=_b[lncap]*lncap if county==3
gen ilab2=_b[lnlab2]*lnlab2 if county==3
gen ibuy2=_b[lnbuy2]*lnbuy2 if county==3
gen icap2=_b[lncap2]*lncap2 if county==3
gen ilabnbuy=_b[lnlabbuy]*lnlabbuy if county==3
gen ibuyncap=_b[lnbuycap]*lnbuycap if county==3
gen ilabncap=_b[lnlabcap]*lnlabcap if county==3
gen iY=10.76253 +ilab+ibuy+icap+ilab2+ibuy2+icap2+ilabnbuy+ibuyncap+ilabncap

/*SFA for turkana sample*/
 sfcross lnproft  lnlab  lnbuy lncap lnlab2  lnbuy2 lncap2 lnlabbuy lnbuycap lnlabcap if county==4, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PE4 if county==4, jlms

/*Generating frontier for turkana*/
gen tlab=_b[lnlab]*lnlab if county==4
gen tbuy=_b[lnbuy]*lnbuy if county==4
gen tcap=_b[lncap]*lncap if county==4
gen tlab2=_b[lnlab2]*lnlab2 if county==4
gen tbuy2=_b[lnbuy2]*lnbuy2 if county==4
gen tcap2=_b[lncap2]*lncap2 if county==4
gen tlabnbuy=_b[lnlabbuy]*lnlabbuy if county==4
gen tbuyncap=_b[lnbuycap]*lnbuycap if county==4
gen tlabncap=_b[lnlabcap]*lnlabcap if county==4
gen tY=11.07909 +tlab+tbuy+tcap+ilab2+tbuy2+tcap2+tlabnbuy+tbuyncap+tlabncap

/*SFA for wajir sample*/
 sfcross lnproft  lnlab  lnbuy lncap lnlab2  lnbuy2 lncap2 lnlabbuy lnbuycap lnlabcap if county==5, model (bc) dist(tn) ort(o) emean(i.( gender traing process )  age experience distnce )
predict PE5 if county==5, jlms

/*Generating frontier for wajir*/
gen wlab=_b[lnlab]*lnlab if county==5
gen wbuy=_b[lnbuy]*lnbuy if county==5
gen wcap=_b[lncap]*lncap if county==5
gen wlab2=_b[lnlab2]*lnlab2 if county==5
gen wbuy2=_b[lnbuy2]*lnbuy2 if county==5
gen wcap2=_b[lncap2]*lncap2 if county==5
gen wlabnbuy=_b[lnlabbuy]*lnlabbuy if county==5
gen wbuyncap=_b[lnbuycap]*lnbuycap if county==5
gen wlabncap=_b[lnlabcap]*lnlabcap if county==5
gen wY= 9.72312  +tlab+tbuy+tcap+ilab2+tbuy2+tcap2+tlabnbuy+tbuyncap+tlabncap
**META FRONTIER ESTIMATION
/*Meta Frontier*/
gen laborr=max(glab, mlab, ilab, tlab, wlab)
gen pricebuy=max(gbuy, mbuy, ibuy, tbuy, wbuy)
gen capitall=max(gcap, mcap, icap, tcap, wcap)
gen laborr2=max(glab2, mlab2, ilab2, tlab2, wlab2)
gen pricebuy2=max(gbuy2, mbuy2, ibuy2, tbuy2, wbuy2)
gen capitall2=max(gcap2, mcap2, icap2, tcap2, wcap2)
gen LnB=max(glabnbuy, mlabnbuy, ilabnbuy, tlabnbuy, wlabnbuy)
gen BnC=max(gbuyncap, mbuyncap, ibuyncap, tbuyncap, wbuyncap)
gen LnC=max(glabncap, mlabncap, ilabncap, tlabncap, wlabncap)
gen Ypool=max(gY, mY, iY, tY, wY)
sfcross Ypool laborr pricebuy capitall laborr2 pricebuy2 capitall2 LnB BnC LnC, model(bc) dist(tn) ort(o)
predict MTE, jlms

gen mtlab=_b[laborr]*laborr
gen mtbuy=_b[pricebuy]*pricebuy
gen mtcap=_b[capitall]*capitall
gen mtlab2=_b[laborr2]*laborr2
gen mtbuy2=_b[pricebuy2]*pricebuy2
gen mtcap2=_b[capitall2]*capitall2
gen mtLnB=_b[LnB]*LnB
gen mtBnC=_b[BnC]*BnC
gen mtLnC=_b[LnC]*LnC
gen Y_star=  9.75265 +mtlab+mtbuy+mtcap+mtlab2+mtbuy2+mtcap2+mtLnB+mtBnC+mtLnC

/*profit efficiecy Gap Ratios*/
gen pegrg=gY/Y_star
gen pegrm=mY/Y_star
gen pegri=iY/Y_star
gen pegrt=tY/Y_star
gen pegrw=wY/Y_star
/*Meta Technical Efficiency*/
gen mteg=pegrg*PE1
gen mtem=pegrm*PE2
gen mtei=pegri*PE3
gen mtet=pegrt*PE4
gen mtew=pegrw*PE5

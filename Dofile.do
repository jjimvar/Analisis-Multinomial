cls
cd "C:\Users\jjime\OneDrive\Escritorio\3ro GANE\Mercados\Stata\2"
use  "Datos.dta", clear

*PARTE I: ANÁLISIS ESTADÍSTICO --> destacar dificultad alternativas independientes
**Gasto Azucar
sum gasto_azucar, detail
gen gast=gastot_pc/12
gen gasrel=(gasto_azucar/gast)*100
sum gasrel, detail
egen a60=pctile(gasrel), p(60) 
replace gasrel=0 if gasrel==.
gen gasazu=0
replace gasazu=1 if gasrel>0 & gasrel<a60
replace gasazu=2 if gasrel>=a60
label define az 0 "Nulo" 1 "Medio-Bajo" 2 "Medio-Alto"
label value gasazu az
tab gasazu
**Renta
gen rent=renta_pc/12
egen r1=pctile(rent), p(1)
keep if rent>=r1
replace rent=rent/100
sum gast rent, detail
**Bien complementario
replace gasto_cafenormal=0 if gasto_cafenormal==.
replace gasto_cafecapsulas=0 if gasto_cafecapsulas==.
replace gasto_lecheentera=0 if gasto_lecheentera==.
replace gasto_lechedescremada=0 if gasto_lechedescremada==.
gen caflec=(gasto_cafenormal+gasto_cafecapsulas+gasto_lecheentera+gasto_lechedescremada)/100
sum caflec, detail
**Educación
gen educ=.
replace educ=0 if estudios<=3
replace educ=1 if estudios>=4
label define ed 0 "Educación Obligatoria" 1 "Educación Posobligatoria"
label values educ ed
**Edad
gen edad=edadsp/10
**Global
global x rent caflec educ edad densidad_alta densidad_media

*PARTE II: MODELIZACIÓN
**Analizamos modelo inicial y probabilidades de consumo
mlogit gasazu $x, robust
margins
**Analizamos efectos marginales
margins, dydx (*)ç
***Sobre la renta
sum rent, detail
scalar a25=r(p25)
scalar a75=r(p75)
scalar list a25 a75
margins, dydx(rent) at(rent=(5.92 12.49))
marginsplot, saving(EMG.gph, replace)
**Relative Risk Ratio
mlogit, rrr
*Bondad del ajuste
***Coeficientes independientes
qui mlogit gasazu $x, robust
test [1]rent-[2]rent=0
test [1]caflec-[2]caflec=0
test [1]educ-[2]educ=0
test [1]edad-[2]edad=0
test [1]densidad_alta-[2]densidad_alta=0
test [1]densidad_media-[2]densidad_media=0
***Alternativas independientes
****Alternativa 1
qui mlogit gasazu $x, base(0)
estimate store mlogit1
qui mlogit gasazu $x if gasazu!=1, base(0)
estimate store logitno1
****Alternativa 2
qui mlogit gasazu $x, base(0) robust
estimate store mlogit2
qui mlogit gasazu $x if gasazu!=2, base(0) robust
estimate store logitno2
****Resultados
hausman mlogit1 logitno1, alleqs constant force
hausman mlogit2 logitno2, alleqs constant force


*PARTE III: SIMULACIÓN
**Suponemos una sociedad mas envejecida:
qui mlogit gasazu $x, robust
predict p0a p1a p2a, p
replace edad=edad+1
predict p0b p1b p2b, p
gen diffp0=(p0b-p0a)*100
gen diffp1=(p1b-p1a)*100
gen diffp2=(p2b-p2a)*100
sum diffp0 diffp1 diffp2

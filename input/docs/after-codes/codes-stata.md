************1999 *****************************
*WRIGHT'S SCHEME
* Self employed vs. salaried.
tab wrkgovt //1. work in gob., 2. state-owned industry, 3.private,
///6.other non profit, 7. refused, 8. self-employed, 9. NA/ DK
* Number employees.
tab selfemp // 1. self employed, 2. work for someone else
tab nemploy // 9995 = No employees (petit bourgeoisie)
tab nemploy nempl_in
label list nemploy
tab NEMPLOY V5
tab ISCO_two NEMPLOY
**1. Property owners classes**
generate self_empl=.
replace self_empl=1 if nemploy>=10 & nemploy<=9990 //capitalists
replace self_empl=2 if nemploy>=2 & nemploy<=9 //small entrepreneurs
replace self_empl=3 if nemploy==9995 //petty bougeoisie
replace self_empl=3 if nemploy==1 //petty bougeoisie
label define selfemplabel 1"Capitalists" 2"Small entrepreneurs" 3"Petty bourgeoisie"
label values self_empl selfemplabel
label var self_empl "Self-employed classes"
tab self_empl
tab self_empl wrkgovt
tab self_empl selfemp
tab selfemp
***2. Salaried classes.
*a. Supervision.
tab wrksup //1. Yes, 2. No.
*b. Skills. Si te fijas: skills tiene unas pequeñas correcciones con Nivel Educacional (marcadas con negro y cursiva)
tab ISCO_two
tab educy_in
tab ISCO_two educy_in
// Descripcion de educy_in
//0 0 years;
//1 Up to 7 years;
//2 8-11 years
//3 12-15 years**
//4 16-19 years**
//5 20 years or more**
//94 Only compulsory
//95 Still at school
//96 Still at college, university***
//97 No formal school
//98 Don't know
//99 No answer
*** Used in the command below
gen skills=. //1. experts, 2. skilled, 3. unskilled
replace skills=1 if educy_in>=4 & educy_in<=5 & ISCO_two>=11 & ISCO_two<=24
replace skills=2 if educy_in>=0 & educy_in<=3 & ISCO_two>=11 & ISCO_two<=24
replace skills=2 if educy_in>=95 & educy_in<=97 & ISCO_two>=11 & ISCO_two<=24
replace skills=2 if ISCO_two>=31 & ISCO_two<=34
replace skills=2 if ISCO_two>=71 & ISCO_two<=74
replace skills=3 if ISCO_two>=41 & ISCO_two<=62
replace skills=3 if ISCO_two>=81 & ISCO_two<=93
label define skills 1" Expert" 2"Skilled" 3"Unskilled"
label values skills skills
tab skills
tab  ISCO_two skills
*c. Salaried Clases (se incluye el filtro de Work type)
tab selfemp // 1. self employed, 2. work for someone else
tab selfemp self_empl
tab wrksup //1. Yes, 2. No.
*final salaried classes variable.
generate salaried=.
replace salaried=1 if selfemp==2 & wrksup==1 & skills==1
replace salaried=2 if selfemp==2 & wrksup==2 & skills==1
replace salaried=3 if selfemp==2 & wrksup==1 & skills==2
replace salaried=5 if selfemp==2 & wrksup==2 & skills==2
replace salaried=4 if selfemp==2 & wrksup==1 & skills==3
replace salaried=6 if selfemp==2 & wrksup==2 & skills==3
#delimit ;
label define salaried_lbl
1 "1. Expert managers/supervisors"
2 "2. Expert without authority"
3 "3. Skilled managers/supervisors"
5 "5. Skilled worker without authority"
4 "4. Unskilled managers/supervisors"
6 "6. Unskilled workers";
#delimit cr
label values salaried salaried_lbl
label var salaried "Salaried classes"
tab salaried
***FINAL SCHEME**
generate wr9=.
replace wr9=1 if self_empl==1
replace wr9=2 if self_empl==2
replace wr9=3 if self_empl==3
replace wr9=4 if salaried==1
replace wr9=5 if salaried==2
replace wr9=6 if salaried==3
replace wr9=7 if salaried==4
replace wr9=8 if salaried==5
replace wr9=9 if salaried==6
#delimit;
label define wr9
1 "1. Capitalists"
2 "2. Small employers"
3 "3. Petty bourgeoisie"
4 "4. Expert managers"
5 "5. Expert"
6 "6. Skilled managers"
8 "8. Skilled workers"
7 "7. Unskilled managers"
9 "9. Unskilled workers";
#delimit cr
label values wr9 wr9
label var wr9 "Class position 9 categories"
tab wr9
tab v3 wr9, row
tab wr9 if v3==30
******2) WRIGHT 10 (9 + PB INFORMAL)
tab ISCO_two wr9
gen wr10=wr9
replace wr10=10 if wr10==3 & ISCO_two>=51 & ISCO_two<=93
lab variable wr10 "Wright 10"
lab def wr10 1 "1. Capitalists" 2 "2. Small employers" 3 "3. Formal PB"
lab def wr10 4 "4. Expert managers/supervisors" 5 "5. Expert no authority", add
lab def wr10 6 "6. Skilled manager/supervisors" 7 "7. Unskilled manager/supervisors", add
lab def wr10 8 "8. Skilled worker" 9 "9. Unskilled workers" 10 "10. Informal self-employed", add
lab values wr10 wr10
tab wr10
tab wr10 wr9
tab wr10 if v3==30
*******************************
********2009 *****************
*WRIGHT'S SCHEME
* Self employed vs. salaried.
tab WRKTYPE //1. work in gob., 2. state-owned industry, 3.private, 4. self-employed.
* Number employees.
tab NEMPLOY // 9995 = No emoloyees (petit bourgeoisie)
*1. Property owners classes
generate self_empl=.
replace self_empl=1 if NEMPLOY>=10 & NEMPLOY<=4000 //capitalits
replace self_empl=2 if NEMPLOY>=2 & NEMPLOY<=9 //small employers
replace self_empl=3 if NEMPLOY==9995 //petty bourgeoise
replace self_empl=3 if NEMPLOY==1 //petty bougeoisie
label define selfemplabel 1"Capitalists" 2"Small entrepreneurs" 3"Petty bourgeoisie"
label values self_empl selfemplabel
label var self_empl "Self-employed classes"
tab self_empl
*2. Salaried classes.
*a. Supervision.
tab WRKSUP //1. Yes, 2. No.
*b. Skills. Si te fijas: skills tiene unas pequeñas correcciones con Nivel Educacional (marcadas con negro y cursiva)
tab ISCO_two
tab EDUCYRS //95. Still at school, 96. Still at college
tab ISCO_two EDUCYRS
gen skills=. //1. experts, 2. skilled, 3. unskilled
replace skills=1 if EDUCYRS>=16 & EDUCYRS<=81 & ISCO_two>=11 & ISCO_two<=24
replace skills=2 if EDUCYRS>=1 & EDUCYRS<=15 & ISCO_two>=11 & ISCO_two<=24
replace skills=2 if EDUCYRS>=95 & EDUCYRS<=96 & ISCO_two>=11 & ISCO_two<=24
replace skills=2 if ISCO_two>=31 & ISCO_two<=34
replace skills=2 if ISCO_two>=71 & ISCO_two<=74
replace skills=3 if ISCO_two>=41 & ISCO_two<=62
replace skills=3 if ISCO_two>=81 & ISCO_two<=93
label define skills 1" Expert" 2"Skilled" 3"Unskilled"
label values skills skills
tab skills
tab  ISCO_two skills
*c. Salaried Clases (se incluye el filtro de Work type)
tab WRKTYPE
generate wage_earners=.
replace wage_earners= 1 if WRKTYPE>=1 & WRKTYPE<=3
replace wage_earners= 1 if WRKTYPE>=5 & WRKTYPE<=6
replace wage_earners= 0 if WRKTYPE==4
label define wageeranerslbl 1"Yes" 0"No"
label values wage_earners wageeranerslbl
tab wage_earners
*final salaried classes variable.
*final salaried classes variable.
tab WRKSUP skills if wage_earners==1
generate salaried=.
replace salaried=1 if wage_earners==1 & WRKSUP==1 & skills==1
replace salaried=2 if wage_earners==1 & WRKSUP==2 & skills==1
replace salaried=3 if wage_earners==1 & WRKSUP==1 & skills==2
replace salaried=5 if wage_earners==1 & WRKSUP==2 & skills==2
replace salaried=4 if wage_earners==1 & WRKSUP==1 & skills==3
replace salaried=6 if wage_earners==1 & WRKSUP==2 & skills==3
#delimit ;
label define salaried
1 "1. Expert managers/supervisors"
2 "2. Expert without authority"
3 "3. Skilled managers/supervisors"
4 "4. Unskilled managers/supervisors"
5 "5. Skilled worker without authority"
6 "6. Unskilled workers";
#delimit cr
label values salaried salaried
label var salaried "Salaried classes"
tab salaried
***FINAL SCHEME**
generate wr9=.
replace wr9=1 if self_empl==1
replace wr9=2 if self_empl==2
replace wr9=3 if self_empl==3
replace wr9=4 if salaried==1
replace wr9=5 if salaried==2
replace wr9=6 if salaried==3
replace wr9=7 if salaried==4
replace wr9=8 if salaried==5
replace wr9=9 if salaried==6
#delimit;
label define wr9
1 "1. Capitalists"
2 "2. Small employers"
3 "3. Petty bourgeoisie"
4 "4. Expert managers"
5 "5. Expert"
6 "6. Skilled managers"
7 "7. Unskilled managers"
8 "8. Skilled workers"
9 "9. Unskilled workers";
#delimit cr
label values wr9 wr9
label var wr9 "Class position"
tab wr9
tab V5 wr9, row
tab wr9 if V5==152
*2) WRIGHT 10 (9 + PB INFORMAL)
gen wr10=wr9
replace wr10=10 if wr9==3 & ISCO_two>=51 & ISCO_two<=93
lab variable wr10 "Wright 10"
lab def wr10 1 "1. Capitalists" 2 "2. Small employers" 3 "3. Formal PB"
lab def wr10 4 "4. Expert managers/supervisors" 5 "5. Expert no authority", add
lab def wr10 6 "6. Skilled manager/supervisors" 7 "7. Unskilled manager/supervisors", add
lab def wr10 8 "8. Skilled worker" 9 "9. Unskilled workers" 10 "10. Informal self-employed", modify
lab values wr10 wr10
%let path= C:\Users\Jordan\Desktop\final;
%put &path;
libname final "&path";

ods html body='C:\Users\Jordan\Desktop\finalproject.html';
proc format;
invalue $convert
'A'=4
'A-'=3.7
'B+'= 3.4
'B'= 3
'B-'= 2.7
'C+'= 2.4
'C'= 2
'C-'= 1.7
'D+'= 1.4
'D'= 1
'D-'= .7
'E','W','UW','WE'= 0
'T','NS','P','I'= .
other= .;
value csemester
1= 'Winter'
3= 'Spring'
4= 'Summer'
5= 'Fall'
;
value cstand
0-29.9='Freshman'
30-59.9='Sophomore'
60-89.9='Junior'
other='Senior'
;
run;

data d1;
infile "C:\Users\Jordan\Desktop\final\*.txt" dsd missover dlm='@';
input netid $  semester 7 @8 year class $10. @22 credit grade $;
if grade= 'P' then credit= .;
GPA= input(grade,$convert.);
points=(credit*GPA);
if substr(class,1,4)='STAT' or substr(class,1,4)='MATH' then do mcred=credit; end;
if substr(class,10,1)= 'R' then do duplicates=class;end;
class=substr(class,1,9);
if semester=2 then do semester=1;end;
if semester=6 then do semester=5; end;
run;

proc sort data=d1 out=d2 nodupkey;
by netid class;
run;
proc sort data=d2 out=d3;
by netid year semester;
run;
%let set=d3;

proc sql;
   create table average as
 	select netid, class, grade, points, sum(mcred)as mscred, credit, year, semester, sum(credit) as creditsum, 
	count(duplicates) as repeated_classes, count(class) as number_classes from &set group by netid;	
	create table semestertable as 	
	select netid, points, year, semester, sum(credit) as semesterav from &set group by netid, year, semester;	
	create table letgradeA as select distinct netid, count(grade) as A_count from &set where grade='A' or grade='A-' group by netid;
	create table letgradeB as select distinct netid, count(grade) as B_count from &set where grade='B' or grade='B+' or grade='B-' group by netid;
	create table letgradeC as select distinct netid, count(grade) as C_count from &set where grade='C' or grade='C-' or grade='C+' group by netid;
	create table letgradeD as select distinct netid, count(grade) as D_count from &set where grade='D' or grade='D+' or grade='D-' group by netid;
	create table lowclass as select distinct netid, count(grade) as lowgrade from &set where grade ='IE' or grade='WE' group by netid;
	create table withdraw as select distinct netid, count(grade) as wgrade from &set where grade ='W' group by netid;
	create table NSIT as select distinct netid, count(grade) as nsgrade from &set where grade='NS' or grade='I' or grade='T' group by netid;
	create table EUW as select distinct netid, count(grade) as euwgrade from &set where grade='E' or grade='UW' group by netid;
	create table totcount as select distinct netid, sum(credit) as totalcred from d3 where grade='A' or grade='A-' or grade='B' or grade='B+' or grade='B-'
	or grade='C' or grade='C-' or grade='C+' or grade='D' or grade='D+' or grade='D-' group by netid;
	quit;

proc sql;
create table culmgpa as
select distinct creditsum, netid, sum(points/creditsum) as culmavg format=COMMA30.2 from average group by netid;
create table culmsemgpa as 
select distinct netid, year, semester, sum(points/semesterav) as semculmavg format=COMMA30.2 from semestertable group by netid, year, semester;
quit;
proc sort data=culmgpa out=d4;
by netid;
run;
data d5;
merge average d4 totcount letgradeA letgradeB letgradeC letgradeD lowclass withdraw NSIT EUW;
by netid;
run;
proc sort data=d5;
by netid year semester;
run;
proc sql;
create table total as
select distinct netid, creditsum, culmavg, totalcred, number_classes, A_count, B_count, C_count, D_Count, wgrade, nsgrade, euwgrade from d5 order by netid;
quit;


proc sql;
create table sem as
select distinct netid, year, semester, creditsum, culmavg, totalcred, number_classes, A_count, B_count, C_count, D_Count, wgrade, lowgrade from d5 order by netid;
quit;
data report1;
merge sem culmsemgpa;
by netid year semester;
run;


data culmgpa2;
set d3;
if substr(class,1,4)='STAT' or substr(class,1,4)='MATH' then output;
run;
%let setm=culmgpa2;
proc sql;
   create table average2 as
 	select netid, class, grade, points,sum(mcred)as mscred2,credit, year, semester,
	sum(credit) as creditsum2, count(class) as number_classes2, count(duplicates) as repeated_classes2 from &setm group by netid;	
	create table letgradeA2 as select distinct netid, count(grade) as A_count2 from &setm where grade='A' or grade='A-' group by netid;
	create table letgradeB2 as select distinct netid, count(grade) as B_count2 from &setm where grade='B' or grade='B+' or grade='B-' group by netid;
	create table letgradeC2 as select distinct netid, count(grade) as C_count2 from &setm where grade='C' or grade='C-' or grade='C+' group by netid;
	create table letgradeD2 as select distinct netid, count(grade) as D_count2 from &setm where grade='D' or grade='D+' or grade='D-' group by netid;
	create table lowclass2 as select distinct netid, count(grade) as lowgrade2 from &setm where grade='IE' or grade='WE' group by netid;
	create table withdraw2 as select distinct netid, count(grade) as wgrade2 from &setm where grade='W' group by netid;
	create table NSIT2 as select distinct netid, count(grade) as nsgrade2 from &setm where grade='NS' or grade='I' or grade='T' group by netid;
	create table EUW2 as select distinct netid, count(grade) as euwgrade2 from &setm where grade='E' or grade='UW' group by netid;
	create table totcount2 as select distinct netid, sum(credit) as totalcred2 from &setm where substr(class,1,4)='MATH' or substr(class,1,4)='STAT' and grade='A' or grade='A-' or grade='B' or grade='B+' or grade='B-'
	or grade='C' or grade='C-' or grade='C+' or grade='D' or grade='D+' or grade='D-' group by netid;
	quit;

proc sql;
create table culmgpa2 as
select distinct creditsum2, netid, sum(points/creditsum2) as culmavg2 format=COMMA30.2 from average2 group by netid;
quit;
proc sort data=culmgpa2 out=mathsci;
by netid;
run;
data mathscien;
merge average2 mathsci totcount2 letgradeA2 letgradeB2 letgradeC2 letgradeD2 lowclass2 withdraw2 NSIT2 EUW2;
by netid;
run;
proc sort data=mathscien;
by netid year semester;
run;
proc sql;
create table total2 as
select distinct netid, creditsum2, totalcred2, culmavg2, number_classes2, A_count2, B_count2, C_count2, D_Count2, wgrade2, nsgrade2, euwgrade2 from mathscien order by netid;
quit;


proc sql;
create table math as
select distinct netid, creditsum, culmavg from d5 where mscred>20 order by netid;
quit;
proc means data=d4 p90 noprint;
var culmavg;
where creditsum >=60 and creditsum <=130;
output out=percentile p90=percent;
run;
data _null_;
  set percentile;
  call symput("toppercent",percent);
run;
proc means data=math p90 noprint;
var culmavg;
output out=percentile2 p90=percent2;
run;
data _null_;
  set percentile2;
  call symput("toppercent2",put(percent2,best30.2));
run;

proc report data=report1 nowd missing;
by netid;
column netid year semester number_classes semculmavg culmavg A_count B_count C_count D_Count lowgrade wgrade creditsum totalcred classtand;
define netid/group;
define semester/order format=csemester.;
define year/order;
define wgrade/display '# Withdraws';
define creditsum/display 'Graded Credit Hours';
define classtand/computed format=cstand. 'Class Standing';
define number_classes/display 'Total # Classes';
define lowgrade/display '# Class < E, UW';
define semculmavg/display 'Semester Average';
define culmavg/display 'Overall Average';
define totalcred/display 'Earned Credit Hours';
compute classtand; classtand=creditsum;endcomp;
break after netid / skip dol dul;
title 'Report 1-Semester and Overall GPA';
run;

data last; 
merge total total2; by netid; run;

proc report data=last nowd missing;
by netid;
column netid culmavg culmavg2 A_count A_count2 B_count B_count2 C_count C_count2 D_Count D_Count2 wgrade wgrade2 nsgrade nsgrade2 euwgrade euwgrade2 creditsum creditsum2 totalcred totalcred2 number_classes number_classes2;
define netid/group;
define culmavg/ display 'GPA';
define culmavg2/ display 'MS GPA';
define A_count/ order '#A';
define B_count/ order '#B';
define C_count/ order '#C';
define D_Count/ order '#D';
define wgrade/ order '#W';
define nsgrade/ order '# NS, I, T';
define euwgrade/ order '#E,UW';
define creditsum/ order '#Graded Credit Hours';
define totalcred/ order '#Earned Credit Hours';
define A_count2/ order 'MS #A';
define B_count2/ order 'MS #B';
define C_count2/ order 'MC #C';
define D_Count2/ order 'MS #D';
define wgrade2/ order 'MS #W';
define nsgrade2/ order 'MS #NS I or T';
define euwgrade2/ order 'MS #E,UW';
define creditsum2/ order 'MS Graded Credit Hours';
define totalcred2/ order 'MS Earned Credit Hours';
define number_classes/display '# Classes';
define number_classes2/display 'MS # Classes';
break after netid /skip dol dul;
title 'Report 2-Overall/Statistics & Math GPA';
run;

data d7;
set d4;
if creditsum >=60 and creditsum <=130 then output;
run;
proc report data=d7 nowd missing SPANROWS;
column netid culmavg;
where culmavg GE &toppercent;
define culmavg/order;
title 'REPORT 3 (TOP 10% of Students with hours between 60 and 130)';
run;

proc report data=math nowd missing SPANROWS;
column netid culmavg;
where culmavg GE &toppercent2;
define culmavg/order;
title 'REPORT 4 (TOP 10% of Students with more than 20 hours in Math/Statistics)';
run;
ods html close;


* ==================
* A mixed integer linear program for the
* job sequencing and tool switching problem with non-identical parallel machines
* ==================

***
* author: D. Calmels
***

* ==================
* Definition of indices, sets and parameters of a sample instance
* ==================

*** Include data set that contains
* machine set @index(m)
* job set @index(j), alias (i,j,r)
* tool set @index(t)
* time set @index(k)
* tools required for job j @set sig(j,t)
* jobs that require tool t @set tau(t,j)
* tool magazine capacity @parameter c(m)
* procssing time of job j on machine m @parameter p(j,m)
* tool switching time per machine @parameter sw(m)
***

***
* Include for example
* sample instance: output1.txt
* replace file path
* $include "file path:\...\"input1.txt
;

*** Additional sets and parameters for position-based formulation
* job aliases
alias(i,j)
alias(r,j)
;

Parameter
g large constant;
g = sum(j,smax(m,p(j,m)))+(card(j)-1)*smax(m,(c(m)*sw(m)));

* ==================
* Definition of variables
* ==================

Variables
* binary variables
x(j,r,m)         if job j is processed in the r^th position on machine m
v(t,r,m)         if tool t is present in machine m during the processing of the r^th job
w(t,r,m)         if tool t is inserted in machine m exactly before the processing of the r^th job

* continuous variables
f(j,r,m)         denotes the completion of job j processed in the r^th position on machine m

*objective function values
tft              total flowtime
fmax             makespan
ts               total number of tool switches
;

Binary Variables
x,v,w;

Positive Variables
f;

* ==================
* Definition of equations
* ==================
*** the numbers are identical to the numbers in the publication
e1(j)
e2(r,m)
e3(r,m)
e4(t,r,m)
e5(r,m)
e6(t,r,m)
e7(j,m)
e8(j,r,m)

* objective functions
e11
e12(j,r,m)
e14

;

e1(j)..
sum((r,m),x(j,r,m)) =e= 1;

e2(r,m)..
sum(j,x(j,r,m)) =l= 1;

e3(r,m)$(ord(r)>1)..
sum(j,x(j,r,m)) =l= sum(j,x(j,r-1,m));

e4(t,r,m)..
sum(j$tau(t,j),x(j,r,m)) =l= v(t,r,m);

e5(r,m)..
sum(t,v(t,r,m)) =l= c(m);

e6(t,r,m)$(ord(r)>1)..
v(t,r,m)-v(t,r-1,m) =l= w(t,r,m);

e7(j,m)..
f(j,'1',m) =e= p(j,m)*x(j,'1',m);

e8(j,r,m)$(ord(r)>1)..
f(j,r,m) =g= sum(i$(ord(i)<>ord(j)),f(i,r-1,m)) + p(j,m)*x(j,r,m) + sw(m) * sum(t,w(t,r,m)) - g*(1-x(j,r,m));

* objective functions
e11..
tft =e= sum((j,r,m),f(j,r,m));

e12(j,r,m)..
fmax =g= f(j,r,m);

e14..
ts =g= sum((t,r,m),w(t,r,m));


* ==================
* Model
* ==================

* Create model 'pb' that uses all variables and constraints
Model pb /all/;

* ==================
* CPLEX Settings
* ==================
* time limit
pb.reslim=3600;
* use all cores
option threads=12;
* other settings
option optcr=0;
$onecho > cplex.opt
lpmethod 4
$offecho

* ==================
* Solve first objective
* ==================
Solve pb using MIP minimizing tft;


* ==================
* Results
* ==================
Variable
time1     CPU used
;
time1.l=pb.resusd;

* save the results in an excel-file 'file.xlsx' in the 'file_path'
* in the workbook 'workbook_tft'
execute_unload "file.gdx" tft.l time1.l
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=tft.l rng='workbook_tft'!a1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=time1.l rng='workbook_tft'!b1'


* ==================
* Solve second objective
* ==================
Solve pb using MIP minimizing fmax;

* ==================
* Results
* ==================
Variable
time2     CPU used

;
time2.l=pb.resusd;

* save the results in an excel-file 'file.xlsx' in the 'file_path'
* in the workbook 'workbook_fmax'
execute_unload "file.gdx" fmax.l time2.l
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=fmax.l rng='workbook_fmax'!a1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=time2.l rng='workbook_fmax'!b1'


* ==================
* Solve third objective
* ==================
Solve pb using MIP minimizing ts;

* ==================
* Results
* ==================
Variable
time3     CPU used
;

time3.l=pb.resusd;

* save the results in an excel-file 'file.xlsx' in the 'file_path'
* in the workbook 'workbook_tft'
execute_unload "file.gdx" ts.l time3.l
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=ts.l rng='workbook_ts'!a1'
execute 'gdxxrw.exe  file.gdx o=C:\'file_path'\file.xlsx var=time3.l rng='workbook_ts'!b1'

* ==================
* End
* ==================

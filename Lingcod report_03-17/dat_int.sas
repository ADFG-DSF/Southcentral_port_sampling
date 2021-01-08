****** Distribution of lingcod effort by stat area*****;
* Modified by MDS 1/2/18 in order to use data from 03-16 for lingcod report. Will use this file to map lc fishing effort
by port and user group from 2003-2016.;
* Modifed AMR 12/17/20 for same report. Create xls file to send to R.
* orginal found here O:\DSF\GOAB\Ling_Reports\LingReport_03-14\Harvest_And_Effort_Distribution\Effort

LIBNAME SASDATA 'O:\DSF\GOAB\SASDATA\Intervw';
TITLE ' ';
ods html close; ods html; run;

* First narrow the data to include only records with stat areas and lingcod effort;
DATA int0317;
	SET sasdata.int9204 sasdata.int05 sasdata.int06 sasdata.int07 sasdata.int08
		sasdata.int09 sasdata.int10 sasdata.int11 sasdata.int12 sasdata.int13
		sasdata.int14 sasdata.int15 sasdata.int16 sasdata.int17;
	if port ne 'CCI';
	if month ge 7;   *exclude data prior to July because fishery closed - can't be effort for lingcod;
	if ADFGStat = . THEN DELETE;
	if User = 'Unknown' or User = '        ' THEN DELETE;
	keep year port user adfgstat target angldays hakept pelkept npkept yekept lckept; 
run;

PROC SORT DATA = int0317;
	BY PORT YEAR USER ADFGSTAT Target;

proc export data = int0317 outfile = 'H:\My Documents\Southcentral halibut and groundfish\Lingcod report_03-17\dat_int.xls'
dbms = xls replace;
run;

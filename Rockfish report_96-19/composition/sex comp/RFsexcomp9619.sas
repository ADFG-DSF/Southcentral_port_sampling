*RFsexcomp9616.sas - rockfish sex composition
SCM 03/9/18
Uses Steve Fleischman version of composition equations;

title;
libname report 'O:\DSF\GOAB\RockfishReport_96-16\DataFiles';
libname awl 'O:\DSF\GOAB\SASDATA\RF';
libname harv 'O:\DSF\GOAB\SASCODE\Harvest\RF';
ods html close;
ods listing image_dpi=300;
ods html;
run;


*---------------------------------------------------------------------------------------------
Sex comp uses same formulas as age comp. For Seward 1996-2000, sex comp estimated from 
pooled data because there were four user groups in the raw data (charter, private, Army, USAF) 
but only two user groups in the SWHS estimates.

All other ports and years use stratified estimates described in the report.
----------------------------------------------------------------------------------------------;

data sexcomp;
	length species $12;
  	set awl.rock9195 awl.rock9600 awl.rock2001 awl.rock2002 awl.rock2003 awl.rock2004	awl.rock2005
			awl.rock2006 awl.rock2007 awl.rock2008 awl.rock2009 awl.rock2010 awl.rock2011 
			awl.rock2012 awl.rock2013 awl.rock2014 awl.rock2015 awl.rock2016 awl.rock2017 awl.rock2018 awl.rock2019;
			length port $5.;
	if sex ne '';
	if sp in(144,168,169) then delete;
	if species = 'Blackgll' then species = 'Blackgill';
	if species = 'Quill' then species = 'Quillback';
	if species = 'Redstrpe' then species = 'Redstripe';
	if species = 'Harleq' then species = 'Harlequin';
	if species = 'Rosethrn' then species = 'Rosethorn';
	if species = 'Shortrkr' then species = 'Shortraker';
	if species = 'Shrpchin' then species = 'Sharpchin';
	if species = 'Silvergr' then species = 'Silvergray';
	if species = 'Vermilon' then species = 'Vermilion';
	if species = 'Yelleye' then species = 'Yelloweye';
	if species = 'Yelltail' then species = 'Yellowtail';
	if sp = 154 then species = 'DuskyDark';
	if port in('CCI','Homer') then port = 'CI';
	if year ge 1996;
run;

*{save starting file for species, age, length, sex comp to data folder for use with subsequent programs.};
libname sexcomp 'H:\My Documents\Southcentral halibut and groundfish\Rockfish report_96-19\sex comp';
data sexcomp.sexcompdata;  
	set sexcomp;
run;

*{frequency of each species by port, user, year};
proc sort data=sexcomp;
	by port user year sp species sex;
proc freq data=sexcomp;
	by port user year sp species;
  tables sex/noprint out=comp;        *output is port year rlen count percent;
proc print data=comp (obs=50) noobs uniform;run;

*{restructure data file so sample size (nj) by each user group is a separate variable,
	all on one line for each species};
data comp;
	set comp;
	if user = 'Charter' then nijC = count; *{number of species i sex j by charter};
	if user = 'Private' then nijP = count; *{private};
	if user = 'Unknown' then nijU = count; *{unknown};
	if user = 'SewMilC' then nijM = count; *{military (Seward only)}.
	drop user count percent;
proc sort data=comp;
	by port year sp species sex;
proc means data=comp noprint;
	by port year sp species sex;
	output out=comp2 sum(nijC nijP nijU nijM)=nijC nijP nijU nijM;
data comp2;
	set comp2;
	if nijC = . then nijC = 0;
	if nijP = . then nijP = 0;
	if nijU = . then nijU = 0;
	if nijM = . then nijM = 0;
	drop _type_ _freq_;
proc print data=comp2 (obs=50) noobs uniform; *{OK so far};
	title 'work.Comp2';
run;

*{obtain and merge total rf sample size for each user group};
proc means data=comp2 noprint;
	by port year sp species;
	output out=totaln sum(nijC nijP nijU nijM)=niC niP niU niM; *{Example nC = total rf sample size all sexes for charters};
*proc print data=totaln;
	*title 'work.totaln';
run;

data comp3;
	merge comp2 totaln;
	by port year sp species;
	drop _type_ _freq_;
	pijC = sum(0,nijC/niC);										*{e.g. proportion of species i sex j in charter sample};
	vpijC = sum(0,pijC*(1-pijC)/(niC-1));
	pijP = sum(0,nijP/niP);
	vpijP = sum(0,pijP*(1-pijP)/(niP-1));
	ni = sum(0,niC,niP);
	*pijU = nijU/niU;									*{not needed in calculations};
	*vpijU = pijU*(1-pijU)/(niU-1);			*{not needed in calculations};
proc print data=comp3 noobs uniform;
	title 'work.Comp3';
run;

*{plot sample sizes};
proc sgpanel data=comp3;
	panelby port/rows=2 columns=3 novarname;
	series x=year y=ni/group=species;
run;
*{it appears that dusky and dark show up now and then at Kodiak and Homer, but 
	black and yelloweye constitute the largest sample sizes regionwide};

data pharv;
	set harv.HarvBySpecies;
	if species = 'Blackgll' then species = 'Blackgill';
	if species = 'Quill' then species = 'Quillback';
	if species = 'Redstrpe' then species = 'Redstripe';
	if species = 'Harleq' then species = 'Harlequin';
	if species = 'Rosethrn' then species = 'Rosethorn';
	if species = 'Shortrkr' then species = 'Shortraker';
	if species = 'Shrpchin' then species = 'Sharpchin';
	if species = 'Silvergr' then species = 'Silvergray';
	if species = 'Vermilon' then species = 'Vermilion';
	if species = 'Yelleye' then species = 'Yelloweye';
	if species = 'Yelltail' then species = 'Yellowtail';
	if sp = 154 then species = 'DuskyDark';
	vHiC = SEHiC**2;
	vHiP = SEHiP**2;
	vHi = SEHi**2;
	keep port year species HiC SEHiC vHiC HiP SEHiP vHiP Hi vHi;
proc print data=pharv;
	title 'work.pharv';
run;

proc sort data=comp3; by port year species;
proc sort data=pharv; by port year species;
data comp4;
	merge comp3 pharv;
	by port year species;
	*{calculations for all ports/years except Seward 1996-2000};
	if port ne 'Seward' or (port = 'Seward' and year ge 2001) then do;
		HijC = pijC*HiC;                                      *{harvest of species i sex j by charter};
		vHijC = sum(0,(pijC**2*vHiC)+(vpijC*HiC**2)-(vpijC*vHiC));   
		SEHijC = sqrt(vHijC);                                 
		HijP = pijP*HiP;                                      *{harv of speecies i sex j by private};
		vHijP = sum(0,(pijP**2*vHiP)+(vpijP*HiP**2)-(vpijP*vHiP));
		SEHijP = sqrt(vHijP);
		Hij = sum(HijC,HijP);                                 *{harvest of species i sex j by all sport};
		vHij = sum(vHijC,vHijP);
		SEHij = sqrt(sum(vHijC,vHijP));
		pij = Hij/Hi;                                         *{proportion of sex j in harvest of species i};
		vpij = sum(0,(1/Hi**2)*(vHiC*(pijC*HiP-HijP)**2/Hi**2 + vHiP*(pijP*HiC-HijC)**2/Hi**2 + vpijC*HiC**2 + vpijP*HiP**2));
		SEpij = sqrt(vpij);
	end;
	*{Calculations for Seward 1996-2000};
	if port = 'Seward' and year ge 1996 and year le 2000 then do;
		pij = sum(nijC,nijP,nijU,nijM)/sum(niC,niP,niU,niM);
		vpij = sum(0,pij*(1-pij)/(sum(niC,niP,niU,niM)-1));
		SEpij = sqrt(vpij);
		Hij = pij*Hi;
		vHij = sum(0,(pij**2*vHi)+(vpij*Hi**2)-(vpij*vHi));
		SEHij = sqrt(vHij);
	end;
	if port = 'Whittier' and year ge 1996 and year le 1998 then delete; *{no data in 1996-1997, too little in 1998};
	*drop vHijC vHijP vpij vHij;
run;

*{print all values used in calculations};
proc print data=comp4 noobs uniform;
	title 'Final sex comp estimates - full detail for verification';
	title2 'Missing variances are not calculable due to n for one user group le 1.';
	*where port = 'Kodiak' and species = 'Quillback' and year = 2008;
run;

data primarycomp4;
	set comp4;
	if (species = 'Black')
		or (species = 'Yelloweye')
		or (species = 'Dark' and port not in ('Seward','Valdez','Whittier'))
		or (species = 'Dusky' and port not in('Seward','Valdez'))
		or (species = 'Copper' and port = 'Valdez')
		or (species = 'Quillback' and (port in('Valdez','Whittier')));
run;

*{Primary species estimates - reduced and formatted};
proc print data=primarycomp4 uniform;
	*EDIT - if species match the list above, otherwise will generate blank lines;
	title 'Sex comp, reduced and formatted.';
	var port year species sex nijC nijP nijU nijM pijC vpijC pijP vpijP HijC HijP Hij SEHij pij SEpij;
	format pijC 5.3 vpijC 6.4 pijP 5.3 vpijP 6.4 HijC 5. HijP 5. Hij 5. SEHij 6.1 pij 5.3 SEpij 6.4;
run;

*{Transpose to put proportions and SEs in columns by sex};
proc sort data=primarycomp4;
	by species port year;
	*{-First do proportions};
proc transpose data=primarycomp4 out=pij_T;
	by species port year;
	id sex;
	var pij;
*{rename pij variables so you can merge proportions and standard errors};
data pij_T; set pij_T;
	pF = F;
	pM = M;
	drop F M _name_;
*proc print data=pij_T; run;
	*{-Next do standard errors};
proc transpose data=primarycomp4 out=SEp_T;
	by species port year;
	id sex;
	var SEpij;
*{rename SEpij variables so you can merge proportions and standard errors};
data SEp_T; set SEp_T;
	SEF = F;
	SEM = M;
	drop F M _name_;
*proc print data=SEp_T; run;

data sexcomp_T;
	merge pij_T SEp_T;
	by species port year;
	if pF = 1 and pM = . then do;
		pM = 0;
		SEM = 0;
	end;
	if pF = . and pM = 1 then do;
		pF = 0;
		SEF = 0;
	end;

proc print data=SexComp_T noobs uniform;
	*title 'Sex comp by species 1996-2015';
run;

proc freq data=sexcomp_T;
	tables port*pF;
run;

*{There are instances where only one sex was seen - this is more likely when sample sizes were small,
	and in that case the standard error will be estimated to be zero, when in fact there is much 
	uncertainty associated with small sample sizes. This section flags estimates from small sample sizes};
proc print data=SexComp_T noobs uniform;
	where SEF = 0;
run;




data totaln2;
	set totaln;
	*{extract only major species same as primarycomp4 above};
	if (species = 'Black')
		or (species = 'Yelloweye')
		or (species = 'Dark' and port not in ('Seward','Valdez','Whittier'))
		or (species = 'Dusky' and port not in('Seward','Valdez'))
		or (species = 'Copper' and port = 'Valdez')
		or (species = 'Quillback' and (port in('Valdez','Whittier')));
	*{calculations for all ports/years except Seward 1996-2000};
	if port ne 'Seward' or (port = 'Seward' and year ge 2001) then do;
	n = sum(niC, niP);
	end;
	*{Calculations for Seward 1996-2000};
	if port = 'Seward' and year ge 1996 and year le 2000 then do;
		n = sum(niC, niP, niU, niM);
	end;
	drop _type_ _freq_ niC niP niU niM;
proc print data=totaln2; run;
proc sort data=totaln2;
	by species port year;
*{merge with SexComp_T to flag small sample sizes - sample sizes that are less than the sample
	size that produces the desired precision, which was ±0.1 for 1996-2008, and ±0.2 for 2009-2011};
data SexComp_T;
	merge SexComp_T totaln2;
	by species port year;
	SE = SEF;
	*{calculations for 1996-2008, d = 0.1 with a = 0.05};
	if year ge 1996 and year le 2008 then n0 = 1.96**2*pF*(1-pF)/0.1**2;
	*{calculations for 2009-2014,d = 0.2 with a = 0.05};
	if year ge 2009 then n0 = 1.96**2*pF*(1-pF)/0.2**2;
	*{New set small sample size flag};
	length small $1;
	if n0 = . or n0 = 0 or n0 > n then small = 'S';
	F95UCL = pF + 2*SE;
	F95LCL = pF - 2*SE;
	drop SEF SEM;
proc print data=SexComp_T noobs uniform;
	title;
run;

*Figures;

ods html close; ods html; ods graphics on/
	antialias=on
	scale=off
	height=3in
	width=7in
	border=off;
data SexComp_T2;
	set SexComp_T;
	if port='Valdez';
	if species='Copper';
	if year ge 1996;
proc sgplot data=SexComp_T2 noautolegend;
	title 'Copper Rockfish Proportion Female, Valdez';
	yaxis values=(0 to 1 by .2);
	vbarparm category=year response=pF /
		limitattrs=(color=black)
		limitlower=F95LCL
		limitupper=F95UCL;
	*	series x =year y=n/y2axis lineattrs=(color=gray thickness=3 pattern=solid) transparency=.7;
		*xaxis label=' ';
		xaxis values=(2004 to 2016 by 1);
		xaxis display=(nolabel);
		yaxis values=(0 to 1 by .2) label='Proportion Female';
	*	y2axis min=0 label='Sample Size';
run;
*yelloweye rockfish sex comp plots;
proc sgpanel data=SexComp_T;
	where species = 'Yelloweye';
	title 'Yelloweye Rockfish - Proportion Female';
	panelby port;
	hbarparm category=year response=pF/
		outline
		fill
		limitlower=F95LCL
		limitupper=F95UCL;
run;

*{Same plots, Seward only};
ods graphics on/
	antialias=on
	scale=off
	height=5in
	width=3in
	border=off;
*black rockfish sex comp plots;
proc sgplot data=SexComp_T;
	where port = 'Seward' and species = 'Black';
	title 'Black Rockfish - Proportion Female';
	hbarparm category=year response=pF/
		outline
		fill
		limitlower=F95LCL
		limitupper=F95UCL;
run;
*yelloweye rockfish sex comp plots;
proc sgplot data=SexComp_T;
	where port = 'Seward' and species = 'Yelloweye';
	title 'Yelloweye Rockfish - Proportion Female';
	hbarparm category=year response=pF/
		outline
		fill
		limitlower=F95LCL
		limitupper=F95UCL;
run;

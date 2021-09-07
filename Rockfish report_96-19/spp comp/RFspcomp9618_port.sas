*---------------------------------------------------------------------------------
RFspcomp9616_awl.sas - rockfish species composition based on AWL samples
SCM 03/15/18
Uses Steve Fleischman version of composition equations

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	1. Insufficient rockfish data to estimate species comp or other objectives
			for CCI. Total rockfish sample size for 1996-2016 was only 86 fish, 83 of
			which were sampled 2010-2016. Instead, CCI and Homer data pooled for 
			estimates for Cook Inlet.  
	2. Don't bother with estimate of species comp for Whittier 1998 - only sampled
			for one month, sample sizes too small, estimates likely to be biased.
	3. At most ports through 2003, we didn't make a distinction between dusky and
			dark rf. Separated dusky/dark at Kodiak in 2003, and at all remaining
			ports starting in 2004. However, there were still some fish from 2004 on
			that were either dusky or dark (couldn't tell), and they were
			coded as either 169 (unspecified pelagic) or 154 (dusky/dark). Species comp
			estimates include the dusky/dark (154) so that proportions for all other 
			species are not inflated. 		
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

NOTE: You must have this file open in Excel for this program to run successfully:
			O:\DSF\GOAB\RockfishReport_96-16\SpeciesComp\rfharvbyuser9616.xlsx.
			Harvest numbers in this file for Cook Inlet include CCI and Homer.

To do: After getting estimates of species comp from AWL data, reload the data
				including unspecified pelagic, demersal, and slope, and compare the 
				estimates of assemblage composition to estimates of assemblage 
				composition from interview data.

---------------------------------------------------------------------------------;

libname rf 'O:\DSF\GOAB\SASDATA\RF';
ods html close;
ods html;
ods listing image_dpi=300;
run;

*{Step 1 - subset and clean up the data};
data spcomp_init;
	length Abbrev $12.;
	length species $12;
	set rf.rock9195 rf.rock9600 rf.rock2001 rf.rock2002 rf.rock2003 rf.rock2004 rf.rock2005 rf.rock2006
		rf.rock2007 rf.rock2008 rf.rock2009 rf.rock2010 rf.rock2011 rf.rock2012 rf.rock2013
		rf.rock2014 rf.rock2015 rf.rock2016 rf.rock2017 rf.rock2018 rf.rock2019;
	if year le 1995 then delete;
	*{Add species abbreviations for cleaner tables};
	*{Write full species common names};
	if sp = 142 then abbrev = 'BLK';
	if sp = 172 then abbrev = 'DUS';
	if sp = 173 then abbrev = 'DRK';
	if sp = 154 then abbrev = 'DUSDRK';
	if sp = 155 then abbrev = 'YTAL';
	if sp = 156 then abbrev = 'WID';
	if sp = 177 then abbrev = 'BGL';
	if sp = 137 then abbrev = 'BOC';
	if sp = 146 then abbrev = 'CAN';
	if sp = 149 then abbrev = 'CHI';
	if sp = 138 then abbrev = 'COP';
	if sp = 176 then abbrev = 'HAR';
	if sp = 136 then abbrev = 'NOR';
	if sp = 147 then abbrev = 'QUI';
	if sp = 158 then abbrev = 'RSTP';
	if sp = 150 then abbrev = 'ROS';
	if sp = 151 then abbrev = 'REYE';
	if sp = 152 then abbrev = 'SRAK';
	if sp = 166 then abbrev = 'SHCN';
	if sp = 157 then abbrev = 'SGRY';
	if sp = 148 then abbrev = 'TIG';
	if sp = 184 then abbrev = 'VER';
	if sp = 145 then abbrev = 'YE';
	*{convert null users from early years to 'Unknown'};
	if user = '' then user = 'Unknown';
	if port in('Homer','CCI') then port = 'CI';
	*{Remove unspecified slope (144), pelagic (169), and demersals (168) when doing species comp estimates, EXCEPT keep 
		unspecified pelagic in 2004-2007 because these are a mixture of dark and dusky that were separated from blacks based 
		on otolith characteristics but couldn't be identified to species based only on the otolith. Species code 154 was used 
		for dark/dusky through 2003 at all ports except Kodiak. Beginning in 2004 we used 172 (dusky) and 173 (dark) and 
		dropped 154 all together. We started using codes 172 and 173 at Kodiak in 2003, but there were still 2 fish coded to
		154 that year. I changed those 2 fish to unspecified pelagics in the raw data file and SAS datasets.
		This analysis combines dark and dusky (DuskyDrk) through 2002 at Kodiak and through 2003 at all other ports.};
	*{Note - Cordova 1999 data already excluded because it was reported elsewhere, don't have data to weight it 
			to include in estimates for EPWS};

	*{Rockfish sample sizes from CCI too small to be representative most years (n=86 fish for entire report period, 
		many years have no samples.	Also, all AWL samples were from charter anglers-no private rockfish ever sampled, 
		yet we have private	harvest	estmated by the SWHS. Can't estimate species comp without data from private sector.
		Therefore, pool AWL samples from CCi and Homer and combine SWHS estimates for CCI and Homer to get
		estimate for Cook Inlet}; 
		*if port in('CCI','Homer') then port = 'CI';

	*{Delete 1998 data for Whittier - only one month of sampling, small 
		sample sizes likely biased for estimating species comp};
		*if port = 'Whittier' and year = 1998 then delete; 
	*{exclude unspec slopes (n=1) unspec demersals (n=11) and unspec pelagics (38)};
		if sp in(144,168,169) then delete;
run;

*{save starting file for species, age, length, sex comp to data folder for use with subsequent programs.};
libname spcomp 'O:\DSF\GOAB\SASCODE\Species_comp';
data spcomp.rfcompdata;  
	set spcomp_init;
run;


*---------------------------------------------------------------------------------------------------------------------------

*{If there is no difference in species comp between sectors, then the charter and private data can be pooled 
	to estimate species comp. If there is a	difference in species comp between sectors but the sample sizes are
	proportional to harvest, then the data can still be pooled to estimate species comp. If the sample sizes 
	are not proportional, a stratified estimator should be used. Rather then do multiple tests to see if sample
	sizes are proportional to harvest by sector, decided to simply be safe and do all estimates as stratified. Use
	SWHS harvest by sector for the strata. 

	However - do not try to stratify Seward estimates by sector before 2001. Reason is that user group was designated
	charter, private, Army, or USAF, but SWHS doesn't have military estimates. See spreadsheet SpeciesCompBefore2001.xlsx
  in the species comp folder. This spreadsheet compares unweighted and weighted estimates, where the weighted estimates
	were based on results from a survey of military anglers in Seward in 1996 only. That survey asked military anglers whether
	they considered their harvest private or charter. The results in this spreadsheet indicate that maximum differences in
	species proportions between weighted and unweighted estimates before 2001 were only 3.4% for blacks and 2.3% for 
	yelloweyes. For these major species the estimates are within the desired relative precision of 10%.
----------------------------------------------------------------------------------------------------------------------------;
data spcomp;
	set spcomp_init;
	*{Exclude rockfish marked as "rare" (Seward only) for species comp -- these are uncommon species that were sampled in
		addition to the usual protocol to boost sample size for age and growth studies. These should be excluded from species 
		comp but included for all other metrics (age/length/sex comp)};
	if rare = 'R' then delete;
run;

*{frequency of each species by port, user, year};
proc sort data=spcomp;
	by port user year species;
proc freq data=spcomp;
	by port user year species;
  tables abbrev/noprint out=comp;  *{output is port year species count percent};
data comp; set comp;
	drop percent;
*proc print data=comp noobs uniform;
*	title 'work.comp: obs freq by species and port';
run;

*{restructure data file so sample size (ni) by each user group is a separate variable,
	all on one line for each species};
data comp;
	set comp;
	if user = 'Charter' then niC = count;
	if user = 'Private' then niP = count;
	if user = 'Unknown' then niU = count;
	if user = 'SewMilC' then niM = count;
	drop user count;
proc sort data=comp;
	by port year species;
proc means data=comp noprint;
	by port year species;
	output out=comp2 sum(niC niP niU niM)=niC niP niU niM;
data comp2;
	set comp2;
	if niC = . then niC = 0;
	if niP = . then niP = 0;
	if niU = . then niU = 0;
	if niM = . then niM = 0;
	drop _type_ _freq_;
proc print data=comp2 noobs uniform; *{OK so far};
	title 'work.Comp2';
run;

data comp2_2;
	length species $12;
	set comp2;
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
run;
proc print data=comp2_2 noobs uniform; *{OK so far};
	title 'work.Comp2_2';
run;

*{obtain and merge total rf sample size for each user group};
proc means data=comp2_2 noprint;
	by port year;
	output out=totaln sum(niC niP niU niM)=nC nP nU nM; *{Example nC = total rf sample size all species for charters};
data comp3;
	merge comp2_2 totaln;
	by port year;
	drop _type_ _freq_;
	n = sum(nC,nP,nU,nM);
	piC = niC/nC;										*{proportion of species i in charter sample};
	vpiC = piC*(1-piC)/(nC-1);
	piP = niP/nP;
	vpiP = piP*(1-piP)/(nP-1);
	*piU = niU/nU;									*{not needed in calculations};
	*vpiU = piU*(1-piU)/(nU-1);			*{not needed in calculations};
proc print data=comp3 noobs uniform;
	title 'work.comp3 species proportions by user';
run;


*{diagnostics - check to see that species proportions by user add up to 1 each year.};
proc means data=comp3 noprint;
	by port year;
	output out=pcheck sum(piC piP)=sumC sumP;
proc print data=pcheck noobs uniform;
	title 'Check to make sure all user group proportions add to 1';
run;


*{obtain and merge SWHS harvest estimates by user group};
*{This worksheet combines CCI and LCI into single estimates for all of Cook Inlet.};
Filename pharv DDE 'Excel|[RFHarvByUser9619.xlsx]Sheet3!R2C1:R121C8'; 
data pharv;
	infile pharv DLM='09'x NOTAB DSD MISSOVER;
	informat port $CHAR8. year 4. HC 12. SEHC 12. HP 12. SEHP 12. H 12. SEH 12.;
 	input port year HC SEHC HP SEHP H SEH;
	vHC = SEHC**2;
	vHP = SEHP**2;
	vH = SEH**2;
	drop SEHC SEHP SEH;
proc print data=pharv noobs uniform; 
	title 'work.pharv - SWHS harvest estimates by sector (charter/private)';
run;

proc sort data = pharv;
	by port year;

data comp4;
	merge comp3 pharv;
	by port year;
	ni = sum(niC,niP,niU,niM);
	pi2 = ni/n;
	*{calculations for all ports/years except Seward 1996-2000};
	if port ne 'NG' or (port = 'NG' and year ge 2001) then do;
		*{Charter harv of sp i = prop of sp i in charter harvest x tot charter harv all sp};
		HiC = piC*HC;  
		vHiC = (piC**2*vHC)+(vpiC*HC**2)-(vpiC*vHC);
		SEHiC = sqrt(vHiC);
		*{Private harv of sp i = prop of sp i in private harvest x tot private harv all sp};
		HiP = piP*HP;
		vHiP = (piP**2*vHP)+(vpiP*HP**2)-(vpiP*vHP);
		SEHiP = sqrt(vHiP);
		*{Total sport harvest of species i};
		Hi = sum(HiC,HiP);
		vHi = sum(vHiC,vHiP);
		SEHi = sqrt(sum(vHiC,vHiP));
		pi = Hi/H;                           *{proportion of species i in the total rockfish harvest H};
		vpi = (1/H**2)*(vHC*(piC*HP-HiP)**2/H**2 + vHP*(piP*HC-HiC)**2/H**2 + vpiC*HC**2 + vpiP*HP**2);
		SEpi = sqrt(vpi);
	end;
	*{Calculations for Seward 1996-2000};
	if port = 'NG' and year ge 1996 and year le 2000 then do;
		pi = sum(niC,niP,niU,niM)/sum(nC,nP,nU,nM);
		vpi = pi*(1-pi)/(sum(nC,nP,nU,nM)-1);
		SEpi = sqrt(vpi);
		Hi = pi*H;
		vHi = (pi**2*vH)+(vpi*H**2)-(vpi*vH);
		SEHi = sqrt(vHi);
	end;
	if port = 'Whittier' and year ge 1996 and year le 1998 then delete; *{no data in 1996-1997, too little in 1998};
	drop vpi;

*{print all values used in calculations};
proc print data=comp4 uniform;
	title 'Final species comp estimates - full detail for verification';
run;

*{Save harvest estimates by species for estimation of age/length/sex comp by species};
libname harv 'O:\DSF\GOAB\SASCODE\Harvest\RF';
data harv.HarvBySpecies; *{save to o:\dsf\GOAB\RockfishReport_96-16\DataFiles\};
	set comp4;
	if year ge 1996;
	keep port year species n abbrev piC vpiC HiC SEHiC piP vpiP HiP SEHiP pi SEpi Hi SEHi;
proc print data=harv.HarvBySpecies_SFmgmt_pwsaport uniform;
	title 'harv.HarvBySpecies - point estimates and variances';
run;

*{all estimates - reduced and formatted};
proc print data=comp4 noobs uniform;
	title 'work.comp4 - Final species comp, reduced and formatted.';
	var port year species n niC niP niU niM ni piC vpiC piP vpiP HC HP H Hi SEHi HiC SEHiC HiP SEHiP pi SEpi;
	format piC 5.3 vpiC 6.4 piP 5.3 vpiP 6.4 HC HP H Hi SEHi HiC SEHiC HiP SEHip 8.1 pi 5.3 SEpi 6.4;
run;

*{sgplot bubble plots for publication, trying to make bubble plot for main RF species};
*{BLACK};
ods html close; 
ods html;
ods graphics on/
	antialias=on
	scale=off
	height=6in
	width=9in
	border=off;
data plotcomp;
	set comp4;
	if port = 'Whittier';
	if year le 2016;
	if year ge 1996;
	if species in('Black','Dark','Dusky','Yelloweye','Copper','China','Quillback','Tiger');
	length rank 3.;
	if species = 'Black' then rank = 1;
	if species = 'Dark' then rank = 2;
	if species = 'Dusky' then rank = 3;
	if species = 'Yelloweye' then rank = 4;
	if species = 'Copper' then rank = 5;
	if species = 'China' then rank = 6;
	if species = 'Quillback' then rank = 7;
	if species = 'Tiger' then rank = 8;
proc sort data=plotcomp;
	by port year rank;
title;
proc sgplot data=plotcomp noautolegend;	
	*title 'Cook Inlet RF species comp';
	*by port;
	*where port = 'CI'; *{EDIT THIS LINE TO PRODUCE PLOTS WITHOUT port NAMES};
		bubble x=year y=species size=pi/fill fillattrs=(color=lightgray) outline lineattrs=(thickness=.01mm) bradiusmin=.3% bradiusmax=3% ;
		series x=year y=n/y2axis lineattrs=(color=gray thickness=3 pattern=solid) transparency=.7;
		*series x=year y=age/group=cohort nomissinggroup lineattrs=(color=black pattern=dot);
		xaxis grid values=(1995 to 2020 by 5) label='Year';
		yaxis type=discrete discreteorder=data grid label='Species';
		y2axis min=0 label='Sample Size';
run;
*Transpose to make appendix table of species proportions by port and year;
proc sort data=comp4;
	by port year;
proc transpose data=comp4 out=comp4_Tpi;
	by port year;
	id abbrev;
	var pi;
run;
data comp4_tpi;
	set comp4_Tpi;
	drop _name_;
title 'Species comp by port (exclude blank columns for appendix table';
proc print data=comp4_Tpi noobs uniform;
	by port;
	var year port BGL BLK BOC CAN CHI COP DRK DUS DUSDRK HAR NOR QUI REYE ROS RSTP SGRY SHCN SRAK TIG VER WID YEYE YTAL;
	format BGL BLK BOC CAN CHI COP DRK DUS DUSDRK HAR NOR 
		QUI REYE ROS RSTP SGRY SHCN SRAK TIG VER WID YEYE YTAL 5.3;
run;

*Transpose to make appendix tables of standard errors of species proportions by port and year;
proc sort data=comp4;
	by port year;
proc transpose data=comp4 out=comp4_TSEpi;
	by port year;
	id abbrev;
	var SEpi;
run;
data comp4_tSEpi;
	set comp4_TSEpi;
	drop _name_;
title 'Standard errors of species comp by port (exclude blank columns for appendix table';
proc print data=comp4_TSEpi noobs uniform;
	by port;
	var year port BGL BLK BOC CAN CHI COP DRK DUS DUSDRK HAR NOR QUI REYE ROS RSTP SGRY SHCN SRAK TIG VER WID YEYE YTAL;
	format BGL BLK BOC CAN CHI COP DRK DUS DUSDRK HAR NOR 
		QUI REYE ROS RSTP SGRY SHCN SRAK TIG VER WID YEYE YTAL 5.3;
run;

*==========================================================================
{Determine which species are primary species region-wide, i.e., which
	species are most common (more than 10%) at at least one port}
===========================================================================;
ods html close;
ods html;
proc sgpanel data=comp4;
	by port;
	panelby species/novarname onepanel;
	series x=year y=pi;
	refline 0.1;
run;
*{RESULT: show table values for black, yelloweye, dark, dusky, dusky/dark, 
	quillback, copper, and other};

*(Define major species and "other" group for table};
data comp5;
	set comp4;
	if abbrev not in('BLK','YEYE','QUI','COP','DUS','DRK','DUSDRK') then abbrev = 'OTH';
proc freq data=comp5;
	table species abbrev;
run;

*Now add up species proportions for 'Other' species;
proc sort data=comp5;
	by port year abbrev;
proc means data=comp5;
	by port year abbrev;
	output out=majorsp sum(pi)=pi;
proc print data=majorsp noobs uniform;
run;

*{Transpose to make table for report};
proc transpose data=majorsp out=majorT;
	by port year;
	id abbrev;
	var pi;
run;
data majorT; set majorT;
	drop _name_;
title 'Main species table';
proc print data=majorT noobs uniform;
	format BLK COP DUSDRK YEYE OTH QUI DRK DUS 5.3;
run;

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 Good to here 3/23
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;



ods graphics on/
	outputfmt=png
	width=5in
	height=8in
	border=off;
title;

ods html style=journal;
ods graphics on/
	outputfmt=png
	width=4in
	height=8in
	border=off;
proc sgpanel data=comp4blackYE;
	title;
	panelby port/rows=5 columns=1 uniscale=all novarname spacing=10;
	series x=year y=pi/group=species lineattrs=(thickness=1);
	rowaxis offsetmin=0;
	band x=year upper=approxuppererr lower=approxlowererr/group=species grouporder=descending fill transparency=.4;
run;
*proc freq data=comp4;
*	table species;
*run;

*{Transpose to get harvest of each species by port and year};
proc transpose data=comp4 out=comp4_THi;
	by port year;
	id species;
	var Hi;
proc print data=comp4_THi noobs uniform;
	title 'Harvest by Species 1996-current';
	var port year Black Blackgill Bocaccio Canary China Copper Dark Dusky DuskyDark Harlequin Northern Quillback
			Redstripe Rosethorn Rougheye Shortraker Silvergray Tiger Vermilion Widow Yelloweye Yellowtail;
	format Black Blackgill Bocaccio Canary China Copper Dark Dusky DuskyDark Harlequin Northern Quillback 
			Redstripe Rosethorn Rougheye Shortraker Silvergray Tiger Vermilion Widow Yelloweye Yellowtail 5.; 
run;
*{Plot it};
proc sgpanel data=comp4_THi;
	title 'Harvest by Species 1996-2015';
	panelby port/uniscale=column rows=5 columns=1;
	series x=year y=Black;
	series x=year y=Yelloweye;
	series x=year y=Dusky;
	series x=year y=Dark;
	series x=year y=Quillback;
run;
*/

*{Better plot with black-yelloweye-other harvest};
title;
data byo;
	set comp4;
	*format Hi comma5.0;
	if species = 'Dusky' or species = 'Dark' or species = 'DuskyDark' then species = 'Dusky/Dark';
	if species ne 'Black' and species ne 'Yelloweye' and species ne 'Dusky/Dark' then species = 'Other';
	if species = 'Black' then	order = 1;  *{order the data for vbar plot below};
	if species = 'Dusky/Dark' then order = 2;
	if species = 'Yelloweye' then order = 3;
	if species = 'Other' then order=4;
	keep port year species order pi Hi;
proc sort data=byo;
	by order;
run;
proc print data=byo noobs uniform; run;

*{data attribute map};
data barcolor;
	length value $12.;
	*retain linecolor 'black';
	input id $ value $ linecolor $ fillcolor $;
	datalines;
		species Black black black
		species Dusky/Dark black gray
		species Yelloweye black ltgray
		species Other black white
		;
run;
*proc print data=barcolor;run;
ods html style=journal;
ods graphics on/
	outputfmt=png
	width=5in
	height=8in
	border=off;
proc sgpanel data=byo dattrmap=barcolor;
	panelby port/uniscale=all rows=5 columns=1 novarname spacing=10;
	format Hi comma6.0;
	vbar year/response=pi group=species grouporder=data stat=sum attrid=species;
	colaxis fitpolicy=thin;
	rowaxis label='Proportion in Harvest';
run;
proc sgpanel data=byo dattrmap=barcolor;
	panelby port/uniscale=all rows=5 columns=1 novarname spacing=10;
	format Hi comma6.0;
	vbar year/response=Hi group=species grouporder=data stat=sum attrid=species;
	colaxis fitpolicy=thin;
	rowaxis label='Harvest (No. Fish)';
run;
ods html style=htmlblue; run;

 

*{Final check - make sure pi adds to 1 across all species for each port and year};
proc means data=comp4 noprint;
	by port year;
	output out=sumpi sum(pi)=sumpi;
proc print data=sumpi noobs uniform;
	title 'Variable sumpi should be 1.0 for each port and year.';
	var port year _freq_ sumpi;
run;

*{Compare number of species and total rockfish sample size};
*{First get the number of species};
data sumpi;
	set sumpi;
	NumSpecies = _freq_;
	drop _type_ _freq_;
proc print data=sumpi; run;
*{Now get the total sample sizes};
proc means data=comp4 noprint;
	by port year;
	output out=sampsize sum(niC niP niU niM)=nC nP nU nM;
data sampsize;
	set sampsize;
	n = sum(nC,nP,nU,nM);
	keep port year n;
data sampsize;
	merge sampsize sumpi;
	by port year;
*proc print data=sampsize; run;

*{Now plot it};
ods html style=journal;
ods graphics on/
	outputfmt=png
	width=8in
	height=3in
	border=off;
proc sgpanel data=sampsize;
	title 'Number of species vs. sample size (n)';
	panelby port/rows=1 columns=5 uniscale=row novarname;
	scatter x=n y=NumSpecies;
	rowaxis integer;
	colaxis min=0;
run;
ods html style=htmlblue; run;



*{Run numbers to determine the "primary" species for age/length/sex composition.};
proc sort data=comp4;
	by port species;
proc means data=comp4 noprint;
 by port species;
 output out=primarysp mean(pi)=meanpi;
proc print data=primarysp noobs uniform;
	title 'Average species proportions 1996-current';
run;

*{plot it};
*{title 'Distribution of estimated species proportions, 1996-current.'};
title;
ods graphics on/
	scale=off
	outputfmt=png
	width=5in
	height=8in
	border=off;
proc sgpanel data=comp4;
	panelby port/uniscale=all rows=5 spacing=5 novarname;
	vbox pi/category=abbrev extreme;
	refline 0;
	colaxis grid;
	rowaxis label='Proportion in Sport Harvest';
	title; 
run;
ods graphics on/reset=all;
run;


*{Convert data to assemblage composition};
data assemb;
	set comp4;
	length Assemb $10.;
	if species in('Black','Dark','Dusky','DuskyDark','Yellowtail','Widow') then Assemb = 'Pelagic';
	else Assemb = 'Nonpelagic';
proc sort data=Assemb;
	by port Assemb year;
proc means data=Assemb noprint;
	by port Assemb year;
	output out=AssembComp sum(Hi HiC HiP pi)=Hasmb HC HP pAsmb;
proc print data=AssembComp noobs uniform; 
	format Hasmb HC HP 5. pAsmb 5.3;
run;
*{plot};
title;
data AssembComp;
	set AssembComp;
	if Assemb = 'Pelagic' then	order = 1;  *{order the data for vbar plot below};
	if Assemb = 'Nonpelagic' then order = 2;
	keep port year assemb order HAsmb;
proc sort data=AssembComp;
	by order;
run;
*{data attribute map};
data barcolor;
	length value $12.;
	input id $ value $ linecolor $ fillcolor $;
	datalines;
		Assemb Pelagic black black
		Assemb Nonpelagic black ltgray
		;
run;
*proc print data=barcolor;run;
ods html style=journal;
ods graphics on/
	outputfmt=png
	width=5in
	height=8in
	border=off;

proc sgpanel data=AssembComp dattrmap=barcolor;
	panelby port/uniscale=all rows=5 columns=1 novarname spacing=10;
	format HAsmb comma6.0;
	vbar year/response=HAsmb group=Assemb grouporder=data stat=sum attrid=Assemb;
	colaxis fitpolicy=thin;
	rowaxis label='Harvest (No. Fish)';
run;
ods html style=htmlblue; run;




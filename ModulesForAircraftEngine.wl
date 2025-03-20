(* ::Package:: *)

(* ::Title:: *)
(*MODULES*)


(* ::Text:: *)
(*Version 2.3.1 (Feb 2025)*)
(*Credits: Mauro Valorani, RIccardo Malpica Galassi, Pietro Paolo Ciottoli*)
(*Special thanks to: Roberto Forte, Daniel D'Onghia, Matteo Buscicchio, Giovanni Pappalardi, Francesco Grani*)


(* ::Section::Closed:: *)
(*Initialization*)


SetDirectory[NotebookDirectory[]]


BeginPackage["ModulesForAircraftEngine`",{"Units`"}]

(*ISA*)
Tstd::usage = " T[h] computes ISA temperature at altitude h ";
\[Theta]std::usage = " \[Theta][h] computes dimensionless ISA temperature at altitude h ";
pstd::usage = " p[h] computes ISA pressure at altitude h ";
\[Delta]std::usage = " \[Delta][h] computes dimensionless ISA pressure at altitude h ";
\[Rho]std::usage = " \[Rho][h,\[CapitalDelta]T] computes ISA density at altitude h, allowing for T deviations from ISA ";
\[Theta]0std::usage = " \[Theta][h,M] computes dimensionless freestream total temperature at altitude h and Mach M";
\[Delta]0std::usage = " \[Delta][h,M] computes dimensionless freestream total pressure at altitude h and Mach M ";

(*Mission*)

cd0::usage = "";
k1::usage = "";
k2::usage = "";
\[Alpha]TF::usage = "";
\[Alpha]LTFmax::usage = "";
\[Alpha]LTFmil::usage = "";
\[Alpha]TJmax::usage = "";
\[Alpha]TJmil::usage = "";


(*Engine*)
MFP::usage="";
mredPar::usage="";
mred::usage="";
FreeStream::usage="Restituisce le variabili di stato nella condizione di Flusso Indisturbato";
DiffuserCompression::usage="Restituisce le variabili di stato all'uscita del Diffusore";
CompressorCompression::usage="Restituisce le variabili di stato all'uscita del Compressore";
BurnerCombustion::usage="Restituisce le variabili di stato all'uscita del Combustore";
TurbineExpansion::usage="Restituisce le variabili di stato all'uscita della Turbina";
NozzleExpansion::usage="Restituisce le variabili di stato all'uscita dell'Ugello";
CalcTurbojetCycle::usage="Restituisce le variabili di stato e le prestazioni di un Turbogetto";
CalcTurboFanCycle::usage="Restituisce le variabili di stato e le prestazioni di un TurboFan";
PRatioComp::usage=""
getEqns::usage=""
BuildEngine::usage=""
testOffDesignEquiPoint::usage=""
calcOffDesignEquiPoint::usage=""
rulet::usage=""
Test::usage=""
DiffuserOutlet::usage""
PsiMap::usage""
cp::usage""
multistageCompressorState::usage""
stageSizingAlongMeanPathLine::usage""
StagePerformanceAtOnePathLine::usage""
StaticPropertiesAcrossStage::usage""
StagePerformance::usage""
StageReducedProperties::usage""
AnnulusGeometry::usage""
InnerStage::usage""
FirstStage::usage""
SizeTurbineStage::usage""
SmartSizeTurbineStage::usage""
SizeTurbine::usage""
TF::usage""
pF::usage""
T0ratio::usage""
p0ratio::usage""
multistageCompressorMap::usage""
PlotCompressorMap::usage""
PRatioMultistageCompressor::usage""
findCompressorStabilityBoundary::usage""
\[Epsilon]::usage""
mredIntake::usage""
mredTurbine::usage""
PratioCrit::usage""
MakeOffDesignStates::usage""
plotCompressorMapFromFile::usage""
plotPoint::usage""
Deg2Rad::usage""
printEqns::usage""


(*Compressor*)
triangleDesignFromEqns::usage = " Solves velocity triangle equations and return a plot";
triangleDesign::usage = " Solves velocity triangle equations starting from figures of merit and return a plot";
estimateNofStages::usage = ""
estimateNofStagesAdvanced::usage = ""
designMultiStageCompressor::usage = ""
nicePlotFromDB::usage = ""
drawCompressor::usage = ""
extractPropertyAlongCompressor::usage = ""


plotOffDesignPerfo::usage = ""
plotOffDesignCycle::usage = ""


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Utilities*)


(* ::Subsection::RGBColor[0, 0, 1]:: *)
(*Thermodynamics*)


(*Constant pressure specific heat -Cp- definition*)
cp[\[Gamma]_,Rgas_]:=(\[Gamma]  Rgas)/(\[Gamma]-1);

(*Density of a real gas \[Rho]=p/(RT) *)
\[Rho][p_,T_,Rgas_]:=p /(Rgas T);

(*Entropy s definition*)
s[p_,T_,\[Gamma]_, Rgas_]:=cp[\[Gamma],Rgas] Log[T/Tref]-Rgas Log[p/Pref]/.{Pref->101325.0,Tref->273.15};

(*Specific Volume*)
vsp[p_,T_,Rgas_]:=1/\[Rho][p,T,Rgas];

(*total pressure*)
p0F[p_,Ma_,\[Gamma]_]:=p (1+(\[Gamma]-1)/2 Ma^2)^(\[Gamma]/(\[Gamma]-1));

(*total temperature*)
T0F[T_,Ma_,\[Gamma]_]:=T(1+(\[Gamma]-1)/2 Ma^2);

(*Static temperature*)
TF[T0_,\[Gamma]air_,Ma_]:=T0/(1+(\[Gamma]air-1)/2 Ma^2);

(*Statica pressure*)
pF[p0_,\[Gamma]air_,Ma_]:=p0/(1+(\[Gamma]air-1)/2 Ma^2)^(\[Gamma]air/(\[Gamma]air-1));



(* ::Subsection::RGBColor[0, 0, 1]:: *)
(*Stagnation ratios*)


(*Pressure Ratio of Stagnation definition p0/p *)
p0ratio[\[Gamma]_,M_]:=(1+(\[Gamma]-1)/2 M^2)^(\[Gamma]/(\[Gamma]-1));

(*Temperature Ratio of Stagnation definition T0/T *)
T0ratio[\[Gamma]_,M_]:=(1+(\[Gamma]-1)/2 M^2);

(*Density Ratio of Stagnation definition \[Rho]0/\[Rho]*)
\[Rho]0ratio[\[Gamma]_,M_]:=T0ratio[\[Gamma],M]^(1/(\[Gamma]-1));




(* ::Subsection::RGBColor[0, 0, 1]:: *)
(*Massflow*)


(*Massflow parameter *)
MFP[\[Gamma]_,M_]:=Sqrt[\[Gamma]]*M*(1.+(\[Gamma]-1)/2 M^2)^(1/2-\[Gamma]/(\[Gamma]-1));

(*Massflow parameter as a function of Pratio*)
mredPar[pratio_,\[Eta]_,\[Gamma]_]:= Sqrt[(2 \[Gamma])/(\[Gamma]-1)](pratio)  Sqrt[\[Eta](1-(pratio)^((\[Gamma]-1)/\[Gamma]))]/(1-\[Eta](1-(pratio)^((\[Gamma]-1)/\[Gamma])))/;pratio>=2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma])));
mredPar[pratio_,\[Eta]_,\[Gamma]_]:= Sqrt[(2 \[Gamma])/(\[Gamma]-1)](2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma]))))  Sqrt[\[Eta](1-(2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma]))))^((\[Gamma]-1)/\[Gamma]))]/(1-\[Eta](1-(2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma]))))^((\[Gamma]-1)/\[Gamma])))/;pratio<2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma])));

(*Reduced Massflow definition *)
mred[massflow_,T0_,p0_,rgas_,area_]:=(massflow Sqrt[rgas T0])/(p0 area);


(* ::Section::Closed:: *)
(*International Standard Atmosphere (ISA) Model*)


(* ::Text:: *)
(*Actual altitude and ISA index:*)
(*1 - Troposphere (-6.05 C/km)  0 < h < 10000 m *)
(*2 - Tropopause (+0.0 C/km)  11000 < h < 20000 m *)
(*3 - Stratosphere (+1.0 C/km)  20000 < h < 32000 m *)


zzz[h_]:=r0*h/(r0+h)/.r0->6356577.
iii[z_]:=Piecewise[{{1,z<=11000},{2,11000<z<=20000},{3,20000<z<32000}}]


(* ::Text:: *)
(*Temperature*)


Tstd[h_]:=Module[
{Lstd,zstd,T0,j,out},

(*Coefficients definitions*)
Lstd={-6.5,0.,1.};
zstd={0.,11000.,20000.};
T0={288.15,216.65,216.65};

(*Index i[z]*)
j=iii[zzz[h]];

(*ISA model*)
out=T0[[j]]+Lstd[[j]]/1000 (zzz[h]-zstd[[j]]);

Return[out];
]


(* ::Text:: *)
(*Dimensionless temperature*)


\[Theta]std[h_]:=Tstd[h]/287;


(* ::Text:: *)
(*Pressure model*)


pstd[h_]:=100*((44331.514-h)/11880.516)^(1./0.1902632);


(* ::Text:: *)
(*Dimensionless pressure*)


\[Delta]std[h_]:=pstd[h]/101325.;


(* ::Text:: *)
(*Density (according to ideal gas EOS) in ISA environment, allowing for Temperature deviations from ISA (\[CapitalDelta]T)*)


\[Rho]std[h_,\[CapitalDelta]T_]:=pstd[h]/(R (Tstd[h]+\[CapitalDelta]T) )/.R->287.;


(* ::Text:: *)
(*Other useful functions (dimensionless freestream total temperature and pressure)*)


\[Theta]0std[h_,M0_]:=\[Theta]std[h](1+0.2 M0^2);
\[Delta]0std[h_,M0_]:=\[Delta]std[h](1+0.2 M0^2)^(\[Gamma]/(\[Gamma]-1))/.\[Gamma]->1.4;


(* ::Section::Closed:: *)
(*Mission Analysis*)


(* ::Subsection:: *)
(*Aerodynamic model*)


k1[commercial_,fighter_,Mach_,AR_,e_]:=Piecewise[{{ki[AR,e]+kv,commercial},{Piecewise[{{0.18,Mach<1},{0.18*Mach,Mach>=1}}],fighter}}]/.kv->0.01;
k2[commercial_,fighter_]:=Piecewise[{{-2kv CLmin,commercial},{0,fighter}}]/.{kv->0.01,CLmin->0.2}; 
ki[AR_,e_]:=1/(Pi AR e);
cd0[commercial_,fighter_,Mach_]:=Piecewise[{{Piecewise[{{0.017,Mach<=0.8},{0.035*Mach-0.011,Mach>0.8}}]+kv CLmin^2,commercial},{Piecewise[{{0.014,Mach<=0.8},{0.035*Mach-0.014,0.8<Mach<1.2},{0.028,Mach>=1.2}}],fighter}}]/.{kv->0.01,CLmin->0.2};


(* ::Subsection:: *)
(*Thrust Lapse model*)


(* ::Subsubsection:: *)
(*High BPR TurboFan*)


\[Alpha]1[h_,M0_,TR_]:=\[Delta]0std[h,M0]*(1-0.49 Sqrt[M0]);
\[Alpha]2[h_,M0_,TR_]:=\[Delta]0std[h,M0]*(1-0.49 Sqrt[M0]-(3(\[Theta]0std[h,M0]-TR))/(1.5+M0));
\[Alpha]TF[h_,M0_,TR_]:=Piecewise[{{\[Alpha]1[h,M0,TR],\[Theta]0std[h,M0]<=TR},{\[Alpha]2[h,M0,TR],\[Theta]0std[h,M0]>TR}}];


(* ::Subsubsection:: *)
(*Low BPR TurboFan (Max Power)*)


\[Alpha]3[h_,M0_,TR_]:=\[Delta]0std[h,M0];
\[Alpha]4[h_,M0_,TR_]:=\[Delta]0std[h,M0]*(1-(3.5*(\[Theta]0std[h,M0]-TR))/\[Theta]0std[h,M0]);
\[Alpha]LTFmax[h_,M0_,TR_]:=Piecewise[{{\[Alpha]3[h,M0,TR],\[Theta]0std[h,M0]<=TR},{\[Alpha]4[h,M0,TR],\[Theta]0std[h,M0]>TR}}];


(* ::Subsubsection:: *)
(*Low BPR TurboFan (Military Power)*)


\[Alpha]5[h_,M0_,TR_]:=0.6*\[Delta]0std[h,M0];
\[Alpha]6[h_,M0_,TR_]:=0.6*\[Delta]0std[h,M0]*(1-(3.8*(\[Theta]0std[h,M0]-TR))/\[Theta]0std[h,M0]);
\[Alpha]LTFmil[h_,M0_,TR_]:=Piecewise[{{\[Alpha]5[h,M0,TR],\[Theta]0std[h,M0]<=TR},{\[Alpha]6[h,M0,TR],\[Theta]0std[h,M0]>TR}}];


(* ::Subsubsection:: *)
(*TurboJet (Max Power)*)


\[Alpha]7[h_,M0_,TR_]:=\[Delta]0std[h,M0]*(1-0.3(\[Theta]0std[h,M0]-1)-0.1* Sqrt[M0]);
\[Alpha]8[h_,M0_,TR_]:=\[Delta]0std[h,M0]*(1-0.3(\[Theta]0std[h,M0]-1)-0.1* Sqrt[M0]-(1.5*(\[Theta]0std[h,M0]-TR))/\[Theta]0std[h,M0]);
\[Alpha]TJmax[h_,M0_,TR_]:=Piecewise[{{\[Alpha]7[h,M0,TR],\[Theta]0std[h,M0]<=TR},{\[Alpha]8[h,M0,TR],\[Theta]0std[h,M0]>TR}}];


(* ::Subsubsection:: *)
(*TurboJet (Military Power)*)


\[Alpha]9[h_,M0_,TR_]:=0.8*\[Delta]0std[h,M0]*(1-0.16* Sqrt[M0]);
\[Alpha]10[h_,M0_,TR_]:=0.8*\[Delta]0std[h,M0]*(1-0.16* Sqrt[M0]-(24*(\[Theta]0std[h,M0]-TR))/((9+M0)*\[Theta]0std[h,M0]));
\[Alpha]TJmil[h_,M0_,TR_]:=Piecewise[{{\[Alpha]9[h,M0,TR],\[Theta]0std[h,M0]<=TR},{\[Alpha]10[h,M0,TR],\[Theta]0std[h,M0]>TR}}];


(* ::Section:: *)
(*Cycle Analysis*)


(* ::Subsection::Closed:: *)
(*Engine Components*)


(* ::Subsubsection::RGBColor[0, 0, 1]::Closed:: *)
(*FreeStream*)


(* ::Text:: *)
(*Modulo che calcola le grandezze statiche e di ristagno del FLUSSO INDISTURBATO*)
(**)
(*	INPUT : [h,M,\[Gamma]air,Rair,report]*)
(*		"h": quota (es. 5000);*)
(*		"M": n\[Degree]Mach di volo (es. 0.83);*)
(*		"\[Gamma]air , Rair" : propriet\[AGrave] del gas (es. Per l'aria secca \[Gamma] = 1.4, R = 287.05 J/kg/K);*)
(*		"report": variabile booleana di stampa*)
(**)
(*	OUTPUT :  { pntOut { }, pnt0Out { }, Va }*)
(*		pntOut = {pa, Ta, \[Rho]a, sa}  --> Grandezze STATICHE:*)
(*			"pa": pressione atmosferica;*)
(*			"Ta": temperatura atmosferica;*)
(*			"\[Rho]a": densit\[AGrave] atmosferica;*)
(*			"sa": entropia atmosferica;*)
(*		pnt0Out = {p0a, T0a, \[Rho]0a, s0a} --> Grandezze GLOBALI:*)
(*			"p0a": pressione atmosferica globale;*)
(*			"T0a": temperatura atmosferica globale;*)
(*			"\[Rho]0a": densit\[AGrave] atmosferica globale;*)
(*			"s0a": entropia atmosferica globale;*)
(*		*)
(*		"Va": velocit\[AGrave] del flusso indisturbato ;*)


FreeStream[h_,M_,\[Gamma]air_,Rair_,report_]:=Module[
	{pa,Ta,\[Rho]a,sa,
	p0a,T0a,\[Rho]0a,s0a,
	pntOut,pnt0Out,
	Va,
	Values,Units,ValuesAndUnits},

(* Grandezze STATICHE *)
	pa=pstd[h];
	Ta=Tstd[h];
	\[Rho]a=\[Rho]std[h,0];
	sa=s[pa,Ta,\[Gamma]air, Rair];

		pntOut={pa,Ta,\[Rho]a,sa};

	Va=M Sqrt[\[Gamma]air Rair  Ta];


(* Grandezze GLOBALI o di RISTAGNO *)
	p0a=pa*p0ratio[\[Gamma]air,M];
	T0a=Ta*T0ratio[\[Gamma]air,M];
	\[Rho]0a=\[Rho][p0a,T0a,Rair];
	s0a=s[p0a,T0a,\[Gamma]air, Rair];

		pnt0Out={p0a,T0a,\[Rho]0a,s0a};

(*Stampa dei parametri*)

If[report,
		Units={" km",""," m/s"," kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K"};
		Values=Join[h/1000,M,Va,pa/1000,Ta,\[Rho]a,sa/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,7}];
		Print["Free Stream Static Condition"];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"Height","Mach","Speed","\!\(\*SubscriptBox[\"p\", \"a\"]\)","\!\(\*SubscriptBox[\"T\", \"a\"]\)","\!\(\*SubscriptBox[\"\[Rho]\", \"a\"]\)","\!\(\*SubscriptBox[\"s\", \"a\"]\)"}},TableDirections->Row
		]];
		Print[""];
		
		Units={" kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K"};
		Values=Join[p0a/1000,T0a,\[Rho]0a,s0a/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,4}];
		Print["Free Stream Stagnation Condition"];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"\!\(\*SubscriptBox[\"p\", 
RowBox[{\"0\", \"a\"}]]\)","\!\(\*SubscriptBox[\"T\", 
RowBox[{\"0\", \"a\"}]]\)","\!\(\*SubscriptBox[\"\[Rho]\", 
RowBox[{\"0\", \"a\"}]]\)","\!\(\*SubscriptBox[\"s\", 
RowBox[{\"0\", \"a\"}]]\)"}},TableDirections->Row
		]];
		Print[""];
	];


	{pntOut,pnt0Out,Va}
]


(* ::Subsubsection::RGBColor[0, 0, 1]::Closed:: *)
(*DiffuserCompression*)


(* ::Text:: *)
(*	Modulo che calcola la Compressione nel DIFFUSORE*)
(**)
(*		INPUT : [pntIn { }, Ma, pa, \[Eta]d, \[Gamma]air, Rair,report]*)
(*			pntIn  = { p01, T01, \[Rho]01, s01 } stato all'ingresso del Diffusore ;	*)
(*				"Ma": n\[Degree]Mach di volo all'ingresso del Diffusore (es. 0.83);*)
(*				"pa": pressione statica all'ingresso del Diffusore;*)
(*				"\[Eta]d" : rendimento adiabatico del Diffusore;*)
(*				"\[Gamma]air , Rair" : propriet\[AGrave] del gas (es. Per l'aria secca \[Gamma] = 1.4, R = 287.05 J/kg/K);*)
(*				"report": variabile booleana di stampa*)
(*			*)
(*		OUTPUT : {pntOut { }, ratioDiffuser, work}*)
(*			pntOut = { p02, T02, \[Rho]02, s02 } stato all'esterno del Diffusore;*)
(*				"ratioDiffuser": rapporto di compressione del Diffusore;*)
(*				"work": potenza specifica del Diffusore (specifica rispetto alla portata d'aria elaborata) [ KJ/kg = kW/(kg/s) ];*)


DiffuserCompression[pntIn_,Ma_,pa_,\[Eta]d_,\[Gamma]air_,Rair_,report_]:=Module[
	{p01,T01,\[Rho]01,s01,
	p02,T02,s02,\[Rho]02,
	work,ratioDiffuser,
	pntOut,
    Values,Units,ValuesAndUnits},

	{p01,T01,\[Rho]01,s01}=pntIn;

	ratioDiffuser=(1+\[Eta]d (\[Gamma]air-1)/2 Ma^2)^(\[Gamma]air/(\[Gamma]air-1));

	p02=pa*ratioDiffuser;  (* p2\[TildeFullEqual]p02 *)
	T02=T01; (* T2 \[TildeFullEqual] T02 = T0a ;  T2 dipende da M0 (quota e V) e Ta (quota) *)
	\[Rho]02=\[Rho][p02,T02,Rair];
	s02=s[p02,T02,\[Gamma]air, Rair];

		pntOut={p02,T02,\[Rho]02,s02};

		work=cp[\[Gamma]air,Rair](T02-T01);


(*Stampa dei parametri*)

	If[report,
		Units={"",""," kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K",""," kW/(kg/s)"};
		Values=Join[\[Gamma]air,\[Eta]d,p02/1000,T02,\[Rho]02,s02/1000,ratioDiffuser,work/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,Length[Units]}];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"\[Gamma]","\!\(\*SubscriptBox[\"\[Eta]\", \"d\"]\)","\!\(\*SubscriptBox[\"p\", \"02\"]\)","\!\(\*SubscriptBox[\"T\", \"02\"]\)","\!\(\*SubscriptBox[\"\[Rho]\", \"02\"]\)","\!\(\*SubscriptBox[\"s\", \"02\"]\)","Pressure ratio diffuser","Specific power"}},TableDirections->Row
		]];
		Print[""];
		
		
	];	


	{pntOut,ratioDiffuser,work}
]


(* ::Subsubsection::RGBColor[0, 0, 1]::Closed:: *)
(*CompressorCompression*)


(* ::Text:: *)
(*	Modulo che calcola la Compressione nel COMPRESSORE *)
(*		*)
(*		INPUT :  [pntIn { }, massflow, \[Beta]c, \[Eta]c, \[Gamma], Rair,report]*)
(*			pntIn = { p01, T01, \[Rho]01, s01 }  stato all'ingresso del Compressore/Fan ;*)
(*				"massflow": portata in massa d'aria specifica elaborata dal Compressore/Fan (specificata rispetto alla portata d'aria primaria) [adimensionale]*)
(*					(es. Per il Turbofan si avr\[AGrave] nel Fan "massflow = 1+BPR" e nel Compressore "massflow = 1")*)
(*				"\[Beta]c": rapporto di compressione del Compressore/Fan;*)
(*				"\[Eta]s" : rendimento politropico (per il compressore)/adiabatico (per il Fan);*)
(*				"\[Gamma]air , Rair" : propriet\[AGrave] del gas (es. Per l'aria secca \[Gamma] = 1.4, R = 287.05 J/kg/K);*)
(*				"report": variabile booleana di stampa*)
(**)
(*		OUTPUT :  {pntOut { }, work}*)
(*			pntOut { p02, T02, \[Rho]02, s02 } : stato all'esterno del Compressore/Fan;*)
(*				"work": potenza specifica del Compressore/Fan (specifica rispetto alla portata d'aria primaria elaborata) [ KJ/kg = kW/(kg/s) ];*)


CompressorCompression[pntIn_, massflow_, \[Beta]c_,\[Eta]c_,\[Gamma]air_,Rair_,report_]:=Module[
	{p01,T01,\[Rho]01,s01,
	p02,T02,\[Rho]02,s02,
	work,
	pntOut,
	Values,Units,ValuesAndUnits},

		{p01,T01,\[Rho]01,s01}=pntIn;

	
	p02=\[Beta]c*p01;
	T02=T01 (1+(\[Beta]c^((\[Gamma]air-1)/\[Gamma]air)-1)/\[Eta]c);
	\[Rho]02=\[Rho][p02,T02,Rair];
	s02=s[p02,T02,\[Gamma]air, Rair];
		pntOut={p02,T02,\[Rho]02,s02};

	work=massflow*cp[\[Gamma]air, Rair]*(T02-T01);


(*Stampa dei parametri*)

	If[report,
		
		Units={"","",""," kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K"," kJ/kg"};
		Values=Join[\[Beta]c,\[Gamma]air,\[Eta]c,p02/1000,T02,\[Rho]02,s02/1000,work/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,Length[Units]}];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"\!\(\*SubscriptBox[\"\[Beta]\", \"c\"]\)","\[Gamma]","\!\(\*SubscriptBox[\"\[Eta]\", \"c\"]\)","\!\(\*SubscriptBox[\"p\", \"02\"]\)","\!\(\*SubscriptBox[\"T\", \"02\"]\)","\!\(\*SubscriptBox[\"\[Rho]\", \"02\"]\)","\!\(\*SubscriptBox[\"s\", \"02\"]\)","Specific power"}},TableDirections->Row
		]];
		Print[""];
			
	];	

	{pntOut,work}
]


(* ::Subsubsection::RGBColor[0, 0, 1]:: *)
(*BurnerCombustion*)


(* ::Text:: *)
(*Modulo che calcola la Combustione nel COMBUSTORE*)
(**)
(*INPUT :  [pntIn { }, \[Eta]b,Qf, T02, \[Gamma]air, Rair, \[Gamma]gas, Rgas,report]*)
(*	pntIn { p01, T01, \[Rho]01, s01 }  stato all'ingresso del Combustore ;*)
(*	"\[Eta]b" : rendimento di combustione (es. \[Eta]b=0.99);*)
(*	"\[Eta]pb" : rendimento pneumatico del Combustore (es. \[Eta]pb=0.95);*)
(*	"Qf": potere calorifico del combustibile (calore o energia rilasciata per unit\[AGrave] di massa del combustibile.  es. Qf = 43*10^6J/kg);*)
(*	"T02": temperatura massima ammissibile in Turbina (es. T02 = 1420K);*)
(*	"\[Gamma]air, Rair, \[Gamma]gas, Rgas" : propriet\[AGrave] del gas (es. Per gas combusti  \[Gamma]gas = 1.34, Rgas=286.77 J/kg/K);*)
(*	"report": variabile booleana di stampa*)
(**)
(*OUTPUT :  { pntOut { }, f, heat }*)
(*	pntOut { p02, T02, \[Rho]02, s02 } stato all'esterno del Combustore;*)
(*	"f": rapporto fra le portate in massa di combustibile e aria elaborata dal Combustore ( f = m_fuel / m_air );*)
(*	 "heat": calore (potenza) specifica del Combustore (specifica rispetto alla portata d'aria elaborata) [ KJ/kg = kW/(kg/s) ];*)


BurnerCombustion[pntIn_,\[Eta]b_,\[Eta]pb_,Qf_,T02_,\[Gamma]air_,Rair_,\[Gamma]gas_,Rgas_,report_]:=Module[
	{p01,T01,\[Rho]01,s01,
	p02,\[Rho]02,s02,
	f,heat,
	pntOut,
	Values,Units,ValuesAndUnits},

	{p01,T01,\[Rho]01,s01}=pntIn;

(* Dal bilancio entalpico in ingresso e in uscita dal combustore:
	m_air*cp_air*T01 + m_fuel*Qfuel*\[Eta]b = [m_air+m_fuel]*cp_gas*T02
che introducendo f pu\[OGrave] essere riscritto come:
	cp_air*T01 + f*Qfuel*\[Eta]b = [1+f]*cp_gas*T02
nota T02, si pu\[OGrave] ricavare f.
*)

	f= (cp[\[Gamma]gas,Rair]T02-cp[\[Gamma]air,Rgas]T01)/(\[Eta]b*Qf-cp[\[Gamma]gas,Rgas]T02);

	p02=\[Eta]pb*p01;
	\[Rho]02=\[Rho][p02,T02,Rgas];
	s02=s[p02,T02,\[Gamma]gas, Rgas];

	pntOut={p02,T02,\[Rho]02,s02};

	heat=f*Qf*\[Eta]b;


	(*Stampa dei parametri*)

	If[report,
		Print["Burner Parameters"];
		Units={"",""," kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K",""," kJ/kg"};
		Values=Join[\[Eta]pb,\[Eta]b,p02/1000,T02,\[Rho]02,s02/1000,f,heat/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,Length[Units]}];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"\!\(\*SubscriptBox[\"\[Eta]\", \"pb\"]\)","\!\(\*SubscriptBox[\"\[Eta]\", \"b\"]\)","\!\(\*SubscriptBox[\"p\", \"02\"]\)","\!\(\*SubscriptBox[\"T\", \"02\"]\)","\!\(\*SubscriptBox[\"\[Rho]\", \"02\"]\)","\!\(\*SubscriptBox[\"s\", \"02\"]\)","f","Specific power"}},TableDirections->Row
		]];
		Print[""];
			
	];

	{pntOut,f,heat}
]


(* ::Subsubsection::RGBColor[0, 0, 1]::Closed:: *)
(*TurbineExpansion*)


(* ::Text:: *)
(*	Modulo che calcola la Espansione nella Turbina*)
(**)
(*		INPUT : [pntIn { }, workCompressor, \[Eta]t, f, \[Eta]mc, \[Eta]mt, \[Gamma]gas, Rgas,report]*)
(*			pntIn { p01, T01, \[Rho]01, s01 }  stato all'ingresso della Turbina ;*)
(*			"workCompressor": potenza specifica del Compressore/Fan collegato alla Turbina ;*)
(*			"\[Eta]t": rendimento adiabatico della Turbina (es. 0.9);*)
(*			"f": rapporto fra le portate in massa di combustibile e aria elaborata dal Combustore ( f = m_fuel / m_air );*)
(*			"\[Eta]mc" : rendimento meccanico del Compressore/Fan collegato alla Turbina (es. 0.87);*)
(*			"\[Eta]mt" : rendimento meccanico della Turbina (es. 0.99);*)
(*			"\[Gamma]gas, Rgas" : propriet\[AGrave] del gas (es. Per gas combusti  \[Gamma]gas = 1.34, Rgas=286.77 J/kg/K);*)
(*			"report": variabile booleana di stampa*)
(**)
(*		OUTPUT :  {pntOut { }, ratioTurbine, work}*)
(*			pntOut { p02, T02, \[Rho]02, s02 }  stato all'esterno della Turbina;*)
(*			"ratioTurbine": rapporto di espansione della Turbina;*)
(*			"work": potenza specifica della Turbina (specifica rispetto alla portata d'aria primaria elaborata) [ KJ/kg = kW/(kg/s) ];*)


TurbineExpansion[pntIn_,workCompressor_,\[Eta]t_,f_,\[Eta]mc_,\[Eta]mt_,\[Gamma]gas_,Rgas_,report_]:=Module[
	{p01,T01,\[Rho]01,s01,
	T02,p02,\[Rho]02,s02,
	work,ratioTurbine,
	pntOut,
	Values,Units,ValuesAndUnits},

	{p01,T01,\[Rho]01,s01}=pntIn;

(* "work": potenza specifica utilizzabile della Turbina (al netto cio\[EGrave] delle perdite meccaniche).
La potenza generata dalla Turbina coincide con la potenza all'albero specifica richiesta dal Compressore/Fan collegato alla Turbina *)

	work=workCompressor/\[Eta]mc; 

(* Dal bilancio delle potenze:
	work = \[Eta]mt*(1+f)*cp_gas*(T01-T02)
nota la temperatura T01 massima ammissibile in Turbina, si pu\[OGrave] ricavare la temperatura T02 in uscita dalla Turbina.
*)

	T02=T01-work/( cp[\[Gamma]gas,Rgas]);
	p02=p01 (1-1/\[Eta]t (1-T02/T01))^(\[Gamma]gas/(\[Gamma]gas-1));
	\[Rho]02=\[Rho][p02,T02,Rgas];
	s02=s[p02,T02,\[Gamma]gas, Rgas];

	ratioTurbine=p02/p01;

	pntOut={p02,T02,\[Rho]02,s02};


(*Stampa dei parametri*)

	If[report,
		
		Units={"",""," kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K",""," kJ/kg"};
		Values=Join[\[Gamma]gas,\[Eta]t,p02/1000,T02,\[Rho]02,s02/1000,ratioTurbine,work/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,Length[Units]}];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"\!\(\*SubscriptBox[\"\[Gamma]\", \"gas\"]\)","\!\(\*SubscriptBox[\"\[Eta]\", \"t\"]\)","\!\(\*SubscriptBox[\"p\", \"02\"]\)","\!\(\*SubscriptBox[\"T\", \"02\"]\)","\!\(\*SubscriptBox[\"\[Rho]\", \"02\"]\)","\!\(\*SubscriptBox[\"s\", \"02\"]\)","Turbine pressure ratio","Specific power"}},TableDirections->Row
		]];
		Print[""];
			
	];

	{pntOut,ratioTurbine,work}
]


(* ::Subsubsection::RGBColor[0, 0, 1]:: *)
(*NozzleExpansion*)


(* ::Text:: *)
(*Modulo che calcola la Espansione nell' Ugello*)
(**)
(*INPUT : [pntIn { }, massflow, pa, \[Eta]n, \[Gamma]gas, Rgas,report]*)
(**)
(*	pntIn { p01, T01, \[Rho]01, s01 }  stato all'ingresso dell'Ugello ;*)
(*	"massflow": portata in massa d'aria specifica elaborata dall' Ugello (specificata rispetto alla portata d'aria primaria) [adimensionale]*)
(*	"pa": pressione atmosferica;*)
(*	"\[Eta]n": rendimento adiabatico dell' Ugello (es. 0.9);*)
(*	"\[Gamma]gas, Rgas" : propriet\[AGrave] del gas (es. Per gas combusti  \[Gamma]gas = 1.34, Rgas=286.77 J/kg/K);*)
(*	"report": variabile booleana di stampa*)
(*	*)
(*OUTPUT :  {pntOut { }, pnt0Out { }, ue, Me, ratioNozzle, work}*)
(**)
(*	pntOut { pe, Te, \[Rho]e, se }  grandezze STATICHE all'esterno dell' Ugello;*)
(*	pnt0Out { p02, T02, \[Rho]02, s02 }  grandezze GLOBALI all'esterno dell' Ugello;*)
(*	"ue": velocit\[AGrave] di efflusso [m/s];*)
(*	"ratioNozzle": rapporto di espansione dell' Ugello;*)
(*	"work": potenza specifica dell' Ugello (specifica rispetto alla portata d'aria primaria elaborata) [ KJ/kg = kW/(kg/s) ];*)


NozzleExpansion[pntIn_,massflow_,pa_,\[Eta]n_,\[Gamma]gas_,Rgas_,report_,convergent_:True]:=Module[
	{p01,T01,\[Rho]01,s01,
	T02,p02,\[Rho]02,s02,
	Te,pe,\[Rho]e,se,
	ue,Me,gap,
	criticRatio,pCrit,ratioNozzle,work,
	pntOut,pnt0Out,
	Values,Units,ValuesAndUnits},

	
	{p01,T01,\[Rho]01,s01}=pntIn;

(* Calcolo della pressione critica pCrit
criticRatio = p01 / criticPressure *)

	criticRatio=1/(1-1/\[Eta]n ((\[Gamma]gas-1)/(\[Gamma]gas+1)))^(\[Gamma]gas/(\[Gamma]gas-1));

	pCrit=p01/criticRatio;



(* Controlla che la pressione a monte dell'ugello non sia pi\[UGrave] bassa della pressione ambiente *)
	If[pa>p01, If[report,Print["Error - Turbine exit Stagnation Pressure pe = ", pCrit," below atmospheric pressure ",pa," !"]];(*False second If*)
				Print["Edit design parameters!";Abort[]]];

(* Verifica le condizioni di ugello adattato o in chocking *)	
	If[pa>=pCrit,
		If[report,Print["Nozzle is adapted"]]; pe=pa; ratioNozzle=p01/pa,
		If[report,Print["Nozzle is choked"]];  
			If[convergent,
				ratioNozzle=criticRatio;pe=pCrit;If[report, Print["Convergent nozzle is underexpanded"]],
				ratioNozzle=p01/pa;pe=pa;If[report,Print["Convergent-divergent nozzle is adapted"]]
			]
	];



(* Grandezze STATICHE *)
	Te=T01 (1-\[Eta]n (1-ratioNozzle^(-((\[Gamma]gas-1)/\[Gamma]gas))));
	\[Rho]e=\[Rho][pe,Te,Rgas];
	se=s[pe,Te,\[Gamma]gas, Rgas];

	pntOut={pe,Te,\[Rho]e,se};


(* Grandezze GLOBALI *)
	T02=T01;
	p02 = p01 /(1+1/\[Eta]n (T02/Te-1))^((\[Gamma]gas-1)/\[Gamma]gas);
	s02=s[p02,T02,\[Gamma]gas, Rgas];
	\[Rho]02=\[Rho][p02,T02,Rgas];

	pnt0Out={p02,T02,\[Rho]02,s02};


	ue=Sqrt[2 cp[\[Gamma]gas, Rgas] (T01-Te)];
	Me=ue/Sqrt[\[Gamma]gas*Rgas*Te];
	work=massflow *cp[\[Gamma]gas, Rgas]*(T01-T02);



(*Stampa dei parametri*)

	If[report,

		Print["Nozzle Static Conditions"];
		Units={""," m/s",""," kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K"};
		Values=Join[ratioNozzle,ue,Me,pe/1000,Te,\[Rho]e,se/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,Length[Units]}];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"Pressure ratio nozzle","Speed","Mach","\!\(\*SubscriptBox[\"p\", \"e\"]\)","\!\(\*SubscriptBox[\"T\", \"e\"]\)","\!\(\*SubscriptBox[\"\[Rho]\", \"e\"]\)","\!\(\*SubscriptBox[\"s\", \"e\"]\)"}},TableDirections->Row
		]];
		
		Print[""];
		Units=0; Values=0;ValuesAndUnits;
		Print["Nozzle Stagnation Conditions"];
		Units={" kPa"," K"," kg/\!\(\*SuperscriptBox[\"m\", \"3\"]\)"," kJ/K"," kJ/kg"};
		Values=Join[p02/1000,T02,\[Rho]02,s02/1000,work/1000];
		ValuesAndUnits=Table[ToString[Values[[i]]]<>Units[[i]],{i,Length[Units]}];
		Print[TableForm[
			{ValuesAndUnits},TableHeadings->{None,{"\!\(\*SubscriptBox[\"p\", \"02\"]\)","\!\(\*SubscriptBox[\"T\", \"02\"]\)","\!\(\*SubscriptBox[\"\[Rho]\", \"02\"]\)","\!\(\*SubscriptBox[\"s\", \"02\"]\)","Specific power"}},TableDirections->Row
		]];
		Print[""];
	];	


	{pntOut,pnt0Out,ue,Me,ratioNozzle,work}
]


(* ::Subsection::GrayLevel[0]:: *)
(*Turbojet Cycle & Performance*)


(* ::Text:: *)
(*Modulo che calcola il ciclo di un motore Turbogetto *)
(**)
(*INPUT : [h, Ma, \[Gamma]air, Rair, \[Eta]d,, \[Beta]c, \[Eta]c, \[Eta]mc, \[Eta]b, \[Eta]pb, Qf, T4, \[Gamma]gas, Rgas, \[Eta]t, \[Eta]mt, \[Eta]n,report]*)
(**)
(*	"h": quota [m] ;*)
(*	"Ma": Mach di volo [adim.];*)
(*	"\[Gamma]air , Rair" : propriet\[AGrave] del gas (es. Per l'aria secca \[Gamma] = 1.4, R = 287.05 J/kg/K);*)
(*	"\[Eta]d" : rendimento adiabatico del Diffusore [adim.];*)
(*	"\[Beta]c": rapporto di compressione del Compressore [adim.];*)
(*            "\[Eta]c": rendimento adiabatico del Compressore [adim.];*)
(*            "\[Eta]mc": rendimento meccanico del Compressore [adim.];*)
(*            "\[Eta]b" : rendimento di combustione (es. \[Eta]b=0.99);*)
(*	"\[Eta]pb" : rendimento pneumatico del Combustore (es. \[Eta]pb=0.95);*)
(*            "Qf": potere calorifico del comcustibile [J/kg];*)
(*            "T4": temperatura max ammissibile in turbina [K];*)
(*            "\[Gamma]gas , Rgas" : propriet\[AGrave] del gas (es. Per gas combusti  \[Gamma]gas = 1.34, Rgas=286.77 J/kg/K);*)
(*             "\[Eta]t": rendimento adiabatico della Turbina [adim.];*)
(*             "\[Eta]mt": rendimento meccanico della Turbina [adim.];*)
(*             "\[Eta]n": rendimento adiabatico dell'Ugello [adim.];*)
(*	"report": variabile booleana di stampa;*)
(*	"nozzleType" : [optional string] "convergent" o "adapted" *)
(*	*)
(*  OUTPUT : {Points, energy, performance}*)
(*   *)
(*   	"Points": vettore dei punti rappresentanti il ciclo *)
(*   	"energy": vettore contenente i lavori compiuti da ciascun componente (presa dinamica, compressore, turbina, ugello)*)
(*   	"performance": vettore contenente i valori delle principali prestazioni del motore 	*)
(*	*)


CalcTurbojetCycle[Ma_,h_,\[Gamma]air_,Rair_,\[Eta]d_,\[Eta]mc_,\[Eta]c_,\[Beta]c_,Qf_,\[Gamma]gas_,Rgas_,\[Eta]b_,\[Eta]pb_,T4_,\[Eta]t_,\[Eta]mt_,\[Eta]n_,report_,nozzleType_:"convergent"]:=Module[
	{pa,Ta,\[Rho]a,sa,
	p0a,T0a,\[Rho]0a,s0a,
	p02,T02,\[Rho]02,s02,
	p03,T03,\[Rho]03,s03,
	p04,T04,\[Rho]04,s04,
	p05,T05,\[Rho]05,s05,
	p09,T09,\[Rho]09,s09,
	p9,T9,\[Rho]9,s9,
	PNTa,PNT0a,PNT02,PNT03,PNT04,PNT05,PNT09,PNT9,
	convergentNozzle,
	Va,
	A9,
	massflow,f,heat,
	ue,Me,
	ratioD,ratioT,ratioN,
	workD,workF,workC,workT,workN,
	impulseSpecificCoreThrust,impulseSpecificFanThrust,impulseSpecificThrust,pressureSpecificCoreThrust,pressureSpecificFanThrust,pressureSpecificThrust,specificThrust,
	Ia,TSFC,
	ueff,
	Pav,Pp,Pd,Pj,
	\[Eta]th,\[Eta]p,\[Eta]o,
	Points,energy,performance,
	UnitMisPer,ValuePerfor
	},


	If[report,
		Print[Style["Engine Input Data",14,Bold,Red]];
		Print[Style["Engine Type: TurboJet"]];
		Print[TableForm[
			{\[Beta]c,T4},
			TableHeadings->{{"\[Beta]c","T4"},None}
		]];
		Print[""];
	
		Print[Style["Adiabatic efficiencies"]];
		Print[TableForm[
				{\[Eta]d,\[Eta]c,\[Eta]b,\[Eta]t,\[Eta]n},
				TableHeadings->{{"\[Eta]d","\[Eta]c","\[Eta]b","\[Eta]t","\[Eta]n"},None}
			]
		];
		Print[""];
		
		Print[Style["Mechanical & pneumatic efficiencies"]];
		Print[TableForm[
				{\[Eta]mc,\[Eta]mt,\[Eta]pb},
			TableHeadings->{{"\[Eta]mc","\[Eta]mt","\[Eta]pb"},None}
		]];
		Print[""];
		Print[Style["Analysis Output",14,Bold,Blue]];

	];



(* FREE STREAM CONDITIONS *)
	If[report,Print[Style["Free Stream Condition",12,Bold,Blue]]];
	{PNTa,PNT0a,Va}=FreeStream[h,Ma,\[Gamma]air,Rair,report];
	{pa,Ta,\[Rho]a,sa}=PNTa;
	{p0a,T0a,\[Rho]0a,s0a}=PNT0a;

(* DIFFUSOR OUTLET CONDITIONS *)
	If[report,Print[Style["Ram diffusion",12,Bold,Blue]]];
	{PNT02,ratioD,workD}=DiffuserCompression[PNT0a,Ma,pa,\[Eta]d,\[Gamma]air,Rair,report];
	{p02,T02,\[Rho]02,s02}=PNT02;

(* COMPRESSOR OUTLET CONDITIONS *)
	If[report,Print[Style["Compressor compression",12,Bold,Blue]]];
	massflow=1.0;
	{PNT03,workC}=CompressorCompression[PNT02,massflow,\[Beta]c,\[Eta]c,\[Gamma]air,Rair,report];
	{p03,T03,\[Rho]03,s03}=PNT03;

(* BURNER *)
	If[report,Print[Style["Burner",12,Bold,Blue]]];
	{PNT04,f,heat}=BurnerCombustion[PNT03,\[Eta]b,\[Eta]pb,Qf,T4,\[Gamma]air,Rair,\[Gamma]gas,Rgas,report];
	{p04,T04,\[Rho]04,s04}=PNT04;

(* TURBINE EXPANSION *)
	If[report,Print[Style["Turbine expansion",12,Bold,Blue]]];
	{PNT05,ratioT,workT}=TurbineExpansion[PNT04,workC,\[Eta]t,f,\[Eta]mc,\[Eta]mt,\[Gamma]gas,Rgas,report];
	{p05,T05,\[Rho]05,s05}=PNT05;

(* NOZZLE EXPANSION *)
	If[nozzleType == "convergent",
       convergentNozzle=True,
       If[nozzleType == "adapted",
         convergentNozzle=False,
         Print["Unknown nozzle type ", nozzleType], Abort[]
      ]
    ];
	If[report,Print[Style["Nozzle expansion ",12,Bold,Blue]]];
	massflow=1.0;
	{PNT9,PNT09,ue,Me,ratioN,workN}=NozzleExpansion[PNT05,massflow,pa,\[Eta]n,\[Gamma]gas,Rgas,report,convergentNozzle];
	{p9,T9,\[Rho]9,s9}=PNT9;
	{p09,T09,\[Rho]09,s09}=PNT09;

	Points={PNTa,PNT02,PNT03,PNT04,PNT05,PNT9};
	energy={workD,workC,workT,workN};


(* ENGINE PERFORMANCE *)

(* aree delle sezioni di uscita degli ugelli -primario e secondario- specificate secondo la portata primaria e ottenute secondo la relazione isentropica portataMassa=\[Rho]*A*u *)
	A9=1/( \[Rho]9 ue);

(* Calcolo spinta specifica rispetto alla portata primaria *)
	impulseSpecificThrust=(1+f)ue-Va;
	pressureSpecificThrust=A9 (p9-pa);
	Ia=impulseSpecificThrust+pressureSpecificThrust;
	
	If[nozzleType=="convergent",
	ueff = Ia, (*use effective exhaust speed, which accounts for pressure thrust in power formulas*)
	ueff = ue];
	
(* Calcolo del consumo specifico in Kg/(h N)*)
	TSFC=3600 f/Ia;

(* Calcolo della potenza disponibile *)
	Pav=f (Qf+Va^2/2);

(* Calcolo della potenza propulsiva *)
	Pp=Ia Va;

(* Calcolo della potenza dissipata *)
	Pd=(1+f) (ueff-Va)^2;

(* Calcolo della potenza del getto  *)
	Pj = 0.5 * (1 + f) * ueff^2 - 0.5 * Va^2;
	
(* Calcolo dei rendimenti *)

(* Rendimento termodinamico*)
	\[Eta]th=Pj/Pav;

(* Rendimento propulsivo *)
	\[Eta]p=Pp/Pj;

(* Rendimento overall *)
	\[Eta]o=Pp/Pav;

	performance={Ia,TSFC,\[Eta]o,\[Eta]th,\[Eta]p,ue};

	If[report,
		UnitMisPer={" m/s"," N\[CenterDot]kg/h","","",""};
		ValuePerfor=Table[ToString[performance[[i]]]<>UnitMisPer[[i]],{i,5}];
		Print[Style["Engine Performance Parameters",12,Bold,Blue]];
		Print[TableForm[
			{ValuePerfor},TableHeadings->{None,{"Ia","TSFC","\[Eta]0","\[Eta]th","\[Eta]p"}},TableDirections->Row
		]];
	];

	{Points,energy,performance}
]


(* ::Subsection::GrayLevel[0]:: *)
(*Turbofan Cycle & Performance*)


(* ::Text:: *)
(*Modulo che calcola il ciclo di un motore TurboFan a Flussi Separati*)
(**)
(*INPUT : [h, Ma, \[Gamma]air, Rair, BPR, \[Eta]d, \[Beta]f, \[Eta]f, \[Eta]mf, \[Beta]c, \[Eta]c, \[Eta]mc, \[Eta]b, \[Eta]pb, Qf, T4, \[Gamma]gas, Rgas, \[Eta]t, \[Eta]mt, \[Eta]n,report]*)
(**)
(*	"h": quota [m] ;*)
(*	"Ma": Mach di volo [adim.];*)
(*	"\[Gamma]air , Rair" : propriet\[AGrave] del gas (es. Per l'aria secca \[Gamma] = 1.4, R = 287.05 J/kg/K);*)
(*	"BPR": rapporto di by-pass [adim.];*)
(*	"\[Eta]d" : rendimento adiabatico del Diffusore [adim.];*)
(*	"\[Beta]f": rapporto di compressione del Fan [adim.];*)
(*            "\[Eta]f": rendimento adiabatico del Fan [adim.]; *)
(*            "\[Eta]mf": rendimento meccanico del Fan [adim.];*)
(*            "\[Beta]c": rapporto di compressione del Compressore [adim.];*)
(*            "\[Eta]c": rendimento adiabatico del Compressore [adim.];*)
(*            "\[Eta]mc": rendimento meccanico del Compressore [adim.];*)
(*            "\[Eta]b" : rendimento di combustione (es. \[Eta]b=0.99);*)
(*	"\[Eta]pb" : rendimento pneumatico del Combustore (es. \[Eta]pb=0.95);*)
(*            "Qf": potere calorifico del comcustibile [J/kg];*)
(*            "T4": temperatura max ammissibile in turbina [K];*)
(*            "\[Gamma]gas , Rgas" : propriet\[AGrave] del gas (es. Per gas combusti  \[Gamma]gas = 1.34, Rgas=286.77 J/kg/K);*)
(*             "\[Eta]t": rendimento adiabatico della Turbina [adim.];*)
(*             "\[Eta]mt": rendimento meccanico della Turbina [adim.];*)
(*             "\[Eta]n": rendimento adiabatico dell'Ugello [adim.];*)
(*	"report": variabile booleana di stampa*)
(*	*)
(*  OUTPUT : {corePoints, fanPoints, energy, performance}*)
(*   *)
(*   	"corePoints": vettore dei punti rappresentanti il ciclo relativo alla portata primaria*)
(*   	"fanPoints": vettore dei punti rappresentanti il ciclo relativo alla portata secondaria*)
(*   	"energy": vettore contenente i lavori compiuti da ciascun componente (presa dinamica, fan, ugello secondario, compressore, turbina HP e LP, ugello primario)*)
(*   	"performance": vettore contenente i valori delle principali prestazioni del motore 	*)
(*	*)


CalcTurboFanCycle[Ma_,h_,\[Gamma]air_,Rair_,\[Eta]d_,\[Eta]f_,\[Eta]mf_,BPR_,\[Beta]f_,\[Eta]mc_,\[Eta]c_,\[Beta]c_,Qf_,\[Gamma]gas_,Rgas_,\[Eta]b_,\[Eta]pb_,T4_,\[Eta]t_,\[Eta]mt_,\[Eta]n_,report_,nozzleType_:"convergent"]:=Module[
	{pa,Ta,\[Rho]a,sa,
	p0a,T0a,\[Rho]0a,s0a,
	p02,T02,\[Rho]02,s02,
	p021,T021,\[Rho]021,s021,
	p19,T19,\[Rho]19,s19,
	p019,T019,\[Rho]019,s019,
	p03,T03,\[Rho]03,s03,
	p04,T04,\[Rho]04,s04,
	p041,T041,\[Rho]041,s041,
	p05,T05,\[Rho]05,s05,
	p9,T9,\[Rho]9,s9,
	p09,T09,\[Rho]09,s09,
	PNTa,PNT0a,PNT02,PNT021,PNT19,PNT019,PNT03,PNT04,PNT041,PNT05,PNT9,PNT09,
	Va,
	massflow,f,heat,
	ue,Me,
	ue19,Me19,
	ratioD,ratioN19,ratioHPT,ratioLPT,ratioN,
	workD,workF,workN19,workC,workHPT,workLPT,workN,
	A9,A19,
	ue19eff,ueeff,
	convergentNozzle,
	impulseSpecificCoreThrust,impulseSpecificFanThrust,impulseSpecificThrust,pressureSpecificCoreThrust,pressureSpecificFanThrust,pressureSpecificThrust,specificThrust,
	Ia,TSFC,
	Pav,Pp,Pd,Pj,
	\[Eta]th,\[Eta]p,\[Eta]o,
	corePoints,fanPoints,energy,performance,
	UnitMisPer,ValuePerfor
	},


	If[report,
		Print[Style["Engine Input Data",14,Bold,Red]];
		Print[Style["Engine Type: TurboFan separate flows"]];
		Print[TableForm[
			{\[Beta]c,\[Beta]f,BPR,T4},
			TableHeadings->{{"\[Beta]c","\[Beta]f","BPR","T4"},None}
		]];
		Print[""];
	
		Print[Style["Adiabatic efficiencies"]];
		Print[TableForm[
				{\[Eta]d,\[Eta]f,\[Eta]c,\[Eta]b,\[Eta]t,\[Eta]n},
				TableHeadings->{{"\[Eta]d","\[Eta]f","\[Eta]c","\[Eta]b","\[Eta]t","\[Eta]n"},None}
			]
		];
		Print[""];
		
		Print[Style["Mechanical & pneumatic efficiencies"]];
		Print[TableForm[
				{\[Eta]mf,\[Eta]mc,\[Eta]mt,\[Eta]pb},
			TableHeadings->{{"\[Eta]mf","\[Eta]mc","\[Eta]mt","\[Eta]pb"},None}
		]];
		Print[""];
		Print[Style["Analysis Output",14,Bold,Blue]];

	];
	
	If[nozzleType == "convergent",
       convergentNozzle=True,
       If[nozzleType == "adapted",
         convergentNozzle=False,
         Print["Unknown nozzle type ", nozzleType], Abort[]
      ]
    ];

(* FREE STREAM CONDITIONS *)
	If[report,Print[Style["Free Stream Condition",12,Bold,Blue]]];
	{PNTa,PNT0a,Va}=FreeStream[h,Ma,\[Gamma]air,Rair,report];
	{pa,Ta,\[Rho]a,sa}=PNTa;
	{p0a,T0a,\[Rho]0a,s0a}=PNT0a;

(* DIFFUSOR OUTLET CONDITIONS *)
	If[report,Print[Style["Ram diffusion",12,Bold,Blue]]];
	{PNT02,ratioD,workD}=DiffuserCompression[PNT0a,Ma,pa,\[Eta]d,\[Gamma]air,Rair,report];
	{p02,T02,\[Rho]02,s02}=PNT02;

(* FAN OUTLET CONDITIONS *)
	If[report,Print[Style["Fan compression",12,Bold,Blue]]];
	massflow=1.0+BPR;
	{PNT021,workF}=CompressorCompression[PNT02,massflow,\[Beta]f,\[Eta]f,\[Gamma]air,Rair,report];
	{p021,T021,\[Rho]021,s021}=PNT021;

(* SECONDARY NOZZLE EXPANSION *)
	If[report,Print[Style["Secondary nozzle expansion",12,Bold,Blue]]];
	massflow=BPR;
	{PNT19,PNT019,ue19,Me19,ratioN19,workN19}=NozzleExpansion[PNT021,massflow,pa,\[Eta]n,\[Gamma]gas,Rgas,report,convergentNozzle];
	{p19,T19,\[Rho]19,s19}=PNT19;
	{p019,T019,\[Rho]019,s019}=PNT019;

(* COMPRESSOR OUTLET CONDITIONS *)
	If[report,Print[Style["Compressor compression",12,Bold,Blue]]];
	massflow=1.0;
	{PNT03,workC}=CompressorCompression[PNT021,massflow,\[Beta]c,\[Eta]c,\[Gamma]air,Rair,report];
	{p03,T03,\[Rho]03,s03}=PNT03;

(* BURNER *)
	If[report,Print[Style["Burner",12,Bold,Blue]]];
	{PNT04,f,heat}=BurnerCombustion[PNT03,\[Eta]b,\[Eta]pb,Qf,T4,\[Gamma]air,Rair,\[Gamma]gas,Rgas,report];
	{p04,T04,\[Rho]04,s04}=PNT04;

(* HIGH PRESSURE TURBINE EXPANSION *)
	If[report,Print[Style["High pression turbine expansion",12,Bold,Blue]]];
	{PNT041,ratioHPT,workHPT}=TurbineExpansion[PNT04,workC,\[Eta]t,f,\[Eta]mc,\[Eta]mt,\[Gamma]gas,Rgas,report];
	{p041,T041,\[Rho]041,s041}=PNT041;

(* LOW PRESSION TURBINE EXPANSION *)
	If[report,Print[Style["Low pression turbine expansion ",12,Bold,Blue]]];
	{PNT05,ratioLPT,workLPT}=TurbineExpansion[PNT041,workF,\[Eta]t,f,\[Eta]mf,\[Eta]mt,\[Gamma]gas,Rgas,report];
	{p05,T05,\[Rho]05,s05}=PNT05;

(* PRIMARY NOZZLE EXPANSION *)
	If[report,Print[Style["Primary nozzle expansion ",12,Bold,Blue]]];
	massflow=1.0;
	{PNT9,PNT09,ue,Me,ratioN,workN}=NozzleExpansion[PNT05,massflow,pa,\[Eta]n,\[Gamma]gas,Rgas,report,convergentNozzle];
	{p9,T9,\[Rho]9,s9}=PNT9;
	{p09,T09,\[Rho]09,s09}=PNT09;

	corePoints={PNTa,PNT02,PNT021,PNT03,PNT04,PNT041,PNT05,PNT9};
	fanPoints= {PNTa,PNT02,PNT021,PNT19};
	energy={workD,workF,workC,workHPT,workLPT,workN,workN19};


(* ENGINE PERFORMANCE *)

(* aree delle sezioni di uscita degli ugelli -primario e secondario- specificate secondo la portata primaria e ottenute secondo la relazione isentropica portataMassa=\[Rho]*A*u *)
	A9=1/( \[Rho]9 ue);
	A19=BPR/( \[Rho]19 ue19);

(* Calcolo spinta specifica rispetto alla portata primaria *)
	impulseSpecificCoreThrust=(1+f)ue-Va;
	impulseSpecificFanThrust=BPR (ue19-Va);
	impulseSpecificThrust=impulseSpecificCoreThrust + impulseSpecificFanThrust;
	pressureSpecificCoreThrust=A9 (p9-pa);
	pressureSpecificFanThrust=A19 (p19-pa);
	pressureSpecificThrust=pressureSpecificCoreThrust+pressureSpecificFanThrust;
	specificThrust=impulseSpecificThrust+pressureSpecificThrust;

(* Calcolo della spinta specifica riferita alla portata complessiva*)
	Ia=specificThrust/(1+BPR);

(* Calcolo del consumo specifico in Kg/(h N)*)
	TSFC=3600 f/specificThrust;
(* Calcolo velocit\[AGrave] effettive, valide anche nel caso di ugello non adattato*)
	
	If[nozzleType=="convergent",
	ue19eff =(impulseSpecificFanThrust+Va)/BPR + pressureSpecificFanThrust;
	ueeff = (impulseSpecificCoreThrust+Va)/(1+f) + pressureSpecificCoreThrust, (*use effective exhaust speed, which accounts for pressure thrust in power formulas*)
	ueeff = ue; 
	ue19eff=ue19];
	
	
(* Calcolo della potenza disponibile riferita alla portata primaria *)
	Pav=f (Qf+Va^2/2);

(* Calcolo della potenza propulsiva riferita alla portata primaria *)
	Pp=specificThrust Va;

(* Calcolo della potenza dissipata riferita alla portata primaria*)
	Pd=((1+f) (ueeff-Va)^2+BPR (ue19eff-Va)^2)/2;

(* Calcolo della potenza del getto riferita alla portata primaria *)
	Pj= 0.5 * ( BPR * ue19eff^2 + (1+f)*ueeff^2 - (1+BPR)*Va^2);

(* Rendimento termodinamico*)
	\[Eta]th=Pj/Pav;

(* Rendimento propulsivo *)
	\[Eta]p=Pp/Pj;

(* Rendimento overall *)
	\[Eta]o=Pp/Pav;

	performance={Ia,TSFC,\[Eta]o,\[Eta]th,\[Eta]p,specificThrust};

	If[report,
		UnitMisPer={" m/s"," N\[CenterDot]kg/h","","",""};
		ValuePerfor=Table[ToString[performance[[i]]]<>UnitMisPer[[i]],{i,5}];
		Print[Style["Engine Performance Parameters",12,Bold,Blue]];
		Print[TableForm[
			{ValuePerfor},TableHeadings->{None,{"Ia","TSFC","\[Eta]0","\[Eta]th","\[Eta]p"}},TableDirections->Row
		]];
	];

	{corePoints,fanPoints,energy,performance}
]


(* ::Section::Closed:: *)
(*Components sizing*)


(* ::Subsection::Closed:: *)
(*Intake*)


(* ::Subsubsection::Closed:: *)
(*Tools*)


SubsonicInletSizing[M3_,A3_,Rtip_,M1_,\[Gamma]air_]:=
Module[{A2,M2,Mach3,rule,A1,\[Gamma],Mach2,dum,Area2,Area3},

(*M3, A3 ed Rtip li ho trovati nello studio del fan mentre M1 lo impongo=0.7*)

A2=Pi*Rtip^2;
rule={\[Gamma]->\[Gamma]air,Area2->A2,Area3->A3,Mach3->M3};
dum=NSolve[Area2/Area3==Mach3/Mach2 ((1+(\[Gamma]-1)/2 Mach2^2)/(1+(\[Gamma]-1)/2 Mach3^2))^((\[Gamma]+1)/(2(\[Gamma]-1)))/.rule,Mach2];
M2=Mach2/.dum[[6]];
A1=A2*M2/M1((1+(\[Gamma]air-1)/2 M1^2)/(1+(\[Gamma]air-1)/2 M2^2))^((\[Gamma]air+1)/(2(\[Gamma]air-1)));


Return[{A1,A2,A3,M1,M2,M3}];
];


(* ::Subsection::Closed:: *)
(*Compressor *)


(* ::Subsubsection::Closed:: *)
(*Tools*)


(* ::Text:: *)
(*Triangles tools*)


plotTriangle[Ca_,U_,Cu1_,Cu2_,Wu1_,Wu2_]:=Module[{DH,def,plot},
DH=Sqrt[Wu2^2+Ca^2]/Sqrt[Wu1^2+Ca^2];
def=(ArcTan[Wu1/Ca]-ArcTan[Wu2/Ca])*360/(2 Pi);
plot=Show[
Graphics[
{Black,AnnotatedArrow[{0,0},{U,0},"U"],
Inset[
Panel
[Grid[{{Text[Style["DH = "<>ToString[DH],16]]},{Text[Style["Deflection (\[Beta]1-\[Beta]2) = "<>ToString[def]<>"\[Degree]",16]]}}]
],
{U/2,1.2*Ca}]}
],
Graphics[{Red,AnnotatedArrow[{U-Cu1,Ca},{U,0},"C1"]}],
Graphics[{Blue,AnnotatedArrow[{U-Cu1,Ca},{0,0},"W1"]}],
Graphics[{Blue,AnnotatedArrow[{Wu2,Ca},{0,0},"W2"]}],
Graphics[{Red,AnnotatedArrow[{Wu2,Ca},{U,0},"C2"]}],PlotRange->{{-0.5,2.5},{-0.1,2.}},ImageSize->500];
Return[plot]
]


triangleDesignFromEqns[psi_,phi_,lambda_]:=Module[{plotTriangles,\[Psi],\[Phi],\[CapitalLambda]R,\[Alpha]1,\[Alpha]2,\[Beta]1,\[Beta]2,a1,a2,b1,b2,U,Ca,Cu1,Cu2,Wu1,Wu2,plot,eqns,desPars,angles},
eqns={\[Psi]==\[Phi](Tan[\[Beta]1]-Tan[\[Beta]2]),
\[CapitalLambda]R==0.5 \[Phi](Tan[\[Beta]1]+Tan[\[Beta]2]),
Tan[\[Alpha]1]+Tan[\[Beta]1]==1/\[Phi],
Tan[\[Alpha]2]+Tan[\[Beta]2]==1/\[Phi]};
desPars={\[Psi]->psi,\[Phi]->phi,\[CapitalLambda]R->lambda};
angles=First[Quiet[Solve[eqns/.desPars,{\[Beta]1,\[Beta]2,\[Alpha]1,\[Alpha]2}]]];
U=1;
Ca=U*\[Phi]/.desPars;
Cu1=Ca*Tan[\[Alpha]1]/.angles/.desPars;
Cu2=Ca*Tan[\[Alpha]2]/.angles/.desPars;
Wu1=Ca*Tan[\[Beta]1]/.angles/.desPars;
Wu2=Ca*Tan[\[Beta]2]/.angles/.desPars;
(*Convert to degrees*)
a1=Rad2Deg[\[Alpha]1];
a2=Rad2Deg[\[Alpha]2];
b1=Rad2Deg[\[Beta]1];
b2=Rad2Deg[\[Beta]2];
(*plot*)
plot=plotTriangle[Ca,U,Cu1,Cu2,Wu1,Wu2];
Return[{{a1,a2,b1,b2},{Ca,U,Cu1,Cu2,Wu1,Wu2},plot}]
];


triangleDesign[\[Tau]s_,DF_,M1_,\[Sigma]_,\[Gamma]_]:=Module[{eqns,sol,angles,\[Alpha]1,\[Alpha]2,U,Ca,Cu1,Wu1,\[Beta]1,Cu2,Wu2,\[Beta]2,a1,a2,b1,b2,plot},
(*Define \[Tau]=\[Tau](M1,\[Alpha]1,\[Alpha]2) and DF=DF(\[Alpha]1,\[Alpha]2,\[Sigma]) assuming repeating stage (\[CapitalLambda]=0.5) and constant axial speed*)

eqns={\[Tau]s==((\[Gamma]-1)M1^2)/(1+(\[Gamma]-1)M1^2/2) (Cos[\[Alpha]1]^2/Cos[\[Alpha]2]^2-1)+1,DF==(1-Cos[\[Alpha]2]/Cos[\[Alpha]1])+((Tan[\[Alpha]2]-Tan[\[Alpha]1])/(2\[Sigma]))Cos[\[Alpha]2]};
(*Solve sys of eqns to get (\[Alpha]1,\[Alpha]2)=f(DF,M1,\[Tau])*)
sol={\[Alpha]1,\[Alpha]2}/.Quiet[NSolve[eqns,{\[Alpha]1,\[Alpha]2}]];
(*Pick real and meaningful solution*)
angles=Select[sol,Element[#,Reals]&&#[[1]]<Pi/2&&#[[2]]<Pi/2 &];
(*Protect against no-solutions*)
If[angles=={},angles={{0,0}}];
{\[Alpha]1,\[Alpha]2}=angles[[1]];
(*Given \[Alpha]1 and \[Alpha]2, compute all other quantities as post-process. All velocities are normalized by C1*)
(*Rotor IN*)
U=Cos[\[Alpha]1](Tan[\[Alpha]1]+Tan[\[Alpha]2]);
Ca=Cos[\[Alpha]1];
Cu1=Sin[\[Alpha]1];
Wu1=U -Sin[\[Alpha]1];
\[Beta]1=ArcTan[Wu1/Ca];
(*Rotor OUT \[Equal] Stator IN*)
Cu2=Cos[\[Alpha]1]Tan[\[Alpha]2];
Wu2=U-Cu2;
\[Beta]2=ArcTan[Wu2/Ca];
(*Convert to degrees*)
a1=Rad2Deg[\[Alpha]1];
a2=Rad2Deg[\[Alpha]2];
b1=Rad2Deg[\[Beta]1];
b2=Rad2Deg[\[Beta]2];
(*plot*)
plot=plotTriangle[Ca,U,Cu1,Cu2,Wu1,Wu2];
Return[{{a1,a2,b1,b2},{Ca,U,Cu1,Cu2,Wu1,Wu2},plot}]
];


(* ::Text:: *)
(*Geometry tools*)


annulusDesign[massflow_,T_,p_,Ca_,\[Gamma]_,R_,rmean_]:=Module[{Area,a,Udim,rtip,rhub},

Area=massflow * R * T/(p* Ca);
rtip=(Area+4 Pi rmean^2)/(4 Pi rmean);
rhub=(-Area+4 Pi rmean^2)/(4 Pi rmean);
Return[{Area,rtip,rhub}]
];


wheelSpeedDesign[massflow_,T_,p_,M_,U_,\[Alpha]1_,\[Alpha]2_,\[Gamma]_,R_,\[Sigma]c_,\[Rho]blade_,taper_]:=Module[{Area,a,C1,MRel,VRel,T0Rel,\[Omega],Udim,rmean,rtip,rhub},
a=Sqrt[\[Gamma] R T];
C1=M*a;
Area=massflow * R * T/(p* C1 Cos[\[Alpha]1*Pi/180]);
\[Omega]=Sqrt[4 Pi \[Sigma]c/(\[Rho]blade*Area*(1+taper))];
Udim=U*C1;
rmean=Udim/\[Omega];
Return[{\[Omega],Udim,rmean}]
];


(* ::Text:: *)
(*Drawing tools*)


AnnotatedArrow[p_,q_,label_]:={Arrowheads[{{.01,.5,Graphics[Inset[Style[label,Medium],{Center,Top}]]},{.03}}],Arrow[{p,q}]};


nicePlotFromDB[DB_,ix_,iy1_,iy2_,labelx_,labely_]:=Module[{pl},
pl=ListPlot[{Table[{DB[[i,ix]],DB[[i,iy1]]},{i,1,Dimensions[DB,1][[1]]}],Table[{DB[[i,ix]],DB[[i,iy2]]},{i,1,Dimensions[DB,1][[1]]}]},Joined->True, Frame->True,Axes->False,FrameLabel->{labelx,labely},PlotStyle->{Black, Red},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],ImageSize->500];
Return[pl]
];


drawCompressor[DB_,iTip_,iHub_,nStages_]:=Module[{x0,compressor,rTip,rHub,hRot,chordRot,gap,hStat,chordStat,GapBox0,gap0,RotBox,rot,HubBox,hub,diskBox,disk,GapBox1,gap1,StatBox,stat,pl},
compressor={};
x0=0;
Do[

rTip=Table[DB[[j,iTip]],{j,(iStage-1)*3+1,(iStage-1)*3+3}];
rHub=Table[DB[[j,iHub]],{j,(iStage-1)*3+1,(iStage-1)*3+3}];


hRot=rTip[[1]]-rHub[[1]];
chordRot=0.4*hRot;
gap=0.1 *chordRot;
hStat=rTip[[2]]-rHub[[2]];
chordStat=0.4*hStat;


GapBox0=Polygon[{{x0-4*gap,rHub[[1]]},{x0,rHub[[1]]},{x0+0.2*chordRot,rTip[[1]]},{x0-4*gap,rTip[[1]]}}];
gap0=Graphics[{{EdgeForm[Thick],White,GapBox0}}];


RotBox=Polygon[{{x0,rHub[[1]]},{x0+chordRot,rHub[[2]]},{x0+0.8*chordRot,rTip[[2]]},{x0+0.2*chordRot,rTip[[1]]}}];
rot=Graphics[{{EdgeForm[Thick],Blue,RotBox},Text[Style["R"<>ToString[iStage],Medium,White],{x0+0.5*chordRot,rHub[[1]]+0.5*hRot}]}];

HubBox=Polygon[{{x0+0.47*chordRot,0},{x0+0.53*chordRot,0},{x0+0.53*chordRot,rHub[[2]]-0.4*chordRot},{x0+0.47*chordRot,rHub[[2]]-0.4*chordRot}}];
hub=Graphics[{{EdgeForm[Thick],Gray,HubBox}}];

diskBox=Polygon[{{x0,rHub[[1]]},{x0+chordRot,rHub[[2]]},{x0+chordRot,rHub[[2]]-0.4*chordRot},{x0,rHub[[2]]-0.4*chordRot}}];
disk=Graphics[{{EdgeForm[Thick],Gray,diskBox}}];

GapBox1=Polygon[{{x0+chordRot,rHub[[2]]},{x0+chordRot+gap+0.2*chordStat,rHub[[2]]},{x0+chordRot+gap,rTip[[2]]},{x0+0.8*chordRot,rTip[[2]]}}];
gap1=Graphics[{{EdgeForm[Thick],White,GapBox1}}];

StatBox=Polygon[{{x0+chordRot+gap+0.2*chordStat,rHub[[2]]},{x0+chordRot+gap+0.8*chordStat,rHub[[3]]},{x0+chordRot+gap+chordStat,rTip[[3]]},{x0+chordRot+gap,rTip[[2]]}}];
stat=Graphics[{{EdgeForm[Thick],Red,StatBox},Text[Style["S"<>ToString[iStage],Medium,White],{x0+chordRot+gap+0.5*chordStat,rHub[[2]]+0.5*hStat}]}];

x0=x0+chordRot+gap+chordStat+gap;
AppendTo[compressor,{gap0,rot,hub,disk,gap1,stat}],

{iStage,1,nStages}];
pl=Show[Reverse[compressor],PlotRange->{All,All},Axes->False,Frame->True,ImageSize->100*nStages];

Return[pl]
]


(* ::Text:: *)
(*DB tools*)


extractPropertyAlongCompressor[DB_,iProp_,nStages_]:=Module[{prop,iStage,j},

prop=Table[DB[[j,iProp]],{j,1,nStages*3-2,3}];
Return[prop]
]


collectCompressorGeometries[allStates_,triangle_,nStages_,massflow_,Rair_]:=Module[{CompAreas,rMean,CompMeanDiameter,rTip,rRoot,\[Alpha]1m,\[Beta]2m,phim,Ncomp,mredm},

    CompAreas=extractPropertyAlongCompressor[allStates,9,nStages];
	rMean=extractPropertyAlongCompressor[allStates,6,nStages];
	CompMeanDiameter=2*rMean;
	rTip=extractPropertyAlongCompressor[allStates,7,nStages];
	rRoot=extractPropertyAlongCompressor[allStates,8,nStages];
	\[Alpha]1m=Table[Deg2Rad[triangle[[1]]],{i,1,nStages}];
	\[Beta]2m=Table[Deg2Rad[triangle[[4]]],{i,1,nStages}];
	phim=Table[triangle[[5]]/triangle[[6]],{i,1,nStages}];
	Ncomp=triangle[[6]]/(2 Pi rMean[[1]]);
	mredm=massflow*Sqrt[Rair allStates[[1,2]]]/(allStates[[1,4]] CompAreas[[1]]);
	
	Return[{CompAreas,rMean,CompMeanDiameter,rTip,rRoot,\[Alpha]1m,\[Beta]2m,phim,Ncomp,mredm}];

]


(* ::Subsubsection::Closed:: *)
(*Size it!*)


estimateNofStages[\[CapitalDelta]T0stage_,\[Beta]c_,T0in_,\[Eta]stage_,\[Gamma]_]:=Module[{n,Tin,T0out,\[CapitalDelta]T0,nStages,est\[CapitalDelta]T0stage},
n=(\[Gamma] \[Eta]stage)/(1-\[Gamma]+\[Gamma] \[Eta]stage);
Tin=TF[T0in,\[Gamma],Min];
T0out=T0in*(\[Beta]c)^((n-1)/n);
\[CapitalDelta]T0=T0out-T0in;
(*The repeating-row design is a constant \[CapitalDelta]T-stage design*)
nStages=Ceiling[\[CapitalDelta]T0/\[CapitalDelta]T0stage];
est\[CapitalDelta]T0stage=\[CapitalDelta]T0/nStages;
Print["With given conditions, #Stages = ",nStages, " with \!\(\*SubscriptBox[\(\[CapitalDelta]T0\), \(stage\)]\) = ", est\[CapitalDelta]T0stage];
Return[{nStages,est\[CapitalDelta]T0stage}];
];


estimateNofStagesAdvanced[\[CapitalDelta]T0stage_,\[Beta]c_,T0in_,\[Eta]stage_,\[Gamma]_]:=Module[{n,Tin,T0out,\[CapitalDelta]T0,nStages,est\[CapitalDelta]T0stage,T0,OPR,\[Beta]stage,act\[CapitalDelta]T0,\[Eta]comp,eps=10^-6,maxIter=100},

n=(\[Gamma] \[Eta]stage)/(1-\[Gamma]+\[Gamma] \[Eta]stage); (*polytropic index*)
T0out=T0in*(\[Beta]c)^((n-1)/n); (*compressor outlet T0, considering an infinite number of stages, having efficiency \[Eta]stage each*)
\[CapitalDelta]T0=T0out-T0in;

Do[

(*The repeating-row design is a constant \[CapitalDelta]T-stage design*)
nStages=Ceiling[\[CapitalDelta]T0/\[CapitalDelta]T0stage];
est\[CapitalDelta]T0stage=\[CapitalDelta]T0/nStages;


OPR=1;
T0=T0in;
Do[

\[Beta]stage=(1+\[Eta]stage*est\[CapitalDelta]T0stage/T0)^(\[Gamma]/(\[Gamma]-1));
T0=T0+est\[CapitalDelta]T0stage;
OPR=OPR*\[Beta]stage;
,{i,1,nStages}];

act\[CapitalDelta]T0=T0in*(OPR^((\[Gamma]-1)/\[Gamma])-1);
\[Eta]comp=act\[CapitalDelta]T0/\[CapitalDelta]T0;

\[CapitalDelta]T0=T0in*(\[Beta]c^((\[Gamma]-1)/\[Gamma])-1)/\[Eta]comp;

If[Abs[OPR-\[Beta]c]<eps,Break[]],
{j,1,maxIter}];

est\[CapitalDelta]T0stage=\[CapitalDelta]T0/nStages;
Print["With given conditions, #Stages = ",nStages, " with \!\(\*SubscriptBox[\(\[CapitalDelta]T0\), \(stage\)]\) = ", est\[CapitalDelta]T0stage];
Print["Compressor efficiency = ",\[Eta]comp, " with \!\(\*SubscriptBox[\(\[CapitalDelta]T0\), \(comp\)]\) = ", \[CapitalDelta]T0];
Return[{nStages,est\[CapitalDelta]T0stage,\[Eta]comp}];
];


designMultiStageCompressor[T0in_,p0in_,\[Beta]c_,\[Eta]stage_,\[Gamma]_,R_,cp_,Min_,DF_,\[Sigma]_,desired\[CapitalDelta]T0stage_,massflow_,\[Sigma]material_,\[Rho]blade_,taper_]:=Module[{pin,Tin,\[CapitalDelta]T0stage,nStages,\[Eta]comp,T0out,\[CapitalDelta]T0,p01,T01,T1,p1,a1,s1,T2,T02,p2,p02,a2,M2,M2rel,s2,T03,p3,s3,\[Tau]stage,\[Beta]stage,p03,\[Alpha]1,\[Alpha]2,\[Beta]1,\[Beta]2,Ca,U,Cu1,Cu2,Wu1,Wu2,C1,M1,C3,T3,a3,M3,M3rel,\[Omega],Udim,M1rel,rmean,rtip1,rhub1,rtip2,rhub2,rtip3,rhub3,CrossSection1,CrossSection2,CrossSection3,saveTriangle,savePNT1,savePNT2,savePNT3,allStages,plot},

(*Inlet static*)
Tin=TF[T0in,\[Gamma],Min];
pin=pF[p0in,\[Gamma],Min];

(*Nr. of stages, \[CapitalDelta]T0stage, and multi-stage compressor efficiency*)
{nStages,\[CapitalDelta]T0stage,\[Eta]comp}=estimateNofStagesAdvanced[desired\[CapitalDelta]T0stage,\[Beta]c,T0in,\[Eta]stage,\[Gamma]];

(*Outlet total*)
T0out=T0in* (1+(\[Beta]c^((\[Gamma]-1)/\[Gamma])-1)/\[Eta]comp);
Print["T0in = ",T0in, " :: T0out = ",T0out];
Print["p0in = ",p0in, " :: p0out = ",p0in*\[Beta]c];
\[CapitalDelta]T0=T0out-T0in;


(*Loop over stages*)
p01=p0in;
T01=T0in;
T1=Tin;
p1=pin;
M1=Min;

allStages={};
Do[
T03=T02=T01+\[CapitalDelta]T0stage;
\[Tau]stage=T03/T01;
(*\[Beta]stage=(\[Tau]stage)^(n/(n-1));*)
\[Beta]stage=(1+\[Eta]stage  \[CapitalDelta]T0stage/T01)^(\[Gamma]/(\[Gamma]-1));
a1=Sqrt[\[Gamma]*R*T1];
C1=M1 * a1;

(*Note: if calculations are correct, next call will return the same results for every stage!*)
{{\[Alpha]1,\[Alpha]2,\[Beta]1,\[Beta]2},{Ca,U,Cu1,Cu2,Wu1,Wu2},plot}=triangleDesign[\[Tau]stage,DF,M1,\[Sigma],\[Gamma]];
saveTriangle={\[Alpha]1,\[Alpha]2,\[Beta]1,\[Beta]2,C1*Ca,C1*U,C1*Cu1,C1*Cu2,C1*Wu1,C1*Wu2,\[Omega]};


If[i==1,
{\[Omega],Udim,rmean}=wheelSpeedDesign[massflow,T1,p1,M1,U,\[Alpha]1,\[Alpha]2,\[Gamma],R,\[Sigma]material,\[Rho]blade,taper];
Print["Wheelspeed = ", \[Omega], " rad/s"];
Print["rmean = ", rmean]
];
{CrossSection1,rtip1,rhub1}=annulusDesign[massflow,T1,p1,Ca*C1,\[Gamma],R,rmean];
M1rel=M1*Cos[Deg2Rad[\[Alpha]1]]/Cos[Deg2Rad[\[Alpha]2]];
s1=s[p1,T1,\[Gamma], R];
savePNT1={T1,T01,p1,p01,s1,rmean,rtip1,rhub1,CrossSection1,M1,M1rel};
AppendTo[allStages,savePNT1];

(*(2) Rotor outlet / stator inlet *)
p2=p1 (1+\[Eta]stage  0.5*\[CapitalDelta]T0stage/T1)^(\[Gamma]/(\[Gamma]-1));(*degree of reaction is 0.5 and \[CapitalDelta]T0\[Equal]\[CapitalDelta]T*)
T2= T1 + 0.5*\[CapitalDelta]T0stage;(*degree of reaction is 0.5 and \[CapitalDelta]T0\[Equal]\[CapitalDelta]T*)
a2=Sqrt[\[Gamma]*R*T2];
M2rel=M1*a1/a2;(*because W2\[Equal]C1*)
M2=M1rel*a1/a2; (*because C2==W1*)
p02=p0F[p2,M2,\[Gamma]]; 
s2=s[p2,T2,\[Gamma], R];
{CrossSection2,rtip2,rhub2}=annulusDesign[massflow,T2,p2,Ca*C1,\[Gamma],R,rmean];
savePNT2={T2,T02,p2,p02,s2,rmean,rtip2,rhub2,CrossSection2,M2,M2rel};
AppendTo[allStages,savePNT2];

(*(3) Stator outlet  *)
p03=p01*\[Beta]stage;
C3=C1; (*Repeating stage!*)
T3=T03-(C3^2/(2*cp));
a3=Sqrt[\[Gamma]*R*T3];
M3=C3/a3;
p3=pF[p03,\[Gamma],M3];
M3rel=M1rel*a1/a3;
s3=s[p3,T3,\[Gamma], R];
Print["Stage # ",i," Pratio = ", \[Beta]stage," p0out = ",p03," T0out = ",T03," Mout= ",M3, " \[Alpha]1 = ",\[Alpha]1," \[Beta]2 = ",\[Beta]2 ", \[CapitalDelta]\[Beta] = ",\[Beta]1-\[Beta]2 ];
{CrossSection3,rtip3,rhub3}=annulusDesign[massflow,T3,p3,Ca*C1,\[Gamma],R,rmean];
savePNT3={T3,T03,p3,p03,s3,rmean,rtip3,rhub3,CrossSection3,M3,M3rel};
AppendTo[allStages,savePNT3];

(*Setting inflow of following stage*)
M1=M3;
T01=T03;
T1=T3;
p01=p03;
p1=p3;

,{i,1,nStages}];

Print["Database contains: T,T0,p,p0,s,rmean,rtip,rhub,CrossSection,M,Mrel"];
Print["Triangle contains: \[Alpha]1,\[Alpha]2,\[Beta]1,\[Beta]2,Ca,U,Cu1,Cu2,Wu1,Wu2,\[Omega]"];

Return[{nStages,allStages,saveTriangle,plot}]
];


(* ::Subsection::Closed:: *)
(*Turbine*)


(* ::Subsubsection::Closed:: *)
(*Tools*)


SmartSizeTurbineStage[M2_,M3R_,U_,T0inlet_,p0inlet_,TratioStage_,cp_,\[Gamma]_]:=Module[
{\[Alpha]2guess,\[Alpha]2,alfa2,
tratio,turning,reaction,beta2,alfa3,beta3,axial,\[CapitalDelta]M,
R,Cax,T01,p01,T02,p02,T2,p2,C2,W2,M2R,p02R,perfo2,T03,p03,T3,p3,C3,W3,p03R,a3,M3,perfo3},

\[Alpha]2guess=85;
While[\[Alpha]2guess>70.,
\[Alpha]2=alfa2/.First[FindRoot[SizeTurbineStage[alfa2,M2,M3R,U,T0inlet,cp,\[Gamma]][[1]]==TratioStage,{alfa2,\[Alpha]2guess}]];
{tratio,turning,reaction,beta2,alfa3,beta3,axial}=SizeTurbineStage[\[Alpha]2,M2,M3R,U,T0inlet,cp,\[Gamma]];
\[Alpha]2guess=\[Alpha]2;
If[\[Alpha]2guess>75.,
\[CapitalDelta]M=0.05;
M2=M2+\[CapitalDelta]M;
If[M3R<0.9,M3R=M3R+\[CapitalDelta]M];
]
];


R = cp*(\[Gamma]-1)/\[Gamma];
Cax = axial*Vprime[cp,T0inlet];
(* Stator upstream (1)*)
T01 = T0inlet;
p01 = p0inlet;

(* Stator downstream / Rotor upstream (2)*)
T02 = T01;
p02 = p01; (*here efficiency needed*)
T2 = TF[T02,\[Gamma],M2];
p2 = pF[p02,\[Gamma],M2];
C2 = M2*Sqrt[\[Gamma] R T2];
W2 = Cax/Cos[Deg2Rad[beta2]];
M2R = W2/Sqrt[\[Gamma] R T2];
p02R = p0ratio[\[Gamma],M2R]*p2;
T02R = T0ratio[\[Gamma],M2R]*T2;

perfo2 = {\[Alpha]2,beta2,T02,T2,p02,p2,p02R,C2,M2,W2,M2R};

(* Rotor downstream (3)*)
T03R = T02R;
T3 = TF[T03R,\[Gamma],M3R];
T03 = TratioStage*T0inlet;
p03R = p02R; (*here efficiency needed*)
p3 = pF[p03R,\[Gamma],M3R];
C3 = Cax/Cos[Deg2Rad[alfa3]];
W3 = Cax/Cos[Deg2Rad[beta3]];
(*a3 = W3/M3R;
T3 = a3^2/(\[Gamma] R);*)
M3 = C3/Sqrt[\[Gamma] R T3];
p03 = p0ratio[\[Gamma],M3]*p3;

perfo3 = {alfa3,beta3,T03,T3,p03,p3,p03R,C3,M3,W3,M3R};

Return[{tratio,turning,reaction,axial,perfo2,perfo3}]
];



CalcNstages[T0inlet_,T0outlet_,U_,cp_]:=Module[
{\[CapitalDelta]T0turbine,psiTotal,\[CapitalDelta]T0stage,nStages,dT0,ToutletStage,TratioStage,TinletStage},

\[CapitalDelta]T0turbine=T0inlet-T0outlet;
psiTotal=PsiTurb[\[CapitalDelta]T0turbine,U,cp];
If[psiTotal>2.0,nStages=Ceiling[psiTotal/2.0],nStages=1];
Print["Number of stages = ",nStages];
Print["Stage loadings =",psiTotal/nStages];
\[CapitalDelta]T0stage=dT0/.First[Solve[PsiTurb[dT0,U,cp]==psiTotal/nStages,dT0]];
ToutletStage={};
TratioStage={};
TinletStage=T0inlet;
Do[
AppendTo[ToutletStage,TinletStage-\[CapitalDelta]T0stage];
AppendTo[TratioStage,(TinletStage-\[CapitalDelta]T0stage)/TinletStage];
TinletStage=ToutletStage[[i]];,
{i,1,nStages}];
Return[{nStages,\[CapitalDelta]T0stage,ToutletStage,TratioStage}];

]


CalcNstageshpt[T0inlet_,T0outlet_,U_,cp_]:=Module[
{\[CapitalDelta]T0turbine,psiTotal,\[CapitalDelta]T0stage,nStages,dT0,ToutletStage,TratioStage,TinletStage},

\[CapitalDelta]T0turbine=T0inlet-T0outlet;
psiTotal=PsiTurb[\[CapitalDelta]T0turbine,U,cp];
If[psiTotal>1.3,nStages=Ceiling[psiTotal/1.3]];
Print["Number of stages = ",nStages];
Print["Stage loadings =",psiTotal/nStages];
\[CapitalDelta]T0stage=dT0/.First[Solve[PsiTurb[dT0,U,cp]==psiTotal/nStages,dT0]];
ToutletStage={};
TratioStage={};
TinletStage=T0inlet;
Do[
AppendTo[ToutletStage,TinletStage-\[CapitalDelta]T0stage];
AppendTo[TratioStage,(TinletStage-\[CapitalDelta]T0stage)/TinletStage];
TinletStage=ToutletStage[[i]];,
{i,1,nStages}];
Return[{nStages,\[CapitalDelta]T0stage,ToutletStage,TratioStage}];

]


CalcNstageslpt[T0inlet_,T0outlet_,U_,cp_]:=Module[
{\[CapitalDelta]T0turbine,psiTotal,\[CapitalDelta]T0stage,nStages,dT0,ToutletStage,TratioStage,TinletStage},

\[CapitalDelta]T0turbine=T0inlet-T0outlet;
psiTotal=PsiTurb[\[CapitalDelta]T0turbine,U,cp];
If[psiTotal>0.9,nStages=Ceiling[psiTotal/.9]];
Print["Number of stages = ",nStages];
Print["Stage loadings =",psiTotal/nStages];
\[CapitalDelta]T0stage=dT0/.First[Solve[PsiTurb[dT0,U,cp]==psiTotal/nStages,dT0]];
ToutletStage={};
TratioStage={};
TinletStage=T0inlet;
Do[
AppendTo[ToutletStage,TinletStage-\[CapitalDelta]T0stage];
AppendTo[TratioStage,(TinletStage-\[CapitalDelta]T0stage)/TinletStage];
TinletStage=ToutletStage[[i]];,
{i,1,nStages}];
Return[{nStages,\[CapitalDelta]T0stage,ToutletStage,TratioStage}];

]


PsiTurb[\[CapitalDelta]T0_,U_,cp_]:=(cp*\[CapitalDelta]T0)/U^2;


SizeTurbineStage[alfa2_,M2_,M3R_,U_,T0inlet_,cp_,\[Gamma]_]:=Module[
{\[CapitalOmega],tratio,turning,reaction,alfa3,beta2,beta3,axial,Ma2R},

\[CapitalOmega]=U/Sqrt[cp*T0inlet];
tratio=\[Tau]ts[\[Gamma],M2,Deg2Rad[alfa2],\[CapitalOmega],M3R];
reaction=Rt[\[Gamma],M2,Deg2Rad[alfa2],\[CapitalOmega],M3R];
alfa3=Rad2Deg[\[Alpha]3[\[Gamma],M2,Deg2Rad[alfa2],\[CapitalOmega],M3R]];
beta2=Rad2Deg[\[Beta]2[\[Gamma],M2,Deg2Rad[alfa2],\[CapitalOmega]]];
beta3=Rad2Deg[\[Beta]3[\[Gamma],M2,Deg2Rad[alfa2],\[CapitalOmega],M3R]]; (* beta3=Rad2Deg[\[Beta]3[\[Gamma],M2,Deg2Rad[alfa2],\[CapitalOmega],M3R]]*)
turning=beta2+beta3;
axial=Cabar[\[Gamma],M2,Deg2Rad[alfa2]];

Return[{tratio,turning,reaction,beta2,alfa3,beta3,axial}]
]


C2bar[\[Gamma]_,M2_]:=Sqrt[((\[Gamma]-1)M2^2)/(1+(\[Gamma]-1)M2^2/2)];
Cabar[\[Gamma]_,M2_,\[Alpha]2_]:=C2bar[\[Gamma],M2] Cos[\[Alpha]2];
Cu2bar[\[Gamma]_,M2_,\[Alpha]2_]:=C2bar[\[Gamma],M2] Sin[\[Alpha]2];
Wu2bar[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_]:=Cu2bar[\[Gamma],M2,\[Alpha]2]-\[CapitalOmega];
\[Beta]2[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_]:=ArcTan[Wu2bar[\[Gamma],M2,\[Alpha]2,\[CapitalOmega]]/Cabar[\[Gamma],M2,\[Alpha]2]];
Tt2rOverTt1[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_]:=1+\[CapitalOmega]^2 (0.5-Cu2bar[\[Gamma],M2,\[Alpha]2]/\[CapitalOmega]);
\[Beta]3[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_]:=ArcTan[Sqrt[Tt2rOverTt1[\[Gamma],M2,\[Alpha]2,\[CapitalOmega]]/Cabar[\[Gamma],M2,\[Alpha]2]^2 ((\[Gamma]-1)M3R^2)/(1+(\[Gamma]-1)M3R^2/2)-1]];
Cu3bar[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_]:=Cabar[\[Gamma],M2,\[Alpha]2]Tan[\[Beta]3[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]]-\[CapitalOmega];
\[Alpha]3[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_]:=ArcTan[Cu3bar[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]/Cabar[\[Gamma],M2,\[Alpha]2]];
T3OverTt1[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_]:=Tt2rOverTt1[\[Gamma],M2,\[Alpha]2,\[CapitalOmega]]/(1+(\[Gamma]-1)M3R^2/2);
\[Tau]ts[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_]:=T3OverTt1[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]+Cabar[\[Gamma],M2,\[Alpha]2]^2*(1+Tan[\[Alpha]3[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]]^2)/2;
Wu3bar[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_]:=Cabar[\[Gamma],M2,\[Alpha]2]Tan[\[Beta]3[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]];
Rt[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_]:=(Wu3bar[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]^2-Wu2bar[\[Gamma],M2,\[Alpha]2,\[CapitalOmega]]^2)(1/(2(1-\[Tau]ts[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R])));
\[Sigma]xr[\[Gamma]_,M2_,\[Alpha]2_,\[CapitalOmega]_,M3R_,Z_]:=1/Z 2 Cos[\[Beta]3[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]]^2(Tan[\[Beta]2[\[Gamma],M2,\[Alpha]2,\[CapitalOmega]]]+Tan[\[Beta]3[\[Gamma],M2,\[Alpha]2,\[CapitalOmega],M3R]]);

Deg2Rad[angledeg_]:=angledeg*Pi/180.;
Rad2Deg[anglerad_]:=anglerad*180./Pi;

Vprime[cp_,T01_]:=Sqrt[cp*T01];





(* ::Subsubsection:: *)
(*Size it!*)


SizeTurbine[T0inlet_,T0outlet_,p0inlet_,U_,cp_,\[Gamma]_]:=Module[
{nStages,\[CapitalDelta]T0stage,ToutletStage,TratioStage,
allPerfo,M2,M3R,tratio,turning,reaction,alfa2,beta2,alfa3,beta3,axial,p0in,
perfo,parnames,\[Alpha]2,perfo2,perfo3,T0in,T02,T2,p02,p2,p02R,C2,W2,M2R,T03,T3,p03,p3,p03R,C3,M3,W3},

{nStages,\[CapitalDelta]T0stage,ToutletStage,TratioStage}=CalcNstages[T0inlet,T0outlet,U,cp];
M3R=0.8;

allPerfo={};
Do[
Print["stage #",n];
If[n==1,
M2=1.0;
T0in = T0inlet;
p0in = p0inlet,
(*else*)
M2=0.9;
T0in = ToutletStage[[n-1]];
p0in = p03;
];

{tratio,turning,reaction,axial,perfo2,perfo3}=SmartSizeTurbineStage[M2,M3R,U,T0in,p0in,TratioStage[[n]],cp,\[Gamma]];
{alfa2,beta2,T02,T2,p02,p2,p02R,C2,M2,W2,M2R}=perfo2;
{alfa3,beta3,T03,T3,p03,p3,p03R,C3,M3,W3,M3R}=perfo3;
perfo={tratio,reaction,alfa2,beta2,T02,T2,p02,p2,p02R,C2,M2,W2,M2R,alfa3,beta3,T03,T3,p03,p3,p03R,C3,M3,W3,M3R,turning,axial*Vprime[cp,T02]};
AppendTo[allPerfo,perfo];
parnames={"\!\(\*SubscriptBox[\(\[Tau]\), \(stage\)]\)","R","\[Alpha]2","\[Beta]2","T02","T2","p02","p2","p02R","C2","M2","W2","M2R","\[Alpha]3","\[Beta]3","T03","T3","p03","p3","p03R","C3","M3","W3","M3R","\[Beta]2+\[Beta]3","Ca"};
Print[TableForm[perfo,TableHeadings->{parnames,None}]],
{n,1,nStages}];

Return[allPerfo]

];


(* ::Section::Closed:: *)
(*Components Performance Maps*)


(* ::Subsection::Closed:: *)
(*Air Intake*)


mredIntakeIsen[A0_,M0_,\[Gamma]_]:=M0 A0 Sqrt[\[Gamma]] (1+(\[Gamma]-1)/2 M0^2)^((\[Gamma]+1)/(2(1-\[Gamma])));

\[Epsilon][M0_]:=1.0-0.1(M0-1)^1.5/; M0>1;
\[Epsilon][M0_]:=1.0/; M0\[LessSlantEqual]1;

mredIntake[A0_,M0_,\[Gamma]_]:=M0 A0 Sqrt[\[Gamma]] (1+(\[Gamma]-1)/2 M0^2)^((\[Gamma]+1)/(2(1-\[Gamma])))/; M0<=1;
mredIntake[A0_,M0_,\[Gamma]_]:=1/\[Epsilon][M0] M0 A0 Sqrt[\[Gamma]] (1+(\[Gamma]-1)/2 M0^2)^((\[Gamma]+1)/(2(1-\[Gamma])))/; M0>1;


(* ::Subsection::Closed:: *)
(*Turbine*)


mredTurbine[pratio_,\[Eta]_,area_,\[Gamma]_]:=area Sqrt[(2 \[Gamma])/(\[Gamma]-1)](pratio)  Sqrt[\[Eta](1-(pratio)^((\[Gamma]-1)/\[Gamma]))]/(1-\[Eta](1-(pratio)^((\[Gamma]-1)/\[Gamma])))/;pratio>=2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma])));
mredTurbine[pratio_,\[Eta]_,area_,\[Gamma]_]:=area Sqrt[(2 \[Gamma])/(\[Gamma]-1)](2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma]))))  Sqrt[\[Eta](1-(2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma]))))^((\[Gamma]-1)/\[Gamma]))]/(1-\[Eta](1-(2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma]))))^((\[Gamma]-1)/\[Gamma])))/;pratio<2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma])));

pratio2Turbs[pratioComp_,area4_,area3_,\[Eta]4_,\[Eta]3_,\[Gamma]_]:=Module[{dum,x},
dum=FindRoot[mredTurb[\!\(TraditionalForm\`x\)/pratioComp,\[Eta]4,area4,\[Gamma]]==mredTurb[1/x,\[Eta]3,area3,\[Gamma]]\!\(TraditionalForm\`x\) Sqrt[1-\[Eta]3(1-(1/x)^((\[Gamma]-1)/\[Gamma]))],\!\(TraditionalForm\`{x, 1.01}\)];
x/.dum[[1]]];


PratioCrit[\[Gamma]_]:=2^(\[Gamma]/(-1+\[Gamma])) (1+\[Gamma])^(-(\[Gamma]/(-1+\[Gamma])));


(* ::Subsection::Closed:: *)
(*Compressor*)


StageReducedProperties[massflow_,Ncomp_,diameter_,area_,alpha1_,p01_,T01_,Rgas_,\[Gamma]_]:=Module[
{sol,U,T,Ca1,Machd,Mach,phi,nred,mred,choked,a1},

nred=(Ncomp diameter)/Sqrt[Rgas T01];

mred=(massflow Sqrt[Rgas T01])/(area p01 );
sol=FindRoot[mred== Sqrt[\[Gamma]]  Machd Cos[alpha1] (1.+(\[Gamma]-1)/2 Machd^2)^(1/2-\[Gamma]/(\[Gamma]-1)),{Machd,0.5,0.,1.}];

Mach=Machd/.sol[[1]];

(*Print["mach : ",Mach];*)

choked=False;

If[Mach>0.99999,
choked=True;
(*Print["choked : ",choked," Mach = ", Mach];*)

Return[{choked,{0,0,0,0,0,0}}]
];

T = TF[T01,\[Gamma],Mach];


Ca1 = Mach Cos[alpha1] Sqrt[\[Gamma] Rgas T];

U = Pi Ncomp diameter;

phi = Ca1/U;


Return[{choked,{Ca1,T,Mach,phi,nred,mred}}]
];


Phi[mred_,nred_,Mach_,\[Gamma]_]:=Module[
{phi,delta},
delta=(\[Gamma]-1.)/2.;
phi=(1./Pi)(mred/nred)*(1.+delta Mach^2)^(1./(\[Gamma]-1.)) 
];

shockLosses[phi_,phid_,posIncidence_,negIncidence_]:=negIncidence (phi-phid)^2/;phi<phid;
shockLosses[phi_,phid_,posIncidence_,negIncidence_]:=posIncidence (phi-phid)^2/;phi>=phid;

Psi[phi_,phid_,alpha1_,beta2_,etaStage_]:=Module[
{psi,k,negIncidence,posIncidence=4,viscLossCoeff,psi0,phi0},
k=Tan[alpha1 ]+Tan[beta2];
viscLossCoeff=(PsiIdeal[phid,alpha1,beta2]/phid^2)*(1.-etaStage);

psi0=0.1;
phi0=0.;
negIncidence=(1. -k phi0-psi0-viscLossCoeff (phi0^2) )/(phi0-1. phid)^2;
psi0=0.0;
phi0=0.8/k;
posIncidence=(1. -k phi0-psi0-viscLossCoeff (phi0^2) )/(phi0-1. phid)^2;
psi=(1.-k  phi)-shockLosses[phi,phid,posIncidence,negIncidence]-viscLossCoeff phi^2;

Return[psi]
];

PsiIdeal[phi_,alpha1_,beta2_]:=Module[
{phimax,psi,k},
k=Tan[alpha1 ]+Tan[beta2];
psi=(1.-k  phi);
Return[psi]
];

EfficiencyComp[phi_,phid_,alpha1_,beta2_,etaStage_]:=Abs[Psi[phi,phid,alpha1,beta2,etaStage]/PsiIdeal[phi,alpha1,beta2]]/;Psi[phi,phid,alpha1,beta2,etaStage]>0;
EfficiencyComp[phi_,phid_,alpha1_,beta2_,etaStage_]:=0/;Psi[phi,phid,alpha1,beta2,etaStage]<=0;

PRatioComp[mred_,nred_,Mach_,phid_,alpha1_,beta2_,\[Gamma]_,etaStage_]:=Module[
{phi,psi,PRatio},
phi=Phi[mred,nred,Mach,\[Gamma]];
psi=Psi[phi,phid,alpha1,beta2,etaStage];
PRatio=(1.+Pi^2((\[Gamma]-1)/\[Gamma])nred^2 psi)^(\[Gamma]/(\[Gamma]-1.));
Return[PRatio]
];

plotPoint[x_,y_, color_] :=  Graphics[{PointSize[Large], color, Point[{x,y}]}];

multistageCompressorState[T01_,p01_,mflow_,Ncomp_  ,phid_,R_,\[Gamma]c_ ,alpha1_,beta2_,Area_,d_,etaStage_]:=Module[
{i,Z,delta,mredtot,nredtot,Ac1,dn,
T0n,p0n,perfo,mredn,nredn,betacn,delT0n,
sol,phin,Un,Mn,Tn,Can,betaCtot,delT0tot,
etaTot,T0nisen, 
nredOnD,mredOnD,choked,turbine,
betaCmap,etaCmap,nredCmap,delT0map,Machmap,
lastStage,lastStagemap,etaCmap1,
chokepoint},

delta=(\[Gamma]c-1.)/2.;

T0n=T01;
p0n=p01;

mredtot=mflow Sqrt[R T01]/(p01 Area[[1]]);
nredtot=(Ncomp d[[1]])/Sqrt[R T01];

Z=Length[d];
chokepoint={0,0};
perfo={};
Do[
lastStage=i;

{choked,{Can,Tn,Mn,phin,nredn,mredn}}=StageReducedProperties[mflow,Ncomp,d[[i]],Area[[i]],alpha1[[i]], p0n,T0n, R,\[Gamma]c];

If[choked,(*Print["choked at mred = ",mredtot," and nred = ", nredtot];*)chokepoint={mredtot,p0n/p01}; Break[]];

betacn=PRatioComp[mredn,nredn,Mn,phid[[i]],alpha1[[i]],beta2[[i]],\[Gamma]c,etaStage];

turbine=False;

If[betacn<1.,turbine=True;Break[]];

phin=Phi[mredn,nredn,Mn,\[Gamma]c];
delT0n=( betacn^((\[Gamma]c-1)/\[Gamma]c)-1)/EfficiencyComp[phin,phid[[i]],alpha1[[i]],beta2[[i]],etaStage];
T0n=T0n(1.+delT0n);

p0n=p0n * betacn ;

AppendTo[perfo,{betacn,delT0n,T0n,p0n,Can,Tn,Mn,phin,nredn,mredn}],
{i,1,Z}];

betaCtot=p0n/p01;
delT0tot=(T0n-T01)/T01;
T0nisen=T01*betaCtot^((\[Gamma]c-1)/\[Gamma]c);
etaTot=etaTotF[T01,T0nisen,T0n];

betaCmap={{mredtot,nredtot},betaCtot };
etaCmap1={{mredtot,nredtot}, etaTot};

etaCmap={mredtot,betaCtot, etaTot};
nredCmap={mredtot,betaCtot, nredtot};
delT0map={mredtot,betaCtot,delT0tot};
Machmap={mredtot,betaCtot,Mn};
lastStagemap={mredtot,betaCtot, lastStage};


Return[{betaCmap,etaCmap,nredCmap,delT0map,Machmap,lastStagemap,etaCmap1,perfo,chokepoint}]
];

etaTotF[T01_,T0nisen_,T0n_]:=(T0nisen-T01)/(T0n-T01)/;T0n-T01!=0; 

etaTotF[T01_,T0nisen_,T0n_]:=0/;T0n-T01==0; 




multistageCompressorMap[massflowOnD_, NcompOnD_, 
nrng_,nmflow_,T01_,p01_, phim_,
Rgas_,\[Gamma]_,
\[Alpha]1m_,\[Beta]2m_,Area_,meanDiameter_,
etaStage_,mapResolution_,dump_]:=Module[
{
rng,rngmin,rngmax,drng,
mflow,mflowmin,mflowmax,dmflow,
betaAll,etaAll,etaAll1,nredAll,delT0All,lastStageAll,MachAll,
betaCmap,etaCmap,nredmap,delT0map,Machmap,lastStagemap,etaCmap1,perfo,
betaF,etaCF,
xmin,xmax,ymin,ymax,
mssflow,Candum,Tndum,Mndum,phindum,nreddum,mreddum,choked,Ngiri,i,chokepoint,chokeAll,
mapPlot
},

(*Determination of maximum massflow as the massflow that causes first stage choking, aka right-hand map limit*)
Do[

Ngiri=NcompOnD*2;
mssflow=massflowOnD+i;
{choked,{Candum,Tndum,Mndum,phindum,nreddum,mreddum}}=StageReducedProperties[mssflow,Ngiri,meanDiameter[[1]],Area[[1]],\[Alpha]1m[[1]],p01,T01,Rgas,\[Gamma]];

If[choked,mflowmax=mssflow-0.5;Break[];],
{i,0,100,0.1}];

Print["First stage choking occurs at massflow = ", mssflow];



{rngmin,rngmax}={0.5 *NcompOnD, 1.08* NcompOnD};
mflowmin=0.0* massflowOnD;

drng=(rngmax-rngmin)/nrng;
dmflow=(mflowmax-mflowmin)/nmflow;

betaAll=etaAll=etaAll1=nredAll=delT0All=lastStageAll=MachAll=chokeAll={};
Do[
Do[
{betaCmap ,etaCmap ,nredmap ,delT0map ,Machmap ,lastStagemap ,etaCmap1 ,perfo, chokepoint }=multistageCompressorState[
T01 ,p01 ,mflow ,rng ,phim, Rgas, \[Gamma] ,\[Alpha]1m, \[Beta]2m, Area, meanDiameter, etaStage ];
AppendTo[betaAll,betaCmap];
AppendTo[etaAll,etaCmap];
AppendTo[etaAll1,etaCmap1];
AppendTo[nredAll,nredmap];
AppendTo[delT0All,delT0map];
AppendTo[MachAll,Machmap];
AppendTo[lastStageAll,lastStagemap];
AppendTo[chokeAll,chokepoint];
,{rng,rngmin,rngmax,drng}
]
,{mflow,mflowmin,mflowmax,dmflow}
];

(*betaF = Interpolation[betaAll];*)
betaF = Interpolation[betaAll,InterpolationOrder->6];
etaCF = Interpolation[etaAll1];

{{xmin,xmax},{ymin,ymax}}=betaF[[1]];

mapPlot=plotCompressorMap[mapResolution,betaF,{xmin,xmax,ymin,ymax},chokeAll];

If[dump,
(*betaAll>>PRatioCompMSdata.m;*)
betaF>>PRatioCompMS.m;
{{xmin,xmax,ymin,ymax},chokeAll}>>PRatioCompMSaux.m,
Return[{
{betaAll,etaAll,etaAll1,nredAll,delT0All,lastStageAll,MachAll},
{betaF,etaCF},
{xmin,xmax,ymin,ymax},chokeAll
}]
];
Return[mapPlot];

];


findCompressorStabilityBoundary[betaF_, xmin_, xmax_,ymin_,ymax_,nrng_,color_]:=Module[
{x,y,plStab,stabBnd,sol,xopt,yopt,point,stabBndF},
plStab={};
stabBnd={};
xopt=xmin;
Do[
(*sol=FindMaximum[betaF[x,y],{x,0.5(xmin+ xmax),xmin, xmax}];*)
sol=FindMaximum[betaF[x,y],{x,xopt,xmin, xmax}];
xopt=x/.sol[[2]];
yopt=sol[[1]];
AppendTo[stabBnd,{xopt,yopt}];
point=plotPoint[xopt,yopt,color] ;
AppendTo[plStab,point],
{y,ymin,ymax,(ymax-ymin)/nrng}
];
stabBndF=Interpolation[stabBnd];

Return[{plStab,stabBnd,stabBndF}]
];



findCompressorChokingBoundary[chokeList_]:=Module[{sortedList,boundaryList,interpBoundary},

sortedList=GatherBy[Sort[chokeList,#1[[1]]<#2[[1]]&],First];
boundaryList=Table[sortedList[[i,1]],{i,1,Length[sortedList]}];
interpBoundary=Interpolation[boundaryList,InterpolationOrder->2];

Return[{boundaryList,interpBoundary}];

];


plotCompressorMap[mapResolution_,PRatioCompMS_,range_,chokeAll_]:=Module[{nrng,nmflow,xmin,xmax,ymin,ymax,lowerLimit,upperlimit,plStab,stabBnd,stabBndF,boundaryList,interpBoundary,betaPlotMap,mapPlot},

nrng=mapResolution;nmflow=mapResolution;
{xmin,xmax,ymin,ymax}=range;

(*compute and plot stability boundary*)
{plStab,stabBnd,stabBndF}=findCompressorStabilityBoundary[PRatioCompMS, xmin, xmax,ymin,ymax,nrng,Red];

(*plot betaMap*)
lowerLimit=1.05*stabBnd[[1,1]];
betaPlotMap=Plot[Table[PRatioCompMS[x,y],{y,ymin,ymax,(ymax-ymin)/nrng}],{x,xmin,xmax},Frame->True,RegionFunction->Function[{x,y},x>lowerLimit]];

{boundaryList,interpBoundary}=findCompressorChokingBoundary[chokeAll];

upperlimit=1.2*stabBnd[[nrng+1,2]];
mapPlot=Show[betaPlotMap,
Plot[interpBoundary[x],{x,xmin,xmax},PlotStyle->White,Filling->Bottom,FillingStyle->Directive[Opacity[1],White]],
ListPlot[chokeAll,PlotStyle->LightGray],
Plot[stabBndF[x],{x,stabBndF["Domain"][[1,1]],stabBndF["Domain"][[1,2]]},Filling->Top,FillingStyle->Directive[Opacity[1],White]],plStab,PlotRange->{{lowerLimit,xmax},{1,upperlimit}},Frame->True,Axes->False,FrameLabel->{"\!\(\*SubscriptBox[\(m\), \(red\)]\)","\[CapitalPi]"}];
Return[mapPlot];
];


plotCompressorMapFromFile[mapResolution_]:=Module[{PRatioCompMS,xmin,xmax,ymin,ymax,chokeAll,mapPlot},

PRatioCompMS=<<"PRatioCompMS.m";
{{xmin,xmax,ymin,ymax},chokeAll}=<<"PRatioCompMSaux.m";
mapPlot=Quiet[plotCompressorMap[mapResolution,PRatioCompMS,{xmin,xmax,ymin,ymax},chokeAll]];
Return[mapPlot];

];


plotPoint[x_,y_, color_] :=  Graphics[{PointSize[Large], color, Point[{x,y}]}];


(* ::Section::Closed:: *)
(*Dimensional Analysis*)


(* ::Text:: *)
(*Module that computes non-dimensional parameters at design point*)


CalcNonDimPars[massflow_,cyclePNTs_,FlightMach_,CompressorPars_,\[Eta]t_,\[Eta]n_,\[Gamma]air_,\[Gamma]gas_,Rair_,Rgas_]:=Module[
{PNTa,PNT02,PNT03,PNT04,PNT05,PNTe,
pa,Ta,rhoa,sa,
p02,T02,rho02,s02,
p03,T03,rho03,s03,
p04,T04,rho04,s04,
p05,T05,rho05,s05,
pe,Te,rhoe,se,
AreaComp, DiameterComp, RPMcomp, etaC,
PratioRam,AreaRam,
mredComp,NredComp,PratioComp,TratioComp,\[CapitalDelta]T0Comp,
PratioBurner,TratioBurner,
TratioMax,
mredTurb,NredTurb,PratioTurb,TratioTurb,\[CapitalDelta]T0Turb,AreaTurb,
PratioNozzle,AreaNozzle,
NonDimPars},



{PNTa,PNT02,PNT03,PNT04,PNT05,PNTe}=cyclePNTs;
{pa,Ta,rhoa,sa}=PNTa;
{p02,T02,rho02,s02}=PNT02;
{p03,T03,rho03,s03}=PNT03;
{p04,T04,rho04,s04}=PNT04;
{p05,T05,rho05,s05}=PNT05;
{pe,Te,rhoe,se}=PNTe;

{AreaComp, DiameterComp, RPMcomp}=CompressorPars;

(*Intake*)
PratioRam=\[Epsilon][FlightMach](1+(\[Gamma]air-1)/2 FlightMach^2)^(\[Gamma]air/(\[Gamma]air-1));
(*AreaRam=(massflow Sqrt[Rair T02]/p02  \[Epsilon][FlightMach](1+(\[Gamma]air-1)/2 FlightMach^2)^(\[Gamma]air/(\[Gamma]air-1)))/mredIntake[1,FlightMach,\[Gamma]air]; *)
AreaRam=(massflow Sqrt[Rair T02]/p02  \[Epsilon][FlightMach])/mredIntake[1,FlightMach,\[Gamma]air];(* 2/6/2024*)

(*Compressor: Area, Diameter, RPM are known from compressor design*)
mredComp=massflow Sqrt[Rair T02]/p02;(*NOT normalized with Area*)
NredComp=(RPMcomp*DiameterComp)/Sqrt[Rair T02];

PratioComp=p03/p02;
TratioComp=T03/T02;
\[CapitalDelta]T0Comp=T03-T02;


(*Burner*)

PratioBurner=p04/p03;
TratioBurner=T04/T03;

TratioMax=T04/T02;

(*Turbine*)

PratioTurb=p04/p05;
TratioTurb=T04/T05;
\[CapitalDelta]T0Turb=T04-T05;
mredTurb=massflow Sqrt[Rgas T04]/p04; (*NOT normalized with Area*)
NredTurb=(RPMcomp DiameterComp)/Sqrt[Rgas T04];
AreaTurb=massflow/mredTurbine[1/PratioTurb,\[Eta]t,1.,\[Gamma]gas]  Sqrt[Rgas T04]/p04;

(*Nozzle*)
PratioNozzle=Min[(PratioComp PratioBurner PratioRam)/PratioTurb  ,1/PratioCrit[\[Gamma]gas]];

AreaNozzle=massflow/mredTurbine[1/PratioNozzle,\[Eta]n,1.,\[Gamma]gas]  Sqrt[Rgas T05]/ p05;

NonDimPars={PratioRam,AreaRam,
mredComp,NredComp,PratioComp,TratioComp,\[CapitalDelta]T0Comp,
PratioBurner,TratioBurner,
TratioMax,
mredTurb,NredTurb,PratioTurb,TratioTurb,\[CapitalDelta]T0Turb,AreaTurb,
PratioNozzle,AreaNozzle};


Return[NonDimPars]

];


(* ::Section::Closed:: *)
(*Build it*)


(* ::Subsection:: *)
(*Engine On-Design and Performance maps*)


BuildEngine[enginePars_,mapResolution_,report_]:=Module[
	{MOnD,z,\[Gamma]air,Rair,\[Eta]d,\[Eta]mc,\[Eta]c,\[Beta]c,Qf,\[Gamma]gas,Rgas,\[Eta]b,\[Eta]pb,T4,\[Eta]t,\[Eta]mt,\[Eta]n,massflowOnD,
	Ma1,etaStage,DF,\[CapitalDelta]T0stage,solidity,\[Sigma]material,\[Rho]blade,taper,
	compressorDesignChoices,
    p0aOnD,T0aOnD,\[Rho]0aOnD,s0aOnD,
	p02OnD,T02OnD,\[Rho]02OnD,s02OnD,
	p03OnD,T03OnD,\[Rho]03OnD,s03OnD,
	p04OnD,T04OnD,\[Rho]04OnD,s04OnD,
	p05OnD,T05OnD,\[Rho]05OnD,s05OnD,
	p9OnD,T9OnD,\[Rho]9OnD,s9OnD,
	PNTa,PNT02,PNT03,PNT04,PNT05,PNT9,
	cyclePNTs,energy,performance,
    nStages,allStates,triangle,plotComp,
    CompAreasD,rMeanD,DiameterD,rTipD,rRootD,\[Alpha]1D,\[Beta]2D,phiD,NcompD,mredD,
    plotCompressor,
    CompressorPars,NonDimPars,
	PratioRamOnD,AreaRamOnD,mredCompOnD,NredCOnD,
    PratioCompOnD,TratioCOnD,\[CapitalDelta]T0CompOnD,PratioBurnerOnD,
    TratioBurnerOnD,TratioMaxOnD,mredTurbOnD,NredTurbOnD,
    PratioTurbOnD,TratioTurbOnD,\[CapitalDelta]T0TurbOnD,AreaTurbOnD,
    PratioNozzleOnD,AreaNozzleOnD,
    varsOnD,designPars,m3m1=1.0,
    nrng,nmflow,dump,mapPlot
	},
		
	{MOnD,z,\[Gamma]air,Rair,\[Eta]d,\[Eta]mc,\[Eta]c,\[Beta]c,Qf,\[Gamma]gas,Rgas,\[Eta]b,\[Eta]pb,T4,\[Eta]t,\[Eta]mt,\[Eta]n,massflowOnD,compressorDesignChoices}=enginePars;
	{Ma1,\[CapitalDelta]T0stage,etaStage,DF,solidity,\[Sigma]material,\[Rho]blade,taper}=compressorDesignChoices;
	
	{cyclePNTs,energy,performance}=CalcTurbojetCycle[MOnD,z,\[Gamma]air,Rair,\[Eta]d,\[Eta]mc,\[Eta]c,\[Beta]c,Qf,\[Gamma]gas,Rgas,\[Eta]b,\[Eta]pb,T4,\[Eta]t,\[Eta]mt,\[Eta]n,report];
	{PNTa,PNT02,PNT03,PNT04,PNT05,PNT9}=cyclePNTs;
	{p0aOnD,T0aOnD,\[Rho]0aOnD,s0aOnD}=PNTa;
	{p02OnD,T02OnD,\[Rho]02OnD,s02OnD}=PNT02;
	{p03OnD,T03OnD,\[Rho]03OnD,s03OnD}=PNT03;
	{p04OnD,T04OnD,\[Rho]04OnD,s04OnD}=PNT04;
	{p05OnD,T05OnD,\[Rho]05OnD,s05OnD}=PNT05;
	{p9OnD,T9OnD,\[Rho]9OnD,s9OnD}=PNT9;

    If[report,
		Print[Style["Sizing Multi Stage Compressor",12,Bold,Blue]];
	];

	{nStages,allStates,triangle,plotComp}=designMultiStageCompressor[T02OnD,p02OnD,\[Beta]c,etaStage,\[Gamma]air,Rair,cp[\[Gamma]air,Rair],Ma1,DF,solidity,\[CapitalDelta]T0stage,massflowOnD,\[Sigma]material,\[Rho]blade,taper];
	
    If[report,
    Show[plotComp]
	];
	
	{CompAreasD,rMeanD,DiameterD,rTipD,rRootD,\[Alpha]1D,\[Beta]2D,phiD,NcompD,mredD}=collectCompressorGeometries[allStates,triangle,nStages,massflowOnD,Rair];
    
    If[report,
    plotCompressor=drawCompressor[allStates,7,8,nStages];Show[plotCompressor]
	];	

    CompressorPars={CompAreasD[[1]], DiameterD[[1]], NcompD};
    NonDimPars=CalcNonDimPars[massflowOnD,cyclePNTs,MOnD,CompressorPars,\[Eta]t,\[Eta]n,\[Gamma]air,\[Gamma]gas,Rair,Rgas];

    {PratioRamOnD,AreaRamOnD,mredCompOnD,NredCOnD,PratioCompOnD,TratioCOnD,\[CapitalDelta]T0CompOnD,PratioBurnerOnD,TratioBurnerOnD,TratioMaxOnD,mredTurbOnD,NredTurbOnD,PratioTurbOnD,TratioTurbOnD,\[CapitalDelta]T0TurbOnD,AreaTurbOnD,PratioNozzleOnD,AreaNozzleOnD} = NonDimPars;

	If[report,
		Print[Style["On Design Parameters",12,Bold,Blue]];
		Print["mredCompOnD :",mredCompOnD];
		Print["PratioCOnD :",PratioCompOnD];
		Print["PratioCombOnD :",PratioBurnerOnD];
        Print["TRatioMaxOnD :", TratioMaxOnD];
		Print["PratioTOnD :",PratioTurbOnD];
		Print["mredTOnD :",mredTurbOnD];
		Print["PratioNOnD :",PratioNozzleOnD];
		Print["NredTOnD :",NredTurbOnD];
		Print["AreaTurbOnD :",AreaTurbOnD];
		Print["AreaNozzleOnD :",AreaNozzleOnD];
		Print["AreaIntakeOnD :",AreaRamOnD];
        Print["AreaCompOnD :",CompAreasD[[1]]];

		
	];

    (*Generating Compressor Map*)
    nrng=mapResolution;nmflow=mapResolution;
    dump=True;
    mapPlot=Quiet[multistageCompressorMap[massflowOnD, NcompD, nrng,nmflow,T02OnD,p02OnD, phiD,Rgas,\[Gamma]air,\[Alpha]1D,\[Beta]2D,CompAreasD,DiameterD,etaStage,mapResolution,dump]];

    (*Generating Other Maps -- TBD*)

    designPars={MOnD,NredCOnD,CompAreasD[[1]],DiameterD[[1]],PratioBurnerOnD,m3m1,AreaTurbOnD,AreaNozzleOnD,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]mc,\[Gamma]gas,\[Gamma]gas,\[Gamma]air,Rgas,z};
	varsOnD={AreaRamOnD,mredCompOnD,TratioMaxOnD,NredTurbOnD,PratioTurbOnD,PratioNozzleOnD};

    Return[{designPars,varsOnD,mapPlot}];

	
]


(* ::Section::Closed:: *)
(*Off-Design exploration *)


(* ::Subsection::Closed:: *)
(*Equilibrium Point Calculation*)


(* ::Text:: *)
(*Module that tests the design equilibrium point*)


testOffDesignEquiPoint[designPars_,varsOnD_,print_]:=Module[
{eqns,
MOnD,alt,NredComp,areaComp,diameter,p03p02,m3m1,areaTurb,areaNozzle,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]m,\[Gamma]t,\[Gamma]n=\[Gamma]t,\[Gamma]c,Rgas,
Mach,nredC,
nredT0,T03T010,mredC0,areaIntake0,pratioTurb0,pratioNozzle0,
vars,
test,
fac,varsSecant,
Pars,
equiState,
T01,p01,
AreaRam,mredComp,TratioMax,NredTurb,PratioTurb,PratioNozzle,
systemSol,equipoint,PratioComp,RPM,mflow,\[CapitalDelta]T012,T03,\[CapitalDelta]T034,Wcomp,Wturb,relerr,
OffDesignPars},

{MOnD,NredComp,areaComp,diameter,p03p02,m3m1,areaTurb,areaNozzle,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]m,\[Gamma]t,\[Gamma]n,\[Gamma]c,Rgas,alt}=designPars;
Pars=Drop[designPars,-1];
{areaIntake0,mredC0,T03T010,nredT0,pratioTurb0,pratioNozzle0}=varsOnD; (*To be used as first guess for findroot, and to test the AE system*)


(*Setting variables vector*)
vars={{T03T01,T03T010},{nredT,nredT0},{mredC,mredC0},{areaIntake,areaIntake0},{pratioTurb,pratioTurb0},{pratioNozzle,pratioNozzle0}};

(*Adapting variables vector to secant method*)
fac=0.05;
varsSecant={{T03T01,(1-fac)T03T010,(1+fac)T03T010},{nredT,(1-fac)nredT0,(1+fac)nredT0},{mredC,(1-fac)mredC0,(1+fac)mredC0},{areaIntake,(1-fac)areaIntake0,(1+fac)areaIntake0},{pratioTurb,(1-fac)pratioTurb0,(1+fac)pratioTurb0},{pratioNozzle,(1-fac)pratioNozzle0,(1+fac)pratioNozzle0}};

(*Self-Test: verify that design values for vars are solution of the system*)
eqns=getEqns[Pars];
test=eqns/.{areaIntake->areaIntake0,mredC->mredC0,T03T01->T03T010,nredT->nredT0,pratioTurb->pratioTurb0,pratioNozzle->pratioNozzle0};

Do[
relerr=(Abs[test[[i]]]);
Print[ToString[Subscript["f",ToString[i]],TraditionalForm]<>"[\!\(\*SubscriptBox[\(x\), \(DES\)]\)] = "<>ToString[relerr, TraditionalForm]];
If[relerr>10^(-3),Print["SelfTest NOT passed",test];Abort[]]
,{i,1,6}
];
If[print,Print["SelfTest passed"]];


(*Getting system of algebraic equations*)
eqns=getEqns[Pars];

(*Solving system of algebraic equations (6 by 6)*)
equiState=solveAEsys[eqns,vars,False];

Print[equiState];

(*Solution*)
mredComp=mredC/.equiState;                    
TratioMax=T03T01/.equiState;        
NredTurb=nredT/.equiState;           
AreaRam=areaIntake/.equiState;    
PratioTurb=pratioTurb/.equiState;    
PratioNozzle=pratioNozzle/.equiState;

(*Computing other observables*)
T01=Tstd[alt]*(1+0.2 (MOnD)^2);
p01=pstd[alt]*(1+0.2 (MOnD)^2)^(\[Gamma]c/(\[Gamma]c-1));
PratioComp=PRatioCompMS[mredComp/areaComp,NredComp]; 
RPM=NredComp Sqrt[Rgas T01]/diameter;           
equipoint={mredComp,PratioComp};  
mflow=mredComp p01/Sqrt[Rgas T01];
\[CapitalDelta]T012=T01/\[Eta]c (PratioComp^((\[Gamma]c-1)/\[Gamma]c)-1);
T03=T01 TratioMax;
\[CapitalDelta]T034=\[Eta]t T03 (1-(1/\!\(TraditionalForm\`PratioTurb\))^((\[Gamma]t-1)/\[Gamma]t));
Wcomp=(mflow m3m1)/\[Eta]m cp[\[Gamma]c,Rgas] \[CapitalDelta]T012/1000.; 
Wturb=mflow cp[\[Gamma]t,Rgas] \[CapitalDelta]T034/1000. ;    

(*Printing results, if requested*)
If[print,
Print["Equilibrium Point              :",equipoint];
Print["Compressor Reduced MFlow       :",mredComp];
Print["Compressor Reduced Shaft Speed :",NredComp];
Print["Compression Ratio              :",PratioComp];
Print["RPM                            :",RPM];
Print["Maximum Temperature Ratio      :",TratioMax];
Print["Turbine Reduced Shaft Speed    :",NredTurb];
Print["Intake Capture Area            :",AreaRam];
Print["Turbine Expansion Ratio        :",PratioTurb];
Print["Nozzle Expansion Ratio         :",PratioNozzle];
Print["Critical Expansion Ratio       :",1/PratioCrit[\[Gamma]n]];
Print["Wcomp [KW]                     :",Wcomp];
Print["Wturb [KW]                     :",Wturb];
];

(*Setting output vector*)
systemSol={AreaRam,mredComp,TratioMax,NredTurb,PratioTurb,PratioNozzle};
OffDesignPars={systemSol,equipoint,NredComp,PratioComp,RPM,mflow,\[CapitalDelta]T012,T03,\[CapitalDelta]T034,Wcomp,Wturb};

Return[OffDesignPars];

]



(* ::Text:: *)
(*Module that computes an off-design equilibrium point*)


calcOffDesignEquiPoint[designPars_,offDesignVars_,firstGuess_,print_]:=Module[
{eqns,
MOnD,alt,NredComp,areaComp,diameter,p03p02,m3m1,areaTurb,areaNozzle,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]m,\[Gamma]t,\[Gamma]n,\[Gamma]c,Rgas,
Mach,nredC,
nredT0,T03T010,mredC0,areaIntake0,pratioTurb0,pratioNozzle0,
vars,
test,
fac,varsSecant,
Pars,
equiState,
AreaRam,mredComp,TratioMax,NredTurb,PratioTurb,PratioNozzle,
systemSol,equipoint,T01,p01,PratioComp,RPM,mflow,\[CapitalDelta]T012,T03,\[CapitalDelta]T034,Wcomp,Wturb,
OffDesignPars},

{MOnD,NredComp,areaComp,diameter,p03p02,m3m1,areaTurb,areaNozzle,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]m,\[Gamma]t,\[Gamma]n,\[Gamma]c,Rgas}=Drop[designPars,-1];
{Mach,nredC,alt}=offDesignVars;
{areaIntake0,mredC0,T03T010,nredT0,pratioTurb0,pratioNozzle0}=firstGuess; (*To be used as first guess for findroot, and to test the AE system*)


(*Setting variables vector*)
vars={{T03T01,T03T010},{nredT,nredT0},{mredC,mredC0},{areaIntake,areaIntake0},{pratioTurb,pratioTurb0},{pratioNozzle,pratioNozzle0}};

(*Adapting variables vector to secant method*)
fac=0.05;
varsSecant={{T03T01,(1-fac)T03T010,(1+fac)T03T010},{nredT,(1-fac)nredT0,(1+fac)nredT0},{mredC,(1-fac)mredC0,(1+fac)mredC0},{areaIntake,(1-fac)areaIntake0,(1+fac)areaIntake0},{pratioTurb,(1-fac)pratioTurb0,(1+fac)pratioTurb0},{pratioNozzle,(1-fac)pratioNozzle0,(1+fac)pratioNozzle0}};

(*Substituting off-design values for {Mach,nredC} in the parametrs vector *)
Pars={Mach,nredC,areaComp,diameter,p03p02,m3m1,areaTurb,areaNozzle,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]m,\[Gamma]t,\[Gamma]n,\[Gamma]c,Rgas};

(*Getting system of algebraic equations*)
eqns=getEqns[Pars];

(*Solving system of algebraic equations (6 by 6)*)
equiState=solveAEsys[eqns,vars,False];

If[print,Print[equiState]];

(*Solution*)
T01=Tstd[alt]*(1+0.2 (Mach)^2);
p01=pstd[alt]*(1+0.2 (Mach)^2)^(\[Gamma]c/(\[Gamma]c-1));
mredComp=mredC/.equiState;                    
TratioMax=T03T01/.equiState;        
NredTurb=nredT/.equiState;           
AreaRam=areaIntake/.equiState;    
PratioTurb=pratioTurb/.equiState;    
PratioNozzle=pratioNozzle/.equiState;

(*Computing other observables*)
PratioComp=PRatioCompMS[mredComp/areaComp,nredC]; 
RPM=nredC Sqrt[Rgas T01]/diameter;           
equipoint={mredComp,PratioComp};  
mflow=mredComp p01/Sqrt[Rgas T01];
\[CapitalDelta]T012=T01/\[Eta]c (PratioComp^((\[Gamma]c-1)/\[Gamma]c)-1);
T03=T01 TratioMax;
\[CapitalDelta]T034=\[Eta]t T03 (1-(1/\!\(TraditionalForm\`PratioTurb\))^((\[Gamma]t-1)/\[Gamma]t));
Wcomp=(mflow m3m1)/\[Eta]m cp[\[Gamma]c,Rgas] \[CapitalDelta]T012/1000.; 
Wturb=mflow cp[\[Gamma]t,Rgas] \[CapitalDelta]T034/1000. ;    

(*Printing results, if requested*)
If[print,
Print["Equilibrium Point              :",equipoint];
Print["Compressor Reduced MFlow       :",mredComp];
Print["Compressor Reduced Shaft Speed :",nredC];
Print["Compression Ratio              :",PratioComp];
Print["RPM                            :",RPM];
Print["Maximum Temperature Ratio      :",TratioMax];
Print["Turbine Reduced Shaft Speed    :",NredTurb];
Print["Intake Capture Area            :",AreaRam];
Print["Turbine Expansion Ratio        :",PratioTurb];
Print["Nozzle Expansion Ratio         :",PratioNozzle];
Print["Critical Expansion Ratio       :",1/PratioCrit[\[Gamma]n]];
Print["Wcomp [KW]                     :",Wcomp];
Print["Wturb [KW]                     :",Wturb];
];

(*Setting output vector*)
systemSol={AreaRam,mredComp,TratioMax,NredTurb,PratioTurb,PratioNozzle};
OffDesignPars={systemSol,equipoint,nredC,PratioComp,RPM,mflow,\[CapitalDelta]T012,T03,\[CapitalDelta]T034,Wcomp,Wturb};

Return[OffDesignPars];

]



(* ::Subsection::Closed:: *)
(*System of Equations, solver and utilities*)


(* ::Subsubsection::Closed:: *)
(*Equations definition*)


getEqns[Pars_]:=Module[
{Mach,nredC,areaComp,diameter,p03p02,m3m1,areaTurb,areaNozzle,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]m,\[Gamma]t,\[Gamma]n,\[Gamma]c,Rgas,
eqns},

{Mach,nredC,areaComp,diameter,p03p02,m3m1,areaTurb,areaNozzle,\[Eta]t,\[Eta]c,\[Eta]n,\[Eta]m,\[Gamma]t,\[Gamma]n,\[Gamma]c,Rgas}=Pars;
eqns={};

(*Import Multi Stage Compressor map*)
PRatioCompMS=<<"PRatioCompMS.m";
(*Print["Compressor Map import: Successful"];*)

(*Note that the variable mredC is defined without Area, while the compressor performance map has been built with the reduced massflow with Area*)

(* Compressor/Intake flow compatibility; it defines the capture area in off-design *)  
(*AppendTo[eqns,mredIntake[areaIntake,Mach,\[Gamma]c]-mredC  \[Epsilon][Mach](1+(\[Gamma]c-1)/2 Mach^2)^(\[Gamma]c/(\[Gamma]c-1))];*)
AppendTo[eqns,mredIntake[areaIntake,Mach,\[Gamma]c]-mredC  \[Epsilon][Mach]];(*2/6/2024*)

(* Compressor/Turbine flow compatibility*)
AppendTo[eqns,mredTurbine[1/pratioTurb,\[Eta]t,areaTurb,\[Gamma]t]-(  (mredC Sqrt[T03T01] m3m1)/(PRatioCompMS[mredC/areaComp,nredC] p03p02)  )];  

(* Turbine/Nozzle flow compatibility; *)
AppendTo[eqns,mredTurbine[1/pratioNozzle,\[Eta]n,areaNozzle,\[Gamma]n]-mredTurbine[1/pratioTurb,\[Eta]t,areaTurb,\[Gamma]t]\!\(TraditionalForm\`pratioTurb\) Sqrt[1-\[Eta]t(1-(1/pratioTurb)^((\[Gamma]t-1)/\[Gamma]t))]];

(* Compressor/Turbine Power balance*)   
AppendTo[eqns, m3m1  \[Eta]t T03T01  (1-(1/pratioTurb)^((\[Gamma]t-1)/\[Gamma]t)) - (1 /\[Eta]m  1/\[Eta]c cp[\[Gamma]c,Rgas]/cp[\[Gamma]t,Rgas] (PRatioCompMS[mredC/areaComp,nredC]^((\[Gamma]c-1)/\[Gamma]c)-1) ) ];

(* Reduced RPM compatibility*)   
AppendTo[eqns,nredT -( nredC Sqrt[1/T03T01])];

(* Pressure ratios compatibility:   Subscript[\[CapitalPi], d]Subscript[\[CapitalPi], C]Subscript[\[CapitalPi], CC] = Subscript[\[CapitalPi], T1] Min[Subscript[\[CapitalPi], N],Subsuperscript[\[CapitalPi], N, *]]  *)  
AppendTo[eqns,pratioNozzle-Evaluate[Min[1/PratioCrit[\[Gamma]n],1/pratioTurb p03p02 PRatioCompMS[mredC/areaComp,nredC] (1+(\[Gamma]c-1)/2 Mach^2)^(\[Gamma]c/(\[Gamma]c-1))]]];

Return[eqns];

]


printEqns[Pars_]:=Module[{eqns},

eqns=getEqns[Pars]/.{areaIntake->Global`Adiffuser,mredC->Global`mRedComp,T03T01->Global`tauMax,nredT->Global`NRedTurb,pratioTurb->Global`\[CapitalPi]Turb,pratioNozzle->Global`\[CapitalPi]Nozzle};
Print[eqns//TableForm];

]


(* ::Subsubsection::Closed:: *)
(*Solver*)


(* ::Text:: *)
(*Solver, with secant method (requires variables with two initial conditions) *)


solveAEsys[eqns_,varsSecant_,debug_]:=Module[
{s=0,e=0,
sol,equiState},

If[debug,Print["Solving algebraic system..."]];

If[debug,Print[eqns]];
If[debug,Print[varsSecant]];

equiState=FindRoot[eqns,varsSecant,Method->"Newton",DampingFactor->0.5,StepMonitor:>s++,EvaluationMonitor:>e++,MaxIterations->10000];
Print["Steps"->s;"Evaluations"->e];

Return[equiState];

]


(* ::Subsubsection::Closed:: *)
(*List of States Generator*)


(* ::Text:: *)
(*Module to Generate Off-Design List of States*)


MakeOffDesignStates[n_,xmin_,xmax_,ymin_,ymax_]:=Module[
{npnts,pnts,i,j,k,listStates},
pnts={};
For[i=2,i\[LessSlantEqual]n+1,
(*Print["Sum i :",i];*)
If[Mod[i,2]!=0,
For[j=1,j\[LessSlantEqual]i-1,
AppendTo[pnts,{j,i-j}];
(*Print[{j,i-j}];*)
j++];
,
For[j=i-1,j\[GreaterSlantEqual]1,
AppendTo[pnts,{j,i-j}];
(*Print[{j,i-j}];*)
j--];
];
i++];
k=0;
For[i=n+2,i\[LessSlantEqual]2 n,
(*Print["Sum i :",i];*)
k=k+1;
If[Mod[i,2]!=0,
For[j=k+1,j\[LessSlantEqual]i-1-k,
AppendTo[pnts,{j,i-j}];
(*Print[{j,i-j}];*)
j++];
,
For[j=i-1-k,j\[GreaterSlantEqual]k+1,
AppendTo[pnts,{j,i-j}];
(*Print[{j,i-j}];*)
j--];
];
i++];
npnts=Dimensions[pnts][[1]];
listStates=Table[{xmax-(pnts[[i,1]]-1)((xmax-xmin)/(n-1)),ymax-(pnts[[i,2]]-1)((ymax-ymin)/(n-1))},{i,1,npnts}];
listStates];


(* ::Subsubsection:: *)
(*Plot utilities*)


(* ::Text:: *)
(*Module to plot Off-Design List of States*)


plotOffDesignPerfo[perfo_,listorderedpoints_,ipar_,name_]:=Module[{mach,nred,n,orderingMach,orderingNred,pointsMach,pointsNred,plotM,plotN,plot},
mach = perfo[[All,1]];
nred=perfo[[All,10]];
n=Sqrt[Length[mach]];
orderingMach = Ordering[listorderedpoints[[All,1]]];
orderingNred = Ordering[listorderedpoints[[All,2]]];
pointsMach=Table[{mach[[i]],perfo[[i,ipar]]},{i,1,n^2}];
pointsNred=Table[{nred[[i]],perfo[[i,ipar]]},{i,1,n^2}];
plotM=ListLinePlot[Table[pointsMach[[orderingMach]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"Mach",name},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plotN=ListLinePlot[Table[pointsNred[[orderingNred]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"RPM",name},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plot=GraphicsRow[{plotM,plotN},ImageSize->1000];
Return[plot]
]


plotOffDesignCycle[perfo_,cyclePars_,listorderedpoints_]:=Module[{n,samples,color,allPlots1,allPlots2,PointsTJ,energyTJ,performanceTJ,allPointsTJ,allperformanceTJ,ioffd,plCore,plIsobaraAmb,plIsobaraTop,alt,\[Gamma]air,Rair,\[Eta]d,\[Eta]mc,\[Eta]c,Qf,\[Gamma]t,Rgas,\[Eta]b,\[Eta]pb,\[Eta]t,\[Eta]mt,\[Eta]n,mach,nred,orderingMach,orderingNred,pointsMach,pointsNred,plotM,plotN,plotThrust,plotFC,plotUexit,plotcycle1,plotcycle2},
{alt,\[Gamma]air,Rair,\[Eta]d,\[Eta]mc,\[Eta]c,Qf,\[Gamma]t,Rgas,\[Eta]b,\[Eta]pb,\[Eta]t,\[Eta]mt,\[Eta]n}=cyclePars;
n=Sqrt[Length[perfo]];

color=ColorData["LightTemperatureMap"]/@(Range[0,n]/n);

allPointsTJ={};
allperformanceTJ={};
Do[
{PointsTJ,energyTJ,performanceTJ}=CalcTurbojetCycle[perfo[[i,1]],alt,\[Gamma]air,Rair,\[Eta]d,\[Eta]mc,\[Eta]c,perfo[[i,9]],Qf,\[Gamma]t,Rgas,\[Eta]b,\[Eta]pb,perfo[[i,13]],\[Eta]t,\[Eta]mt,\[Eta]n,False];
AppendTo[allPointsTJ,PointsTJ];
AppendTo[allperformanceTJ,performanceTJ],
{i,1,Length[perfo]}];

mach=perfo[[All,1]];
nred=perfo[[All,10]];
orderingMach = Ordering[listorderedpoints[[All,1]]];
orderingNred = Ordering[listorderedpoints[[All,2]]];
pointsMach=Table[{mach[[i]],allperformanceTJ[[i,1]]*perfo[[i,11]]},{i,1,n^2}];
pointsNred=Table[{nred[[i]],allperformanceTJ[[i,1]]*perfo[[i,11]]},{i,1,n^2}];
plotM=ListLinePlot[Table[pointsMach[[orderingMach]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"Mach","Thrust [N]"},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plotN=ListLinePlot[Table[pointsNred[[orderingNred]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"RPM","Thrust [N]"},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plotThrust=GraphicsRow[{plotM,plotN},ImageSize->1000];

pointsMach=Table[{mach[[i]],allperformanceTJ[[i,2]]*allperformanceTJ[[i,1]]*perfo[[i,11]]},{i,1,n^2}];
pointsNred=Table[{nred[[i]],allperformanceTJ[[i,2]]*allperformanceTJ[[i,1]]*perfo[[i,11]]},{i,1,n^2}];
plotM=ListLinePlot[Table[pointsMach[[orderingMach]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"Mach","Fuel Consumption [Kg/h]"},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plotN=ListLinePlot[Table[pointsNred[[orderingNred]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"RPM","Fuel Consumption [Kg/h]"},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plotFC=GraphicsRow[{plotM,plotN},ImageSize->1000];

pointsMach=Table[{mach[[i]],allperformanceTJ[[i,6]]},{i,1,n^2}];
pointsNred=Table[{nred[[i]],allperformanceTJ[[i,6]]},{i,1,n^2}];
plotM=ListLinePlot[Table[pointsMach[[orderingMach]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"Mach","nozzle exit V [m/s]"},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plotN=ListLinePlot[Table[pointsNred[[orderingNred]][[(i*n)+1;;(i+1)*n]],{i,0,n-1}],PlotRange->{All,All},Frame->True,FrameLabel->{"RPM","nozzle exit V [m/s]"},PlotTheme->"Business",PlotStyle->ColorData["LightTemperatureMap"]/@(Range[0,n]/n)];
plotUexit=GraphicsRow[{plotM,plotN},ImageSize->1000];

(*Plot cycles at highest Mach number*)
allPlots1={};
samples = Ordering[listorderedpoints[[All,2]]][[-n;;]];
Do[
PointsTJ=allPointsTJ[[samples[[i]]]];
If[i==1,
plIsobaraAmb=Plot[{PointsTJ[[1,2]]*Exp[(S-PointsTJ[[1,4]])/cp[\[Gamma]t, Rgas]]},{S,0,PointsTJ[[6,4]]},PlotStyle->{Black,Dashed}]
];
plCore=ListPlot[Table[{PointsTJ[[i,4]],PointsTJ[[i,2]]},{i,1,Length[PointsTJ]}],Joined->True,PlotMarkers->{Automatic, Small},AspectRatio->1,ImageSize->700,Frame->True,FrameLabel->{"s","T"},PlotStyle->color[[-i]]];
AppendTo[allPlots1,plCore],
{i,1,n}];
(*plIsobaraTop=Plot[{PointsTJ[[3,2]]*Exp[(S-PointsTJ[[3,4]])/cp[\[Gamma]t, Rgas]]},{S,0,PointsTJ[[6,4]]},PlotStyle->{Black,Dashed}];*)
plotcycle1=Show[{allPlots1,plIsobaraAmb},PlotRange->{All,All}];

(*Plot cycles at lowest Mach number*)
allPlots2={};
samples = Ordering[listorderedpoints[[All,2]]][[;;n]];
Do[
PointsTJ=allPointsTJ[[samples[[i]]]];
If[i==1,
plIsobaraAmb=Plot[{PointsTJ[[1,2]]*Exp[(S-PointsTJ[[1,4]])/cp[\[Gamma]t, Rgas]]},{S,0,PointsTJ[[6,4]]},PlotStyle->{Black,Dashed}]
];
plCore=ListPlot[Table[{PointsTJ[[i,4]],PointsTJ[[i,2]]},{i,1,Length[PointsTJ]}],Joined->True,PlotMarkers->{Automatic, Small},AspectRatio->1,ImageSize->700,Frame->True,FrameLabel->{"s","T"},PlotStyle->color[[-i]]];
AppendTo[allPlots2,plCore],
{i,1,n}];
(*plIsobaraTop=Plot[{PointsTJ[[3,2]]*Exp[(S-PointsTJ[[3,4]])/cp[\[Gamma]t, Rgas]]},{S,0,PointsTJ[[6,4]]},PlotStyle->{Black,Dashed}];*)
plotcycle2=Show[{allPlots2,plIsobaraAmb},PlotRange->{All,All}];

Return[{plotThrust,plotFC,plotUexit,plotcycle1,plotcycle2}]
]


End[]
EndPackage[]

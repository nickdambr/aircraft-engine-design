(* ::Package:: *)

(* ::Title:: *)
(*Progetto Motori Aeronautici*)
(*Niccol\[OGrave] D'Ambrosio (2001431), Francesco Daniele (2008421), Elisa Jacopucci (1987922), Matteo Grippo ()*)
(**)
(**)


(* ::Input:: *)
(**)
(**)


(* ::Text:: *)
(**)
(*Real Data (Eurofighter Typhoon , EJ200 Mk.100)*)
(**)
(*Wing span = 10.95 m*)
(*Wing area = 51.2 m^2*)
(*AR = 2.34*)
(*Length = 15.96 m*)
(*Height = 5.28 m*)
(*Empty weight = 11000 kg*)
(*Gross weight = 16000 kg*)
(*MTOW = 23500 Kg*)
(*Max payload = 6500 kg*)
(*SuperCruise Mach = 1.1*)
(*Max Mach / Max speed @ altitude = 2.0 / 2495 km/h*)
(*Max Mach / Max speed @ sea level = 1.25 / 1530 km/h*)
(*Service ceiling = 16764 m = 55000 ft*)
(*Initial climb velocity : 255 m/s (climb at 11.000 m in circa 150 sec)*)
(*G limit : +9 / -3 G*)
(*Instantaneous turn rate (ITR) : circa 30 gradi/sec*)
(*Supported turn rate (STR) : circa 20 gradi/sec*)
(*Range = 2900 Km*)
(*Seats = 1*)
(*TO time < 8 s*)
(**)
(*Engine Design*)
(**)
(*Twin-spool TurboFan with Afterburner*)
(*3 stages LPC*)
(*5 stages HPC*)
(*1 stage HPT*)
(*1 stages LPT*)
(*Inlet diameter = 0.74 m*)
(*Fan diameter = 0.74 m*)
(*Overall Length = 4 m*)
(*Weight = 1037 kg*)
(**)
(*Engine performance*)
(**)
(*Thrust @ TO without reheat = 20000 lbf = 90 kN*)
(*Thrust @ TO with reheat = 13500 lbf = 60 kN*)
(*T/W = 10*)
(*Air massflow @ TO =  75/77 kg/s *)
(*Specific thrust @ TO = ? m/s*)
(*OPR = 26*)
(*BPR = 0.4*)
(**)
(*Thrust @ CRZ =  lbf = ? N*)
(*TSFC @ CRZ = ? Kg / N hr*)
(*Estimated(*) air massflow @ CRZ \[TildeEqual] ? Kg/s *)
(*Estimated(*) Specific thrust @ CRZ \[TildeEqual] ? m/s*)
(**)
(*(*) Computed as \[Rho](h) Subscript[M, Fan] Sqrt[\[Gamma]RT] Subscript[A, Fan], assuming Subscript[M, Fan]=0.7*)


(* ::Text:: *)
(**)


(* ::Chapter:: *)
(*Initialization*)


(* ::Input:: *)
(*Clear["Global`*"]*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]] (*setta il percorso della directory e le librerie*)*)


(* ::Input:: *)
(*"C:\\Users\\Elisa\\OneDrive\\Documenti\\GitHub\\LDG_MOTORI"*)


(* ::Input:: *)
(*$ContextPath*)


(* ::Chapter:: *)
(*Constraint Analysis*)


(* ::Subsection:: *)
(*Aerodynamics: Subscript[C, Subscript[D, 0]], Subscript[k, 1],Subscript[k, 2] (Polar)*)


(* ::Text:: *)
(*Fighter aircraft Subscript[C, Subscript[D, 0]]vs Mach*)


(* ::Input:: *)
(*Plot[cd0[False,True,M],{M,0,2},Frame->True,*)
(*FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"]]*)


(* ::Subsection:: *)
(*Propulsion: Thrust Lapse \[Alpha]*)


(* ::Text:: *)
(*How (full) thrust varies with altitude and Mach number. *)


(* ::Text:: *)
(*T=\[Alpha] Subscript[T, SLS]*)


(* ::Subsection:: *)
(*Low bypass ratio Turbofan*)


(* ::Subsubsection:: *)
(*Max Power*)


(* ::Input:: *)
(*TR=1.1;*)
(*Plot[{\[Alpha]LTFmax[0,M0,TR],\[Alpha]LTFmax[10000,M0,TR]},{M0,0,2},PlotRange->{All,{0,1.4}},AxesLabel->{"M0","\[Alpha]"},PlotLegends->{"Sea Level","10000 m"},Frame->True,*)
(*FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"]]*)


(* ::Subsubsection:: *)
(*Military Power*)


(* ::Input:: *)
(*Plot[{\[Alpha]LTFmil[0,M0,TR],\[Alpha]LTFmil[10000,M0,TR]},{M0,0,2},PlotRange->{All,{0,1.4}},AxesLabel->{"M0","\[Alpha]"},PlotLegends->{"Sea Level","10000 m"},Frame->True,*)
(*FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"]]*)


(* ::Subsection:: *)
(*Master Equation*)


(* ::Input:: *)
(*RHS=\[Beta]/\[Alpha] (q /(\[Beta] WLG) (K1 ((n \[Beta])/q  WLG)^2+K2((n \[Beta])/q  WLG)+CD0+CDR)+Ps/V);*)
(*TW==RHS*)


(* ::Input:: *)
(*a=(1.4*287*Tstd[10000])^0.5*)


(* ::Subsection:: *)
(*Flight Phases*)


(* ::Subsubsection:: *)
(*Aircraft Settings*)


(* ::Input:: *)
(*commercial=False;*)
(*If[commercial,fighter=False,fighter=True];*)
(**)
(*AR=2.4;*)
(*e=0.8;*)
(*TR=1.1;*)


(* ::Subsubsection:: *)
(*Mission phases 1-2: Takeoff, no obstacle*)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Subsubsection:: *)
(*Mission phases 6-7: Supersonic penetration *)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Subsubsection:: *)
(*Mission phases 7-8: Combat turn 1*)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Subsubsection:: *)
(*Mission phases 7-8: Combat turn 2*)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Subsubsection:: *)
(*Mission phases 7-8: Horizontal acceleration*)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Subsubsection:: *)
(*Mission phases 8-9: Escape dash*)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(*Mission phases 13-14: Landing, no reverse thrust *)


(* ::Text:: *)
(*dh/dt=0*)
(*Subscript[s, L] assigned*)
(*Subscript[C, Subscript[L, max]]assigned *)
(*\[Rho] assigned*)
(*Subscript[V, TD]=Subscript[k, TD]Subscript[V, Stall]*)


(* ::Input:: *)
(*hfield=362.;*)
(*\[Gamma]=1.4;*)
(*R=287.;*)
(*Vstall= 46.17;*)
(*Ml = kTD*Vstall/Sqrt[\[Gamma] R Tstd[hfield]];*)
(*CL = 0.8*CLmax/kTD^2;*)
(*CDRc = 0.5348;*)
(*CD=k1[commercial,fighter,Ml,AR,e]*CL^2+k2[commercial,fighter]*CL+cd0[commercial,fighter,Ml];*)
(*rulel={kTD->1.15,*)
(*tfr->3.,*)
(*sL->700.,*)
(*\[Mu]B-> 0.18,*)
(*\[Xi]L -> CD+CDRc-\[Mu]B*CL,*)
(*CLmax->2.,*)
(*\[Beta]->0.56,*)
(*\[Alpha]->0.,*)
(*g0->9.81};*)
(*rulel//TableForm*)


(* ::Input:: *)
(*rulelcolddensity=\[Rho]->\[Rho]std[hfield,-15.]*)
(*rulelhotdensity=\[Rho]->\[Rho]std[hfield,+30]*)


(* ::Input:: *)
(*WLG=((-b+Sqrt[b^2+4a*c])/(2a))^2//.rulelhotdensity//.rulel;*)
(*b = tfr*kTD *Sqrt[2\[Beta]/(\[Rho]* CLmax)]//.rulelhotdensity//.rulel;*)
(**)
(*a = \[Beta]/(\[Rho]*g0*\[Xi]L)*Log[1+\[Xi]L/(\[Mu]B*CLmax/kTD^2)]//.rulelhotdensity//.rulel;*)
(*c = sL//.rulelhotdensity//.rulel;*)


(* ::Input:: *)
(*WLG*)


(* ::Input:: *)
(*pluto=ContourPlot[x==5035.59,{x,0,8000},{y,0,8000},ContourStyle->{Blue,Thick},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Input:: *)
(*pippo=RegionPlot[x>5035.59,{x,0,8000},{y,0,8000},PlotStyle->LightBlue,BoundaryStyle->None]*)
(**)


(* ::Input:: *)
(*paperino=Show[pippo,pluto,Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"]]*)


(* ::Subsubsection:: *)
(*Maximum Mach number @ altitude*)


(* ::Text:: *)
(*dh/dt=0*)
(*dV/dt=0*)
(*n=1 (L=W)*)
(*q assigned*)


(* ::Input:: *)
(*hmma=12192;*)
(*p0mma=pstd[hmma];*)
(*M0mma=2.;*)
(*\[Beta]mma=0.78;*)
(**)
(*rulemma={Ps->0,*)
(*n->1,*)
(*q->0.5 \[Gamma] p0mma M0mma^2,*)
(*M0->M0mma,*)
(*h->hmma,*)
(*\[Beta]->\[Beta]mma,*)
(*\[Alpha]->\[Alpha]LTFmax[hmma,M0mma,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,M0mma,AR,e],K2->k2[commercial,fighter],CD0->cd0[commercial,fighter,M0mma],\[Gamma]->1.4};*)
(*rulemma//TableForm*)


(* ::Input:: *)
(*TWmma=RHS//.rulemma;*)
(*plotmma=Plot[TWmma,{WLG,4000,8000},Filling->Bottom,PlotRange->{All,{0.,1.34}},Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"]]*)


(* ::Input:: *)
(*TWmma*)


(* ::Subsubsection:: *)
(*Maximum Mach number @ sea level*)


(* ::Text:: *)
(*dh/dt=0*)
(*dV/dt=0*)
(*n=1 (L=W)*)
(*q assigned*)


(* ::Input:: *)
(*hmms=0.;*)
(*p0mms=pstd[hmms];*)
(*M0mms=1.25;*)
(*\[Beta]mms=0.78;*)
(**)
(*rulemms={Ps->0,*)
(*n->1,*)
(*q->0.5 \[Gamma] p0mms M0mms^2,*)
(*M0->M0mms,*)
(*h->hmms,*)
(*\[Beta]->\[Beta]mms,*)
(*\[Alpha]->\[Alpha]LTFmax[hmms,M0mms,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,M0mma,AR,e],K2->k2[commercial,fighter],CD0->cd0[commercial,fighter,M0mma],\[Gamma]->1.4};*)
(*rulemms//TableForm*)


(* ::Input:: *)
(*TWmms=RHS//.rulemms;*)
(*plotmms=Plot[TWmms,{WLG,0,8000},Filling->Bottom,PlotRange->{All,{0.,1.34}},Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"]]*)


(* ::Input:: *)
(*TWmms*)


(* ::Subsubsection:: *)
(*Supercruise @ altitude *)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Subsubsection:: *)
(*Acceleration @ sea level*)
(**)
(**)
(**)
(**)
(**)
(**)
(**)
(**)


(* ::Subsubsection:: *)
(*Service Ceiling*)


(* ::Text:: *)
(*The maximum altitude at which the aircraft can cruise still having 100ft/min of remaining climb rate. The aircraft has to fly at its maximum efficiency attitude (Subscript[C, L]= Sqrt[Subscript[C, Subscript[D, 0]]\[Pi] AR e]) and full thrust*)


(* ::Text:: *)
(*dV/dt=0*)
(*n=1 (L=W)*)
(*Subscript[C, L]assigned -> maximum efficiency Subscript[C, L]*)
(*h assigned*)
(*dh/dt assigned -> 0.5 m/s = 100 ft/min*)


(* ::Input:: *)
(*ht=0.5;*)
(*hsc=16764;*)
(*p0sc=pstd[hsc];*)
(*\[Beta]sc=0.78;*)
(*M0Crit=0.8;*)
(*CLe=Sqrt[cd0[commercial,fighter,M0Crit]*Pi AR e];*)
(**)


(* ::Input:: *)
(*rulesc={Ps->ht,*)
(*n->1,*)
(*q->0.5 \[Gamma] p0sc M0Crit^2,*)
(*h->hsc,*)
(*\[Beta]->\[Beta]sc,*)
(*\[Alpha]->\[Alpha]LTFmil[hsc,M0Crit,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,M0Crit,AR,e],*)
(*K2->k2[commercial,fighter],*)
(*CD0->cd0[commercial,fighter,M0sc],*)
(*V->Sqrt[2 \[Beta] WLG /( \[Rho]std[hsc,0] CLe)],*)
(*M0sc->V/Sqrt[\[Gamma] R Tstd[hsc]],\[Gamma]->1.4,R->287.};*)


(* ::Input:: *)
(*TWsc =RHS//.rulesc;*)


(* ::Input:: *)
(*plotsc=Plot[TWsc,{WLG,0,8000},Filling->Bottom,Frame->True,PlotRange->{All,{0.24,5}},FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"]]*)


(* ::Subsubsection:: *)
(*Case 1: Constant Altitude/Speed Cruise (Subscript[P, s]=0)*)


(* ::Text:: *)
(*dh/dt=0*)
(*dV/dt=0*)
(*n=1 (L=W)*)
(*q assigned*)


(* ::Input:: *)
(*hcrz=11000;*)
(*p0crz=pstd[hcrz];*)
(*M0crz=0.85;*)
(*\[Beta]crz=0.85;*)
(**)
(*rule1={Ps->0,*)
(*n->1,*)
(*q->0.5 \[Gamma] p0crz M0crz^2,*)
(*M0->M0crz,*)
(*h->hcrz,*)
(*\[Beta]->\[Beta]crz,*)
(*\[Alpha]->\[Alpha]LTFmax[hcrz,M0crz,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,M0crz,AR,e],K2->k2[commercial,fighter],CD0->cd0[commercial,fighter,M0crz],\[Gamma]->1.4};*)
(*rule1//TableForm*)


(* ::Input:: *)
(*TW1=RHS//.rule1;*)
(*plot1=Plot[TW1,{WLG,4000,8000},Filling->Bottom,PlotRange->{All,{0.,1.34}},Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Input:: *)
(*TW1*)


(* ::Subsubsection:: *)
(*Case 2: Constant speed climb (Subscript[P, s]=dh/dt)*)


(* ::Text:: *)
(*dV/dt=0*)
(*n\[TildeTilde]1*)
(*h assigned*)
(*dh/dt assigned*)
(*q assigned*)


(* ::Input:: *)
(*hcas2=11000;*)
(*p0cas2=pstd[hcrz];*)
(*M0cas2=0.85;*)
(*\[Beta]cas2=0.85;*)
(**)
(*rule2={Ps->dhdt,*)
(*n->1,*)
(*q->0.5 \[Gamma] p0crz M0cas2^2,*)
(*M0->M0cas2,*)
(*h->hcas2,*)
(*\[Beta]->\[Beta]cas2,*)
(*\[Alpha]->\[Alpha]TF[hcas2,M0cas2,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,M0cas2,AR,e],K2->k2[commercial,fighter],CD0->cd0[commercial,fighter,M0cas2],\[Gamma]->1.4};*)
(*rule2//TableForm*)


(* ::Input:: *)
(*TW1=RHS//.rule2;*)
(*plot1=Plot[TW1,{WLG,4000,8000},Filling->Bottom,PlotRange->{All,{0.24,0.34}},Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Subsubsection:: *)
(*Case 3: Constant speed/altitude turn (Subscript[P, s]=0)*)


(* ::Text:: *)
(*dh/dt=0*)
(*dV/dt=0*)
(*n>1*)
(*h, q assigned*)


(* ::Input:: *)
(*hTurn=11000;*)
(*p0Turn=pstd[hTurn];*)
(*\[Beta]Turn=0.85;*)
(*nTurn=1.1;*)
(*M0Turn=0.85;*)
(**)
(*rule3={Ps->0,*)
(*n->nTurn,*)
(*q->0.5 \[Gamma] p0Turn M0Turn^2,*)
(*h->hTurn,*)
(*\[Beta]->\[Beta]Turn,*)
(*\[Alpha]->\[Alpha]TF[hTurn,M0Turn,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,M0Turn,AR,e],K2->k2[commercial,fighter],CD0->cd0[commercial,fighter,M0Turn],M0->M0Turn,\[Gamma]->1.4};*)
(*rule3//TableForm*)


(* ::Input:: *)
(*TW3 =RHS//.rule3;*)
(*plot3=Plot[TW3,{WLG,4000,8000},Filling->Bottom,PlotRange->{All,{0.24,0.34}},Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Subsubsection:: *)
(*Case 4: Horizontal acceleration (Subscript[P, s]=(V/g0)(dV/dt))*)


(* ::Text:: *)
(*dh/dt=0*)
(*dV/dt assigned*)
(*n=1 (L=W)*)


(* ::Input:: *)
(*g0=9.81;*)
(*hAcc=10000;*)
(*p0Acc=pstd[hAcc];*)
(*MAcc1=0.7;*)
(*MAcc2=0.8;*)
(*Mavg=(MAcc2+MAcc1)/2;*)
(*VAcc1=MAcc1*Sqrt[\[Gamma] R Tstd[hAcc]];*)
(*VAcc2=MAcc2*Sqrt[\[Gamma] R Tstd[hAcc]];*)
(*\[CapitalDelta]T=180;*)
(*dV=(VAcc2-VAcc1)/\[CapitalDelta]T;*)
(*\[Beta]Acc=0.85;*)
(**)
(*rule4={Ps->V*dV/g0,*)
(*n->1,*)
(*q->0.5 \[Gamma] p0Acc Mavg^2,*)
(*h->hTurn,*)
(*\[Beta]->\[Beta]Acc,*)
(*\[Alpha]->\[Alpha]TF[hAcc,Mavg,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,Mavg,AR,e],K2->k2[commercial,fighter],CD0->cd0[commercial,fighter,Mavg],*)
(*M0->Mavg,\[Gamma]->1.4,R->287};*)
(*rule4//TableForm*)


(* ::Input:: *)
(*TW4 =RHS//.rule4;*)
(*plot4=Plot[TW4,{WLG,4000,8000},Filling->Bottom,Frame->True,PlotRange->{All,{0.24,1.34}},FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Subsubsection:: *)
(*Case 5: Takeoff Roll (Subscript[s, G]) when Subscript[T, SL]>>(D+R)*)


(* ::Text:: *)
(*dh/dt=0*)
(*Subscript[s, G] assigned*)
(*Subscript[C, Subscript[L, max]]assigned *)
(*\[Rho] assigned*)
(*Subscript[V, TO]=Subscript[k, TO]Subscript[V, Stall]*)


(* ::Input:: *)
(*hfield=1500;*)
(*rule5={kTO->1.2,*)
(*sG->2000,*)
(*CLmax->2.4,*)
(*\[Beta]->1,*)
(*\[Alpha]->\[Alpha]TF[hfield,0,TR],*)
(*tr->0};*)
(*rule5//TableForm*)


(* ::Input:: *)
(*rule5colddensity=\[Rho]->\[Rho]std[hfield,-15.]*)
(*rule5hotdensity=\[Rho]->\[Rho]std[hfield,+30]*)


(* ::Input:: *)
(*TW5=(\[Beta]^2/\[Alpha] kTO^2/(sG \[Rho] g0 CLmax) WLG);*)


(* ::Input:: *)
(*plot5=Plot[{TW5/.rule5hotdensity/.rule5,TW5/.rule5colddensity/.rule5},{WLG,4000,8000},Filling->Bottom,PlotStyle->{Red,Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Subsubsection:: *)
(*Case 8: Service Ceiling (Subscript[P, s]=dh/dt)*)


(* ::Text:: *)
(*The maximum altitude at which the aircraft can cruise still having 100ft/min of remaining climb rate. The aircraft has to fly at its maximum efficiency attitude (Subscript[C, L]= Sqrt[Subscript[C, Subscript[D, 0]]\[Pi] AR e]) and full thrust*)


(* ::Text:: *)
(*dV/dt=0*)
(*n=1 *)
(*Subscript[C, L]assigned -> maximum efficiency Subscript[C, L]*)
(*h assigned*)
(*dh/dtassigned -> 0.5 m/s = 100 ft/min*)


(* ::Input:: *)
(*ht=0.5;*)
(*hCeil=11500;*)
(*p0Ceil=pstd[hCeil];*)
(*\[Beta]Ceil=0.78;*)
(*M0Crit=0.8;*)
(*CLe=Sqrt[cd0[commercial,fighter,M0Crit]*Pi AR e];*)
(**)


(* ::Input:: *)
(*rule8={Ps->ht,*)
(*n->1,*)
(*q->0.5 \[Gamma] p0Ceil M0Crit^2,*)
(*h->hCeil,*)
(*\[Beta]->\[Beta]Ceil,*)
(*\[Alpha]->\[Alpha]TF[hCeil,M0Crit,TR],*)
(*CDR->0,*)
(*K1->k1[commercial,fighter,M0Crit,AR,e],*)
(*K2->k2[commercial,fighter],*)
(*CD0->cd0[commercial,fighter,M0Ceil],*)
(*V->Sqrt[2 \[Beta] WLG /( \[Rho]std[hCeil,0] CLe)],*)
(*M0Ceil->V/Sqrt[\[Gamma] R Tstd[hCeil]],\[Gamma]->1.4,R->287.};*)


(* ::Input:: *)
(*TW8 =RHS//.rule8;*)


(* ::Input:: *)
(*plot8=Plot[TW8,{WLG,4000,8000},Filling->Bottom,Frame->True,PlotRange->{All,{0.24,0.34}},FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"},PlotStyle->{Blue},PlotRange->{All,All},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Subsection:: *)
(*Constraint Diagram*)


(* ::Text:: *)
(*The solution space is the white region over the constraint curves *)


(* ::Input:: *)
(*Show[plot1,plot3,plot4,plot5,plot8,Frame->True,FrameLabel->{"\!\(\*FractionBox[\(W\), \(S\)]\) [N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)]","\!\(\*FractionBox[\(T\), \(W\)]\)"}]*)


(* ::Input:: *)
(*TWratio=0.31;*)
(*WLG0=6220.;*)
(*Print[Style["Aircraft characteristic ratios",20,Bold,Red]];*)
(*	Print[TableForm[*)
(*	{TWratio,WLG0},*)
(*	TableHeadings->{{"Take-Off-Thrust-to-MTOW ratio (T/W) ","Wing loading (W/S) "},None}*)
(*	]*)
(*];*)


(* ::Subsection:: *)
(*Constant PS contours*)


(* ::DisplayFormula:: *)
(*V (T-D)/W=(d(V^2/(2Subscript[g, 0])+h))/dt=Subscript[dz, e]/dt=Subscript[P, S]*)


(* ::Text:: *)
(*where Subscript[z, e] represents the aircraft mechanical energy (kinetic + potential) and is often referred to as "energy height". Subscript[P, S] is the time rate of change of the energy height and is called weight specific excess power.*)
(**)
(*Once T/W and W/S are selected, it is possible to compute the weight specific excess power for flight at any chosen \[Beta], n, altitude, and velocity. Fighter pilots routinely compare these Ps diagrams for their aircraft with those of their potential adversaries in order to determine where in the flight envelope they enjoy the greatest combat advantage.  CIAOOOOOOOOOOO*)


(* ::Input:: *)
(*commercial=False;*)
(*fighter=True;*)
(**)
(*Quiet[rulePS={n->1,\[Beta]->0.97,\[Alpha]->\[Alpha]TJmil[h,M0,1.0],TW->1.,WLG->4000,K1->k1[commercial,fighter,M0,AR,e],K2->k2[commercial,fighter],CD0->cd0[commercial,fighter,M0],q->0.5 \[Rho]std[h,0]V^2,M0->(V/Sqrt[\[Gamma] R Tstd[h]]),AR->8,e->0.8,\[Gamma]->1.4,R->287};]*)
(**)
(**)
(*Show[*)
(*Table[*)
(*ContourPlot[*)
(*Evaluate[V(\[Alpha]/\[Beta] TW-K1 n^2 \[Beta]/q WLG - K2 n - CD0/(\[Beta] WLG/q))==cost//.rulePS],*)
(*{V,0,600},{h,0,18000},*)
(*AspectRatio->0.5,*)
(*ContourLabels->None,*)
(*FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],*)
(*Frame->True,*)
(*FrameLabel->{"V[m/s]","h[m]"}],*)
(*{cost,0,150,10}],*)
(*Table[ContourPlot[h+V^2/(2*9.81)==c1,{V,0,600},{h,0,18000},*)
(*ContourStyle->{Black,Thin},*)
(*ContourLabels->None],*)
(*{c1,0,38000,1000}]*)
(*]*)


(* ::Chapter:: *)
(*Mission Analysis*)


(* ::Subsection:: *)
(*TSFC Constants*)


(* ::Input:: *)
(*c1TF=0.45;*)
(*c2TF=0.54;*)
(*c1LTFmil=0.9;*)
(*c2LTFmil=0.3;*)
(*c1LTFmax=1.6;*)
(*c2LTFmax=0.27;*)
(*c1TJmil=1.1;*)
(*c2TJmil=0.3;*)
(*c1TJmax=1.5;*)
(*c2TJmax=0.23;*)


(* ::Subsection:: *)
(*Best Subsonic Cruise (@ Best Mach = Subscript[M, crit])*)


(* ::Text:: *)
(**)


(* ::Input:: *)
(*piCRZ[\[CapitalDelta]S_,C1_,C2_,M0_,CD0_,CDR_,K1_,K2_,a_]:=Exp[-((C1/M0+C2)/3600)(Sqrt[4(CD0+CDR)K1]+K2) \[CapitalDelta]S/a]*)


(* ::Input:: *)
(*range=4100000; (*~ 2200 nm*)*)
(*AR=8;*)
(*e=0.8;*)
(*Mcrz=0.79;*)
(*commercial=True;*)
(*fighter=False;*)


(* ::Input:: *)
(*\[CapitalPi]CRZ=piCRZ[\[CapitalDelta]S,C1,C2,M0,CD0,CDR,K1,K2,a]//.{C1->0.45,C2->0.54,CD0->cd0[commercial,fighter,M0],CDR->0,K1->k1[commercial,fighter,M0,AR,e],K2->k2[commercial,fighter],M0->Mcrz,a->Sqrt[\[Gamma] R Tstd[hcrz]],\[CapitalDelta]S->range,\[Gamma]->1.4,R->287.}*)


(* ::Subsection:: *)
(*Best Subsonic Cruise Altitude*)


(* ::Text:: *)
(**)


(* ::Subsubsection:: *)
(*Initial BCA*)


(* ::Input:: *)
(*\[Beta]init=0.95;*)
(*WLG0=6220;*)


(* ::Input:: *)
(*\[Delta]init=(2\[Beta])/(\[Gamma] pstd[0] M0^2) 1/Sqrt[(CD0+CDR)/K1] WLG0//.{CD0->cd0[commercial,fighter,M0],CDR->0,K1->k1[commercial,fighter,M0,AR,e],M0->0.8,\[Beta]->\[Beta]init,\[Gamma]->1.4}*)


(* ::Input:: *)
(*Quiet[BCAinit=h/.Solve[Reduce[\[Delta]std[h]==\[Delta]init],h][[1]]]*)


(* ::Input:: *)
(*Solve[delta==(2\[Beta])/(\[Gamma]a p0 M0^2) 1/Sqrt[(CD0+CDR)/K1] WLG,\[Beta]]*)


(* ::Subsubsection:: *)
(*2000 ft steps*)


(* ::Text:: *)
(*Step Climb*)


(* ::Input:: *)
(*steps=Table[(delta Sqrt[(CD0+CDR)/K1] M0^2 pstd[0] \[Gamma])/(2 WLG)//.{CD0->cd0[commercial,fighter,M0],CDR->0,K1->k1[commercial,fighter,M0,AR,e],WLG->WLG0,M0->0.8,delta->\[Delta]std[BCAinit+step/3.2808],\[Gamma]->1.4},{step,2000,8000,2000}]*)


(* ::Input:: *)
(*stepsrange=Quiet[Table[Solve[steps[[j]]==Exp[-((C1/M0+C2)/3600)(Sqrt[4(CD0+CDR)K1]+K2) \[CapitalDelta]S/a]//.{C1->0.45,C2->0.54,CD0->cd0[commercial,fighter,M0],CDR->0,K1->k1[commercial,fighter,M0,AR,e],K2->k2[commercial,fighter],M0->0.8,a->Sqrt[\[Gamma] R Tstd[hcrz]],\[Gamma]->1.4,R->287.},\[CapitalDelta]S][[1,1]],{j,1,4}]]*)


(* ::Input:: *)
(*\[CapitalDelta]S1=\[CapitalDelta]S/.stepsrange[[1]]*)
(*\[CapitalDelta]S2=\[CapitalDelta]S/.stepsrange[[2]]*)
(*\[CapitalDelta]S3=\[CapitalDelta]S/.stepsrange[[3]]*)
(*\[CapitalDelta]S4=\[CapitalDelta]S/.stepsrange[[4]]*)


(* ::Subsection:: *)
(*TO Weight estimation*)


(* ::Input:: *)
(*\[CapitalGamma][WTO_]:=1.02*WTO^(-0.06);*)
(*seats=189+6;*)
(*baggage=90;*)
(*cargo = 3000;*)
(*Payload=80*seats+23*baggage+cargo;*)
(*Print["Payload = "<>ToString[Payload]]*)


(* ::Input:: *)
(*WTOguess=200000.;*)
(*sol=FindRoot[WTOff==Payload/(1-\[Beta]init(1-\[CapitalPi]CRZ)-\[CapitalGamma][WTOff]),{WTOff,WTOguess}]*)


(* ::Input:: *)
(*WTO=WTOff/.sol;*)
(*Wempty=\[CapitalGamma][WTO]*WTO;*)
(*Wfuel=WTO-Payload-Wempty;*)
(*Thrust=TWratio*WTO*9.81/2.; (*TWratio is a result of the constraint analysis; assuming twin-engine a/c*)*)
(*Print[Style["Aircraft Specifications",20,Bold,Red]];*)
(*	Print[TableForm[*)
(*	{WTO,Wempty,Payload,Wempty+Payload,Wfuel,Thrust},*)
(*	TableHeadings->{{"MTOW [Kg] ","OEW [Kg] ","Payload [Kg] ","MZFW [Kg]","MFW [Kg] ","TO Thrust (one engine) [N]"},None}*)
(*	]*)
(*];*)


(* ::Subsection:: *)
(*Payload-Range Diagram*)


(* ::Text:: *)
(*This is a typical performance diagram of civil aviation aircrafts. It depicts the achievable mission range for a given payload. *)


(* ::Input:: *)
(*Payload+(\[Beta]init*(1-\[CapitalPi]CRZ))*WTO+Wempty*)


(* ::Input:: *)
(*m2nm=0.000539957;(*meters to nautical miles*)*)


(* ::Input:: *)
(*payloadrange={{0,Wempty+Payload},{range*m2nm,Wempty+Payload}};*)
(*towrange={{0,Wempty+Payload},{range*m2nm,Wempty+Payload+Wfuel}};*)
(*FuelCapacity = 20700;*)
(*PL=Payload;*)
(*Fu=Wfuel;*)
(*mtow=WTO;*)
(*newrange=range;*)
(*(*MTOW-limited range*)*)
(*While[Fu<FuelCapacity,*)
(*PL=PL-50;*)
(*Fu=WTO-Wempty-PL;*)
(*rangeguess=newrange;*)
(*sol=FindRoot[mtow==PL+\[Beta]init*(1-piCRZ[rng,c1TF,c2TF,Mcrz,cd0[commercial,fighter,Mcrz],0,k1[commercial,fighter,Mcrz,AR,e],k2[commercial,fighter],Sqrt[1.4*287* Tstd[hcrz]]])*mtow+Wempty,{rng,rangeguess}];*)
(*newrange=rng/.sol;*)
(*AppendTo[payloadrange,{newrange*m2nm,Wempty+PL}];*)
(*AppendTo[towrange,{newrange*m2nm,Wempty+PL+Fu}]*)
(*]*)
(*(*Fuel-capacity-limited range*)*)
(*While[PL>50,*)
(*PL=PL-50;*)
(*Fu=FuelCapacity;*)
(*tow=Wempty+Fu+PL;*)
(*rangeguess=newrange;*)
(*sol=FindRoot[tow==PL+\[Beta]init*(1-piCRZ[rng,c1TF,c2TF,Mcrz,cd0[commercial,fighter,Mcrz],0,k1[commercial,fighter,Mcrz,AR,e],k2[commercial,fighter],Sqrt[1.4*287* Tstd[hcrz]]])*tow+Wempty,{rng,rangeguess}];newrange=rng/.sol;*)
(*AppendTo[payloadrange,{newrange*m2nm,Wempty+PL}];*)
(*AppendTo[towrange,{newrange*m2nm,Wempty+PL+Fu}]*)
(*]*)


(* ::Input:: *)
(*ListLinePlot[{payloadrange,towrange},FrameStyle->Directive[Black,FontSize->16,FontFamily->"Arial"],*)
(*Frame->True,*)
(*FrameLabel->{"Range [nautical miles]","Mass [kg]"},*)
(*GridLines->Automatic,*)
(*PlotLegends->{"OEW+Payload","TOW"}]*)

(* ::Package:: *)

BeginPackage["Labor`"];


Begin["Error`"];


GError[a_,b_,c_]:= Module[{},(Total[Map[Abs,MapThread[D,{Table[a,{i,1,Length[b]}],b}]]*c])];


Gauss[a_]:= Module[{},{Mean[a],StandardDeviation[a]/Sqrt[Length[a]]}];


End[];
Begin["Optic`"];


Transmission[T0_,T_,dT0_,dT_]:=Module[{f,fe,t,t0,dt,dt0},
f=t/t0;
fe=Error`GError[f,{t,t0},{dt,dt0}];
{f,fe}/.{t->T,t0->T0,dt->dT,dt0->dT0}];


Adsorbance[T0_,T_,dT0_,dT_]:=Module[{f,fe,t,dt,trans},
trans=Transmission[T0,T,dT0,dT];
f=Log[1/t];
fe=Error`GError[f,{t},{dt}];
{f,fe}/.{t->trans[[1]],dt->trans[[2]]}];


MolExtintion[X_,C_,I0_,IX_,dX_,dC_,dI0_,dIX_]:=Module[{f,fe,AD,ad,dc,dx,c,x,dad},
AD=Adsorbance[IX,I0,dI0,dIX];
f=-ad/(c x);
fe=Error`GError[f,{ad,c,x},{dad,dc,dx}];
{f,fe}/.{ad->AD[[1]],dad->AD[[2]],c->C,dc->dC,x->X,dx->dX}]


End[];
Begin["Basic`"];


Fadd[va_,dva_,vb_,dvb_]:=Module[{f,fe,a,b,da,db},
f=a+b;
fe=Error`GError[f,{a,b},{da,db}];
{f,fe}/.{a->va,b->vb,da->dva,db->dvb}]


Fdiv[a1_,da1_,b1_,db1_]:= Module[{f,fe,a,b,da,db},
f=a/b;
fe = Error`GError[f,{a,b},{da,db}];
{f,fe}/.{a->a1,da->da1,b-> b1,db->db1}]


Fmult[\[Gamma]_,d\[Gamma]_,\[Delta]_,d\[Delta]_]:= Module[{f,fe,a,b,da,db},
f=a*b;
fe = Error`GError[f,{a,b},{da,db}];
{f,fe}/.{a->\[Gamma],da->d\[Gamma],b-> \[Delta],db->d\[Delta]}]


End[];
Begin["Elec`"];


Fc[\[Gamma]_,d\[Gamma]_,\[Delta]_,d\[Delta]_,\[Chi]_,d\[Chi]_]:= Module[{f,fe,a,b,c,da,db,dc},
f=a/(b*c);
fe = Error`GError[f,{a,b,c},{da,db,dc}];
{f,fe}/.{a->\[Gamma],da->d\[Gamma],b-> \[Delta],db->d\[Delta],c->\[Chi],dc->d\[Chi]}]


End[];


EndPackage[]

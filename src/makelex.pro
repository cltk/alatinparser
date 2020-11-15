
% MAKELEX: building vocabulary file for ALP, A Latin Parser				
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Latest Update : November 2020
%  Archibald Michiels
% amichiels@uliege.be


% Directions for use
%
% To start the program running type "go." when it has finished loading.
% Do not type in the quotes but make sure you do type in the dot (: go.)



%
% DECLARATIONS
%
:- set_prolog_flag(double_quotes, codes). % only necessary if home-made 'getsentence' is used (equivalent of readln)
				          % 'getsentence' IS made use of in the present version !!!!!!!!!!!!!!!!!!!

:- style_check(-singleton).
:- style_check(-discontiguous).

:- dynamic(lex/3).

% the 'lex' predicate is partly fed by macro-expansion clauses executed at run-time
% and must therefore be declared dynamic


ifthen(Condition,Goal) :- Condition -> Goal ; true.
ifthenelse(Condition, ThenGoal, ElseGoal) :- Condition -> ThenGoal ; ElseGoal.

%
% GO STEP
%

go :- nl,
     write('MAKELEX, building vocabulary file for ALP' ),nl,
     write('------------------------------------------'),
     nl,nl,
     write('A.Michiels, amichiels@uliege.be'),nl,nl,
     write('Expanding the lexical macros...'),nl,nl,
     mkthem,mkmore,					% generating the morphological variants
     
     % writing a file VOCFILE containing the vocabulary in expanded format:
                                      open(vocfile,write,Voc),
                                      tell(Voc),
                                      listing(lex/3),
                                      told,
                                      tell(user),
                                      % to be used for debugging purposes after sorting with 'sort' (e.g. sort vocfile > vc)
                                      % the file should never sport a variable as
                                      % first arg of a lex/3 clause - trouble on the way...
     
     write('Done').
  



% Verbs
%%%%%%%

/*

TEMPLATES
%%%%%%%%%

verb([v(Full_Lex,Conj,xxx,RootPft,RootSup)],Class,Kind)

verb([v(v,5,pres,pft,sup)],tr_cod,std).
verb([v(v,5,pres,pft,sup)],tr_cod,std).
verb([v(v,5,pres,pft,sup)],tr_cod,std).
verb([v(v,5,pres,pft,sup)],tr_cod,std).
verb([v(v,5,pres,pft,sup)],tr_cod,std).
verb([v(v,5,pres,pft,sup)],tr_cod,std).

verb([v(v,4,pres,pft,sup)],tr_cod,std).
verb([v(v,4,pres,pft,sup)],tr_cod,std).
verb([v(v,4,pres,pft,sup)],tr_cod,std).
verb([v(v,4,pres,pft,sup)],tr_cod,std).
verb([v(v,4,pres,pft,sup)],tr_cod,std).
verb([v(v,4,pres,pft,sup)],tr_cod,std).
verb([v(v,4,pres,pft,sup)],tr_cod,std).

verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).
verb([v(v,3,pres,pft,sup)],tr_cod,std).

verb([v(v,2,pres,pft,sup)],tr_cod,std).
verb([v(v,2,pres,pft,sup)],tr_cod,std).
verb([v(v,2,pres,pft,sup)],tr_cod,std).
verb([v(v,2,pres,pft,sup)],tr_cod,std).
verb([v(v,2,pres,pft,sup)],tr_cod,std).

verb([v(v,1,pres,pft,sup)],tr_cod,std).
verb([v(v,1,pres,pft,sup)],tr_cod,std).
verb([v(v,1,pres,pft,sup)],tr_cod,std).
verb([v(v,1,pres,pft,sup)],tr_cod,std).






verb([v(i,1,pres,sup)],tr_cod,dep).
verb([v(i,1,pres,sup)],tr_cod,dep).
verb([v(i,1,pres,sup)],tr_cod,dep).
verb([v(i,1,pres,sup)],tr_cod,dep).
verb([v(i,1,pres,sup)],tr_cod,dep).

verb([v(i,2,pres,sup)],tr_cod,dep).
verb([v(i,2,pres,sup)],tr_cod,dep).
verb([v(i,2,pres,sup)],tr_cod,dep).
verb([v(i,2,pres,sup)],tr_cod,dep).
verb([v(i,2,pres,sup)],tr_cod,dep).
verb([v(i,2,pres,sup)],tr_cod,dep).
verb([v(i,2,pres,sup)],tr_cod,dep).

verb([v(i,3,pres,sup)],tr_cod,dep).
verb([v(i,3,pres,sup)],tr_cod,dep).
verb([v(i,3,pres,sup)],tr_cod,dep).
verb([v(i,3,pres,sup)],tr_cod,dep).
verb([v(i,3,pres,sup)],tr_cod,dep).
verb([v(i,3,pres,sup)],tr_cod,dep).
verb([v(i,3,pres,sup)],tr_cod,dep).

verb([v(i,5,pres,sup)],tr_cod,dep).
verb([v(i,5,pres,sup)],tr_cod,dep).


*/





verb([v(abhorrere,2,abhorre,abhorru,xxx)],intr,std).
verb([v(abire,4,xxx,abi,abit)],intr,std).
verb([v(accipere,5,accip,accep,accept)],tr_cod,std).
verb([v(adducere,3,adduc,addux,adduct)],tr_cod,std).
verb([v(adesse,3,xxx,adfu,xxx)],intr,std).           
verb([v(agere,3,ag,eg,act)],tr_cod,std).
verb([v(amare,1,am,amau,amat)],tr_cod,std).
verb([v(ambulare,1,ambul,ambulau,ambulat)],intr,std).
verb([v(amittere,3,amitt,amis,amiss)],tr_cod,std).
verb([v(angere,3,ang,xxx,xxx)],tr_cod,std).
verb([v(audere,2,aude,xxx,xxx)],tr_cod,std).
verb([v(audere,2,xxx,aus)],tr_cod,dep).
verb([v(audire,4,audi,audiu,audit)],tr_cod,std).
verb([v(cadere,3,cad,cecid,cas)],intr,std).
verb([v(canere,3,can,cecin,cant)],tr_cod,std).
verb([v(capere,5,cap,cep,capt)],tr_cod,std).
verb([v(cedere,3,ced,cess,cess)],intr,std).
verb([v(cenare,1,cen,cenau,cenat)],intr,std).
verb([v(clamare,1,clam,clamau,clamat)],tr_cod,std).
verb([v(claudere,3,claud,claus,claus)],tr_cod,std).
verb([v(coepi,3,xxx,coep,xxx)],tr_cod,std). 			% treated as lacking everything except perfect tense and derived variants
verb([v(coepi,3,xxx,coept)],tr_cod,dep).    			% treated as a deponent - semantically preserves 'active' voice
verb([v(cogere,3,cog,coeg,coact)],tr_cod,std).
verb([v(cognoscere,3,cognosc,cognou,cognit)],tr_cod,std).
verb([v(colere,3,col,colu,cult)],tr_cod,std).
verb([v(concupiscere,3,concupisc,concupiu,concupit)],tr_cod,std).
verb([v(credere,3,cred,credid,credit)],tr_inf,std).
verb([v(dare,1,d,ded,dat)],tr_cod_coi,std).
verb([v(debere,2,debe,debu,debut)],tr_cod,std).
verb([v(delere,2,dele,deleu,delet)],tr_cod,std).
verb([v(dementare,1,dement,dementau,dementat)],tr_cod,std).
verb([v(dicere,3,dic,dix,dict)],tr_inf,std).
verb([v(diripere,5,dirip,diripu,dirept)],tr_cod,std).
verb([v(docere,2,doce,docu, doct)],tr_cod_coi,std).
verb([v(donare,1,don,donau,donat)],tr_cod_coi,std).
verb([v(dubitare,1,dubit,dubitau,dubitat)],intr,std).
verb([v(ducere,3,duc,dux,duct)],tr_cod,std).
verb([v(eligere,3,elig,eleg,elect)],tr_cod,std).
verb([v(eripere,5,erip,eripu,erept)],tr_cod,std).  
verb([v(errare,1,err,errau,errat)],intr,std).
verb([v(excipere,5,excip,excep,except)],tr_cod,std).
verb([v(exhaurire,4,exhauri,exhaus,exhaust)],tr_cod,std).
verb([v(existimare,1,existim,existimau,existimat)],tr_inf,std).
verb([v(facere,5,fac,fec,fact)],tr_cod,std).
verb([v(ferre,3,xxx,tul,lat)],tr_cod,std).
verb([v(frangere,3,frang,freg,fract)],tr_cod,std).
verb([v(fugere,5,fug,fug,fugit)],intr,std).
verb([v(gaudere,2,gaude,xxx,xxx)],intr,std).
verb([v(gaudere,2,xxx,gauis)],tr_cod,dep).
verb([v(gerere,3,ger,gess,gest)],tr_cod,std).
verb([v(habere,2,habe,habu,habit)],tr_cod,std).
verb([v(hortari,1,hort,hortat)],tr_cod,dep).
verb([v(iacere,2,iace,iacui,xxx)],intr,std).
verb([v(ignoscere,3,ignosc,ignou,ignot)],tr_cod,std).
verb([v(incipere,5,incip,incep,incept)],tr_cod,std).
verb([v(indicare,1,indic,indicau,indicat)],tr_cod,std).
verb([v(insanire,4,insani,insaniu,insanit)],intr,std).
verb([v(interdicere,3,interdic,interdix,interdict)],tr_cod_cplt,std).
verb([v(interficere,5,interfic,interfec,interfect)],tr_cod,std).
verb([v(inuenire,4,inueni,inuen,inuent)],tr_cod,std).
verb([v(ire,4,xxx,i,it)],intr,std).
verb([v(iubere,2,iube,iuss,iuss)],tr_inf,std).
verb([v(iudicare,1,iudic,iudicau,iudicat)],tr_inf,std).
verb([v(iuuare,1,iuu,iuu,iut)],tr_cod,std).
verb([v(laudare,1,laud,laudau,laudat)],tr_cod,std).
verb([v(legere,3,leg,leg,lect)],tr_cod,std).
verb([v(linquere,3,linqu,liqu,lict)],tr_cod,std).
verb([v(loqui,3,loqu,locut)],intr,dep).
verb([v(ludere,3,lud,lus,lus)],intr,std).
verb([v(manere,2,mane,mans,mans)],intr,std).
verb([v(miscere,2,misce,miscu,mixt)],tr_cod,std).
verb([v(mittere,3,mitt,mis,miss)],tr_cod_coi,std).
verb([v(mori,5,mor,mortu)],intr,dep).
verb([v(mouere,2,moue,mou,mot)],tr_cod,std).
verb([v(mutare,1,mut,mutau,mutat)],tr_cod,std).
verb([v(narrare,1,narr,narrau,narrat)],tr_cod,std).
verb([v(nasci,3,nasc,nat)],intr,dep).
verb([v(negare,1,neg,negau,negat)],tr_inf,std).
verb([v(nescire,4,nesci,nesciu,nescit)],tr_inf,std).
verb([v(noscere,3,xxx,nou,xxx)],tr_cod,std).
verb([v(nuntiare,1,nunti,nuntiau,nuntiat)],tr_inf,std).
verb([v(obliuisci,3,obliuisc,oblit)],tr_cod,dep).
verb([v(obtemperare,1,obtemper,obtemperau,obtemperat)],intr,std).
verb([v(oppugnare,1,oppugn,oppugnau,oppugnat)],tr_cod,std).
verb([v(optare,1,opt,optau,optat)],tr_cod,std).
verb([v(orare,1,or,orau,orat)],tr,std).
verb([v(ostendere,3,ostend,ostend,ostent)],tr_cod,std).
verb([v(parare,1,par,parau,parat)],tr,std).
verb([v(parcere,3,parc,peperc,pars)],tr_cod,std).
verb([v(pati,5,pat,pass)],tr_cod,dep).
verb([v(perdere,3,perd,perdid,perdit)],tr_cod,std).
verb([v(perire,4,xxx,peri,perit)],intr,std).
verb([v(perspicere,5,perspic,perspex,perspect)],tr_cod,std).
verb([v(peruenire,4,peruen,peruen,peruent)],intr,std).
verb([v(petere,3,pet,petiu,petit)],tr_cod,std).
verb([v(placere,2,place,placu,placit)],tr_coi,std).
verb([v(ponere,3,pon,posu,posit)],tr_cod,std).
verb([v(praebere,2,praebe,praebu,praebit)],tr_cod,std).
verb([v(praestare,1,praest,praestit,praestat)],intr,std).
verb([v(premere,3,prem,press,press)],tr_cod,std).
verb([v(procurare,1,procur,procurau,procurat)],tr_cod,std).
verb([v(pugnare,1,pugn,pugnau,pugnat)],intr,std).
verb([v(putare,1,put,putau,putat)],tr_inf,std).
verb([v(quaerere,3,quaer,quaesiu,quaesit)],tr_cod,std).
verb([v(rapere,5,rap,rapu,rapt)],tr_cod,std).
verb([v(recipere,5,recip,recep,recept)],tr_cod,std).
verb([v(recubare,1,recub,recubau,recubat)],intr,std).
verb([v(reddere,3,redd,redid,redit)],tr_cod,std).
verb([v(redire,4,xxx,redi,redit)],intr,std).
verb([v(referre,3,xxx,rettul,relat)],tr_cod,std).
verb([v(regnare,1,regn,regnau,regnat)],intr,std).
verb([v(relinquere,3,relinqu,reliqu,relict)],tr_cod,std).
verb([v(renouare,1,renou,renouau,renouat)],tr_cod,std).
verb([v(reri,2,re,rat)],tr_inf,dep).
verb([v(rogare,1,rog,rogau,rogat)],tr_cod,std).
verb([v(scire,4,sci,sciu,scit)],tr_inf,std).
verb([v(scribere,3,scrib,scrips,script)],tr_cod,std).
verb([v(sentire,4,senti,sens,sens)],tr_cod,std).
verb([v(sequi_1,3,sequ,secut)],intr,dep).
verb([v(sequi_2,3,sequ,secut)],tr_cod,dep).
verb([v(serpere,3,serp,serps,xxx)],intr,std).
verb([v(seruare,1,seru,seruau,seruat)],tr_cod,std).
verb([v(sinere,3,sin,siu,sit)],tr_inf,std).
verb([v(solere,2,sole,xxx,xxx)],tr_cod,std).
verb([v(solere,2,xxx,solit)],tr_cod,dep).
verb([v(soluere,3,solu,solu,solut)],tr_cod,std).
verb([v(stare,1,st,stet,stat)],intr,std).
verb([v(studere,2,stude,studu,xxx)],tr_cod_dat,std).
verb([v(suspendere,3,suspend,suspend,suspens)],tr_cod_coi,std).
verb([v(tacere,2,tace,tacu,tacit)],intr,std).
verb([v(tegere,3,teg,tex,tect)],tr_cod,std).
verb([v(tenere,2,tene,tenu,tent)],tr_cod,std).
verb([v(timere,2,time,timu,xxx)],tr_cod,std).
verb([v(tollere,3,toll,sustul,sublat)],tr_cod,std).
verb([v(tradere,3,trad,tradid,tradit)],tr_inf,std).
verb([v(trahere,3,trah,trax,tract)],tr_cod,std).
verb([v(uenire,4,ueni,uen,uent)],intr,std).
verb([v(uereri,2,uere,uerit)],tr,dep).
verb([v(uertere,3,uert,uert,uers)],intr,std).
verb([v(uidere,2,uide,uid,uis)],tr_cod,std).
verb([v(uincere,3,uinc,uic,uict)],tr_cod,std).
verb([v(uitare,1,uit,uitau,uitat)],tr_cod,std).
verb([v(uiuere,3,uiu,uix,uict)],intr,std).
verb([v(uocare,1,uoc,uocau,uocat)],tr_cod,std).
verb([v(uti,3,ut,us)],tr_cod_abl,dep).



verb([v(aux,posse,possum,potes,potest,possumus,potestis,possunt,
            possim,possis,possit,possimus,possitis,possint,
            poteram,poteras,poterat,poteramus,poteratis,poterant,
            potero,poteris,poterit,poterimus,poteritis,poterunt,
            possem,posses,posset,possemus,possetis,possent,
            posse,potu)],tr_inf_no_subj,std).
verb([v(aux,uelle,uolo,uis,uult,uolumus,uultis,uolunt,
        uelim,uelis,uelit,uelimus,uelitis,uelint,
        uolebam,uolebas,uolebat,uolebamus,uolebatis,uolebant,
        uolam,uoles,uolet,uolemus,uoletis,uolent,
        uellem,uelles,uellet,uollemus,uolletis,uollent,
        uelle,uolu)],tr_inf_no_subj,std).
verb([v(aux,nolle,nolo,xxx,xxx,nolumus,xxx,nolunt,
        nolim,nolis,nolit,nolimus,nolitis,nolint,
        nolebam,nolebas,nolebat,nolebamus,nolebatis,nolebant,
        xxx,xxx,xxx,xxx,xxx,xxx,
        nollem,nolles,nollet,nollemus,nolletis,nollent,
        nolle,nolu)],tr_inf_no_subj,std).
verb([v(aux,malle,malo,mauis,mauult,malumus,mauultis,malunt,
        malim,malis,malit,malimus,malitis,malint,
        malebam,malebas,malebat,malebamus,malebatis,malebant,
        xxx,xxx,xxx,xxx,xxx,xxx,
        mallem,malles,mallet,mallemus,malletis,mallent,
        malle,malu)],tr_inf_no_subj,std).
verb([v(aux,fieri,fio, fis,fit,fimus, fitis, fiunt,
        fiam, fias, fiat, fiamus, fiatis, fiant,
        fiebam,fiebas,fiebat,fiebamus,fiebatis,fiebant,
        fiam,fies,fiet,fiemus,fietis,fient,
        fierem,fieres,fieret,fieremus,fieretis,fierent,
        fieri,xxx)],vpred,std).


verb([v(imp,decet,decet,deceat,decebat,decebit,deceret,decere,decu)],vimp,std).
verb([v(imp,paenitet,paenitet,paeniteat,paenitebat,paenitebit,paeniteret,paenitere,paenitu)],vimp,std).
verb([v(imp,placet,placet,placeat,placebat,placebit,placeret,placere,placu)],vimp,std).
verb([v(imp,licet,licet,liceat,licebat,licebit,liceret,licere,licu)],vimp,std).
verb([v(imp,iuuat,iuuat,iuuet,iuuabat,iuuabit,iuuaret,iuuare,iuu)],vimp,std).
verb([v(imp,fugit,fugit,fugiat,fugiebat,fugiet,fugeret,fugere,fug)],vimp,std).
verb([v(imp,fallit,fallit,fallat,fallebat,fallet,falleret,fallere,fefell)],vimp,std).


verb([v(esse,3,xxx,fu,xxx)],v_esse,std).





%%%%%
% ADJ
%%%%%



% 1


/* % unus, unius, uni
declension_endings(adj,a1,
                 [_NIL,um,ius,i,o,
                  a,am,ius,i,a,
                  um,um,ius,i,o,
                  i,os,orum,is,is,
                 ae,as,arum,is,is,
                 a,a,orum,is,is]).
*/

adj(alius,ali,a1,tool,[xxx,xxx]).
adj(alter,alter,a1,tool,[xxx,xxx]).
adj(neuter,neutr,a1,tool,[xxx,xxx]).
adj(nullus,null,a1,int,[xxx,xxx]).
adj(solus,sol,a1,std,[xxx,xxx]).   
adj(totus,tot,a1,std,[xxx,xxx]).
adj(ullus,ull,a1,int,[xxx,xxx]).
adj(unus,un,a1,tool,[xxx,xxx]).
adj(uter,utr,a1,tool,[xxx,xxx]).





adj(quantus,quant,1,int,[xxx,xxx]).



adj(aestiuus,aestiu,1,std,[xxx,xxx]).
adj(alienus,alien,1,std,[alien,alienissim]).
adj(altus,alt,1,std,[alt,altissim]).
adj(asper,asper,1,std,[asper,asperim]).
adj(beatus,beat,1,std,[beat,beatissim]).
adj(bonus,bon,1,std,[mel,optim]).
adj(caecus,caec,1,std,[caec,caecissim]).
adj(certus,cert,1,std,[cert,certissim]).
adj(cunctus,cunct,1,std,[xxx,xxx]).
adj(cupidus,cupid,1,std,[cupid,cupidissim]).
adj(dexter,dextr,1,std,[xxx,xxx]).
adj(dignus,dign,1,std,[dign,dignissim]).
adj(doctus,doct,1,std,[doct,doctissim]).
adj(durus,dur,1,std,[dur,durissim]).
adj(extremus,extrem,1,std,[xxx,xxx]).
adj(ferus,fer,1,std,[feroc,ferocissim]).
adj(formosus,formos,1,std,[formos,formosissim]).
adj(fortunatus,fortunat,1,std,[fortunat,fortunatissim]).
adj(graecus,graec,1,std,[xxx,xxx]).
adj(humanus,human,1,std,[human,humanissim]).
adj(impudicus,impudic,1,std,[impudic,impudicissim]).
adj(incommodus,incommod,1,std,[incommod,incommodissim]).
adj(iucundus,iucund,1,std,[iucund,iucundissim]).
adj(laetus,laet,1,std,[laet,laetissim]).
adj(liber,liber,1,std,[liber,liberrim]).
adj(longus,long,1,std,[long,longissim]).
adj(magnus,magn,1,std,[ma,maxim]).
adj(malus,mal,1,std,[pe,pessim]).
adj(medius,medi,1,std,[xxx,xxx]).
adj(miser,miser,1,std,[miser,miserrim]).
adj(mortuus,mortu,1,std,[xxx,xxx]).
adj(multus,mult,1,std,[xxx,xxx]).
adj(nouus,nou,1,std,[recent,recentissim]).
adj(paruus,paru,1,std,[min,minim]).
adj(patulus,patul,1,std,[patul,patulissim]).
adj(peritus,perit,1,std,[perit,peritissim]).
adj(primus,prim,1,std,[xxx,xxx]).
adj(propinquus,propinqu,1,std,[prop,proxim]).
adj(prosper,prosper,1,std,[prosper,prosperrim]).
adj(publicus,public,1,std,[public,publicissim]).
adj(pulcher,pulchr,1,std,[pulchr,pulcherrim]).
adj(romanus,roman,1,std,[xxx,xxx]).
adj(sacer,sacr,1,std,[sanct,sanctissim]).
adj(saeuus,saeu,1,std,[saeu,saeuissim]).
adj(saluus,salu,1,std,[xxx,xxx]).
adj(superuacuus,superuacu,1,std,[xxx,xxx]).
adj(superus,super,1,std,[xxx,xxx]).
adj(supremus,suprem,1,std,[xxx,xxx]).
adj(tener,tener,1,std,[tener,tenerrim]).
adj(uerus,uer,1,std,[uer,uerissim]).
adj(uiuus,uiu,1,std,[xxx,xxx]).
adj(ultimus,ultim,1,std,[xxx,xxx]).
adj(uotiuus,uotiu,1,std,[xxx,xxx]).
adj(uuidus,uuid,1,std,[uuid,uuidissim]).


adj(meus,me,1,poss,[xxx,xxx]).
adj(tuus,tu,1,poss,[xxx,xxx]).
adj(suus,su,1,poss,[xxx,xxx]).
adj(noster,nostr,1,poss,[xxx,xxx]).
adj(uester,uestr,1,poss,[xxx,xxx]).




% 2

/* % fortis, fortis, forte
declension_endings(adj,2,
                  [_NIL,em,is,i,i,
                   e,e,is,i,i,
                  es,es,ium,ibus,ibus,
                  ia,ia,ium,ibus,ibus]).

*/

adj(qualis,qual,2,int,[xxx,xxx]).

adj(amabilis,amabil,2,std,[amabil,amabilissim]).
adj(breuis,breu,2,std,[breu,breuissim]).
adj(difficilis,difficil,2,std,[difficil,difficilim]).
adj(diues,diuit,2,std,[diuit,diuitissim]).
adj(facilis,facil,2,std,[facil,facillim]).
adj(fortis,fort,2,std,[fort,fortissim]).
adj(grandis,grand,2,std,[grand,grandissim]).
adj(grauis,grau,2,std,[grau,grauissim]).
adj(leuis,leu,2,std,[leu,leuissim]).
adj(singularis,singular,2,std,[singular,singularissim]).
adj(tristis,trist,2,std,[trist,tristissim]).
adj(uetus,ueter,2,std,[uetust,ueterrim]).



% ingens, ingens, ingens
% declension_endings(adj,a2,
  %                [_NIL,em,is,i,i,
  %                 _NIL,_NIL,is,i,i,
  %                es,es,ium,ibus,ibus,
  %                ia,ia,ium,ibus,ibus]).



adj(felix,felic,a2,std,[felic,felicissim]).
adj(ingens,ingent,a2,std,[ingent,ingentissim]).
adj(par,par,a2,std,[xxx,xxx]).
adj(potens,potent,a2,std,[potent,potentissim]).
adj(praestans,praestant,a2,std,[praestant,praestantissim]).
adj(uictrix,uictric,a2,std,[xxx,xxx]).






%%%%%%%
% NOUNS
%%%%%%%



noun(1,fem,amica,amic,class:common,sem:[hum],[]).
noun(1,fem,ancilla,ancill,class:common,sem:[hum],[]).
noun(1,fem,aqua,aqu,class:common,sem:[thing],[]).
noun(1,fem,auaritia,auariti,class:common,sem:[abstract],[]).
noun(1,fem,cannae,cann,class:common,sem:[city, thing],[nb:pl]). % pluralia tantum
noun(1,fem,causa,caus,class:common,sem:[thing, abstract],[]).
noun(1,fem,copiae,copi,class:common,sem:[hum],[nb:pl]). % pluralia tantum
noun(1,fem,corona,coron,class:common,sem:[thing],[]).
noun(1,fem,cura,cur,class:common,sem:[abstract],[]).
noun(1,fem,dea,de,class:common,sem:[hum],[]).
noun(1,fem,diuitiae,diuiti,class:common,sem:[thing],[nb:pl]).  % pluralia tantum
noun(1,fem,domina,domin,class:common,sem:[hum],[]).
noun(1,fem,ecclesia,ecclesi,class:common,sem:[hum, thing,loc],[]).
noun(1,fem,epistula,epistul,class:common,sem:[thing, abstract],[]).
noun(1,fem,fama,fam,class:common,sem:[abstract,hum],[]).
noun(1,fem,flamma,flamm,class:common,sem:[thing],[]).
noun(1,fem,fortuna,fortun,class:common,sem:[abstract,hum],[]).
noun(1,fem,gallia,galli,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,gloria,glori,class:common,sem:[abstract],[]).
noun(1,fem,grammatica,grammatic,class:common,sem:[abstract],[]).
noun(1,fem,hedera,heder,class:common,sem:[thing],[]).
noun(1,fem,historia,histori,class:common,sem:[abstract],[]).
noun(1,fem,hora,hor,class:common,sem:[time],[]).
noun(1,fem,inuidia,inuidi,class:common,sem:[abstract],[]).
noun(1,fem,ira,ir,class:common,sem:[abstract],[]).
noun(1,fem,italia,itali,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,iustitia,iustiti,class:common,sem:[abstract],[]).
noun(1,fem,litterae,litter,class:common,sem:[thing, abstract],[nb:pl]). % pluralia tantum
noun(1,fem,lutetia,luteti,class:common,sem:[city,thing],[nb:sg]).
noun(1,fem,memoria,memori,class:common,sem:[abstract],[]).
noun(1,fem,natura,natur,class:common,sem:[abstract, thing],[]).
noun(1,fem,parsimonia,parsimoni,class:common,sem:[abstract],[]).
noun(1,fem,patientia,patienti,class:common,sem:[abstract],[]).
noun(1,fem,pecunia,pecuni,class:common,sem:[thing],[]).
noun(1,fem,poena,poen,class:common,sem:[abstract],[]).
noun(1,fem,puella,puell,class:common,sem:[hum],[]).
noun(1,fem,regina,regin,class:common,sem:[hum],[]).
noun(1,fem,roma,rom,class:common,sem:[city,thing],[nb:sg]).
noun(1,fem,sapientia,sapienti,class:common,sem:[abstract],[]).
noun(1,fem,sardinia,sardini,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,scientia,scienti,class:common,sem:[abstract],[]).
noun(1,fem,serua,seru,class:common,sem:[hum, thing],[]).
noun(1,fem,sicilia,sicili,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,silua,silu,class:common,sem:[thing,loc],[]).
noun(1,fem,tabula,tabul,class:common,sem:[thing],[]).
noun(1,fem,terra,terr,class:common,sem:[thing,loc],[]).
noun(1,fem,thessalia,thessali,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,troia,troi,class:common,sem:[city,thing],[nb:sg]).
noun(1,fem,uia,ui,class:common,sem:[abstract,thing,loc],[]).
noun(1,fem,uictoria,uictori,class:common,sem:[abstract],[]).
noun(1,fem,uita,uit,class:common,sem:[thing, abstract],[]).
noun(1,fem,umbra,umbr,class:common,sem:[thing,loc],[]).
noun(1,fem,unda,und,class:common,sem:[thing],[]).



noun(1,masc,agricola,agricol,class:common,sem:[hum],[]).
noun(1,masc,belga,belg,class:common,sem:[hum],[]).
noun(1,masc,nauta,naut,class:common,sem:[hum],[]).







noun(2, masc, amicus, amic,class:common, sem:[hum],[]).
noun(2, masc, animus, anim, class:common,sem:[abstract, thing],[]).
noun(2, masc, annus, ann,class:common, sem:[time, abstract],[]).
noun(2, masc, danaus, dana,class:common, sem:[hum],[nb:pl]).
noun(2, masc, deus, de,class:common, sem:[hum],[]).
noun(2, masc, discipulus, discipul,class:common, sem:[hum],[]).
noun(2, masc, dominus, domin,class:common, sem:[hum],[]).
noun(2, masc, equus, equ,class:common, sem:[thing, hum],[]).
noun(2, masc, filius, fili,class:common, sem:[hum],[]).
noun(2, masc, gallus, gall,class:common, sem:[hum],[nb:pl]).
noun(2, masc, germanus, german,class:common, sem:[hum],[nb:pl]).
noun(2, masc, graecus, graec,class:common, sem:[hum],[nb:pl]).
noun(2, masc, hortus, hort, class:common,sem:[loc,thing],[]).
noun(2, masc, inimicus, inimic,class:common, sem:[hum],[]).
noun(2, masc, legatus, legat,class:common, sem:[hum],[]).
noun(2, masc, libellus, libell, class:common,sem:[abstract, thing],[]).
noun(2, masc, locus, loc, class:common,sem:[loc,abstract, thing],[]).
noun(2, masc, modus, mod, class:common,sem:[abstract, thing],[]).
noun(2, masc, natus, nat,class:common, sem:[hum],[]).
noun(2, masc, oculus, ocul, class:common,sem:[abstract, thing],[]).
noun(2, masc, populus, popul,class:common, sem:[hum],[]).
noun(2, masc, romanus, roman,class:common, sem:[hum],[nb:pl]).
noun(2, masc, seruus, seru,class:common, sem:[hum, thing],[]).
noun(2, masc, troianus, troian,class:common, sem:[hum],[nb:pl]).
noun(2, masc, uentus, uent,class:common, sem:[thing],[]).




noun(2, masc, ager, agr,class:common, sem:[thing],[]).
noun(2, masc, faber, fabr,class:common, sem:[hum],[]).
noun(2, masc, gener, gener,class:common, sem:[hum],[]).
noun(2, masc, liber, libr,class:common, sem:[thing,abstract],[]).
noun(2, masc, liberi,liber,class:common,sem:[hum],[nb:pl]). % pluralia tantum
noun(2, masc, magister, magistr,class:common, sem:[hum],[]).
noun(2, masc, puer, puer,class:common, sem:[hum],[]).
noun(2, masc, socer, socer,class:common, sem:[hum],[]).
noun(2, masc, uesper, uesper,class:common, sem:[time],[]).
noun(2, masc, uir, uir,class:common, sem:[hum],[]).




noun(2, fem, fagus, fag,class:common, sem:[thing],[]).
noun(2, fem, laurus, laur, class:common,sem:[thing],[]).





noun(2, neuter, auxilium, auxili,class:common, sem:[thing, abstract],[]).
noun(2, neuter, bellum, bell,class:common, sem:[thing],[]).
noun(2, neuter, beneficium, benefici,class:common, sem:[abstract, thing],[]).
noun(2, neuter, bonum, bon,class:common, sem:[thing, abstract],[]).
noun(2, neuter, caelum, cael,class:common, sem:[thing],[]).
noun(2, neuter, consilium, consili,class:common, sem:[abstract,hum],[]).
noun(2, neuter, documentum, document,class:common, sem:[thing,abstract],[]).
noun(2, neuter, donum, don,class:common, sem:[thing],[]).
noun(2, neuter, fatum, fat,class:common, sem:[abstract],[]).
noun(2, neuter, ferrum, ferr,class:common, sem:[thing,abstract],[]).
noun(2, neuter, imperium, imperi,class:common, sem:[thing, abstract],[]).
noun(2, neuter, incommodum, incommod,class:common, sem:[abstract],[]).
noun(2, neuter, ingenium, ingeni,class:common, sem:[abstract],[]).
noun(2, neuter, lugdunum, lugdun,class:common, sem:[city,thing, abstract],[nb:sg]). % only sing
noun(2, neuter, negotium, negoti,class:common, sem:[abstract],[]).
noun(2, neuter, peccatum, peccat,class:common, sem:[abstract],[]).
noun(2, neuter, periculum, pericul,class:common, sem:[thing,abstract],[]).
noun(2, neuter, praeceptum, praecept,class:common, sem:[abstract],[]).
noun(2, neuter, praemium, praemi,class:common, sem:[thing, abstract],[]).
noun(2, neuter, proelium, proeli,class:common, sem:[thing, abstract],[]).
noun(2, neuter, regnum, regn,class:common, sem:[thing, abstract],[]).
noun(2, neuter, scutum, scut,class:common, sem:[thing],[]).
noun(2, neuter, signum, sign,class:common, sem:[thing, abstract],[]).
noun(2, neuter, silentium, silenti,class:common, sem:[abstract],[]).
noun(2, neuter, solacium, solaci,class:common, sem:[abstract],[]).
noun(2, neuter, telum,tel,class:common, sem:[thing],[]).
noun(2, neuter, templum, templ,class:common, sem:[thing, loc],[]).
noun(2, neuter, uerbum, uerb,class:common, sem:[abstract],[]).
noun(2, neuter, uestimentum,uestiment,class:common, sem:[thing],[]).
noun(2, neuter, uinum, uin,class:common, sem:[thing],[]).


noun(2, neuter, arma, arm,class:common, sem:[thing],[nb:pl]).  % pluralia tantum
noun(2, neuter, castra, castr,class:common, sem:[thing, loc],[nb:pl]).  % pluralia tantum
noun(2, neuter, milia, mili,class:common, sem:_,[nb:pl]).  % pluralia tantum








noun(3,masc,amor,amor,um,class:common,sem:[abstract],[]).
noun(3,masc,ciuis,ciu,ium,class:common,sem:[hum],[]).
noun(3,masc,consul,consul,um,class:common,sem:[hum],[]).
noun(3,masc,dolor,dolor,um,class:common,sem:[abstract],[]).
noun(3,masc,dux,duc,um,class:common,sem:[hum],[]).
noun(3,masc,eques,equit,um,class:common,sem:[hum],[]).
noun(3,masc,error,error,um,class:common,sem:[abstract],[]).
noun(3,masc,finis,fin,ium,class:common,sem:[thing,loc],[]).
noun(3,masc,frater,fratr,um,class:common,sem:[hum],[]).
noun(3,masc,homo,homin,um,class:common,sem:[hum],[]).
noun(3,masc,honor,honor,um,class:common,sem:[abstract],[]).
noun(3,masc,hostis,host,ium,class:common,sem:[hum],[]).
noun(3,masc,ignis,ign,ium,class:common,sem:[thing],[]).
noun(3,masc,imperator,imperator,um,class:common,sem:[hum],[]).
noun(3,masc,iuuenis,iuuen,um,class:common,sem:[hum],[]).
noun(3,masc,labor,labor,um,class:common,sem:[abstract],[]).
noun(3,masc,leo,leon,um,class:common,sem:[thing, hum],[]).
noun(3,masc,maiores,maior,um,class:common,sem:[hum],[nb:pl]).    % pluralia tantum
noun(3,masc,miles,milit,um,class:common,sem:[hum],[]).
noun(3,masc,mons,mont,ium,class:common,sem:[loc,thing],[]).
noun(3,masc,mos,mor,um,class:common,sem:[abstract],[]).
noun(3,masc,orator,orator,um,class:common,sem:[hum],[]).
noun(3,masc,paries,pariet,um,class:common,sem:[thing],[]).
noun(3,masc,pater,patr,um,class:common,sem:[hum],[]).
noun(3,masc,pes,ped,um,class:common,sem:[thing, abstract],[]).
noun(3,masc,piscis,pisc,ium,class:common,sem:[thing],[]).     
noun(3,masc,praetor,praetor,um,class:common,sem:[hum],[]).
noun(3,masc,rex,reg,um,class:common,sem:[hum],[]).
noun(3,masc,sanguis,sangu,ium,class:common,sem:[thing],[]).                                 
noun(3,masc,senator,senator,um,class:common,sem:[hum],[]).
noun(3,masc,sermo,sermon,um,class:common,sem:[thing, abstract],[]).
noun(3,masc,sol,sol,um,class:common,sem:[thing, abstract],[nb:sg]).




noun(3,fem,ars,art,ium,class:common,sem:[abstract],[]).
noun(3,fem,caedes,caed,ium,class:common,sem:[abstract],[]).
noun(3,fem,carthago,carthagin,um,class:common, sem:[city,thing, abstract],[nb:sg]).
noun(3,fem,celebritas,celebritat,um,class:common,sem:[abstract],[]).
noun(3,fem,ciuitas,ciuitat,um,class:common,sem:[hum],[]).
noun(3,fem,cohors,cohort,ium,class:common,sem:[hum],[]).
noun(3,fem,clades,clad,um,class:common,sem:[abstract],[]).
noun(3,fem,consuetudo,consuetudin,um,class:common,sem:[abstract],[]).
noun(3,fem,cupiditas,cupiditat,um,class:common,sem:[abstract],[]).
noun(3,fem,fines,fin,ium,class:common,sem:[thing, loc],[nb:pl]). % pluralia tantum
noun(3,fem,gens,gent,ium,class:common,sem:[hum],[]).
noun(3,fem,hiems,hiem,um,class:common,sem:[abstract,time],[]).
noun(3,fem,immortalitas,immortalitat,um,class:common,sem:[abstract, thing],[]).
noun(3,fem,karthago,karthagin,um,class:common, sem:[city,thing, abstract],[nb:sg]).
noun(3,fem,legio,legion,um,class:common,sem:[hum, thing],[]).
noun(3,fem,lex,leg,um,class:common,sem:[abstract],[]).
noun(3,fem,lux,luc,um,class:common,sem:[thing],[]).
noun(3,fem,magnitudo,magnitudin,um,class:common,sem:[abstract],[]).
noun(3,fem,mater,matr,um,class:common,sem:[hum],[]).
noun(3,fem,mens,ment,ium,class:common,sem:[abstract],[]).
noun(3,fem,mors,mort,ium,class:common,sem:[abstract, thing, hum],[]). 
noun(3,fem,nox,noct,um,class:common,sem:[abstract, time],[]).
noun(3,fem,nux,nuc,um,class:common,sem:[thing],[]).
noun(3,fem,pars,part,ium,class:common,sem:[abstract, thing, hum, loc],[]).
noun(3,fem,pax,pac,um,class:common,sem:[abstract],[nb:sg]).
noun(3,fem,potestas,potestat,um,class:common,sem:[abstract],[]).
noun(3,fem,quies,quiet,um,class:common,sem:[abstract],[nb:sg]).
noun(3,fem,ratio,ration,um,class:common,sem:[abstract],[]).
noun(3,fem,salus,salut,um,class:common,sem:[abstract],[]).
noun(3,fem,sollemnitas,sollemnitat,um,class:common,sem:[abstract],[]).
noun(3,fem,uires,uir,ium,class:common,sem:[thing,abstract],[nb:pl]). % pluralia tantum
noun(3,fem,uirtus,uirtut,um,class:common,sem:[abstract],[]).
noun(3,fem,uox,uoc,um,class:common,sem:[abstract],[]).
noun(3,fem,urbs,urb,ium,class:common,sem:[hum,thing,loc],[]).
noun(3,fem,uxor,uxor,um,class:common,sem:[hum],[]).



noun(3,or([masc,fem]),coniux,coniug,um,class:common,sem:[hum],[]).
noun(3,or([masc,fem]),parens,parent,um,class:common,sem:[hum],[]).





noun(3,neuter,caput,capit,um,class:common,sem:[thing,hum,abstract],[]).
noun(3,neuter,carmen,carmin,um,class:common,sem:[thing,abstract],[]).
noun(3,neuter,cor,cord,um,class:common,sem:[thing,abstract],[]).
noun(3,neuter,corpus,corpor,um,class:common,sem:[thing],[]).
noun(3,neuter,facinus,facinor,um,class:common,sem:[abstract],[]).
noun(3,neuter,flumen,flumin,um,class:common,sem:[thing, loc],[]).
noun(3,neuter,genus,gener,um,class:common,sem:[abstract],[]).
noun(3,neuter,iter,itiner,um,class:common,sem:[abstract, loc],[]).
noun(3,neuter,litus,litor,um,class:common,sem:[abstract, loc],[]).
noun(3,neuter,mare,mar,um,class:common,sem:[thing, loc],[nb:sg]).
noun(3,neuter,munus,muner,um,class:common,sem:[abstract,thing],[]).
noun(3,neuter,nomen,nomin,um,class:common,sem:[thing],[]).
noun(3,neuter,opus,oper,um,class:common,sem:[abstract,thing],[]).
noun(3,neuter,os,or,um,class:common,sem:[thing],[]).
noun(3,neuter,pectus,pector,um,class:common,sem:[thing],[]).
noun(3,neuter,scelus,sceler,um,class:common,sem:[abstract],[]).
noun(3,neuter,tegmen,tegmin,um,class:common,sem:[thing, loc],[]).
noun(3,neuter,tempora,tempor,um,class:common,sem:[thing],[nb:pl]).
noun(3,neuter,tempus,tempor,um,class:common,sem:[abstract,time],[]).


% special wordforms for MARE

lex(mari,noun,[pos:noun,txt:mari,lex:mare, case:abl, gender:neuter, class:common,number:sing,sem:[thing, loc]]).
lex(maria,noun,[pos:noun,txt:maria,lex:mare, case:or([nom,acc]), gender:neuter, class:common,number:pl,sem:[thing, loc]]).






noun(4,fem,iuuentus,iuuent,class:common,sem:[abstract, time],[]).
noun(4,fem,manus,man,class:common,sem:[thing],[]).

noun(4,masc,consulatus,consulat,class:common,sem:[abstract, time],[]).
noun(4,masc,exercitus,exercit,class:common,sem:[thing,hum, loc],[]).
noun(4,masc,fructus,fruct,class:common,sem:[thing,abstract],[]).
noun(4,masc,metus,met,class:common,sem:[abstract],[]).
noun(4,masc,spiritus,spirit,class:common,sem:[abstract],[]).
noun(4,masc,sumptus,sumpt,class:common,sem:[thing,abstract],[]).
noun(4,masc,uultus,uult,class:common,sem:[thing,abstract],[]).







noun(5,or([masc,fem]),dies,di,class:common,sem:[time, thing, abstract],[]).
noun(5,fem,fides,fid,class:common,sem:[abstract],[nb:sg]).
noun(5,fem,res,r,class:common,sem:[thing, abstract],[]).
noun(5,fem,spes,sp,class:common,sem:[abstract],[]).







%%%%%%%
% NAMES
%%%%%%%

% Gaia, Marcus, etc.  used as proper names for individuals
% therefore no plural generated

noun(1,fem,gaia,gai,class:proper,sem:[hum],[nb:sg]).

noun(1,masc,catilina,catilin,class:proper,sem:[hum],[nb:sg]).


noun(2,masc,marcus,marc,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,augustinus,augustin,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,petrus,petr,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,homerus,homer,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,catullus,catull,class:proper,sem:[hum],[nb:sg]).

noun(3,masc,caesar,caesar,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,cicero,ciceron,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,hamilcar,hamilcar,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,hannibal,hannibal,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,macro,macron,um,class:proper,sem:[hum],[nb:sg]).

noun(3,fem,uenus,uener,um,class:proper,sem:[hum],[nb:sg]).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GENERATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% An awful amount of morphological generation is involved here.
% It should be realised that while a parser is being developped, tested and debugged,
% it is far better to have an EXACT account of the information associated with each morphological variant
% and to be able to modify at will the information carried by each variant.

% The vocabulary file (called 'vocfile'),
% once sorted, gives us a very simple way of inspecting that information
% for any wordform the parser works with.

% Overgeneration is a ***sin***, comparable to allowing the grammar to parse ungrammatical strings,
% leads to spurious ambiguities and increases the cost of the parsing procedure.

% A morphological analyser is better left aside until the parsing procedures 
% are felt to be accurate and robust.
% That time is, I'm afraid, not around the corner.

% Sorting the vocfile is achieved by a call to a unix-like 'sort' procedure :
% e.g. 'sort vocfile > vf.pl'
% vf, being a Prolog file, can then be examined by Prolog calls 
% as well as inspected as the text file it is by any textual inspector program.


% pattern:

% lex(Textual_form,Record_Box_in_db,Feature_List)

%
% macro expansion
%

% These are the procedures responsible for wordform generation


% repeat-fail loop through the fail in the macroprocedures

mkthem :-
 repeat,
 macro_verb,            % 'standard' verbs
 macro_dep,		% deponent verbs
 macro_aux,             % auxiliaries
 macro_imp,             % impersonal verbs
 macro_adj_1,		% bonus
 macro_adj_1a,          % unus    (unius,uni...)
 macro_adj_2,		% fortis
 macro_adj_2a,		% ingens
 macro_adj_all,         % comparatives and superlatives
 macro_noun_1,		% regina
 macro_noun_2_masc,		% dominus
 macro_noun_2_fem,      % fagus 
 macro_noun_2_n,	% templum
 macro_noun_3_mf,	% miles
 macro_noun_3_n,	% nomen
 macro_noun_4_mf,	% exercitus
 macro_noun_5.		% res
 

%

% five verb types
%

% standard (std)
%

% amo, amas, amat
verb_endings(1, o,as,at,amus,atis,ant, are,                % indicative present / infinitive present
                em,es,et,emus,etis,ent,                    % subjunctive present
                er,eris,etur,emur,emini,entur,             % subjunctive present passive
                a,ate,                                     % imperative active
                ato,ato,atote,anto,			   % imperative future 2nd and 3rd person, sg and pl
                abam,abas,abat,abamus,abatis,abant,        % imperfect indicative
                abar,abaris,abatur,abamur,abamini,abantur, % imperfect indicative passive
                abo,abis,abit,abimus,abitis,abunt,         % future indicative
                abor,aberis,abitur,abimur,abimini,abuntur, % future indicative passive
                arem,ares,aret,aremus,aretis,arent,        % subj imperfect active
                arer,areris,aretur,aremur,aremini,arentur, % subjunctive imperfect passive
                i, isti, it, imus, istis, erunt,ere,       % perfect indicative
                isse,and,ans,ant,                          % perfect infinitive / gerund root / present participle nom / pr part root
                or,aris,atur,amur,amini,antur,ari,         % present indicative passive
                eram, eras,erat,eramus,eratis,erant,       % pluperfect indicative
                ero,eris,erit,erimus,eritis,erint,         % future perfect indicative
                erim,eris,erit,erimus,eritis,erint,        % future perfect subjunctive
                issem,isses,isset,issemus,issetis,issent). % pluperfect subjunctive

% habeo, habes, habet
verb_endings(2, o,s,t,mus,tis,nt, re,
                am,as,at,amus,atis,ant,
                ar,aris,atur,amur,amini,antur,
                '',te,
                to,to,tote,nto,
                bam,bas,bat,bamus,batis,bant,
                bar,baris,batur,bamur,bamini,bantur,
                bo,bis,bit,bimus,bitis,bunt,
                bor,beris,bitur,bimur,bimini,buntur,
                rem,res,ret,remus,retis,rent,
                rer,reris,retur,remur,remini,rentur,
                i, isti, it, imus, istis, erunt,ere,
                isse,nd,ns,nt,
                or,ris,tur,mur,mini,ntur,ri,
                eram, eras,erat,eramus,eratis,erant,
                ero,eris,erit,erimus,eritis,erint,
                erim,eris,erit,erimus,eritis,erint,
                issem,isses,isset,issemus,issetis,issent).

% mitto, mittis, mittit
verb_endings(3, o, is, it, imus, itis, unt, ere,
                am,as,at,amus,atis,ant,
                ar,aris,atur,amur,amini,antur,
                e,ite,
                ito,ito,itote,unto,
                ebam,ebas,ebat,ebamus,ebatis,ebant,
                ebar,ebaris,ebatur,ebamur,ebamini,ebantur,
                am,es,et,emus,etis,ent,
                ar,eris,etur,emur,emini,entur,
                erem,eres,eret,eremus,eretis,erent,
                erer,ereris,eretur,eremur,eremini,erentur,
                i, isti, it, imus,istis, erunt,ere,
                isse,end,ens,ent,
                or,eris,itur,imur,imini,untur,i,
                eram, eras,erat,eramus,eratis,erant,
                ero,eris,erit,erimus,eritis,erint,
                erim,eris,erit,erimus,eritis,erint,
                issem,isses,isset,issemus,issetis,issent).

% insanio, insanis, insanit
verb_endings(4, o,s,t,mus,tis,unt,re,
                am,as,at,amus,atis,ant,
                 ar,aris,atur,amur,amini,antur,
                '',te,
                 to,to,tote,unto,
                ebam,ebas,ebat,ebamus,ebatis,ebant,
                ebar,ebaris,ebatur,ebamur,ebamini,ebantur,
                am,es,et,emus,etis,ent,
                ar,eris,etur,emur,emini,entur,
                rem,res,ret,remus,retis,rent,
                rer,reris,retur,remur,remini,rentur,
                i,isti,it,imus,istis,erunt,ere,
                isse,end,ens,ent,
                or,ris,tur,mur,mini,untur,ri,
                eram, eras,erat,eramus,eratis,erant,
                ero,eris,erit,erimus,eritis,erint,
                erim,eris,erit,erimus,eritis,erint,
                issem,isses,isset,issemus,issetis,issent).

% capio, capis, capit
verb_endings(5, io,is,it,imus,itis,iunt,ere,
                iam,ias,iat,iamus,iatis,iant,
                iar,iaris,iatur,iamur,iamini,iantur,
                i,ite,
                ito,ito,itote,iunto,
                iebam,iebas,iebat,iebamus,iebatis,iebant,
                iebar,iebaris,iebatur,iebamur,iebamini,iebantur,
                iam,ies,iet,iemus,ietis,ient,
                iar,ieris,ietur,iemur,iemini,ientur,
                erem,eres,eret,eremus,eretis,erent,
                erer,ereris,eretur,eremur,eremini,erentur,
                i,isti,it,imus,istis,erunt,ere,
                isse,iend,iens,ient,
                ior,eris,itur,imur,imini,iuntur,i,
                eram, eras,erat,eramus,eratis,erant,
                ero,eris,erit,erimus,eritis,erint,
                erim,eris,erit,erimus,eritis,erint,
                issem,isses,isset,issemus,issetis,issent).

% deponent (dep)
%

% hortor
verb_endings_d(1, or,aris,atur,amur,amini,antur,                  % present indicative
                  er,eris,etur,emur,emini,entur,                  % present subjunctive
                  abar,abaris,abatur,abamur,abamini,abantur,      % imperfect indicative
                  abor,aberis,abitur,abimur,abimini,abuntur,      % future indicative
                  arer,areris,aretur,aremur,aremini,arentur,      % subjunctive imperfect
                  ari,and,ans,ant,                                % infinitive / gerund root / present part nom / pr part root
                  are,amini).					  % imperative

% uereor
verb_endings_d(2, or,ris,tur,mur,mini,ntur,
                  ar,aris,atur,amur,amini,antur,
                  bar,baris,batur,bamur,bamini,bantur,
                  bor,beris,bitur,bimur,bimini,buntur,
                  rer,reris,retur,remur,remini,rentur,
                  ri,nd,ns,nt,
                  re,mini).

% sequor
verb_endings_d(3, or,eris,itur,imur,imini,untur,
                  ar,aris,atur,amur,amini,antur,
                  ebar,ebaris,ebatur,ebamur,ebamini,ebantur,
                  ar,eris,etur,emur,emini,entur,
                  erer,ereris,eretur,eremur,eremini,erentur,
                  i,end,ens,ent,
                  ere,imini).

% largior
verb_endings_d(4, or,ris,tur,mur,mini,untur,
                  ar,aris,atur,amur,amini,antur,
                  ebar,ebaris,ebatur,ebamur,ebamini,ebantur,
                  ar,eris,etur,emur,emini,entur,
                  rer,reris,retur,remur,remini,rentur,
                  ri,end,ens,ent,
                  re,mini).

% patior
verb_endings_d(5,ior,eris,itur,imur,imini,iuntur,
                 iar,iaris,iatur,iamur,iamini,iantur,
                 iebar,iebaris,iebatur,iebamur,iebamini,iebantur,
                 iar,ieris,ietur,iemur,iemini,ientur,
                 erer,ereris,eretur,eremur,eremini,erentur,
                 i,iend,iens,ient,
                 ere,imini).

% auxiliaries and impersonal
%

% most of their conjugated forms are entered as such in the verb entry fed to lex

% fui, fuisti, fuit, etc
verb_endings(aux,
             i,isti,it,imus,istis,erunt,ere,      % perfect indicative
             isse,				  % perfect infinitive
             eram, eras,erat,eramus,eratis,erant,       % pluperfect indicative
             ero,eris,erit,erimus,eritis,erint,         % future perfect indicative
             erim,eris,erit,erimus,eritis,erint,        % future perfect subjunctive
             issem,isses,isset,issemus,issetis,issent). % pluperfect subjunctive

% decuit, decuisse, etc.
verb_endings(imp,
             it,isse,erat,erit,isset). % impersonal verbs

%
% MACROS
%

macro_verb :-

verb([v(Lex,Conj,RootPr,RootPft,RootSup)],Class,std),

verb_endings(Conj,
               Pr1sg,Pr2sg,Pr3sg,Pr1pl,Pr2pl,Pr3pl,
               PrInf,
               Pr1sgsubj,Pr2sgsubj,Pr3sgsubj,Pr1plsubj,Pr2plsubj,Pr3plsubj,
               PPr1sgsubj,PPr2sgsubj,PPr3sgsubj,PPr1plsubj,PPr2plsubj,PPr3plsubj,
               Imp2sg,Imp2pl,
               Imp2sgfut,Imp3sgfut,Imp2plfut,Imp3plfut,
               Pr1sgimpft,Pr2sgimpft,Pr3sgimpft,Pr1plimpft,Pr2plimpft,Pr3plimpft,
               PPr1sgimpft,PPr2sgimpft,PPr3sgimpft,PPr1plimpft,PPr2plimpft,PPr3plimpft,
               Pr1sgfut,Pr2sgfut,Pr3sgfut,Pr1plfut,Pr2plfut,Pr3plfut,
               PPr1sgfut,PPr2sgfut,PPr3sgfut,PPr1plfut,PPr2plfut,PPr3plfut,
               Pr1sgimpfts,Pr2sgimpfts,Pr3sgimpfts,Pr1plimpfts,Pr2plimpfts,Pr3plimpfts,
               Pr1sgimpftsp,Pr2sgimpftsp,Pr3sgimpftsp,Pr1plimpftsp,Pr2plimpftsp,Pr3plimpftsp,
               Pft1sg,Pft2sg,Pft3sg,Pft1pl,Pft2pl,Pft3pl,Pft3plbis,
               PftInf,Ger,PrParts,PrPart,
               Prp1sg,Prp2sg,Prp3sg,Prp1pl,Prp2pl,Prp3pl,
               PrPInf,
               PluPft1sg,PluPft2sg,PluPft3sg,PluPft1pl,PluPft2pl,PluPft3pl,
               FPft1sg,FPft2sg,FPft3sg,FPft1pl,FPft2pl,FPft3pl,
               Pft1sgs,Pft2sgs,Pft3sgs,Pft1pls,Pft2pls,Pft3pls,
               PluPft1sgs,PluPft2sgs,PluPft3sgs,PluPft1pls,PluPft2pls,PluPft3pls),


% first declension adj endings used for past participles and gerundives

declension_endings(adj,1,[_,Acmsg,Genmsg,Datmsg,Ablmsg,
                  Nfsg,Acfsg,Genfsg,Datfsg,Ablfsg,
                  Nnsg,Acnsg,Gennsg,Datnsg,Ablnsg,
                  Nmpl,Acmpl,Genmpl,Datmpl,Ablmpl,
                  Nfpl,Acfpl,Genfpl,Datfpl,Ablfpl,
                  Nnpl,Acnpl,Gennpl,Datnpl,Ablnpl]),

% for present participles

declension_endings(partpr,[Accmfsg2,Geng3sg2,Datg3sg2,Abl1g3sg2,Abl2g3sg2,
                           Nommfpl2,Accmfpl2,Nomnpl2,Accnpl2,
                           Geng3pl2, Datg3pl2,Ablg3pl2]),


ifthen(RootPr \= xxx,                                        % the verb does have the required root
							     % we use xxx throughout to indicate absence of a root

% present participles

(atom_concat(RootPr,PrParts,PrPartB),  % am+ans -> amans, etc
atom_concat(RootPr,PrPart,PrPartA),  % am+ant- -> amant-, etc

% PrPartB (amans, etc) : Nominative masc fem neuter sg + acc neuter sg

atom_concat(PrPartA,Accmfsg2,PrPAccmfsg),      % acc masc fem sg
atom_concat(PrPartA,Geng3sg2,PrPGeng3sg),    % gen 3 genders sg
atom_concat(PrPartA,Datg3sg2,PrPDatg3sg),    % dat 3 genders sg
atom_concat(PrPartA,Abl1g3sg2,PrPAbl1g3sg),     % i-abl 3 genders sg
atom_concat(PrPartA,Abl2g3sg2,PrPAbl2g3sg),           % e-abl 3 genders sg

atom_concat(PrPartA,Nommfpl2,PrPNommfpl),
atom_concat(PrPartA,Accmfpl2,PrPAccmfpl),
atom_concat(PrPartA,Nomnpl2,PrPNomnpl),
atom_concat(PrPartA,Accnpl2,PrPAccnpl),
atom_concat(PrPartA,Geng3pl2,PrPGeng3pl),
atom_concat(PrPartA,Datg3pl2,PrPDatg3pl),
atom_concat(PrPartA,Ablg3pl2,PrPAblg3pl),

% indicative present active

atom_concat(RootPr,Pr1sg,V1sg),
atom_concat(RootPr,Pr2sg,V2sg),
atom_concat(RootPr,Pr3sg,V3sg),
atom_concat(RootPr,Pr1pl,V1pl),
atom_concat(RootPr,Pr2pl,V2pl),
atom_concat(RootPr,Pr3pl,V3pl),

% infinitive present active

atom_concat(RootPr,PrInf,Inf),

% subjunctive present active

atom_concat(RootPr,Pr1sgsubj,V1sgsubj),
atom_concat(RootPr,Pr2sgsubj,V2sgsubj),
atom_concat(RootPr,Pr3sgsubj,V3sgsubj),
atom_concat(RootPr,Pr1plsubj,V1plsubj),
atom_concat(RootPr,Pr2plsubj,V2plsubj),
atom_concat(RootPr,Pr3plsubj,V3plsubj),

% indicative imperfect active

atom_concat(RootPr,Pr1sgimpft,V1sgimpft),
atom_concat(RootPr,Pr2sgimpft,V2sgimpft),
atom_concat(RootPr,Pr3sgimpft,V3sgimpft),
atom_concat(RootPr,Pr1plimpft,V1plimpft),
atom_concat(RootPr,Pr2plimpft,V2plimpft),
atom_concat(RootPr,Pr3plimpft,V3plimpft),

% subjunctive imperfect active

atom_concat(RootPr,Pr1sgimpfts,V1sgimpfts),
atom_concat(RootPr,Pr2sgimpfts,V2sgimpfts),
atom_concat(RootPr,Pr3sgimpfts,V3sgimpfts),
atom_concat(RootPr,Pr1plimpfts,V1plimpfts),
atom_concat(RootPr,Pr2plimpfts,V2plimpfts),
atom_concat(RootPr,Pr3plimpfts,V3plimpfts),

% indicative futur active

atom_concat(RootPr,Pr1sgfut,V1sgfut),
atom_concat(RootPr,Pr2sgfut,V2sgfut),
atom_concat(RootPr,Pr3sgfut,V3sgfut),
atom_concat(RootPr,Pr1plfut,V1plfut),
atom_concat(RootPr,Pr2plfut,V2plfut),
atom_concat(RootPr,Pr3plfut,V3plfut),

% imperative present active

atom_concat(RootPr,Imp2sg,V2sgimp),
atom_concat(RootPr,Imp2pl,V2plimp),

% imperative future

atom_concat(RootPr,Imp2sgfut,V2sgimpfut),
atom_concat(RootPr,Imp2plfut,V2plimpfut),
atom_concat(RootPr,Imp3sgfut,V3sgimpfut),
atom_concat(RootPr,Imp3plfut,V3plimpfut),

% gerunds

atom_concat(RootPr,Ger,Gerbase),
atom_concat(Gerbase,um,Gerundum),
atom_concat(Gerbase,i,Gerundum_gen),
atom_concat(Gerbase,o,Gerundum_abl),

% third person middle or passive : monetur, insanitur, etc.
%

atom_concat(RootPr,Prp3sg,V3Psg),			% insanitur
atom_concat(RootPr,PPr3sgsubj,PV3sgsubj),		% insaniatur
atom_concat(RootPr,PPr3sgimpft,PV3sgimpft),		% insaniebatur
atom_concat(RootPr,PPr3sgfut,PV3sgfut))),                 % insanietur


% infinitive present passive
%

atom_concat(RootPr,PrPInf,PInf),        % amari true passive but also middle : pugnari (coeptum est)



ifthen(RootPft \= xxx,   				% we do have a 'perfect' root
							% remember that xxx is used when the corresponding root is missing 
							% or shouldn't be used - study the case of coepi

% perfect active

(atom_concat(RootPft,Pft1sg,V1sgpft),
atom_concat(RootPft,Pft2sg,V2sgpft),
atom_concat(RootPft,Pft3sg,V3sgpft),
atom_concat(RootPft,Pft1pl,V1plpft),
atom_concat(RootPft,Pft2pl,V2plpft),
atom_concat(RootPft,Pft3pl,V3plpft),
atom_concat(RootPft,Pft3plbis,V3plpftbis),

atom_concat(RootPft,PftInf,Infp),

% indicative pluperfect active

atom_concat(RootPft,PluPft1sg,PluV1sgpft),
atom_concat(RootPft,PluPft2sg,PluV2sgpft),
atom_concat(RootPft,PluPft3sg,PluV3sgpft),
atom_concat(RootPft,PluPft1pl,PluV1plpft),
atom_concat(RootPft,PluPft2pl,PluV2plpft),
atom_concat(RootPft,PluPft3pl,PluV3plpft),

% future perfect active

atom_concat(RootPft,FPft1sg,FV1sgpft),
atom_concat(RootPft,FPft2sg,FV2sgpft),
atom_concat(RootPft,FPft3sg,FV3sgpft),
atom_concat(RootPft,FPft1pl,FV1plpft),
atom_concat(RootPft,FPft2pl,FV2plpft),
atom_concat(RootPft,FPft3pl,FV3plpft),

% subjunctive perfect active

atom_concat(RootPft,Pft1sgs,V1sgpfts),
atom_concat(RootPft,Pft2sgs,V2sgpfts),
atom_concat(RootPft,Pft3sgs,V3sgpfts),
atom_concat(RootPft,Pft1pls,V1plpfts),
atom_concat(RootPft,Pft2pls,V2plpfts),
atom_concat(RootPft,Pft3pls,V3plpfts),

% subjunctive pluperfect active

atom_concat(RootPft,PluPft1sgs,PluV1sgpfts),
atom_concat(RootPft,PluPft2sgs,PluV2sgpfts),
atom_concat(RootPft,PluPft3sgs,PluV3sgpfts),
atom_concat(RootPft,PluPft1pls,PluV1plpfts),
atom_concat(RootPft,PluPft2pls,PluV2plpfts),
atom_concat(RootPft,PluPft3pls,PluV3plpfts)  )),

ifthen(RootSup \= xxx,					% we have a supine
(atom_concat(RootSup,um,Supin),
 atom_concat(RootSup,ur,PartFut),

% future participle

atom_concat(PartFut,us,PFFNmsg),
atom_concat(PartFut,Acmsg,PFFAcmsg),
atom_concat(PartFut,Genmsg,PFFGenmsg),
atom_concat(PartFut,Datmsg,PFFDatmsg),
atom_concat(PartFut,Ablmsg,PFFAblmsg),
atom_concat(PartFut,Nfsg,PFFNfsg),
atom_concat(PartFut,Acfsg,PFFAcfsg),
atom_concat(PartFut,Genfsg,PFFGenfsg),
atom_concat(PartFut,Datfsg,PFFDatfsg),
atom_concat(PartFut,Ablfsg,PFFAblfsg),
atom_concat(PartFut,Nnsg,PFFNnsg),
atom_concat(PartFut,Acnsg,PFFAcnsg),
atom_concat(PartFut,Gennsg,PFFGennsg),
atom_concat(PartFut,Datnsg,PFFDatnsg),
atom_concat(PartFut,Ablnsg,PFFAblnsg),
atom_concat(PartFut,Nmpl,PFFNmpl),
atom_concat(PartFut,Acmpl,PFFAcmpl),
atom_concat(PartFut,Genmpl,PFFGenmpl),
atom_concat(PartFut,Datmpl,PFFDatmpl),
atom_concat(PartFut,Ablmpl,PFFAblmpl),
atom_concat(PartFut,Nfpl,PFFNfpl),
atom_concat(PartFut,Acfpl,PFFAcfpl),
atom_concat(PartFut,Genfpl,PFFGenfpl),
atom_concat(PartFut,Datfpl,PFFDatfpl),
atom_concat(PartFut,Ablfpl,PFFAblfpl),
atom_concat(PartFut,Nnpl,PFFNnpl),
atom_concat(PartFut,Acnpl,PFFAcnpl),
atom_concat(PartFut,Gennpl,PFFGennpl),
atom_concat(PartFut,Datnpl,PFFDatnpl),
atom_concat(PartFut,Ablnpl,PFFAblnpl) )),

%
ifthen( (Class=tr_cod;Class=tr_cod_coi;Class=tr_cod_cplt;
         Class=tr_cod_prep_cplt;Class=tr_inf),
                                          % transitive verbs, the only ones to sport gerundives
					% not to be included tr_cod_dat (studere), tr_cod_abl (uti)

% GERUNDIVES
                                          % listing of transitive classes to be revised regularly !!!!!!
% THEN-CLAUSE
(  ifthen(RootPr \= xxx,
(atom_concat(Gerbase,us,FNmsg),
atom_concat(Gerbase,Acmsg,FAcmsg),
atom_concat(Gerbase,Genmsg,FGenmsg),
atom_concat(Gerbase,Datmsg,FDatmsg),
atom_concat(Gerbase,Ablmsg,FAblmsg),
atom_concat(Gerbase,Nfsg,FNfsg),
atom_concat(Gerbase,Acfsg,FAcfsg),
atom_concat(Gerbase,Genfsg,FGenfsg),
atom_concat(Gerbase,Datfsg,FDatfsg),
atom_concat(Gerbase,Ablfsg,FAblfsg),
atom_concat(Gerbase,Nnsg,FNnsg),
atom_concat(Gerbase,Acnsg,FAcnsg),
atom_concat(Gerbase,Gennsg,FGennsg),
atom_concat(Gerbase,Datnsg,FDatnsg),
atom_concat(Gerbase,Ablnsg,FAblnsg),
atom_concat(Gerbase,Nmpl,FNmpl),
atom_concat(Gerbase,Acmpl,FAcmpl),
atom_concat(Gerbase,Genmpl,FGenmpl),
atom_concat(Gerbase,Datmpl,FDatmpl),
atom_concat(Gerbase,Ablmpl,FAblmpl),
atom_concat(Gerbase,Nfpl,FNfpl),
atom_concat(Gerbase,Acfpl,FAcfpl),
atom_concat(Gerbase,Genfpl,FGenfpl),
atom_concat(Gerbase,Datfpl,FDatfpl),
atom_concat(Gerbase,Ablfpl,FAblfpl),
atom_concat(Gerbase,Nnpl,FNnpl),
atom_concat(Gerbase,Acnpl,FAcnpl),
atom_concat(Gerbase,Gennpl,FGennpl),
atom_concat(Gerbase,Datnpl,FDatnpl),
atom_concat(Gerbase,Ablnpl,FAblnpl),

% SING

% nominative masc sing
asserta(lex(FNmsg,gdiv,[pos:gdiv,txt:FNmsg ,case:nom, gender:masc, number:sing,lex:Lex,
            class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),

% accusative masc sing
asserta(lex(FAcmsg,gdiv,[pos:gdiv,txt:FAcmsg , case:acc, gender:masc, number:sing,lex:Lex,
            class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% genitive masc sing
asserta(lex(FGenmsg,gdiv,[pos:gdiv,txt:FGenmsg , case:gen, gender:masc, number:sing,lex:Lex,
           class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% dative masc sing
asserta(lex(FDatmsg,gdiv,[pos:gdiv,txt:FDatmsg , case:dat, gender:masc, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% ablative masc sing
asserta(lex(FAblmsg,gdiv,[pos:gdiv,txt:FAblmsg , case:abl, gender:masc, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),

% nominative fem sing
asserta(lex(FNfsg,gdiv,[pos:gdiv,txt:FNfsg , case:nom, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% accusative fem sing
asserta(lex(FAcfsg,gdiv,[pos:gdiv,txt:FAcfsg , case:acc, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% genitive fem sing
asserta(lex(FGenfsg,gdiv,[pos:gdiv,txt:FGenfsg , case:gen, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% dative fem sing
asserta(lex(FDatfsg,gdiv,[pos:gdiv,txt:FDatfsg , case:dat, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% ablative fem sing
asserta(lex(FAblfsg,gdiv,[pos:gdiv,txt:FAblfsg , case:abl, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),

% nominative n sing
asserta(lex(FNnsg,gdiv,[pos:gdiv,txt:FNnsg , case:nom, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% accusative n sing
asserta(lex(FAcnsg,gdiv,[pos:gdiv,txt:FAcnsg , case:acc, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% genitive n sing
asserta(lex(FGennsg,gdiv,[pos:gdiv,txt:FGennsg , case:gen, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% dative n sing
asserta(lex(FDatnsg,gdiv,[pos:gdiv,txt:FDatnsg , case:dat, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% ablative n sing
asserta(lex(FAblnsg,gdiv,[pos:gdiv,txt:FAblnsg , case:abl, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),

% PLURAL

% nominative masc pl
asserta(lex(FNmpl,gdiv,[pos:gdiv,txt:FNmpl , case:nom, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% accusative masc pl
asserta(lex(FAcmpl,gdiv,[pos:gdiv,txt:FAcmpl , case:acc, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% genitive masc pl
asserta(lex(FGenmpl,gdiv,[pos:gdiv,txt:FGenmpl , case:gen, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% dative masc pl
asserta(lex(FDatmpl,gdiv,[pos:gdiv,txt:FDatmpl , case:dat, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% ablative masc pl
asserta(lex(FAblmpl,gdiv,[pos:gdiv,txt:FAblmpl , case:abl, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),

% nominative fem pl
asserta(lex(FNfpl,gdiv,[pos:gdiv,txt:FNfpl , case:nom, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% accusative fem pl
asserta(lex(FAcfpl,gdiv,[pos:gdiv,txt:FAcfpl , case:acc, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% genitive fem pl
asserta(lex(FGenfpl,gdiv,[pos:gdiv,txt:FGenfpl , case:gen, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% dative fem pl
asserta(lex(FDatfpl,gdiv,[pos:gdiv,txt:FDatfpl , case:dat, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% ablative fem pl
asserta(lex(FAblfpl,gdiv,[pos:gdiv,txt:FAblfpl , case:abl, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),

% nominative n pl
asserta(lex(FNnpl,gdiv,[pos:gdiv,txt:FNnpl , case:nom, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% accusative n pl
asserta(lex(FAcnpl,gdiv,[pos:gdiv,txt:FAcnpl , case:acc, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% genitive n pl
asserta(lex(FGennpl,gdiv,[pos:gdiv,txt:FGennpl , case:gen, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% dative n pl
asserta(lex(FDatnpl,gdiv,[pos:gdiv,txt:FDatnpl , case:dat, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),
% ablative n pl
asserta(lex(FAblnpl,gdiv,[pos:gdiv,txt:FAblnpl , case:abl, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:Kind,mood:gerund,person:3])),


% indicative present passive
%
atom_concat(RootPr,Prp1sg,V1Psg),
atom_concat(RootPr,Prp2sg,V2Psg),

% third person : see above (also necessary for intransitives on account of middle voice)

atom_concat(RootPr,Prp1pl,V1Ppl),
atom_concat(RootPr,Prp2pl,V2Ppl),
atom_concat(RootPr,Prp3pl,V3Ppl),

% indicative present passive voice
%

% third person singular present tense
asserta(lex(V3Psg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:V3Psg ,tense:present,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl present tense
asserta(lex(V3Ppl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:V3Ppl ,tense:present,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular present tense
asserta(lex(V1Psg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:V1Psg ,tense:present,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular present tense
asserta(lex(V2Psg,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:V2Psg ,tense:present,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural present tense
asserta(lex(V1Ppl,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:V1Ppl ,tense:present,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural present tense
asserta(lex(V2Ppl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:V2Ppl ,tense:present,kind:Kind,mood:indicative,
                   number:pl,person:2])),


% subjunctive present passive

atom_concat(RootPr,PPr1sgsubj,PV1sgsubj),
atom_concat(RootPr,PPr2sgsubj,PV2sgsubj),

% third person : see above - also necessary for intransitives because of middle voice

atom_concat(RootPr,PPr1plsubj,PV1plsubj),
atom_concat(RootPr,PPr2plsubj,PV2plsubj),
atom_concat(RootPr,PPr3plsubj,PV3plsubj),

% subjunctive present passive
%

% third person singular present tense subj pass
asserta(lex(PV3sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV3sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),

% third person pl present tense subj pass
asserta(lex(PV3plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV3plsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular present tense subj pass
asserta(lex(PV1sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV1sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),

% second person singular present tense subj pass
asserta(lex(PV2sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV2sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),

% first person plural present tense subj pass
asserta(lex(PV1plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV1plsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),

% second person plural present tense subj pass
asserta(lex(PV2plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV2plsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:pl,person:2])),




% subjunctive imperfect passive

atom_concat(RootPr,Pr1sgimpftsp,V1sgimpftsp),
atom_concat(RootPr,Pr2sgimpftsp,V2sgimpftsp),
atom_concat(RootPr,Pr3sgimpftsp,V3sgimpftsp),
atom_concat(RootPr,Pr1plimpftsp,V1plimpftsp),
atom_concat(RootPr,Pr2plimpftsp,V2plimpftsp),
atom_concat(RootPr,Pr3plimpftsp,V3plimpftsp),

% indicative imperfect passive

atom_concat(RootPr,PPr1sgimpft,PV1sgimpft),
atom_concat(RootPr,PPr2sgimpft,PV2sgimpft),

% third person sg see above - necessary for middle

atom_concat(RootPr,PPr1plimpft,PV1plimpft),
atom_concat(RootPr,PPr2plimpft,PV2plimpft),
atom_concat(RootPr,PPr3plimpft,PV3plimpft),


% indicative imperfect passive voice
%


% third person singular  impft
asserta(lex(PV3sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV3sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl impft
asserta(lex(PV3plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV3plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular impft
asserta(lex(PV1sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV1sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular  impft
asserta(lex(PV2sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV2sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural  impft
asserta(lex(PV1plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV1plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural impft
asserta(lex(PV2plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV2plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:pl,person:2])),


% subjunctive imperfect passive
%

% third person singular  impft
asserta(lex(V3sgimpftsp,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:V3sgimpftsp ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),

% third person pl impft
asserta(lex(V3plimpftsp,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:V3plimpftsp ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular impft
asserta(lex(V1sgimpftsp,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:V1sgimpftsp ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),

% second person singular  impft
asserta(lex(V2sgimpftsp,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:V2sgimpftsp ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),

% first person plural  impft
asserta(lex(V1plimpftsp,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:V1plimpftsp ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),

% second person plural impft
asserta(lex(V2plimpftsp,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:V2plimpftsp ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:2])),

% indicative futur passive

atom_concat(RootPr,PPr1sgfut,PV1sgfut),
atom_concat(RootPr,PPr2sgfut,PV2sgfut),

% third person - see above

atom_concat(RootPr,PPr1plfut,PV1plfut),
atom_concat(RootPr,PPr2plfut,PV2plfut),
atom_concat(RootPr,PPr3plfut,PV3plfut),

% infinitive present passive
asserta(lex(PInf,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:pass,txt:PInf ,tense:present,kind:Kind,mood:infinitive,
                   number:Nb,person:P])),


% indicative future passive
%

% third person singular  fut pass
asserta(lex(PV3sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV3sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl fut pass
asserta(lex(PV3plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV3plfut ,tense:future,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular fut pass
asserta(lex(PV1sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV1sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular  fut pass
asserta(lex(PV2sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV2sgfut ,tense:future,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural  fut pass
asserta(lex(PV1plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:pass,txt:PV1plfut ,tense:future,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural fut pass
asserta(lex(PV2plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:pass,txt:PV2plfut ,tense:future,kind:Kind,mood:indicative,
                   number:pl,person:2])) )),

% PAST PARTICIPLES
%
ifthen(RootSup \= xxx,

(atom_concat(RootSup,us,PPFNmsg),
atom_concat(RootSup,Acmsg,PPFAcmsg),
atom_concat(RootSup,Genmsg,PPFGenmsg),
atom_concat(RootSup,Datmsg,PPFDatmsg),
atom_concat(RootSup,Ablmsg,PPFAblmsg),
atom_concat(RootSup,Nfsg,PPFNfsg),
atom_concat(RootSup,Acfsg,PPFAcfsg),
atom_concat(RootSup,Genfsg,PPFGenfsg),
atom_concat(RootSup,Datfsg,PPFDatfsg),
atom_concat(RootSup,Ablfsg,PPFAblfsg),
atom_concat(RootSup,Nnsg,PPFNnsg),
atom_concat(RootSup,Acnsg,PPFAcnsg),
atom_concat(RootSup,Gennsg,PPFGennsg),
atom_concat(RootSup,Datnsg,PPFDatnsg),
atom_concat(RootSup,Ablnsg,PPFAblnsg),
atom_concat(RootSup,Nmpl,PPFNmpl),
atom_concat(RootSup,Acmpl,PPFAcmpl),
atom_concat(RootSup,Genmpl,PPFGenmpl),
atom_concat(RootSup,Datmpl,PPFDatmpl),
atom_concat(RootSup,Ablmpl,PPFAblmpl),
atom_concat(RootSup,Nfpl,PPFNfpl),
atom_concat(RootSup,Acfpl,PPFAcfpl),
atom_concat(RootSup,Genfpl,PPFGenfpl),
atom_concat(RootSup,Datfpl,PPFDatfpl),
atom_concat(RootSup,Ablfpl,PPFAblfpl),
atom_concat(RootSup,Nnpl,PPFNnpl),
atom_concat(RootSup,Acnpl,PPFAcnpl),
atom_concat(RootSup,Gennpl,PPFGennpl),
atom_concat(RootSup,Datnpl,PPFDatnpl),
atom_concat(RootSup,Ablnpl,PPFAblnpl),

% SING

% nominative masc sing
asserta(lex(PPFNmsg,p_p,[pos:p_p,txt:PPFNmsg ,case:nom, gender:masc, number:sing,lex:Lex,
            class:Class,type:p_p,kind:Kind,mood:participle,person:3])),

% accusative masc sing
asserta(lex(PPFAcmsg,p_p,[pos:p_p,txt:PPFAcmsg , case:acc, gender:masc, number:sing,lex:Lex,
            class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% genitive masc sing
asserta(lex(PPFGenmsg,p_p,[pos:p_p,txt:PPFGenmsg , case:gen, gender:masc, number:sing,lex:Lex,
           class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% dative masc sing
asserta(lex(PPFDatmsg,p_p,[pos:p_p,txt:PPFDatmsg , case:dat, gender:masc, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% ablative masc sing
asserta(lex(PPFAblmsg,p_p,[pos:p_p,txt:PPFAblmsg , case:abl, gender:masc, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),

% nominative fem sing
asserta(lex(PPFNfsg,p_p,[pos:p_p,txt:PPFNfsg , case:nom, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% accusative fem sing
asserta(lex(PPFAcfsg,p_p,[pos:p_p,txt:PPFAcfsg , case:acc, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% genitive fem sing
asserta(lex(PPFGenfsg,p_p,[pos:p_p,txt:PPFGenfsg , case:gen, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% dative fem sing
asserta(lex(PPFDatfsg,p_p,[pos:p_p,txt:PPFDatfsg , case:dat, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% ablative fem sing
asserta(lex(PPFAblfsg,p_p,[pos:p_p,txt:PPFAblfsg , case:abl, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),


% nominative n sing
asserta(lex(PPFNnsg,p_p,[pos:p_p,txt:PPFNnsg , case:nom, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% accusative n sing
asserta(lex(PPFAcnsg,p_p,[pos:p_p,txt:PPFAcnsg , case:acc, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% genitive n sing
asserta(lex(PPFGennsg,p_p,[pos:p_p,txt:PPFGennsg , case:gen, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% dative n sing
asserta(lex(PPFDatnsg,p_p,[pos:p_p,txt:PPFDatnsg , case:dat, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% ablative n sing
asserta(lex(PPFAblnsg,p_p,[pos:p_p,txt:PPFAblnsg , case:abl, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),

% PLURAL

% nominative masc pl
asserta(lex(PPFNmpl,p_p,[pos:p_p,txt:PPFNmpl , case:nom, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% accusative masc pl
asserta(lex(PPFAcmpl,p_p,[pos:p_p,txt:PPFAcmpl , case:acc, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% genitive masc pl
asserta(lex(PPFGenmpl,p_p,[pos:p_p,txt:PPFGenmpl , case:gen, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% dative masc pl
asserta(lex(PPFDatmpl,p_p,[pos:p_p,txt:PPFDatmpl , case:dat, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% ablative masc pl
asserta(lex(PPFAblmpl,p_p,[pos:p_p,txt:PPFAblmpl , case:abl, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),

% nominative fem pl
asserta(lex(PPFNfpl,p_p,[pos:p_p,txt:PPFNfpl , case:nom, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% accusative fem pl
asserta(lex(PPFAcfpl,p_p,[pos:p_p,txt:PPFAcfpl , case:acc, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% genitive fem pl
asserta(lex(PPFGenfpl,p_p,[pos:p_p,txt:PPFGenfpl , case:gen, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% dative fem pl
asserta(lex(PPFDatfpl,p_p,[pos:p_p,txt:PPFDatfpl , case:dat, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% ablative fem pl
asserta(lex(PPFAblfpl,p_p,[pos:p_p,txt:PPFAblfpl , case:abl, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),

% nominative n pl
asserta(lex(PPFNnpl,p_p,[pos:p_p,txt:PPFNnpl , case:nom, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% accusative n pl
asserta(lex(PPFAcnpl,p_p,[pos:p_p,txt:PPFAcnpl , case:acc, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% genitive n pl
asserta(lex(PPFGennpl,p_p,[pos:p_p,txt:PPFGennpl , case:gen, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% dative n pl
asserta(lex(PPFDatnpl,p_p,[pos:p_p,txt:PPFDatnpl , case:dat, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])),
% ablative n pl
asserta(lex(PPFAblnpl,p_p,[pos:p_p,txt:PPFAblnpl , case:abl, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_p,kind:Kind,mood:participle,person:3])) ))

 )), % END THEN-CLAUSE

% END OF TREATMENT OF TRANSITIVE VERBS ALONE

%

ifthen(RootPr \= xxx,


% third person middle
%

% third person singular present tense
(asserta(lex(V3Psg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:middle,txt:V3Psg ,tense:present,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person singular present tense subj
asserta(lex(PV3sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:middle,txt:PV3sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),

% third person singular future tense
asserta(lex(PV3sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:middle,txt:PV3sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:sing,person:3])),


% third person singular imperfect
asserta(lex(PV3sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:middle,txt:PV3sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% present infinitive middle (also intrans : pugnari (coeptum est)

asserta(lex(PInf,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:middle,txt:PInf ,tense:present,kind:Kind,mood:infinitive,
                   number:Nb,person:P])),




atom_concat(Gerbase,um,Gerundum),         % amandum
atom_concat(Gerbase,i,Gerundum_gen),      % amandi
atom_concat(Gerbase,o,Gerundum_abl),      % amando

% gerund -UM
asserta(lex(Gerundum,v,[pos:v,class:Class,type:gerund,lex:Lex,
                  txt:Gerundum, kind:Kind,mood:gerund,
                  person:3,case:acc])),

% genitive gerund -I
asserta(lex(Gerundum_gen,v,[pos:v,class:Class,type:gerund,lex:Lex,
                  txt:Gerundum_gen ,kind:Kind,mood:gerund,
                  person:3,case:gen])),
% dative abl -O
asserta(lex(Gerundum_abl,v,[pos:v,class:Class,type:gerund,lex:Lex,
                  txt:Gerundum_abl ,kind:Kind,mood:gerund,
                  person:3,case:abl])),


% indicative present tense active
%

% third person singular present tense
asserta(lex(V3sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sg ,tense:present,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl present tense
asserta(lex(V3pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3pl ,tense:present,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular present tense
asserta(lex(V1sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sg ,tense:present,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular present tense
asserta(lex(V2sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sg ,tense:present,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural present tense
asserta(lex(V1pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1pl ,tense:present,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural present tense
asserta(lex(V2pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2pl ,tense:present,kind:Kind,mood:indicative,
                   number:pl,person:2])),

% subjunctive present active
%

% third person singular present tense subj
asserta(lex(V3sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),

% third person pl present tense subj
asserta(lex(V3plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular present tense subj
asserta(lex(V1sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),

% second person singular present tense subj
asserta(lex(V2sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),

% first person plural present tense subj
asserta(lex(V1plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),

% second person plural present tense subj
asserta(lex(V2plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:pl,person:2])),

% indicative imperfect active
%

% third person singular  impft
asserta(lex(V3sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl impft
asserta(lex(V3plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular impft
asserta(lex(V1sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular  impft
asserta(lex(V2sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural  impft
asserta(lex(V1plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural impft
asserta(lex(V2plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:pl,person:2])),


% subjunctive imperfect active
%

% third person singular  impft
asserta(lex(V3sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),

% third person pl impft
asserta(lex(V3plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular impft
asserta(lex(V1sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),

% second person singular  impft
asserta(lex(V2sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),

% first person plural  impft
asserta(lex(V1plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),

% second person plural impft
asserta(lex(V2plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:2])),




% indicative future active
%

% third person singular  fut
asserta(lex(V3sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl fut
asserta(lex(V3plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plfut ,tense:future,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular fut
asserta(lex(V1sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular  fut
asserta(lex(V2sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgfut ,tense:future,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural  fut
asserta(lex(V1plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plfut ,tense:future,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural fut
asserta(lex(V2plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plfut ,tense:future,kind:Kind,mood:indicative,
                   number:pl,person:2])),


% imperative present active
%

% second person singular present tense imp
asserta(lex(V2sgimp,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimp ,tense:present,kind:Kind,mood:imperative,
                   number:sing,person:2])),

% second person plural present tense imp
asserta(lex(V2plimp,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimp ,tense:present,kind:Kind,mood:imperative,
                   number:pl,person:2])),

% imperative future
%

% second person singular future tense imp
asserta(lex(V2sgimpfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimpfut ,tense:future,kind:Kind,mood:imperative,
                   number:sing,person:2])),

% second person plural future tense imp
asserta(lex(V2plimpfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimpfut ,tense:future,kind:Kind,mood:imperative,
                   number:pl,person:2])),


% third person singular future tense imp
asserta(lex(V3sgimpfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3sgimpfut ,tense:future,kind:Kind,mood:imperative,
                   number:sing,person:3])),

% third person plural future tense imp
asserta(lex(V3plimpfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plimpfut ,tense:future,kind:Kind,mood:imperative,
                   number:pl,person:3])),


% Present Participles
%

% SING

% nominative masc fem neuter sing
asserta(lex(PrPartB,p_pr,[pos:p_pr,txt:PrPartB ,case:nom, gender:_, number:sing,lex:Lex,
            class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% accusative neuter sg
asserta(lex(PrPartB,p_pr,[pos:p_pr,txt:PrPartB ,case:acc, gender:neuter, number:sing,lex:Lex,
            class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% accusative masc fem sing
asserta(lex(PrPAccmfsg,p_pr,[pos:p_pr,txt:PrPAccmfsg , case:acc, gender:or([masc,fem]), number:sing,lex:Lex,
            class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% genitive g3 sg
asserta(lex(PrPGeng3sg,p_pr,[pos:p_pr,txt:PrPGeng3sg, case:gen, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% dative g3 sg
asserta(lex(PrPDatg3sg,p_pr,[pos:p_pr,txt:PrPDatg3sg, case:dat, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% ablative g3 sg ending in -i
asserta(lex(PrPAbl1g3sg,p_pr,[pos:p_pr,txt:PrPAbl1g3sg, case:abl, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3,typeabl:i])),
% ablative g3 sg ending in -e
asserta(lex(PrPAbl2g3sg,p_pr,[pos:p_pr,txt:PrPAbl2g3sg, case:abl, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3,typeabl:e])),

% PLURAL

% nominative masc fem pl
asserta(lex(PrPNommfpl,p_pr,[pos:p_pr,txt:PrPNommfpl, case:nom, gender:or([masc,fem]), number:pl,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% accusative masc fem pl
asserta(lex(PrPAccmfpl,p_pr,[pos:p_pr,txt:PrPAccmfpl, case:acc, gender:or([masc,fem]), number:pl,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% nominative neuter pl
asserta(lex(PrPNomnpl,p_pr,[pos:p_pr,txt:PrPNomnpl, case:nom, gender:neuter, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% accusative neuter pl
asserta(lex(PrPAccnpl,p_pr,[pos:p_pr,txt:PrPAccnpl, case:acc, gender:neuter, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% genitive g3 pl
asserta(lex(PrPGeng3pl,p_pr,[pos:p_pr,txt:PrPGeng3pl, case:gen, gender:_, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% dative g3 pl
asserta(lex(PrPDatg3pl,p_pr,[pos:p_pr,txt:PrPDatg3pl, case:dat, gender:_, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),
% ablative g3 pl
asserta(lex(PrPAblg3pl,p_pr,[pos:p_pr,txt:PrPAblg3pl, case:abl, gender:_, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:Kind,mood:participle,person:3])),

% infinitive present active
%

% present infinitive active
asserta(lex(Inf,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:act,txt:Inf ,tense:present,kind:Kind,mood:infinitive,
                   number:Nb,person:P])))),


% supine                                  % amatum
%
ifthen(RootSup \= xxx,
        asserta(lex(Supin,v,[pos:v,class:Class,type:supine,lex:Lex,
                  txt:Supin ,kind:Kind,mood:supine,
                  person:3,case:_]))
       ),

ifthen(RootPft \= xxx,   

% perfect indicative active
%

% third person singular pft tense
(asserta(lex(V3sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgpft ,tense:perfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),
% third person pl pft tense
asserta(lex(V3plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plpft ,tense:perfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),
% third person pl pft tense second form
asserta(lex(V3plpftbis,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plpftbis ,tense:perfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),
% first person singular pft tense
asserta(lex(V1sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgpft ,tense:perfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),
% second person singular pft tense
asserta(lex(V2sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgpft ,tense:perfect,kind:Kind,mood:indicative,
                   number:sing,person:2])),
% first person plural pft tense
asserta(lex(V1plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plpft ,tense:perfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),
% second person plural pft tense
asserta(lex(V2plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plpft ,tense:perfect,kind:Kind,mood:indicative,
                    number:pl,person:2])),


% pluperfect indicative active
%

% third person singular plupft tense
asserta(lex(PluV3sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV3sgpft ,tense:pluperfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),
% third person pl plupft tense
asserta(lex(PluV3plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV3plpft ,tense:pluperfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular plupft tense
asserta(lex(PluV1sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1sgpft ,tense:pluperfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),
% second person singular plupft tense
asserta(lex(PluV2sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,kind:Kind,mood:indicative,
                   voice:act,txt:PluV2sgpft ,tense:pluperfect,
                   number:sing,person:2])),
% first person plural plupft tense
asserta(lex(PluV1plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1plpft ,tense:pluperfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),
% second person plural plupft tense
asserta(lex(PluV2plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV2plpft ,tense:pluperfect,kind:Kind,mood:indicative,
                    number:pl,person:2])),


%  future perfect indicative active
%

% third person singular future perfect tense
asserta(lex(FV3sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:FV3sgpft ,tense:future_perfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),
% third person pl future perfect tense
asserta(lex(FV3plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:FV3plpft ,tense:future_perfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular future perfect tense
asserta(lex(FV1sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:FV1sgpft ,tense:future_perfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),
% second person singular future perfect tense
asserta(lex(FV2sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:FV2sgpft ,tense:future_perfect,kind:Kind,mood:indicative,
                   number:sing,person:2])),
% first person plural future perfect tense
asserta(lex(FV1plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:FV1plpft ,tense:future_perfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),
% second person plural future perfect tense
asserta(lex(FV2plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:FV2plpft ,tense:future_perfect,kind:Kind,mood:indicative,
                    number:pl,person:2])),


% subjunctive perfect active
%

% third person singular pft tense subjunctive
asserta(lex(V3sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),
% third person pl pft tense subjunctive
asserta(lex(V3plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular pft tense subjunctive
asserta(lex(V1sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),
% second person singular pft tense subjunctive
asserta(lex(V2sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),
% first person plural pft tense subjunctive
asserta(lex(V1plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),
% second person plural pft tense subjunctive
asserta(lex(V2plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                    number:pl,person:2])),

% subjunctive pluperfect active
%

% third person singular plupft tense subjunctive
asserta(lex(PluV3sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV3sgpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),
% third person pl plupft tense subjunctive
asserta(lex(PluV3plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV3plpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular plupft tense subjunctive
asserta(lex(PluV1sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1sgpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),
% second person singular plupft tense subjunctive
asserta(lex(PluV2sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,kind:Kind,mood:subjunctive,
                   voice:act,txt:PluV2sgpfts ,tense:pluperfect,
                   number:sing,person:2])),
% first person plural plupft tense subjunctive
asserta(lex(PluV1plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1plpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),
% second person plural plupft tense subjunctive
asserta(lex(PluV2plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV2plpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                    number:pl,person:2])), 
% past infinitive active
%

asserta(lex(Infp,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:act,txt:Inf ,tense:past,kind:Kind,mood:infinitive,
                   number:Nb,person:P])) )),

% Future Participles
%

% SING
ifthen(RootSup \= xxx,
% nominative masc sing
(asserta(lex(PFFNmsg,p_f,[pos:p_f,txt:PFFNmsg ,case:nom, gender:masc, number:sing,lex:Lex,
            class:Class,type:p_f,kind:Kind,mood:participle,person:3])),

% accusative masc sing
asserta(lex(PFFAcmsg,p_f,[pos:p_f,txt:PFFAcmsg , case:acc, gender:masc, number:sing,lex:Lex,
            class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% genitive masc sing
asserta(lex(PFFGenmsg,p_f,[pos:p_f,txt:PFFGenmsg , case:gen, gender:masc, number:sing,lex:Lex,
           class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% dative masc sing
asserta(lex(PFFDatmsg,p_f,[pos:p_f,txt:PFFDatmsg , case:dat, gender:masc, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% ablative masc sing
asserta(lex(PFFAblmsg,p_f,[pos:p_f,txt:PFFAblmsg , case:abl, gender:masc, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),

% nominative fem sing
asserta(lex(PFFNfsg,p_f,[pos:p_f,txt:PFFNfsg , case:nom, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% accusative fem sing
asserta(lex(PFFAcfsg,p_f,[pos:p_f,txt:PFFAcfsg , case:acc, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% genitive fem sing
asserta(lex(PFFGenfsg,p_f,[pos:p_f,txt:PFFGenfsg , case:gen, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% dative fem sing
asserta(lex(PFFDatfsg,p_f,[pos:p_f,txt:PFFDatfsg , case:dat, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% ablative fem sing
asserta(lex(PFFAblfsg,p_f,[pos:p_f,txt:PFFAblfsg , case:abl, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),


% nominative n sing
asserta(lex(PFFNnsg,p_f,[pos:p_f,txt:PFFNnsg , case:nom, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% accusative n sing
asserta(lex(PFFAcnsg,p_f,[pos:p_f,txt:PFFAcnsg , case:acc, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% genitive n sing
asserta(lex(PFFGennsg,p_f,[pos:p_f,txt:PFFGennsg , case:gen, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% dative n sing
asserta(lex(PFFDatnsg,p_f,[pos:p_f,txt:PFFDatnsg , case:dat, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% ablative n sing
asserta(lex(PFFAblnsg,p_f,[pos:p_f,txt:PFFAblnsg , case:abl, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),

% PLURAL

% nominative masc pl
asserta(lex(PFFNmpl,p_f,[pos:p_f,txt:PFFNmpl , case:nom, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% accusative masc pl
asserta(lex(PFFAcmpl,p_f,[pos:p_f,txt:PFFAcmpl , case:acc, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% genitive masc pl
asserta(lex(PFFGenmpl,p_f,[pos:p_f,txt:PFFGenmpl , case:gen, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% dative masc pl
asserta(lex(PFFDatmpl,p_f,[pos:p_f,txt:PFFDatmpl , case:dat, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% ablative masc pl
asserta(lex(PFFAblmpl,p_f,[pos:p_f,txt:PFFAblmpl , case:abl, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),

% nominative fem pl
asserta(lex(PFFNfpl,p_f,[pos:p_f,txt:PFFNfpl , case:nom, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% accusative fem pl
asserta(lex(PFFAcfpl,p_f,[pos:p_f,txt:PFFAcfpl , case:acc, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% genitive fem pl
asserta(lex(PFFGenfpl,p_f,[pos:p_f,txt:PFFGenfpl , case:gen, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% dative fem pl
asserta(lex(PFFDatfpl,p_f,[pos:p_f,txt:PFFDatfpl , case:dat, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% ablative fem pl
asserta(lex(PFFAblfpl,p_f,[pos:p_f,txt:PFFAblfpl , case:abl, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),

% nominative n pl
asserta(lex(PFFNnpl,p_f,[pos:p_f,txt:PFFNnpl , case:nom, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% accusative n pl
asserta(lex(PFFAcnpl,p_f,[pos:p_f,txt:PFFAcnpl , case:acc, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% genitive n pl
asserta(lex(PFFGennpl,p_f,[pos:p_f,txt:PFFGennpl , case:gen, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% dative n pl
asserta(lex(PFFDatnpl,p_f,[pos:p_f,txt:PFFDatnpl , case:dat, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])),
% ablative n pl
asserta(lex(PFFAblnpl,p_f,[pos:p_f,txt:PFFAblnpl , case:abl, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_f,kind:Kind,mood:participle,person:3])) )),

fail.

macro_verb.
% allows the fail boundary to be crossed

%
% DEPONENTS
%

macro_dep :-

verb([v(Lex,Conj,RootPr,RootSup)],Class,dep),

verb_endings_d(Conj,Pr1sg,Pr2sg,Pr3sg,Pr1pl,Pr2pl,Pr3pl,
                    Pr1sgsubj,Pr2sgsubj,Pr3sgsubj,Pr1plsubj,Pr2plsubj,Pr3plsubj,
                    Pr1sgimpft,Pr2sgimpft,Pr3sgimpft,Pr1plimpft,Pr2plimpft,Pr3plimpft,
                    Pr1sgfut,Pr2sgfut,Pr3sgfut,Pr1plfut,Pr2plfut,Pr3plfut,
                    Pr1sgimpfts,Pr2sgimpfts,Pr3sgimpfts,Pr1plimpfts,Pr2plimpfts,Pr3plimpfts,
                    PrInf,Ger,PrParts,PrPart,
                    Imp2sg,Imp2pl),

declension_endings(adj,1,[_,Acmsg,Genmsg,Datmsg,Ablmsg,
                  Nfsg,Acfsg,Genfsg,Datfsg,Ablfsg,
                  Nnsg,Acnsg,Gennsg,Datnsg,Ablnsg,
                  Nmpl,Acmpl,Genmpl,Datmpl,Ablmpl,
                  Nfpl,Acfpl,Genfpl,Datfpl,Ablfpl,
                  Nnpl,Acnpl,Gennpl,Datnpl,Ablnpl]),


declension_endings(partpr,[Accmfsg2,Geng3sg2,Datg3sg2,Abl1g3sg2,Abl2g3sg2,
                           Nommfpl2,Accmfpl2,Nomnpl2,Accnpl2,
                           Geng3pl2, Datg3pl2,Ablg3pl2]),


ifthen(RootPr \= xxx,
% indicative present active

(atom_concat(RootPr,Pr1sg,V1sg),
atom_concat(RootPr,Pr2sg,V2sg),
atom_concat(RootPr,Pr3sg,V3sg),
atom_concat(RootPr,Pr1pl,V1pl),
atom_concat(RootPr,Pr2pl,V2pl),
atom_concat(RootPr,Pr3pl,V3pl),

% subjunctive present active

atom_concat(RootPr,Pr1sgsubj,V1sgsubj),
atom_concat(RootPr,Pr2sgsubj,V2sgsubj),
atom_concat(RootPr,Pr3sgsubj,V3sgsubj),
atom_concat(RootPr,Pr1plsubj,V1plsubj),
atom_concat(RootPr,Pr2plsubj,V2plsubj),
atom_concat(RootPr,Pr3plsubj,V3plsubj),

% indicative imperfect active

atom_concat(RootPr,Pr1sgimpft,V1sgimpft),
atom_concat(RootPr,Pr2sgimpft,V2sgimpft),
atom_concat(RootPr,Pr3sgimpft,V3sgimpft),
atom_concat(RootPr,Pr1plimpft,V1plimpft),
atom_concat(RootPr,Pr2plimpft,V2plimpft),
atom_concat(RootPr,Pr3plimpft,V3plimpft),

% indicative futur active

atom_concat(RootPr,Pr1sgfut,V1sgfut),
atom_concat(RootPr,Pr2sgfut,V2sgfut),
atom_concat(RootPr,Pr3sgfut,V3sgfut),
atom_concat(RootPr,Pr1plfut,V1plfut),
atom_concat(RootPr,Pr2plfut,V2plfut),
atom_concat(RootPr,Pr3plfut,V3plfut),

% subjunctive imperfect active

atom_concat(RootPr,Pr1sgimpfts,V1sgimpfts),
atom_concat(RootPr,Pr2sgimpfts,V2sgimpfts),
atom_concat(RootPr,Pr3sgimpfts,V3sgimpfts),
atom_concat(RootPr,Pr1plimpfts,V1plimpfts),
atom_concat(RootPr,Pr2plimpfts,V2plimpfts),
atom_concat(RootPr,Pr3plimpfts,V3plimpfts),

% imperative present active

atom_concat(RootPr,Imp2sg,VImp2sg),
atom_concat(RootPr,Imp2pl,VImp2pl),


% infinitive present active

atom_concat(RootPr,PrInf,Inf),

% present participles

atom_concat(RootPr,PrParts,PrPartB),  % hort+ans -> hortans, etc
atom_concat(RootPr,PrPart,PrPartA),  % hort+ant- -> hortant-, etc

% PrPartB (hortans, etc) : Nominative masc fem neuter sg + acc neuter sg

atom_concat(PrPartA,Accmfsg2,PrPAccmfsg),      % acc masc fem sg
atom_concat(PrPartA,Geng3sg2,PrPGeng3sg),    % gen 3 genders sg
atom_concat(PrPartA,Datg3sg2,PrPDatg3sg),    % dat 3 genders sg
atom_concat(PrPartA,Abl1g3sg2,PrPAbl1g3sg),     % i-abl 3 genders sg
atom_concat(PrPartA,Abl2g3sg2,PrPAbl2g3sg),           % e-abl 3 genders sg

atom_concat(PrPartA,Nommfpl2,PrPNommfpl),
atom_concat(PrPartA,Accmfpl2,PrPAccmfpl),
atom_concat(PrPartA,Nomnpl2,PrPNomnpl),
atom_concat(PrPartA,Accnpl2,PrPAccnpl),
atom_concat(PrPartA,Geng3pl2,PrPGeng3pl),
atom_concat(PrPartA,Datg3pl2,PrPDatg3pl),
atom_concat(PrPartA,Ablg3pl2,PrPAblg3pl),

% gerunds

atom_concat(RootPr,Ger,Gerbase),
atom_concat(Gerbase,um,Gerundum),
atom_concat(Gerbase,i,Gerundum_gen),
atom_concat(Gerbase,o,Gerundum_abl),

% gerund -UM
asserta(lex(Gerundum,v,[pos:v,class:Class,type:gerund,lex:Lex,
                  txt:Gerundum, kind:Kind,mood:gerund,
                  person:3,case:acc])),

% genitive gerund -I
asserta(lex(Gerundum_gen,v,[pos:v,class:Class,type:gerund,lex:Lex,
                  txt:Gerundum_gen ,kind:Kind,mood:gerund,
                  person:3,case:gen])),
% dative abl -O
asserta(lex(Gerundum_abl,v,[pos:v,class:Class,type:gerund,lex:Lex,
                  txt:Gerundum_abl ,kind:Kind,mood:gerund,
                  person:3,case:abl])),





ifthen( (Class=tr_cod;Class=tr_cod_coi;Class=tr_cod_cplt;
         Class=tr_cod_prep_cplt;Class=tr_inf),
                                          % transitive verbs, the only ones to sport gerundives
					% not to be included tr_cod_dat (studere), tr_cod_abl (uti)

% gerundives

(atom_concat(Gerbase,us,FNmsg),
atom_concat(Gerbase,Acmsg,FAcmsg),
atom_concat(Gerbase,Genmsg,FGenmsg),
atom_concat(Gerbase,Datmsg,FDatmsg),
atom_concat(Gerbase,Ablmsg,FAblmsg),
atom_concat(Gerbase,Nfsg,FNfsg),
atom_concat(Gerbase,Acfsg,FAcfsg),
atom_concat(Gerbase,Genfsg,FGenfsg),
atom_concat(Gerbase,Datfsg,FDatfsg),
atom_concat(Gerbase,Ablfsg,FAblfsg),
atom_concat(Gerbase,Nnsg,FNnsg),
atom_concat(Gerbase,Acnsg,FAcnsg),
atom_concat(Gerbase,Gennsg,FGennsg),
atom_concat(Gerbase,Datnsg,FDatnsg),
atom_concat(Gerbase,Ablnsg,FAblnsg),
atom_concat(Gerbase,Nmpl,FNmpl),
atom_concat(Gerbase,Acmpl,FAcmpl),
atom_concat(Gerbase,Genmpl,FGenmpl),
atom_concat(Gerbase,Datmpl,FDatmpl),
atom_concat(Gerbase,Ablmpl,FAblmpl),
atom_concat(Gerbase,Nfpl,FNfpl),
atom_concat(Gerbase,Acfpl,FAcfpl),
atom_concat(Gerbase,Genfpl,FGenfpl),
atom_concat(Gerbase,Datfpl,FDatfpl),
atom_concat(Gerbase,Ablfpl,FAblfpl),
atom_concat(Gerbase,Nnpl,FNnpl),
atom_concat(Gerbase,Acnpl,FAcnpl),
atom_concat(Gerbase,Gennpl,FGennpl),
atom_concat(Gerbase,Datnpl,FDatnpl),
atom_concat(Gerbase,Ablnpl,FAblnpl),


% SING

% nominative masc sing
asserta(lex(FNmsg,gdiv,[pos:gdiv,txt:FNmsg ,case:nom, gender:masc, number:sing,lex:Lex,
            class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),

% accusative masc sing
asserta(lex(FAcmsg,gdiv,[pos:gdiv,txt:FAcmsg , case:acc, gender:masc, number:sing,lex:Lex,
            class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% genitive masc sing
asserta(lex(FGenmsg,gdiv,[pos:gdiv,txt:FGenmsg , case:gen, gender:masc, number:sing,lex:Lex,
           class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% dative masc sing
asserta(lex(FDatmsg,gdiv,[pos:gdiv,txt:FDatmsg , case:dat, gender:masc, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% ablative masc sing
asserta(lex(FAblmsg,gdiv,[pos:gdiv,txt:FAblmsg , case:abl, gender:masc, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),

% nominative fem sing
asserta(lex(FNfsg,gdiv,[pos:gdiv,txt:FNfsg , case:nom, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% accusative fem sing
asserta(lex(FAcfsg,gdiv,[pos:gdiv,txt:FAcfsg , case:acc, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% genitive fem sing
asserta(lex(FGenfsg,gdiv,[pos:gdiv,txt:FGenfsg , case:gen, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% dative fem sing
asserta(lex(FDatfsg,gdiv,[pos:gdiv,txt:FDatfsg , case:dat, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% ablative fem sing
asserta(lex(FAblfsg,gdiv,[pos:gdiv,txt:FAblfsg , case:abl, gender:fem, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),


% nominative n sing
asserta(lex(FNnsg,gdiv,[pos:gdiv,txt:FNnsg , case:nom, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% accusative n sing
asserta(lex(FAcnsg,gdiv,[pos:gdiv,txt:FAcnsg , case:acc, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% genitive n sing
asserta(lex(FGennsg,gdiv,[pos:gdiv,txt:FGennsg , case:gen, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% dative n sing
asserta(lex(FDatnsg,gdiv,[pos:gdiv,txt:FDatnsg , case:dat, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% ablative n sing
asserta(lex(FAblnsg,gdiv,[pos:gdiv,txt:FAblnsg , case:abl, gender:neuter, number:sing,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),

% PLURAL

% nominative masc pl
asserta(lex(FNmpl,gdiv,[pos:gdiv,txt:FNmpl , case:nom, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% accusative masc pl
asserta(lex(FAcmpl,gdiv,[pos:gdiv,txt:FAcmpl , case:acc, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% genitive masc pl
asserta(lex(FGenmpl,gdiv,[pos:gdiv,txt:FGenmpl , case:gen, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% dative masc pl
asserta(lex(FDatmpl,gdiv,[pos:gdiv,txt:FDatmpl , case:dat, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% ablative masc pl
asserta(lex(FAblmpl,gdiv,[pos:gdiv,txt:FAblmpl , case:abl, gender:masc, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),

% nominative fem pl
asserta(lex(FNfpl,gdiv,[pos:gdiv,txt:FNfpl , case:nom, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% accusative fem pl
asserta(lex(FAcfpl,gdiv,[pos:gdiv,txt:FAcfpl , case:acc, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% genitive fem pl
asserta(lex(FGenfpl,gdiv,[pos:gdiv,txt:FGenfpl , case:gen, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% dative fem pl
asserta(lex(FDatfpl,gdiv,[pos:gdiv,txt:FDatfpl , case:dat, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% ablative fem pl
asserta(lex(FAblfpl,gdiv,[pos:gdiv,txt:FAblfpl , case:abl, gender:fem, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),

% nominative n pl
asserta(lex(FNnpl,gdiv,[pos:gdiv,txt:FNnpl , case:nom, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% accusative n pl
asserta(lex(FAcnpl,gdiv,[pos:gdiv,txt:FAcnpl , case:acc, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% genitive n pl
asserta(lex(FGennpl,gdiv,[pos:gdiv,txt:FGennpl , case:gen, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% dative n pl
asserta(lex(FDatnpl,gdiv,[pos:gdiv,txt:FDatnpl , case:dat, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3])),
% ablative n pl
asserta(lex(FAblnpl,gdiv,[pos:gdiv,txt:FAblnpl , case:abl, gender:neuter, number:pl,lex:Lex,
        class:Class,type:gdiv,kind:dep,mood:gerund,person:3]))
)),


% imperative present active
%

% second person singular present tense imp
asserta(lex(VImp2sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:VImp2sg ,tense:present,kind:dep,mood:imperative,
                   number:sing,person:2])),

% second person plural present tense imp
asserta(lex(VImp2pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:VImp2pl ,tense:present,kind:dep,mood:imperative,
                   number:pl,person:2])),

% indicative present tense active
%

% third person singular present tense
asserta(lex(V3sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sg ,tense:present,kind:dep,mood:indicative,
                  number:sing,person:3])),

% third person pl present tense
asserta(lex(V3pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3pl ,tense:present,kind:dep,mood:indicative,
                   number:pl,person:3])),

% first person singular present tense
asserta(lex(V1sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sg ,tense:present,kind:dep,mood:indicative,
                  number:sing,person:1])),

% second person singular present tense
asserta(lex(V2sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sg ,tense:present,kind:dep,mood:indicative,
                   number:sing,person:2])),

% first person plural present tense
asserta(lex(V1pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1pl ,tense:present,kind:dep,mood:indicative,
                  number:pl,person:1])),

% second person plural present tense
asserta(lex(V2pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2pl ,tense:present,kind:dep,mood:indicative,
                   number:pl,person:2])),


% subjunctive present active
%

% third person singular present tense subj
asserta(lex(V3sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgsubj ,tense:present,kind:dep,mood:subjunctive,
                  number:sing,person:3])),

% third person pl present tense subj
asserta(lex(V3plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plsubj ,tense:present,kind:dep,mood:subjunctive,
                   number:pl,person:3])),

% first person singular present tense subj
asserta(lex(V1sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgsubj ,tense:present,kind:dep,mood:subjunctive,
                  number:sing,person:1])),

% second person singular present tense subj
asserta(lex(V2sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgsubj ,tense:present,kind:dep,mood:subjunctive,
                   number:sing,person:2])),

% first person plural present tense subj
asserta(lex(V1plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plsubj ,tense:present,kind:dep,mood:subjunctive,
                  number:pl,person:1])),

% second person plural present tense subj
asserta(lex(V2plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plsubj ,tense:present,kind:dep,mood:subjunctive,
                   number:pl,person:2])),

% indicative imperfect active
%

% third person singular  impft
asserta(lex(V3sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpft ,tense:imperfect,kind:dep,mood:indicative,
                  number:sing,person:3])),

% third person pl impft
asserta(lex(V3plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plimpft ,tense:imperfect,kind:dep,mood:indicative,
                   number:pl,person:3])),

% first person singular impft
asserta(lex(V1sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgimpft ,tense:imperfect,kind:dep,mood:indicative,
                  number:sing,person:1])),

% second person singular  impft
asserta(lex(V2sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimpft ,tense:imperfect,kind:dep,mood:indicative,
                   number:sing,person:2])),

% first person plural  impft
asserta(lex(V1plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plimpft ,tense:imperfect,kind:dep,mood:indicative,
                  number:pl,person:1])),

% second person plural impft
asserta(lex(V2plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimpft ,tense:imperfect,kind:dep,mood:indicative,
                   number:pl,person:2])),

% subjunctive imperfect active
%

% third person singular  impft
asserta(lex(V3sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpfts ,tense:imperfect,kind:dep,mood:subjunctive,
                  number:sing,person:3])),

% third person pl impft
asserta(lex(V3plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plimpfts ,tense:imperfect,kind:dep,mood:subjunctive,
                   number:pl,person:3])),

% first person singular impft
asserta(lex(V1sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgimpfts ,tense:imperfect,kind:dep,mood:subjunctive,
                  number:sing,person:1])),

% second person singular  impft
asserta(lex(V2sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimpfts ,tense:imperfect,kind:dep,mood:subjunctive,
                   number:sing,person:2])),

% first person plural  impft
asserta(lex(V1plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plimpfts ,tense:imperfect,kind:dep,mood:subjunctive,
                  number:pl,person:1])),

% second person plural impft
asserta(lex(V2plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimpfts ,tense:imperfect,kind:dep,mood:subjunctive,
                   number:pl,person:2])),


% indicative future active
%

% third person singular  fut
asserta(lex(V3sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgfut ,tense:future,kind:dep,mood:indicative,
                  number:sing,person:3])),

% third person pl fut
asserta(lex(V3plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plfut ,tense:future,kind:dep,mood:indicative,
                   number:pl,person:3])),

% first person singular fut
asserta(lex(V1sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgfut ,tense:future,kind:dep,mood:indicative,
                  number:sing,person:1])),

% second person singular  fut
asserta(lex(V2sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgfut ,tense:future,kind:dep,mood:indicative,
                   number:sing,person:2])),

% first person plural  fut
asserta(lex(V1plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plfut ,tense:future,kind:dep,mood:indicative,
                  number:pl,person:1])),

% second person plural fut
asserta(lex(V2plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plfut ,tense:future,kind:dep,mood:indicative,
                   number:pl,person:2])),


% infinitive present active
%

% present infinitive active
asserta(lex(Inf,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:act,txt:Inf ,tense:present,kind:dep,mood:infinitive,
                   number:Nb,person:P])),

% Present Participles
%

% SING

% nominative masc fem neuter sing
asserta(lex(PrPartB,p_pr,[pos:p_pr,txt:PrPartB ,case:nom, gender:_, number:sing,lex:Lex,
            class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% accusative neuter sg
asserta(lex(PrPartB,p_pr,[pos:p_pr,txt:PrPartB ,case:acc, gender:neuter, number:sing,lex:Lex,
            class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% accusative masc fem sing
asserta(lex(PrPAccmfsg,p_pr,[pos:p_pr,txt:PrPAccmfsg , case:acc, gender:or([masc,fem]), number:sing,lex:Lex,
            class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% genitive g3 sg
asserta(lex(PrPGeng3sg,p_pr,[pos:p_pr,txt:PrPGeng3sg, case:gen, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% dative g3 sg
asserta(lex(PrPDatg3sg,p_pr,[pos:p_pr,txt:PrPDatg3sg, case:dat, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% ablative g3 sg ending in -i
asserta(lex(PrPAbl1g3sg,p_pr,[pos:p_pr,txt:PrPAbl1g3sg, case:abl, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3,typeabl:i])),
% ablative g3 sg ending in -e
asserta(lex(PrPAbl2g3sg,p_pr,[pos:p_pr,txt:PrPAbl2g3sg, case:abl, gender:_, number:sing,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3,typeabl:e])),

% PLURAL

% nominative masc fem pl
asserta(lex(PrPNommfpl,p_pr,[pos:p_pr,txt:PrPNommfpl, case:nom, gender:or([masc,fem]), number:pl,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% accusative masc fem pl
asserta(lex(PrPAccmfpl,p_pr,[pos:p_pr,txt:PrPAccmfpl, case:acc, gender:or([masc,fem]), number:pl,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% nominative neuter pl
asserta(lex(PrPNomnpl,p_pr,[pos:p_pr,txt:PrPNomnpl, case:nom, gender:neuter, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% accusative neuter pl
asserta(lex(PrPAccnpl,p_pr,[pos:p_pr,txt:PrPAccnpl, case:acc, gender:neuter, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% genitive g3 pl
asserta(lex(PrPGeng3pl,p_pr,[pos:p_pr,txt:PrPGeng3pl, case:gen, gender:_, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% dative g3 pl
asserta(lex(PrPDatg3pl,p_pr,[pos:p_pr,txt:PrPDatg3pl, case:dat, gender:_, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])),
% ablative g3 pl
asserta(lex(PrPAblg3pl,p_pr,[pos:p_pr,txt:PrPAblg3pl, case:abl, gender:_, number:pl,lex:Lex,
           class:Class,type:p_pr,kind:dep,mood:participle,person:3])) )),


% -US,A,UM PARTICIPLES
%

% nom and acc only :
% hortatus,hortatum,hortati, hortatos [sum,es,est,...sunt,esse]
% hortata,hortatam,hortatae,hortatas
% hortatum,hortata

ifthen(RootSup \= xxx,
(atom_concat(RootSup,us,PPFNmsg),
atom_concat(RootSup,Acmsg,PPFAcmsg),
atom_concat(RootSup,Nfsg,PPFNfsg),
atom_concat(RootSup,Acfsg,PPFAcfsg),
atom_concat(RootSup,Nnsg,PPFNnsg),
atom_concat(RootSup,Acnsg,PPFAcnsg),
atom_concat(RootSup,Nmpl,PPFNmpl),
atom_concat(RootSup,Acmpl,PPFAcmpl),
atom_concat(RootSup,Nfpl,PPFNfpl),
atom_concat(RootSup,Acfpl,PPFAcfpl),
atom_concat(RootSup,Nnpl,PPFNnpl),
atom_concat(RootSup,Acnpl,PPFAcnpl),

% SING

% nominative masc sing
asserta(lex(PPFNmsg,p_p,[pos:p_p,txt:PPFNmsg ,case:nom, gender:masc, number:sing,lex:Lex,
            class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% accusative masc sing
asserta(lex(PPFAcmsg,p_p,[pos:p_p,txt:PPFAcmsg , case:acc, gender:masc, number:sing,lex:Lex,
            class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% nominative fem sing
asserta(lex(PPFNfsg,p_p,[pos:p_p,txt:PPFNfsg , case:nom, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% accusative fem sing
asserta(lex(PPFAcfsg,p_p,[pos:p_p,txt:PPFAcfsg , case:acc, gender:fem, number:sing,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% nominative n sing
asserta(lex(PPFNnsg,p_p,[pos:p_p,txt:PPFNnsg , case:nom, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% accusative n sing
asserta(lex(PPFAcnsg,p_p,[pos:p_p,txt:PPFAcnsg , case:acc, gender:neuter, number:sing,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),

% PLURAL

% nominative masc pl
asserta(lex(PPFNmpl,p_p,[pos:p_p,txt:PPFNmpl , case:nom, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% accusative masc pl
asserta(lex(PPFAcmpl,p_p,[pos:p_p,txt:PPFAcmpl , case:acc, gender:masc, number:pl,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% nominative fem pl
asserta(lex(PPFNfpl,p_p,[pos:p_p,txt:PPFNfpl , case:nom, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% accusative fem pl
asserta(lex(PPFAcfpl,p_p,[pos:p_p,txt:PPFAcfpl , case:acc, gender:fem, number:pl,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% nominative n pl
asserta(lex(PPFNnpl,p_p,[pos:p_p,txt:PPFNnpl , case:nom, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])),
% accusative n pl
asserta(lex(PPFAcnpl,p_p,[pos:p_p,txt:PPFAcnpl , case:acc, gender:neuter, number:pl,lex:Lex,
        class:Class,type:p_p,kind:dep,mood:participle,person:3])) )),

fail.

macro_dep.
% allows the fail boundary to be crossed










% Auxiliaries - only POSSE/VELLE/NOLLE/MALLE and FIERI for the time being....
%

macro_aux :-

verb([v(aux,Lex,V1sg,V2sg,V3sg,V1pl,V2pl,V3pl,
            V1sgsubj,V2sgsubj,V3sgsubj,V1plsubj,V2plsubj,V3plsubj,
            V1sgimpft,V2sgimpft,V3sgimpft,V1plimpft,V2plimpft,V3plimpft,
            V1sgfut,V2sgfut,V3sgfut,V1plfut,V2plfut,V3plfut,
            V1sgimpfts,V2sgimpfts,V3sgimpfts,V1plimpfts,V2plimpfts,V3plimpfts,
            Inf,RootPft)],
            Class,Kind),

verb_endings(aux,Pft1sg,Pft2sg,Pft3sg,Pft1pl,Pft2pl,Pft3pl,Pft3plbis,PftInf,
                 PluPft1sg,PluPft2sg,PluPft3sg,PluPft1pl,PluPft2pl,PluPft3pl,
                 FPft1sg,FPft2sg,FPft3sg,FPft1pl,FPft2pl,FPft3pl,
                 Pft1sgs,Pft2sgs,Pft3sgs,Pft1pls,Pft2pls,Pft3pls,
                 PluPft1sgs,PluPft2sgs,PluPft3sgs,PluPft1pls,PluPft2pls,PluPft3pls),

atom_concat(RootPft,Pft1sg,V1sgpft),
atom_concat(RootPft,Pft2sg,V2sgpft),
atom_concat(RootPft,Pft3sg,V3sgpft),
atom_concat(RootPft,Pft1pl,V1plpft),
atom_concat(RootPft,Pft2pl,V2plpft),
atom_concat(RootPft,Pft3pl,V3plpft),
atom_concat(RootPft,Pft3plbis,V3plpftbis),
atom_concat(RootPft,PftInf,Infp),

% indicative pluperfect active

atom_concat(RootPft,PluPft1sg,PluV1sgpft),
atom_concat(RootPft,PluPft2sg,PluV2sgpft),
atom_concat(RootPft,PluPft3sg,PluV3sgpft),
atom_concat(RootPft,PluPft1pl,PluV1plpft),
atom_concat(RootPft,PluPft2pl,PluV2plpft),
atom_concat(RootPft,PluPft3pl,PluV3plpft),

% future perfect active

atom_concat(RootPft,FPft1sg,FV1sgpft),
atom_concat(RootPft,FPft2sg,FV2sgpft),
atom_concat(RootPft,FPft3sg,FV3sgpft),
atom_concat(RootPft,FPft1pl,FV1plpft),
atom_concat(RootPft,FPft2pl,FV2plpft),
atom_concat(RootPft,FPft3pl,FV3plpft),

% subjunctive perfect active

atom_concat(RootPft,Pft1sgs,V1sgpfts),
atom_concat(RootPft,Pft2sgs,V2sgpfts),
atom_concat(RootPft,Pft3sgs,V3sgpfts),
atom_concat(RootPft,Pft1pls,V1plpfts),
atom_concat(RootPft,Pft2pls,V2plpfts),
atom_concat(RootPft,Pft3pls,V3plpfts),

% subjunctive pluperfect active

atom_concat(RootPft,PluPft1sgs,PluV1sgpfts),
atom_concat(RootPft,PluPft2sgs,PluV2sgpfts),
atom_concat(RootPft,PluPft3sgs,PluV3sgpfts),
atom_concat(RootPft,PluPft1pls,PluV1plpfts),
atom_concat(RootPft,PluPft2pls,PluV2plpfts),
atom_concat(RootPft,PluPft3pls,PluV3plpfts), 



% indicative present
%

ifthen(Lex\=nolle,
% third person singular present tense
asserta(lex(V3sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sg ,tense:present,kind:Kind,mood:indicative,
                  number:sing,person:3])) ),

% third person pl present tense
asserta(lex(V3pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3pl ,tense:present,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular present tense
asserta(lex(V1sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sg ,tense:present,kind:Kind,mood:indicative,
                  number:sing,person:1])),

ifthen(Lex\=nolle,
% second person singular present tense
asserta(lex(V2sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sg ,tense:present,kind:Kind,mood:indicative,
                   number:sing,person:2])) ),

% first person plural present tense
asserta(lex(V1pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1pl ,tense:present,kind:Kind,mood:indicative,
                  number:pl,person:1])),

ifthen(Lex\=nolle,
% second person plural present tense
asserta(lex(V2pl,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2pl ,tense:present,kind:Kind,mood:indicative,
                   number:pl,person:2])) ),

% subjunctive present
%

% third person singular present tense subj
asserta(lex(V3sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),

% third person pl present tense subj
asserta(lex(V3plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular present tense subj
asserta(lex(V1sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),

% second person singular present tense subj
asserta(lex(V2sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),

% first person plural present tense subj
asserta(lex(V1plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),

% second person plural present tense subj
asserta(lex(V2plsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plsubj ,tense:present,kind:Kind,mood:subjunctive,
                   number:pl,person:2])),

% indicative imperfect
%

% third person singular  impft
asserta(lex(V3sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl impft
asserta(lex(V3plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular impft
asserta(lex(V1sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular  impft
asserta(lex(V2sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural  impft
asserta(lex(V1plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural impft
asserta(lex(V2plimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimpft ,tense:imperfect,kind:Kind,mood:indicative,
                   number:pl,person:2])),


% indicative future
%
ifthen( (Lex\=nolle,Lex\=malle),

% third person singular  indic fut
(asserta(lex(V3sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl indic fut
asserta(lex(V3plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plfut ,tense:future,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular indic fut
asserta(lex(V1sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular indic fut
asserta(lex(V2sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgfut ,tense:future,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural  indic fut
asserta(lex(V1plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plfut ,tense:future,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural indic fut
asserta(lex(V2plfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plfut ,tense:future,kind:Kind,mood:indicative,
                   number:pl,person:2])) )),


% subjunctive imperfect
%

% third person singular  impft subj
asserta(lex(V3sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),

% third person pl impft subj
asserta(lex(V3plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular impft subj
asserta(lex(V1sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),

% second person singular  impft subj
asserta(lex(V2sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),

% first person plural  impft subj
asserta(lex(V1plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),

% second person plural impft subj
asserta(lex(V2plimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:2])),


% infinitive present
%

% present infinitive
asserta(lex(Inf,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:act,txt:Inf ,tense:present,kind:Kind,mood:infinitive,
                   number:Nb,person:P])),

ifthen(RootPft \= xxx,

% indicative perfect
%

% third person singular pft tense
(asserta(lex(V3sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgpft ,tense:perfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),

% third person pl pft tense
asserta(lex(V3plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plpft ,tense:perfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% third person pl pft tense second form
asserta(lex(V3plpftbis,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plpftbis ,tense:perfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular present tense
asserta(lex(V1sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgpft ,tense:perfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),

% second person singular present tense
asserta(lex(V2sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgpft ,tense:perfect,kind:Kind,mood:indicative,
                   number:sing,person:2])),

% first person plural present tense
asserta(lex(V1plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plpft ,tense:perfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),

% second person plural present tense
asserta(lex(V2plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plpft ,tense:perfect,kind:Kind,mood:indicative,
                    number:pl,person:2])),

% perfect infinitive
asserta(lex(Infp,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:act,txt:Inf,tense:past,kind:Kind,mood:infinitive,
                   number:Nb,person:P])),

% pluperfect indicative active
%

% third person singular plupft tense
asserta(lex(PluV3sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV3sgpft ,tense:pluperfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),
% third person pl plupft tense
asserta(lex(PluV3plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV3plpft ,tense:pluperfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular plupft tense
asserta(lex(PluV1sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1sgpft ,tense:pluperfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),
% second person singular plupft tense
asserta(lex(PluV2sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,kind:Kind,mood:indicative,
                   voice:act,txt:PluV2sgpft ,tense:pluperfect,
                   number:sing,person:2])),
% first person plural plupft tense
asserta(lex(PluV1plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1plpft ,tense:pluperfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),
% second person plural plupft tense
asserta(lex(PluV2plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV2plpft ,tense:pluperfect,kind:Kind,mood:indicative,
                    number:pl,person:2])),


%  future perfect indicative active
%

% third person singular future perfect tense
asserta(lex(FV3sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:FV3sgpft ,tense:future_perfect,kind:Kind,mood:indicative,
                  number:sing,person:3])),
% third person pl future perfect tense
asserta(lex(FV3plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:FV3plpft ,tense:future_perfect,kind:Kind,mood:indicative,
                   number:pl,person:3])),

% first person singular future perfect tense
asserta(lex(FV1sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:FV1sgpft ,tense:future_perfect,kind:Kind,mood:indicative,
                  number:sing,person:1])),
% second person singular future perfect tense
asserta(lex(FV2sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:FV2sgpft ,tense:future_perfect,kind:Kind,mood:indicative,
                   number:sing,person:2])),
% first person plural future perfect tense
asserta(lex(FV1plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:FV1plpft ,tense:future_perfect,kind:Kind,mood:indicative,
                  number:pl,person:1])),
% second person plural future perfect tense
asserta(lex(FV2plpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:FV2plpft ,tense:future_perfect,kind:Kind,mood:indicative,
                    number:pl,person:2])),


% subjunctive perfect active
%

% third person singular pft tense subjunctive
asserta(lex(V3sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),
% third person pl pft tense subjunctive
asserta(lex(V3plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V3plpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular pft tense subjunctive
asserta(lex(V1sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1sgpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),
% second person singular pft tense subjunctive
asserta(lex(V2sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2sgpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                   number:sing,person:2])),
% first person plural pft tense subjunctive
asserta(lex(V1plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V1plpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),
% second person plural pft tense subjunctive
asserta(lex(V2plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:V2plpfts ,tense:perfect,kind:Kind,mood:subjunctive,
                    number:pl,person:2])),

% subjunctive pluperfect active
%

% third person singular plupft tense subjunctive
asserta(lex(PluV3sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV3sgpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:3])),
% third person pl plupft tense subjunctive
asserta(lex(PluV3plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV3plpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                   number:pl,person:3])),

% first person singular plupft tense subjunctive
asserta(lex(PluV1sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1sgpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                  number:sing,person:1])),
% second person singular plupft tense subjunctive
asserta(lex(PluV2sgpfts,v,[pos:v,class:Class,type:finite,lex:Lex,kind:Kind,mood:subjunctive,
                   voice:act,txt:PluV2sgpfts ,tense:pluperfect,
                   number:sing,person:2])),
% first person plural plupft tense subjunctive
asserta(lex(PluV1plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:PluV1plpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                  number:pl,person:1])),
% second person plural plupft tense subjunctive
asserta(lex(PluV2plpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                   voice:act,txt:PluV2plpfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                    number:pl,person:2]))  )),

fail.

macro_aux.

% NOLLE imperative
%

% present

% second person singular pr tense imp
lex(noli,v,[pos:v,class:v_nolle,type:finite,lex:nolle,
                   voice:act,txt:noli ,tense:present,kind:std,mood:imperative,
                   number:sing,person:2]).

% second person plural pr tense imp
lex(nolite,v,[pos:v,class:v_nolle,type:finite,lex:nolle,
                   voice:act,txt:nolite,tense:present,kind:std,mood:imperative,
                   number:pl,person:2]).

% future

% second person singular future tense imp
lex(nolito,v,[pos:v,class:v_nolle,type:finite,lex:nolle,
                   voice:act,txt:nolito ,tense:future,kind:std,mood:imperative,
                   number:sing,person:2]).

% second person plural future tense imp
lex(nolitote,v,[pos:v,class:v_nolle,type:finite,lex:nolle,
                   voice:act,txt:nolitote,tense:future,kind:std,mood:imperative,
                   number:pl,person:2]).


% third person singular future tense imp
lex(nolito,v,[pos:v,class:v_nolle,type:finite,lex:nolle,
                   voice:act,txt:nolito,tense:future,kind:std,mood:imperative,
                   number:sing,person:3]).

% third person plural future tense imp
lex(nolunto,v,[pos:v,class:v_nolle,type:finite,lex:nolle,
                   voice:act,txt:nolunto,tense:future,kind:std,mood:imperative,
                   number:pl,person:3]).







%
% ESSE %
%

% indicative present
%

% third person singular present tense
lex(est,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:est ,tense:present,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl present tense
lex(sunt,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:sunt ,tense:present,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular present tense
lex(sum,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:sum ,tense:present,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular present tense
lex(es,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:es ,tense:present,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural present tense
lex(sumus,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:sumus ,tense:present,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural present tense
lex(estis,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:estis ,tense:present,kind:std,mood:indicative,
                   number:pl,person:2]).

% subjunctive present
%

% third person singular present tense
lex(sit,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:sit ,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person pl present tense
lex(sint,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:sint ,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% first person singular present tense
lex(sim,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:sim,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% second person singular present tense
lex(sis,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:sis ,tense:present,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% first person plural present tense
lex(simus,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:simus ,tense:present,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural present tense
lex(sitis,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:sitis ,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:2]).

% indicative future
%

% third person singular 
lex(erit,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:erit ,tense:future,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(erunt,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:erunt ,tense:future,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(ero,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:ero,tense:future,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(eris,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:eris ,tense:future,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(erimus,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:erimus ,tense:future,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(eritis,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:eritis ,tense:future,kind:std,mood:indicative,
                   number:pl,person:2]).


% indicative imperfect
%

% third person singular 
lex(erat,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:erat ,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(erant,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:erant ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(eram,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:eram,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(eras,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:eras ,tense:imperfect,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(eramus,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:eramus ,tense:imperfect,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(eratis,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:eratis ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:2]).

% subjunctive imperfect
%

% third person singular 
lex(esset,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:esset ,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person singular 
lex(foret,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:foret ,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:3]).

% third person pl 
lex(essent,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:essent ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% third person pl 
lex(forent,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:forent ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:3]).

% first person singular 
lex(essem,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:essem,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% first person singular 
lex(forem,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:forem,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:1]).

% second person singular 
lex(esses,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:esses ,tense:imperfect,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% second person singular 
lex(fores,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:fores ,tense:imperfect,kind:std,mood:subjunctive,
                   number:sing,person:2]).

% first person plural 
lex(essemus,v,[pos:v,class:v_esse,type:finite,lex:esse,
                  voice:act,txt:essemus ,tense:imperfect,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural 
lex(essetis,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:essetis ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:2]).

% future participle esse
%

% SING

% nominative masc sing
lex(futurus,p_f,[pos:p_f,txt:futurus ,case:nom, gender:masc, number:sing,lex:esse,
            type:p_f,kind:std,mood:participle,person:3]).

% accusative masc sing
lex(futurum,p_f,[pos:p_f,txt:futurum , case:acc, gender:masc, number:sing,lex:esse,
            type:p_f,kind:std,mood:participle,person:3]).
% genitive masc sing
lex(futuri,p_f,[pos:p_f,txt:futuri , case:gen, gender:masc, number:sing,lex:esse,
           type:p_f,kind:std,mood:participle,person:3]).
% dative masc sing
lex(futuro,p_f,[pos:p_f,txt:futuro , case:dat, gender:masc, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% ablative masc sing
lex(futuro,p_f,[pos:p_f,txt:futuro , case:abl, gender:masc, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).

% nominative fem sing
lex(futura,p_f,[pos:p_f,txt:futura , case:nom, gender:fem, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% accusative fem sing
lex(futuram,p_f,[pos:p_f,txt:futuram, case:acc, gender:fem, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% genitive fem sing
lex(futurae,p_f,[pos:p_f,txt:futurae, case:gen, gender:fem, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% dative fem sing
lex(futurae,p_f,[pos:p_f,txt:futurae , case:dat, gender:fem, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% ablative fem sing
lex(futura,p_f,[pos:p_f,txt:futura, case:abl, gender:fem, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).

% nominative n sing
lex(futurum,p_f,[pos:p_f,txt:futurum , case:nom, gender:neuter, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% accusative n sing
lex(futurum,p_f,[pos:p_f,txt:futurum , case:acc, gender:neuter, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% genitive n sing
lex(futuri,p_f,[pos:p_f,txt:futuri , case:gen, gender:neuter, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% dative n sing
lex(futuro,p_f,[pos:p_f,txt:futuro , case:dat, gender:neuter, number:sing,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% ablative n sing
lex(futuro,p_f,[pos:p_f,txt:futuro , case:abl, gender:neuter, number:sing,lex:esse,
       type:p_f,kind:std,mood:participle,person:3]).

% PLURAL

% nominative masc pl
lex(futuri,p_f,[pos:p_f,txt:futuri , case:nom, gender:masc, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% accusative masc pl
lex(futuros,p_f,[pos:p_f,txt:futuros , case:acc, gender:masc, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% genitive masc pl
lex(futurorum,p_f,[pos:p_f,txt:futurorum , case:gen, gender:masc, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% dative masc pl
lex(futuris,p_f,[pos:p_f,txt:futuris , case:dat, gender:masc, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% ablative masc pl
lex(futuris,p_f,[pos:p_f,txt:futuris , case:abl, gender:masc, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).

% nominative fem pl
lex(futurae,p_f,[pos:p_f,txt:futurae , case:nom, gender:fem, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% accusative fem pl
lex(futuras,p_f,[pos:p_f,txt:futuras , case:acc, gender:fem, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% genitive fem pl
lex(futurarum,p_f,[pos:p_f,txt:futurarum , case:gen, gender:fem, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% dative fem pl
lex(futuris,p_f,[pos:p_f,txt:futuris , case:dat, gender:fem, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% ablative fem pl
lex(futuris,p_f,[pos:p_f,txt:futuris , case:abl, gender:fem, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).

% nominative n pl
lex(futura,p_f,[pos:p_f,txt:futura , case:nom, gender:neuter, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% accusative n pl
lex(futura,p_f,[pos:p_f,txt:futura , case:acc, gender:neuter, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% genitive n pl
lex(futurorum,p_f,[pos:p_f,txt:futurorum , case:gen, gender:neuter, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% dative n pl
lex(futuris,p_f,[pos:p_f,txt:futuris , case:dat, gender:neuter, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).
% ablative n pl
lex(futuris,p_f,[pos:p_f,txt:futuris , case:abl, gender:neuter, number:pl,lex:esse,
        type:p_f,kind:std,mood:participle,person:3]).

% esse et fore
%

lex(fore,v,[pos:v,class:v_esse,type:nonfinite,lex:esse,
                   voice:act,txt:fore,tense:future,kind:std,mood:infinitive,
                   number:Nb,person:P]).

lex(esse,v,[pos:v,class:v_esse,type:nonfinite,lex:esse,
                   voice:act,txt:esse,tense:present,kind:std,mood:infinitive,
                   number:Nb,person:P]).

% iri
%

lex(iri, v, 
      [pos:v, class:v_ire, type:nonfinite, lex:ire, voice:pass, txt:iri, 
       tense:future, kind:std, mood:infinitive, number:_, person:_]).



% imperative
%

% present

% second person singular pr tense imp
lex(es,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:es ,tense:present,kind:std,mood:imperative,
                   number:sing,person:2]).

% second person plural pr tense imp
lex(este,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:este,tense:present,kind:std,mood:imperative,
                   number:pl,person:2]).

% future

% second person singular future tense imp
lex(esto,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:esto ,tense:future,kind:std,mood:imperative,
                   number:sing,person:2]).

% second person plural future tense imp
lex(estote,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:estote,tense:future,kind:std,mood:imperative,
                   number:pl,person:2]).


% third person singular future tense imp
lex(esto,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:esto,tense:future,kind:std,mood:imperative,
                   number:sing,person:3]).

% third person plural future tense imp
lex(sunto,v,[pos:v,class:v_esse,type:finite,lex:esse,
                   voice:act,txt:sunto,tense:future,kind:std,mood:imperative,
                   number:pl,person:3]).








% IMPERSONAL VERBS
%

macro_imp :-

verb([v(imp,Lex,V3sg,V3sgsubj,V3sgimpft,V3sgfut,V3sgimpfts,Inf,RootPft)],Class,Kind),

verb_endings(imp,Pft,PftInf,Ppfi,Fpfis,Ppfs),

atom_concat(RootPft,Pft,V3sgpft),
atom_concat(RootPft,PftInf,Infp),
atom_concat(RootPft,Ppfi,V3sgppfti),
atom_concat(RootPft,Fpfis,V3sgFpfis),
atom_concat(RootPft,Ppfs,V3sgppfts),

% indicative present
%
asserta(lex(V3sg,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sg ,tense:present,kind:Kind,mood:indicative,
                  number:_,person:_])),
% subjunctive present
%
asserta(lex(V3sgsubj,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgsubj ,tense:present,kind:Kind,mood:subjunctive,
                  number:_,person:_])),
% indicative imperfect
%
asserta(lex(V3sgimpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpft ,tense:imperfect,kind:Kind,mood:indicative,
                  number:_,person:_])),
% indicative future
%
asserta(lex(V3sgfut,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgfut ,tense:future,kind:Kind,mood:indicative,
                  number:_,person:_])),
% subjunctive imperfect
%
asserta(lex(V3sgimpfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgimpfts ,tense:imperfect,kind:Kind,mood:subjunctive,
                  number:_,person:_])),
% infinitive present
%
asserta(lex(Inf,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:act,txt:Inf ,tense:present,kind:Kind,mood:infinitive,
                   number:_,person:_])),
% indicative perfect
%
asserta(lex(V3sgpft,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgpft ,tense:perfect,kind:Kind,mood:indicative,
                  number:_,person:_])),
% perfect infinitive
%
asserta(lex(Infp,v,[pos:v,class:Class,type:nonfinite,lex:Lex,
                   voice:act,txt:Infp,tense:past,kind:Kind,mood:infinitive,
                   number:_,person:_])),
% subjunctive perfect active
%
asserta(lex(V3sgFpfis,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgFpfis ,tense:perfect,kind:Kind,mood:subjunctive,
                  number:_,person:_])),
% future perfect indicative active
%
asserta(lex(V3sgFpfis,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgFpfis ,tense:future_perfect,kind:Kind,mood:indicative,
                  number:_,person:_])),
% subjunctive pluperfect active
%
asserta(lex(V3sgppfts,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgppfts ,tense:pluperfect,kind:Kind,mood:subjunctive,
                  number:_,person:_])),
% pluperfect indicative active
%
asserta(lex(V3sgppfti,v,[pos:v,class:Class,type:finite,lex:Lex,
                  voice:act,txt:V3sgppfti ,tense:pluperfect,kind:Kind,mood:indicative,
                  number:_,person:_])),

fail.

macro_imp.






%
% IRREGULAR VERBS (VERY SKETCHY)
%


%
% FERRE
%

% indicative present
%

% third person singular present tense
lex(fert,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:fert ,tense:present,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl present tense
lex(ferunt,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferunt ,tense:present,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular present tense
lex(fero,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:fero ,tense:present,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular present tense
lex(fers,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:fers ,tense:present,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural present tense
lex(ferimus,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferimus ,tense:present,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural present tense
lex(fertis,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:fertis ,tense:present,kind:std,mood:indicative,
                   number:pl,person:2]).

% subjunctive present
%

% third person singular present tense
lex(ferat,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferat ,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person pl present tense
lex(ferant,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferant ,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% first person singular present tense
lex(feram,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:feram,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% second person singular present tense
lex(feras,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:feras ,tense:present,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% first person plural present tense
lex(feramus,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:feramus ,tense:present,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural present tense
lex(feratis,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:feratis ,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:2]).

% subjunctive present passive
%

% third person singular present tense
lex(feratur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:feratur ,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person pl present tense
lex(ferantur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferantur ,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% first person singular present tense
lex(ferar,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferar,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% second person singular present tense
lex(feraris,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:feraris ,tense:present,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% first person plural present tense
lex(feramur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:feramur,tense:present,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural present tense
lex(feramini,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:feramini,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:2]).

% indicative future
%

% third person singular 
lex(feret,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:feret ,tense:future,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(ferent,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferent ,tense:future,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(feram,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:feram,tense:future,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(feres,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:feres ,tense:future,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(feremus,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:feremus ,tense:future,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(feretis,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:feretis ,tense:future,kind:std,mood:indicative,
                   number:pl,person:2]).

% indicative future passive
%

% third person singular 
lex(feretur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:feretur ,tense:future,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(ferentur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferentur ,tense:future,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(ferar,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferar,tense:future,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(fereris,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:fereris ,tense:future,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(feremur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:feremur ,tense:future,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(feremini,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:feremini ,tense:future,kind:std,mood:indicative,
                   number:pl,person:2]).

% indicative imperfect
%

% third person singular 
lex(ferebat,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferebat ,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(ferebant,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferebant ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(ferebam,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferebam,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(ferebas,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferebas ,tense:imperfect,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(ferebamus,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferebamus ,tense:imperfect,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(ferebatis,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferebatis ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:2]).

% indicative imperfect passive
%

% third person singular 
lex(ferebatur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferebatur ,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(ferebantur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferebantur ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(ferebar,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferebar,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(ferebaris,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferebaris ,tense:imperfect,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(ferebamur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferebamur ,tense:imperfect,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(ferebamini,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferebamini ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:2]).


% subjunctive imperfect
%

% third person singular 
lex(ferret,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferret ,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person pl 
lex(ferrent,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferrent ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% first person singular 
lex(ferrem,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferrem,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% second person singular 
lex(ferres,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferres ,tense:imperfect,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% first person plural 
lex(ferremus,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:act,txt:ferremus ,tense:imperfect,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural 
lex(ferretis,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferretis ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:2]).

% subjunctive imperfect passive
%

% third person singular 
lex(ferretur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferretur ,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person pl 
lex(ferrentur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferrentur ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% first person singular 
lex(ferrer,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferrer,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% second person singular 
lex(ferreris,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferreris ,tense:imperfect,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% first person plural 
lex(ferremur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferremur ,tense:imperfect,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural 
lex(ferremini,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferremini ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:2]).

% indicative present passive 
%

% third person singular present tense passive
lex(fertur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:fertur ,tense:present,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl present tense
lex(feruntur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:feruntur ,tense:present,kind:std,mood:indicative,
                   number:pl,person:3]).
% second person singular present tense passive
lex(ferris,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:ferris ,tense:present,kind:std,mood:indicative,
                  number:sing,person:2]).
% second person pl present tense
lex(ferimini,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferimini ,tense:present,kind:std,mood:indicative,
                   number:pl,person:2]).
% first person singular present tense passive
lex(feror,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:pass,txt:feror ,tense:present,kind:std,mood:indicative,
                  number:sing,person:1]).
% first person pl present tense
lex(ferimur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:pass,txt:ferimur ,tense:present,kind:std,mood:indicative,
                   number:pl,person:1]).

% middle
%

% third person singular present tense middle
lex(fertur,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                  voice:middle,txt:fertur ,tense:present,kind:std,mood:indicative,
                  number:sing,person:3]).

% infinitive
%

lex(ferre,v,[pos:v,class:v_ferre,type:nonfinite,lex:ferre,
                   voice:act,txt:ferre,tense:present,kind:std,mood:infinitive,
                   number:Nb,person:P]).
lex(ferri,v,[pos:v,class:v_ferre,type:nonfinite,lex:ferre,
                   voice:pass,txt:ferri,tense:present,kind:std,mood:infinitive,
                   number:Nb,person:P]).

% gerunds
%

% gerund -UM
lex(ferendum,v,[pos:v,class:v_ferre,type:gerund,lex:ferre,
                  txt:ferendum, kind:std,mood:gerund,
                  person:3,case:acc]).
% gerund -I
lex(ferendi,v,[pos:v,class:v_ferre,type:gerund,lex:ferre,
                  txt:ferendi, kind:std,mood:gerund,
                  person:3,case:gen]).

% gerund -O
lex(ferendo,v,[pos:v,class:v_ferre,type:gerund,lex:ferre,
                  txt:ferendo, kind:std,mood:gerund,
                  person:3,case:abl]).


% imperative
%

% present

% second person singular pr tense imp
lex(fer,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:fer ,tense:present,kind:std,mood:imperative,
                   number:sing,person:2]).
% second person plural pr tense imp
lex(ferte,v,[pos:v,class:v_ferre,type:finite,lex:ferre,
                   voice:act,txt:ferte,tense:present,kind:std,mood:imperative,
                   number:pl,person:2]).

% present participle
%

% SING

% nominative masc fem neuter sing
lex(ferens,p_pr,[pos:p_pr,txt:ferens ,case:nom, gender:_, number:sing,lex:ferre,
            class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% accusative neuter sg
lex(ferens,p_pr,[pos:p_pr,txt:ferens ,case:acc, gender:neuter, number:sing,lex:ferre,
            class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% accusative masc fem sing
lex(ferentem,p_pr,[pos:p_pr,txt:ferentem , case:acc, gender:or([masc,fem]), number:sing,lex:ferre,
            class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% genitive g3 sg
lex(ferentis,p_pr,[pos:p_pr,txt:ferentis, case:gen, gender:_, number:sing,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% dative g3 sg
lex(ferenti,p_pr,[pos:p_pr,txt:ferenti, case:dat, gender:_, number:sing,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% ablative g3 sg ending in -i
lex(ferenti,p_pr,[pos:p_pr,txt:ferenti, case:abl, gender:_, number:sing,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3,typeabl:i]).
% ablative g3 sg ending in -e
lex(ferente,p_pr,[pos:p_pr,txt:ferente, case:abl, gender:_, number:sing,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3,typeabl:e]).

% PLURAL

% nominative masc fem pl
lex(ferentes,p_pr,[pos:p_pr,txt:ferentes, case:nom, gender:or([masc,fem]), number:pl,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% accusative masc fem pl
lex(ferentes,p_pr,[pos:p_pr,txt:ferentes, case:acc, gender:or([masc,fem]), number:pl,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% nominative neuter pl
lex(ferentia,p_pr,[pos:p_pr,txt:ferentia, case:nom, gender:neuter, number:pl,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% accusative neuter pl
lex(ferentia,p_pr,[pos:p_pr,txt:ferentia, case:acc, gender:neuter, number:pl,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% genitive g3 pl
lex(ferentium,p_pr,[pos:p_pr,txt:ferentium, case:gen, gender:_, number:pl,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% dative g3 pl
lex(ferentibus,p_pr,[pos:p_pr,txt:ferentibus, case:dat, gender:_, number:pl,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).
% ablative g3 pl
lex(ferentibus,p_pr,[pos:p_pr,txt:ferentibus, case:abl, gender:_, number:pl,lex:ferre,
           class:v_ferre,type:p_pr,kind:std,mood:participle,person:3]).







%
% IRE
%

% indicative present
%

% third person singular present tense
lex(it,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:it ,tense:present,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl present tense
lex(eunt,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:eunt ,tense:present,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular present tense
lex(eo,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:eo ,tense:present,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular present tense
lex(is,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:is ,tense:present,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural present tense
lex(imus,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:imus ,tense:present,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural present tense
lex(itis,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:itis ,tense:present,kind:std,mood:indicative,
                   number:pl,person:2]).


% subjunctive present
%

% third person singular present tense
lex(eat,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:eat ,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person pl present tense
lex(eant,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:eant ,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% first person singular present tense
lex(eam,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:eam ,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% second person singular present tense
lex(eas,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:eas ,tense:present,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% first person plural present tense
lex(eamus,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:eamus ,tense:present,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural present tense
lex(eatis,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:eatis ,tense:present,kind:std,mood:subjunctive,
                   number:pl,person:2]).


% indicative imperfect
%

% third person singular 
lex(ibat,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:ibat ,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(ibant,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ibant ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(ibam,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:ibam ,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(ibas,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ibas ,tense:imperfect,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(ibamus,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:ibamus ,tense:imperfect,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(ibatis,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ibatis ,tense:imperfect,kind:std,mood:indicative,
                   number:pl,person:2]).


% indicative future
%

% third person singular 
lex(ibit,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:ibit ,tense:future,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl 
lex(ibunt,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ibunt ,tense:future,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular 
lex(ibo,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:ibo ,tense:future,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular 
lex(ibis,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ibis ,tense:future,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural 
lex(ibimus,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:ibimus ,tense:future,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural 
lex(ibitis,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ibitis ,tense:future,kind:std,mood:indicative,
                   number:pl,person:2]).

% subjunctive imperfect
%

% third person singular 
lex(iret,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:iret ,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% third person pl 
lex(irent,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:irent ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:3]).
% first person singular 
lex(irem,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:irem ,tense:imperfect,kind:std,mood:subjunctive,
                  number:sing,person:1]).
% second person singular 
lex(ires,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ires ,tense:imperfect,kind:std,mood:subjunctive,
                   number:sing,person:2]).
% first person plural 
lex(iremus,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:act,txt:iremus ,tense:imperfect,kind:std,mood:subjunctive,
                  number:pl,person:1]).
% second person plural 
lex(iretis,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:iretis ,tense:imperfect,kind:std,mood:subjunctive,
                   number:pl,person:2]).

% middle third person
%

% third person singular present tense middle
lex(itur,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:middle,txt:itur ,tense:present,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person singular imperfect tense middle
lex(ibatur,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:middle,txt:ibatur ,tense:imperfect,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person singular present tense middle subjunctive
lex(eatur,v,[pos:v,class:v_ire,type:finite,lex:ire,
                  voice:middle,txt:eatur ,tense:present,kind:std,mood:subjunctive,
                  number:sing,person:3]).
% supine                                  % amatum
lex(itum,v,[pos:v,class:v_ire,type:supine,lex:ire,
                  txt:itum ,kind:std,mood:supine,
                  person:3,case:_]).
% gerund -UM
lex(eundum,v,[pos:v,class:v_ire,type:gerund,lex:ire,
                  txt:eundum, kind:std,mood:gerund,
                  person:3,case:acc]).
% gerund -I
lex(eundi,v,[pos:v,class:v_ire,type:gerund,lex:ire,
                  txt:eundi, kind:std,mood:gerund,
                  person:3,case:gen]).

% gerund -O
lex(eundo,v,[pos:v,class:v_ire,type:gerund,lex:ire,
                  txt:eundo, kind:std,mood:gerund,
                  person:3,case:abl]).


% infinitive
%

lex(ire,v,[pos:v,class:v_ire,type:nonfinite,lex:ire,
                   voice:act,txt:ire,tense:present,kind:std,mood:infinitive,
                   number:Nb,person:P]).

% imperative
%

% present

% second person singular pr tense imp
lex(i,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:i ,tense:present,kind:std,mood:imperative,
                   number:sing,person:2]).
% second person plural pr tense imp
lex(ite,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:ite,tense:present,kind:std,mood:imperative,
                   number:pl,person:2]).

% present participle
%

% SING

% nominative masc fem neuter sing
lex(iens,p_pr,[pos:p_pr,txt:iens ,case:nom, gender:_, number:sing,lex:ire,
            class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% accusative neuter sg
lex(iens,p_pr,[pos:p_pr,txt:iens ,case:acc, gender:neuter, number:sing,lex:ire,
            class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% accusative masc fem sing
lex(euntem,p_pr,[pos:p_pr,txt:euntem , case:acc, gender:or([masc,fem]), number:sing,lex:ire,
            class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% genitive g3 sg
lex(euntis,p_pr,[pos:p_pr,txt:euntis, case:gen, gender:_, number:sing,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% dative g3 sg
lex(eunti,p_pr,[pos:p_pr,txt:eunti, case:dat, gender:_, number:sing,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% ablative g3 sg ending in -i
lex(eunti,p_pr,[pos:p_pr,txt:eunti, case:abl, gender:_, number:sing,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3,typeabl:i]).
% ablative g3 sg ending in -e
lex(eunte,p_pr,[pos:p_pr,txt:eunte, case:abl, gender:_, number:sing,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3,typeabl:e]).

% PLURAL

% nominative masc fem pl
lex(euntes,p_pr,[pos:p_pr,txt:euntes, case:nom, gender:or([masc,fem]), number:pl,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% accusative masc fem pl
lex(euntes,p_pr,[pos:p_pr,txt:euntes, case:acc, gender:or([masc,fem]), number:pl,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% nominative neuter pl
lex(euntia,p_pr,[pos:p_pr,txt:euntia, case:nom, gender:neuter, number:pl,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% accusative neuter pl
lex(euntia,p_pr,[pos:p_pr,txt:euntia, case:acc, gender:neuter, number:pl,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% genitive g3 pl
lex(euntium,p_pr,[pos:p_pr,txt:euntium, case:gen, gender:_, number:pl,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% dative g3 pl
lex(euntibus,p_pr,[pos:p_pr,txt:euntibus, case:dat, gender:_, number:pl,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).
% ablative g3 pl
lex(euntibus,p_pr,[pos:p_pr,txt:euntibus, case:abl, gender:_, number:pl,lex:ire,
           class:v_ire,type:p_pr,kind:std,mood:participle,person:3]).


% special forms second person perfect
%

% second person singular pft tense
lex(isti,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:isti ,tense:perfect,kind:std,mood:indicative,
                   number:sing,person:2]).

% second person plural pft tense
lex(istis,v,[pos:v,class:v_ire,type:finite,lex:ire,
                   voice:act,txt:istis ,tense:perfect,kind:std,mood:indicative,
                    number:pl,person:2]).








%
% MEMINI
%

% perfect indicative active
%

% third person singular pft tense
lex(meminit,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                  voice:act,txt:meminit ,tense:perfect,kind:std,mood:indicative,
                  number:sing,person:3]).
% third person pl pft tense
lex(meminerunt,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                   voice:act,txt:meminerunt ,tense:perfect,kind:std,mood:indicative,
                   number:pl,person:3]).
% third person pl pft tense second form
lex(meminere,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                   voice:act,txt:meminere ,tense:perfect,kind:std,mood:indicative,
                   number:pl,person:3]).
% first person singular pft tense
lex(memini,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                  voice:act,txt:memini,tense:perfect,kind:std,mood:indicative,
                  number:sing,person:1]).
% second person singular pft tense
lex(meministi,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                   voice:act,txt:meministi ,tense:perfect,kind:std,mood:indicative,
                   number:sing,person:2]).
% first person plural pft tense
lex(meminimus,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                  voice:act,txt:meminimus ,tense:perfect,kind:std,mood:indicative,
                  number:pl,person:1]).
% second person plural pft tense
lex(meministis,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                   voice:act,txt:meministis,tense:perfect,kind:std,mood:indicative,
                    number:pl,person:2]).

% infinitive
%

lex(meminisse,v,[pos:v,class:tr_cod,type:nonfinite,lex:memini,
                  voice:act,txt:meminisse ,tense:perfect,kind:std,mood:infinitive,
                  number:_,person:_]).

% imperative
%

% future

% second person singular future tense imp
lex(memento,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                   voice:act,txt:memento ,tense:future,kind:std,mood:imperative,
                   number:sing,person:2]).
% second person plural future tense imp
lex(mementote,v,[pos:v,class:tr_cod,type:finite,lex:memini,
                   voice:act,txt:mememtote,tense:future,kind:std,mood:imperative,
                   number:pl,person:2]).





%%%%%%%%%%%%%
% ADJECTIVES
%%%%%%%%%%%%%



% bonus, bona, bonum
declension_endings(adj,1,
                 [_NIL,um,i,o,o,
                  a,am,ae,ae,a,
                  um,um,i,o,o,
                  i,os,orum,is,is,
                 ae,as,arum,is,is,
                 a,a,orum,is,is]).

% superlatives:
declension_endings(adj,sup,
                 [us,um,i,o,o,
                  a,am,ae,ae,a,
                  um,um,i,o,o,
                  i,os,orum,is,is,
                 ae,as,arum,is,is,
                 a,a,orum,is,is]).

% comparatives:
declension_endings(adj,comp,
                  [ior,iorem,ioris,iori,iore,
                   ius,ius,ioris,iori,iori,
                  iores,iores,iorum,ioribus,ioribus,
                  iora,iora,iorum,ioribus,ioribus]).

% unus, unius, uni
declension_endings(adj,a1,
                 [_NIL,um,ius,i,o,
                  a,am,ius,i,a,
                  um,um,ius,i,o,
                  i,os,orum,is,is,
                 ae,as,arum,is,is,
                 a,a,orum,is,is]).

% fortis, fortis, forte
declension_endings(adj,2,
                  [_NIL,em,is,i,i,
                   e,e,is,i,i,
                  es,es,ium,ibus,ibus,
                  ia,ia,ium,ibus,ibus]).

% ingens, ingens, ingens
declension_endings(adj,a2,
                  [_NIL,em,is,i,i,
                   _NIL,_NIL,is,i,i,
                  es,es,ium,ibus,ibus,
                  ia,ia,ium,ibus,ibus]).

% present participle
declension_endings(partpr,[em,is,i,i,e,es,es,ia,ia,ium,ibus,ibus]).

% NOUNS
%

% regina
declension_endings(n,1,fem,[a,am,ae,ae,a,
                     ae,as,arum,is,is]).

% dominus
declension_endings(n,2,masc,[us,um,i,o,o,
                     i,os,orum,is,is]).

% templum
declension_endings(n,2,neuter,[um,um,i,o,o,
                       a,a,orum,is,is]).


% ratio
declension_endings(n,3,mf,[_NIL,em,is,i,e,
                  es,es,ibus,ibus]).

% flumen
declension_endings(n,3,neuter,[_NIL,_NIL,is,i,e,
                       a,a,ibus,ibus]).

% exercitus
declension_endings(n,4,mf,[_NIL,um,us,ui,u,
                  us,us,uum,ibus,ibus]).

% res
declension_endings(n,5,mf,[es,em,ei,ei,e,
                   es,es,erum,ebus,ebus]).

%

% COMPARATIVES AND SUPERLATIVES
%

macro_adj_all :-
adj(FNmsg,Root,_,Type,[Comp,Sup]),

% superlatives:
declension_endings(adj,sup,
                 [Nmsgs,Acmsgs,Genmsgs,Datmsgs,Ablmsgs,
                  Nfsgs,Acfsgs,Genfsgs,Datfsgs,Ablfsgs,
                  Nnsgs,Acnsgs,Gennsgs,Datnsgs,Ablnsgs,
                  Nmpls,Acmpls,Genmpls,Datmpls,Ablmpls,
                  Nfpls,Acfpls,Genfpls,Datfpls,Ablfpls,
                  Nnpls,Acnpls,Gennpls,Datnpls,Ablnpls]),

% comparatives:
declension_endings(adj,comp,
                  [Nmfsgc,Acmfsgc,Genmfsgc,Datmfsgc,Ablmfsgc,
                    Nnsgc,Acnsgc,Gennsgc,Datnsgc,Ablnsgc,
                  Nmfplc,Acmfplc,Genmfplc,Datmfplc,Ablmfplc,
                  Nnplc,Acnplc,Gennplc,Datnplc,Ablnplc]),


ifthen(Sup \= xxx,

(atom_concat(Sup,Nmsgs,FNmsgs),
atom_concat(Sup,Acmsgs,FAcmsgs),
atom_concat(Sup,Genmsgs,FGenmsgs),
atom_concat(Sup,Datmsgs,FDatmsgs),
atom_concat(Sup,Ablmsgs,FAblmsgs),
atom_concat(Sup,Nfsgs,FNfsgs),
atom_concat(Sup,Acfsgs,FAcfsgs),
atom_concat(Sup,Genfsgs,FGenfsgs),
atom_concat(Sup,Datfsgs,FDatfsgs),
atom_concat(Sup,Ablfsgs,FAblfsgs),
atom_concat(Sup,Nnsgs,FNnsgs),
atom_concat(Sup,Acnsgs,FAcnsgs),
atom_concat(Sup,Gennsgs,FGennsgs),
atom_concat(Sup,Datnsgs,FDatnsgs),
atom_concat(Sup,Ablnsgs,FAblnsgs),
atom_concat(Sup,Nmpls,FNmpls),
atom_concat(Sup,Acmpls,FAcmpls),
atom_concat(Sup,Genmpls,FGenmpls),
atom_concat(Sup,Datmpls,FDatmpls),
atom_concat(Sup,Ablmpls,FAblmpls),
atom_concat(Sup,Nfpls,FNfpls),
atom_concat(Sup,Acfpls,FAcfpls),
atom_concat(Sup,Genfpls,FGenfpls),
atom_concat(Sup,Datfpls,FDatfpls),
atom_concat(Sup,Ablfpls,FAblfpls),
atom_concat(Sup,Nnpls,FNnpls),
atom_concat(Sup,Acnpls,FAcnpls),
atom_concat(Sup,Gennpls,FGennpls),
atom_concat(Sup,Datnpls,FDatnpls),
atom_concat(Sup,Ablnpls,FAblnpls),


% SUPERLATIVES

% SING

% nominative masc sing
asserta(lex(FNmsgs,adj,[pos:adj,txt:FNmsgs,lex:FNmsg, type:Type, case:nom, gender:masc, number:sing, degree:sup])),
% accusative masc sing
asserta(lex(FAcmsgs,adj,[pos:adj,txt:FAcmsgs,lex:FNmsg, type:Type,case:acc, gender:masc, number:sing, degree:sup])),
% genitive masc sing
asserta(lex(FGenmsgs,adj,[pos:adj,txt:FGenmsgs,lex:FNmsg,type:Type, case:gen, gender:masc, number:sing, degree:sup])),
% dative masc sing
asserta(lex(FDatmsgs,adj,[pos:adj,txt:FDatmsgs,lex:FNmsg,type:Type, case:dat, gender:masc, number:sing, degree:sup])),
% ablative masc sing
asserta(lex(FAblmsgs,adj,[pos:adj,txt:FAblmsgs,lex:FNmsg,type:Type, case:abl, gender:masc, number:sing, degree:sup])),
% nominative fem sing
asserta(lex(FNfsgs,adj,[pos:adj,txt:FNfsgs,lex:FNmsg,type:Type, case:nom, gender:fem, number:sing, degree:sup])),
% accusative fem sing
asserta(lex(FAcfsgs,adj,[pos:adj,txt:FAcfsgs,lex:FNmsg,type:Type, case:acc, gender:fem, number:sing, degree:sup])),
% genitive fem sing
asserta(lex(FGenfsgs,adj,[pos:adj,txt:FGenfsgs,lex:FNmsg, type:Type,case:gen, gender:fem, number:sing, degree:sup])),
% dative fem sing
asserta(lex(FDatfsgs,adj,[pos:adj,txt:FDatfsgs,lex:FNmsg, type:Type,case:dat, gender:fem, number:sing, degree:sup])),
% ablative fem sing
asserta(lex(FAblfsgs,adj,[pos:adj,txt:FAblfsgs,lex:FNmsg, type:Type,case:abl, gender:fem, number:sing, degree:sup])),
% nominative n sing
asserta(lex(FNnsgs,adj,[pos:adj,txt:FNnsgs,lex:FNmsg, type:Type,case:nom, gender:neuter, number:sing, degree:sup])),
% accusative n sing
asserta(lex(FAcnsgs,adj,[pos:adj,txt:FAcnsgs,lex:FNmsg, type:Type,case:acc, gender:neuter, number:sing, degree:sup])),
% genitive n sing
asserta(lex(FGennsgs,adj,[pos:adj,txt:FGennsgs,lex:FNmsg,type:Type, case:gen, gender:neuter, number:sing, degree:sup])),
% dative n sing
asserta(lex(FDatnsgs,adj,[pos:adj,txt:FDatnsgs,lex:FNmsg,type:Type, case:dat, gender:neuter, number:sing, degree:sup])),
% ablative n sing
asserta(lex(FAblnsgs,adj,[pos:adj,txt:FAblnsgs,lex:FNmsg,type:Type, case:abl, gender:neuter, number:sing, degree:sup])),


% PLURAL

% nominative masc pl
asserta(lex(FNmpls,adj,[pos:adj,txt:FNmpls,lex:FNmsg,type:Type, case:nom, gender:masc, number:pl, degree:sup])),
% accusative masc pl
asserta(lex(FAcmpls,adj,[pos:adj,txt:FAcmpls,lex:FNmsg, type:Type,case:acc, gender:masc, number:pl, degree:sup])),
% genitive masc pl
asserta(lex(FGenmpls,adj,[pos:adj,txt:FGenmpls,lex:FNmsg, type:Type, case:gen, gender:masc, number:pl, degree:sup])),
% dative masc pl
asserta(lex(FDatmpls,adj,[pos:adj,txt:FDatmpls,lex:FNmsg, type:Type, case:dat, gender:masc, number:pl, degree:sup])),
% ablative masc pl
asserta(lex(FAblmpls,adj,[pos:adj,txt:FAblmpls,lex:FNmsg, type:Type, case:abl, gender:masc, number:pl, degree:sup])),
% nominative fem pl
asserta(lex(FNfpls,adj,[pos:adj,txt:FNfpls,lex:FNmsg, type:Type, case:nom, gender:fem, number:pl, degree:sup])),
% accusative fem pl
asserta(lex(FAcfpls,adj,[pos:adj,txt:FAcfpls,lex:FNmsg, type:Type, case:acc, gender:fem, number:pl, degree:sup])),
% genitive fem pl
asserta(lex(FGenfpls,adj,[pos:adj,txt:FGenfpls,lex:FNmsg, type:Type, case:gen, gender:fem, number:pl, degree:sup])),
% dative fem pl
asserta(lex(FDatfpls,adj,[pos:adj,txt:FDatfpls,lex:FNmsg, type:Type, case:dat, gender:fem, number:pl, degree:sup])),
% ablative fem pl
asserta(lex(FAblfpls,adj,[pos:adj,txt:FAblfpls,lex:FNmsg, type:Type, case:abl, gender:fem, number:pl, degree:sup])),
% nominative n pl
asserta(lex(FNnpls,adj,[pos:adj,txt:FNnpls,lex:FNmsg, type:Type, case:nom, gender:neuter, number:pl, degree:sup])),
% accusative n pl
asserta(lex(FAcnpls,adj,[pos:adj,txt:FAcnpls,lex:FNmsg, type:Type, case:acc, gender:neuter, number:pl, degree:sup])),
% genitive n pl
asserta(lex(FGennpls,adj,[pos:adj,txt:FGennpls,lex:FNmsg, type:Type, case:gen, gender:neuter, number:pl, degree:sup])),
% dative n pl
asserta(lex(FDatnpls,adj,[pos:adj,txt:FDatnpls,lex:FNmsg, type:Type, case:dat, gender:neuter, number:pl, degree:sup])),
% ablative n pl
asserta(lex(FAblnpls,adj,[pos:adj,txt:FAblnpls,lex:FNmsg, type:Type, case:abl, gender:neuter, number:pl, degree:sup])) )),

% COMPARATIVES
%

ifthen(Comp \= xxx,

(atom_concat(Comp,Nmfsgc,FNmfsgc),
atom_concat(Comp,Acmfsgc,FAcmfsgc),
atom_concat(Comp,Genmfsgc,FGenmfsgc),
atom_concat(Comp,Datmfsgc,FDatmfsgc),
atom_concat(Comp,Ablmfsgc,FAblmfsgc),

atom_concat(Comp,Nnsgc,FNnsgc),
atom_concat(Comp,Acnsgc,FAcnsgc),
atom_concat(Comp,Gennsgc,FGennsgc),
atom_concat(Comp,Datnsgc,FDatnsgc),
atom_concat(Comp,Ablnsgc,FAblnsgc),

atom_concat(Comp,Nmfplc,FNmfplc),
atom_concat(Comp,Acmfplc,FAcmfplc),
atom_concat(Comp,Genmfplc,FGenmfplc),
atom_concat(Comp,Datmfplc,FDatmfplc),
atom_concat(Comp,Ablmfplc,FAblmfplc),

atom_concat(Comp,Nnplc,FNnplc),
atom_concat(Comp,Acnplc,FAcnplc),
atom_concat(Comp,Gennplc,FGennplc),
atom_concat(Comp,Datnplc,FDatnplc),
atom_concat(Comp,Ablnplc,FAblnplc),


% SING

% nominative masc fem sing
asserta(lex(FNmfsgc,adj,[pos:adj,txt:FNmfsgc,lex:FNmsg, type:Type, case:nom, gender:masc, number:sing, degree:comp])),
% accusative masc fem sing
asserta(lex(FAcmfsgc,adj,[pos:adj,txt:FAcmfsgc,lex:FNmsg, type:Type,case:acc, gender:masc, number:sing, degree:comp])),
% genitive masc fem sing
asserta(lex(FGenmfsgc,adj,[pos:adj,txt:FGenmfsgc,lex:FNmsg,type:Type, case:gen, gender:masc, number:sing, degree:comp])),
% dative masc fem sing
asserta(lex(FDatmfsgc,adj,[pos:adj,txt:FDatmfsgc,lex:FNmsg,type:Type, case:dat, gender:masc, number:sing, degree:comp])),
% ablative masc fem sing
asserta(lex(FAblmfsgc,adj,[pos:adj,txt:FAblmfsgc,lex:FNmsg,type:Type, case:abl, gender:masc, number:sing, degree:comp])),

% nominative masc fem sing
asserta(lex(FNmfsgc,adj,[pos:adj,txt:FNmfsgc,lex:FNmsg, type:Type, case:nom, gender:fem, number:sing, degree:comp])),
% accusative masc fem sing
asserta(lex(FAcmfsgc,adj,[pos:adj,txt:FAcmfsgc,lex:FNmsg, type:Type,case:acc, gender:fem, number:sing, degree:comp])),
% genitive masc fem sing
asserta(lex(FGenmfsgc,adj,[pos:adj,txt:FGenmfsgc,lex:FNmsg,type:Type, case:gen, gender:fem, number:sing, degree:comp])),
% dative masc fem sing
asserta(lex(FDatmfsgc,adj,[pos:adj,txt:FDatmfsgc,lex:FNmsg,type:Type, case:dat, gender:fem, number:sing, degree:comp])),
% ablative masc fem sing
asserta(lex(FAblmfsgc,adj,[pos:adj,txt:FAblmfsgc,lex:FNmsg,type:Type, case:abl, gender:fem, number:sing, degree:comp])),


% nominative n sing
asserta(lex(FNnsgc,adj,[pos:adj,txt:FNnsgc,lex:FNmsg, type:Type,case:nom, gender:neuter, number:sing, degree:comp])),
% accusative n sing
asserta(lex(FAcnsgc,adj,[pos:adj,txt:FAcnsgc,lex:FNmsg, type:Type,case:acc, gender:neuter, number:sing, degree:comp])),
% genitive n sing
asserta(lex(FGennsgc,adj,[pos:adj,txt:FGennsgc,lex:FNmsg,type:Type, case:gen, gender:neuter, number:sing, degree:comp])),
% dative n sing
asserta(lex(FDatnsgc,adj,[pos:adj,txt:FDatnsgc,lex:FNmsg,type:Type, case:dat, gender:neuter, number:sing, degree:comp])),
% ablative n sing
asserta(lex(FAblnsgc,adj,[pos:adj,txt:FAblnsgc,lex:FNmsg,type:Type, case:abl, gender:neuter, number:sing, degree:comp])),


% PLURAL

% nominative masc fem pl
asserta(lex(FNmfplc,adj,[pos:adj,txt:FNmfplc,lex:FNmsg,type:Type, case:nom, gender:masc, number:pl, degree:comp])),
% accusative masc fem pl
asserta(lex(FAcmfplc,adj,[pos:adj,txt:FAcmfplc,lex:FNmsg, type:Type,case:acc, gender:masc, number:pl, degree:comp])),
% genitive masc fem pl
asserta(lex(FGenmfplc,adj,[pos:adj,txt:FGenmfplc,lex:FNmsg, type:Type, case:gen, gender:masc, number:pl, degree:comp])),
% dative masc fem pl
asserta(lex(FDatmfplc,adj,[pos:adj,txt:FDatmfplc,lex:FNmsg, type:Type, case:dat, gender:masc, number:pl, degree:comp])),
% ablative masc fem pl
asserta(lex(FAblmfplc,adj,[pos:adj,txt:FAblmfplc,lex:FNmsg, type:Type, case:abl, gender:masc, number:pl, degree:comp])),

% nominative masc fem pl
asserta(lex(FNmfplc,adj,[pos:adj,txt:FNmfplc,lex:FNmsg,type:Type, case:nom, gender:fem, number:pl, degree:comp])),
% accusative masc fem pl
asserta(lex(FAcmfplc,adj,[pos:adj,txt:FAcmfplc,lex:FNmsg, type:Type,case:acc, gender:fem, number:pl, degree:comp])),
% genitive masc fem pl
asserta(lex(FGenmfplc,adj,[pos:adj,txt:FGenmfplc,lex:FNmsg, type:Type, case:gen, gender:fem, number:pl, degree:comp])),
% dative masc fem pl
asserta(lex(FDatmfplc,adj,[pos:adj,txt:FDatmfplc,lex:FNmsg, type:Type, case:dat, gender:fem, number:pl, degree:comp])),
% ablative masc fem pl
asserta(lex(FAblmfplc,adj,[pos:adj,txt:FAblmfplc,lex:FNmsg, type:Type, case:abl, gender:fem, number:pl, degree:comp])),



% nominative n pl
asserta(lex(FNnplc,adj,[pos:adj,txt:FNnplc,lex:FNmsg, type:Type, case:nom, gender:neuter, number:pl, degree:comp])),
% accusative n pl
asserta(lex(FAcnplc,adj,[pos:adj,txt:FAcnplc,lex:FNmsg, type:Type, case:acc, gender:neuter, number:pl, degree:comp])),
% genitive n pl
asserta(lex(FGennplc,adj,[pos:adj,txt:FGennplc,lex:FNmsg, type:Type, case:gen, gender:neuter, number:pl, degree:comp])),
% dative n pl
asserta(lex(FDatnplc,adj,[pos:adj,txt:FDatnplc,lex:FNmsg, type:Type, case:dat, gender:neuter, number:pl, degree:comp])),
% ablative n pl
asserta(lex(FAblnplc,adj,[pos:adj,txt:FAblnplc,lex:FNmsg, type:Type, case:abl, gender:neuter, number:pl, degree:comp])) )),

fail.

macro_adj_all.

%

macro_adj_1 :-
adj(FNmsg,Root,1,Type,[Comp,Sup]),
declension_endings(adj,1,[_,Acmsg,Genmsg,Datmsg,Ablmsg,
                  Nfsg,Acfsg,Genfsg,Datfsg,Ablfsg,
                  Nnsg,Acnsg,Gennsg,Datnsg,Ablnsg,
                  Nmpl,Acmpl,Genmpl,Datmpl,Ablmpl,
                  Nfpl,Acfpl,Genfpl,Datfpl,Ablfpl,
                  Nnpl,Acnpl,Gennpl,Datnpl,Ablnpl]),


atom_concat(Root,Acmsg,FAcmsg),
atom_concat(Root,Genmsg,FGenmsg),
atom_concat(Root,Datmsg,FDatmsg),
atom_concat(Root,Ablmsg,FAblmsg),
atom_concat(Root,Nfsg,FNfsg),
atom_concat(Root,Acfsg,FAcfsg),
atom_concat(Root,Genfsg,FGenfsg),
atom_concat(Root,Datfsg,FDatfsg),
atom_concat(Root,Ablfsg,FAblfsg),
atom_concat(Root,Nnsg,FNnsg),
atom_concat(Root,Acnsg,FAcnsg),
atom_concat(Root,Gennsg,FGennsg),
atom_concat(Root,Datnsg,FDatnsg),
atom_concat(Root,Ablnsg,FAblnsg),
atom_concat(Root,Nmpl,FNmpl),
atom_concat(Root,Acmpl,FAcmpl),
atom_concat(Root,Genmpl,FGenmpl),
atom_concat(Root,Datmpl,FDatmpl),
atom_concat(Root,Ablmpl,FAblmpl),
atom_concat(Root,Nfpl,FNfpl),
atom_concat(Root,Acfpl,FAcfpl),
atom_concat(Root,Genfpl,FGenfpl),
atom_concat(Root,Datfpl,FDatfpl),
atom_concat(Root,Ablfpl,FAblfpl),
atom_concat(Root,Nnpl,FNnpl),
atom_concat(Root,Acnpl,FAcnpl),
atom_concat(Root,Gennpl,FGennpl),
atom_concat(Root,Datnpl,FDatnpl),
atom_concat(Root,Ablnpl,FAblnpl),


% SING

% nominative masc sing
asserta(lex(FNmsg,adj,[pos:adj,txt:FNmsg,lex:FNmsg, type:Type, case:nom, gender:masc, number:sing, degree:pos])),
% accusative masc sing
asserta(lex(FAcmsg,adj,[pos:adj,txt:FAcmsg,lex:FNmsg, type:Type,case:acc, gender:masc, number:sing, degree:pos])),
% genitive masc sing
asserta(lex(FGenmsg,adj,[pos:adj,txt:FGenmsg,lex:FNmsg,type:Type, case:gen, gender:masc, number:sing, degree:pos])),
% dative masc sing
asserta(lex(FDatmsg,adj,[pos:adj,txt:FDatmsg,lex:FNmsg,type:Type, case:dat, gender:masc, number:sing, degree:pos])),
% ablative masc sing
asserta(lex(FAblmsg,adj,[pos:adj,txt:FAblmsg,lex:FNmsg,type:Type, case:abl, gender:masc, number:sing, degree:pos])),
% nominative fem sing
asserta(lex(FNfsg,adj,[pos:adj,txt:FNfsg,lex:FNmsg,type:Type, case:nom, gender:fem, number:sing, degree:pos])),
% accusative fem sing
asserta(lex(FAcfsg,adj,[pos:adj,txt:FAcfsg,lex:FNmsg,type:Type, case:acc, gender:fem, number:sing, degree:pos])),
% genitive fem sing
asserta(lex(FGenfsg,adj,[pos:adj,txt:FGenfsg,lex:FNmsg, type:Type,case:gen, gender:fem, number:sing, degree:pos])),
% dative fem sing
asserta(lex(FDatfsg,adj,[pos:adj,txt:FDatfsg,lex:FNmsg, type:Type,case:dat, gender:fem, number:sing, degree:pos])),
% ablative fem sing
asserta(lex(FAblfsg,adj,[pos:adj,txt:FAblfsg,lex:FNmsg, type:Type,case:abl, gender:fem, number:sing, degree:pos])),
% nominative n sing
asserta(lex(FNnsg,adj,[pos:adj,txt:FNnsg,lex:FNmsg, type:Type,case:nom, gender:neuter, number:sing, degree:pos])),
% accusative n sing
asserta(lex(FAcnsg,adj,[pos:adj,txt:FAcnsg,lex:FNmsg, type:Type,case:acc, gender:neuter, number:sing, degree:pos])),
% genitive n sing
asserta(lex(FGennsg,adj,[pos:adj,txt:FGennsg,lex:FNmsg,type:Type, case:gen, gender:neuter, number:sing, degree:pos])),
% dative n sing
asserta(lex(FDatnsg,adj,[pos:adj,txt:FDatnsg,lex:FNmsg,type:Type, case:dat, gender:neuter, number:sing, degree:pos])),
% ablative n sing
asserta(lex(FAblnsg,adj,[pos:adj,txt:FAblnsg,lex:FNmsg,type:Type, case:abl, gender:neuter, number:sing, degree:pos])),


% PLURAL

% nominative masc pl
asserta(lex(FNmpl,adj,[pos:adj,txt:FNmpl,lex:FNmsg,type:Type, case:nom, gender:masc, number:pl, degree:pos])),
% accusative masc pl
asserta(lex(FAcmpl,adj,[pos:adj,txt:FAcmpl,lex:FNmsg, type:Type,case:acc, gender:masc, number:pl, degree:pos])),
% genitive masc pl
asserta(lex(FGenmpl,adj,[pos:adj,txt:FGenmpl,lex:FNmsg, type:Type, case:gen, gender:masc, number:pl, degree:pos])),
% dative masc pl
asserta(lex(FDatmpl,adj,[pos:adj,txt:FDatmpl,lex:FNmsg, type:Type, case:dat, gender:masc, number:pl, degree:pos])),
% ablative masc pl
asserta(lex(FAblmpl,adj,[pos:adj,txt:FAblmpl,lex:FNmsg, type:Type, case:abl, gender:masc, number:pl, degree:pos])),
% nominative fem pl
asserta(lex(FNfpl,adj,[pos:adj,txt:FNfpl,lex:FNmsg, type:Type, case:nom, gender:fem, number:pl, degree:pos])),
% accusative fem pl
asserta(lex(FAcfpl,adj,[pos:adj,txt:FAcfpl,lex:FNmsg, type:Type, case:acc, gender:fem, number:pl, degree:pos])),
% genitive fem pl
asserta(lex(FGenfpl,adj,[pos:adj,txt:FGenfpl,lex:FNmsg, type:Type, case:gen, gender:fem, number:pl, degree:pos])),
% dative fem pl
asserta(lex(FDatfpl,adj,[pos:adj,txt:FDatfpl,lex:FNmsg, type:Type, case:dat, gender:fem, number:pl, degree:pos])),
% ablative fem pl
asserta(lex(FAblfpl,adj,[pos:adj,txt:FAblfpl,lex:FNmsg, type:Type, case:abl, gender:fem, number:pl, degree:pos])),
% nominative n pl
asserta(lex(FNnpl,adj,[pos:adj,txt:FNnpl,lex:FNmsg, type:Type, case:nom, gender:neuter, number:pl, degree:pos])),
% accusative n pl
asserta(lex(FAcnpl,adj,[pos:adj,txt:FAcnpl,lex:FNmsg, type:Type, case:acc, gender:neuter, number:pl, degree:pos])),
% genitive n pl
asserta(lex(FGennpl,adj,[pos:adj,txt:FGennpl,lex:FNmsg, type:Type, case:gen, gender:neuter, number:pl, degree:pos])),
% dative n pl
asserta(lex(FDatnpl,adj,[pos:adj,txt:FDatnpl,lex:FNmsg, type:Type, case:dat, gender:neuter, number:pl, degree:pos])),
% ablative n pl
asserta(lex(FAblnpl,adj,[pos:adj,txt:FAblnpl,lex:FNmsg, type:Type, case:abl, gender:neuter, number:pl, degree:pos])),

fail.

macro_adj_1.

%
macro_adj_1a :-
adj(FNmsg,Root,a1,Type,[Comp,Sup]),
declension_endings(adj,a1,[_,Acmsg,Genmsg,Datmsg,Ablmsg,
                  Nfsg,Acfsg,Genfsg,Datfsg,Ablfsg,
                  Nnsg,Acnsg,Gennsg,Datnsg,Ablnsg,
                  Nmpl,Acmpl,Genmpl,Datmpl,Ablmpl,
                  Nfpl,Acfpl,Genfpl,Datfpl,Ablfpl,
                  Nnpl,Acnpl,Gennpl,Datnpl,Ablnpl]),


atom_concat(Root,Acmsg,FAcmsg),
atom_concat(Root,Genmsg,FGenmsg),
atom_concat(Root,Datmsg,FDatmsg),
atom_concat(Root,Ablmsg,FAblmsg),
atom_concat(Root,Nfsg,FNfsg),
atom_concat(Root,Acfsg,FAcfsg),
atom_concat(Root,Genfsg,FGenfsg),
atom_concat(Root,Datfsg,FDatfsg),
atom_concat(Root,Ablfsg,FAblfsg),
atom_concat(Root,Nnsg,FNnsg),
atom_concat(Root,Acnsg,FAcnsg),
atom_concat(Root,Gennsg,FGennsg),
atom_concat(Root,Datnsg,FDatnsg),
atom_concat(Root,Ablnsg,FAblnsg),
atom_concat(Root,Nmpl,FNmpl),
atom_concat(Root,Acmpl,FAcmpl),
atom_concat(Root,Genmpl,FGenmpl),
atom_concat(Root,Datmpl,FDatmpl),
atom_concat(Root,Ablmpl,FAblmpl),
atom_concat(Root,Nfpl,FNfpl),
atom_concat(Root,Acfpl,FAcfpl),
atom_concat(Root,Genfpl,FGenfpl),
atom_concat(Root,Datfpl,FDatfpl),
atom_concat(Root,Ablfpl,FAblfpl),
atom_concat(Root,Nnpl,FNnpl),
atom_concat(Root,Acnpl,FAcnpl),
atom_concat(Root,Gennpl,FGennpl),
atom_concat(Root,Datnpl,FDatnpl),
atom_concat(Root,Ablnpl,FAblnpl),


% SING

% nominative masc sing
asserta(lex(FNmsg,adj,[pos:adj,txt:FNmsg,lex:FNmsg, type:Type, case:nom, gender:masc, number:sing, degree:pos])),
% accusative masc sing
asserta(lex(FAcmsg,adj,[pos:adj,txt:FAcmsg,lex:FNmsg, type:Type,case:acc, gender:masc, number:sing, degree:pos])),
% genitive masc sing
asserta(lex(FGenmsg,adj,[pos:adj,txt:FGenmsg,lex:FNmsg,type:Type, case:gen, gender:masc, number:sing, degree:pos])),
% dative masc sing
asserta(lex(FDatmsg,adj,[pos:adj,txt:FDatmsg,lex:FNmsg,type:Type, case:dat, gender:masc, number:sing, degree:pos])),
% ablative masc sing
asserta(lex(FAblmsg,adj,[pos:adj,txt:FAblmsg,lex:FNmsg,type:Type, case:abl, gender:masc, number:sing, degree:pos])),
% nominative fem sing
asserta(lex(FNfsg,adj,[pos:adj,txt:FNfsg,lex:FNmsg,type:Type, case:nom, gender:fem, number:sing, degree:pos])),
% accusative fem sing
asserta(lex(FAcfsg,adj,[pos:adj,txt:FAcfsg,lex:FNmsg,type:Type, case:acc, gender:fem, number:sing, degree:pos])),
% genitive fem sing
asserta(lex(FGenfsg,adj,[pos:adj,txt:FGenfsg,lex:FNmsg, type:Type,case:gen, gender:fem, number:sing, degree:pos])),
% dative fem sing
asserta(lex(FDatfsg,adj,[pos:adj,txt:FDatfsg,lex:FNmsg, type:Type,case:dat, gender:fem, number:sing, degree:pos])),
% ablative fem sing
asserta(lex(FAblfsg,adj,[pos:adj,txt:FAblfsg,lex:FNmsg, type:Type,case:abl, gender:fem, number:sing, degree:pos])),
% nominative n sing
asserta(lex(FNnsg,adj,[pos:adj,txt:FNnsg,lex:FNmsg, type:Type,case:nom, gender:neuter, number:sing, degree:pos])),
% accusative n sing
asserta(lex(FAcnsg,adj,[pos:adj,txt:FAcnsg,lex:FNmsg, type:Type,case:acc, gender:neuter, number:sing, degree:pos])),
% genitive n sing
asserta(lex(FGennsg,adj,[pos:adj,txt:FGennsg,lex:FNmsg,type:Type, case:gen, gender:neuter, number:sing, degree:pos])),
% dative n sing
asserta(lex(FDatnsg,adj,[pos:adj,txt:FDatnsg,lex:FNmsg,type:Type, case:dat, gender:neuter, number:sing, degree:pos])),
% ablative n sing
asserta(lex(FAblnsg,adj,[pos:adj,txt:FAblnsg,lex:FNmsg,type:Type, case:abl, gender:neuter, number:sing, degree:pos])),


% PLURAL

% nominative masc pl
asserta(lex(FNmpl,adj,[pos:adj,txt:FNmpl,lex:FNmsg,type:Type, case:nom, gender:masc, number:pl, degree:pos])),
% accusative masc pl
asserta(lex(FAcmpl,adj,[pos:adj,txt:FAcmpl,lex:FNmsg, type:Type,case:acc, gender:masc, number:pl, degree:pos])),
% genitive masc pl
asserta(lex(FGenmpl,adj,[pos:adj,txt:FGenmpl,lex:FNmsg, type:Type, case:gen, gender:masc, number:pl, degree:pos])),
% dative masc pl
asserta(lex(FDatmpl,adj,[pos:adj,txt:FDatmpl,lex:FNmsg, type:Type, case:dat, gender:masc, number:pl, degree:pos])),
% ablative masc pl
asserta(lex(FAblmpl,adj,[pos:adj,txt:FAblmpl,lex:FNmsg, type:Type, case:abl, gender:masc, number:pl, degree:pos])),
% nominative fem pl
asserta(lex(FNfpl,adj,[pos:adj,txt:FNfpl,lex:FNmsg, type:Type, case:nom, gender:fem, number:pl, degree:pos])),
% accusative fem pl
asserta(lex(FAcfpl,adj,[pos:adj,txt:FAcfpl,lex:FNmsg, type:Type, case:acc, gender:fem, number:pl, degree:pos])),
% genitive fem pl
asserta(lex(FGenfpl,adj,[pos:adj,txt:FGenfpl,lex:FNmsg, type:Type, case:gen, gender:fem, number:pl, degree:pos])),
% dative fem pl
asserta(lex(FDatfpl,adj,[pos:adj,txt:FDatfpl,lex:FNmsg, type:Type, case:dat, gender:fem, number:pl, degree:pos])),
% ablative fem pl
asserta(lex(FAblfpl,adj,[pos:adj,txt:FAblfpl,lex:FNmsg, type:Type, case:abl, gender:fem, number:pl, degree:pos])),
% nominative n pl
asserta(lex(FNnpl,adj,[pos:adj,txt:FNnpl,lex:FNmsg, type:Type, case:nom, gender:neuter, number:pl, degree:pos])),
% accusative n pl
asserta(lex(FAcnpl,adj,[pos:adj,txt:FAcnpl,lex:FNmsg, type:Type, case:acc, gender:neuter, number:pl, degree:pos])),
% genitive n pl
asserta(lex(FGennpl,adj,[pos:adj,txt:FGennpl,lex:FNmsg, type:Type, case:gen, gender:neuter, number:pl, degree:pos])),
% dative n pl
asserta(lex(FDatnpl,adj,[pos:adj,txt:FDatnpl,lex:FNmsg, type:Type, case:dat, gender:neuter, number:pl, degree:pos])),
% ablative n pl
asserta(lex(FAblnpl,adj,[pos:adj,txt:FAblnpl,lex:FNmsg, type:Type, case:abl, gender:neuter, number:pl, degree:pos])),

fail.

macro_adj_1a.

%

macro_adj_2 :-
adj(FNmfsg,Root,2,Type,[Comp,Sup]),
declension_endings(adj,2,[_,Acmfsg,Genmfsg,Datmfsg,Ablmfsg,
                  Nnsg,Acnsg,Gennsg,Datnsg,Ablnsg,
                  Nmfpl,Acmfpl,Genmfpl,Datmfpl,Ablmfpl,
                  Nnpl,Acnpl,Gennpl,Datnpl,Ablnpl]),


atom_concat(Root,Acmfsg,FAcmfsg),
atom_concat(Root,Genmfsg,FGenmfsg),
atom_concat(Root,Datmfsg,FDatmfsg),
atom_concat(Root,Ablmfsg,FAblmfsg),
atom_concat(Root,Nnsg,FNnsg),
atom_concat(Root,Acnsg,FAcnsg),
atom_concat(Root,Gennsg,FGennsg),
atom_concat(Root,Datnsg,FDatnsg),
atom_concat(Root,Ablnsg,FAblnsg),
atom_concat(Root,Nmfpl,FNmfpl),
atom_concat(Root,Acmfpl,FAcmfpl),
atom_concat(Root,Genmfpl,FGenmfpl),
atom_concat(Root,Datmfpl,FDatmfpl),
atom_concat(Root,Ablmfpl,FAblmfpl),
atom_concat(Root,Nnpl,FNnpl),
atom_concat(Root,Acnpl,FAcnpl),
atom_concat(Root,Gennpl,FGennpl),
atom_concat(Root,Datnpl,FDatnpl),
atom_concat(Root,Ablnpl,FAblnpl),


% SING

% nominative m
asserta(lex(FNmfsg,adj,[pos:adj,txt:FNmfsg,lex:FNmfsg, type:Type, case:nom, gender:masc, number:sing, degree:pos])),
% accusative m
asserta(lex(FAcmfsg,adj,[pos:adj,txt:FAcmfsg,lex:FNmfsg, type:Type, case:acc, gender:masc, number:sing, degree:pos])),
% genitive m
asserta(lex(FGenmfsg,adj,[pos:adj,txt:FGenmfsg,lex:FNmfsg, type:Type, case:gen, gender:masc, number:sing, degree:pos])),
% dative m
asserta(lex(FDatmfsg,adj,[pos:adj,txt:FDatmfsg,lex:FNmfsg, type:Type, case:dat, gender:masc, number:sing, degree:pos])),
% ablative m
asserta(lex(FAblmfsg,adj,[pos:adj,txt:FAblmfsg,lex:FNmfsg, type:Type, case:abl, gender:masc, number:sing, degree:pos])),
% nominative f
asserta(lex(FNmfsg,adj,[pos:adj,txt:FNmfsg,lex:FNmfsg, type:Type, case:nom, gender:fem, number:sing, degree:pos])),
% accusative f
asserta(lex(FAcmfsg,adj,[pos:adj,txt:FAcmfsg,lex:FNmfsg, type:Type, case:acc, gender:fem, number:sing, degree:pos])),
% genitive f
asserta(lex(FGenmfsg,adj,[pos:adj,txt:FGenmfsg,lex:FNmfsg, type:Type, case:gen, gender:fem, number:sing, degree:pos])),
% dative f
asserta(lex(FDatmfsg,adj,[pos:adj,txt:FDatmfsg,lex:FNmfsg, type:Type, case:dat, gender:fem, number:sing, degree:pos])),
% ablative f
asserta(lex(FAblmfsg,adj,[pos:adj,txt:FAblmfsg,lex:FNmfsg, type:Type, case:abl, gender:fem, number:sing, degree:pos])),
% nominative n
asserta(lex(FNnsg,adj,[pos:adj,txt:FNnsg,lex:FNmfsg, type:Type, case:nom, gender:neuter, number:sing, degree:pos])),
% accusative n
asserta(lex(FAcnsg,adj,[pos:adj,txt:FAcnsg,lex:FNmfsg, type:Type, case:acc, gender:neuter, number:sing, degree:pos])),
% genitive n
asserta(lex(FGennsg,adj,[pos:adj,txt:FGennsg,lex:FNmfsg, type:Type, case:gen, gender:neuter, number:sing, degree:pos])),
% dative n
asserta(lex(FDatnsg,adj,[pos:adj,txt:FDatnsg,lex:FNmfsg, type:Type, case:dat, gender:neuter, number:sing, degree:pos])),
% ablative n
asserta(lex(FAblnsg,adj,[pos:adj,txt:FAblnsg,lex:FNmfsg, type:Type, case:abl, gender:neuter, number:sing, degree:pos])),

% PLURAL

% nominative masc pl
asserta(lex(FNmfpl,adj,[pos:adj,txt:FNmfpl,lex:FNmfsg, type:Type, case:nom, gender:masc, number:pl, degree:pos])),
% accusative masc pl
asserta(lex(FAcmfpl,adj,[pos:adj,txt:FAcmfpl,lex:FNmfsg, type:Type, case:acc, gender:masc, number:pl, degree:pos])),
% genitive masc pl
asserta(lex(FGenmfpl,adj,[pos:adj,txt:FGenmfpl,lex:FNmfsg, type:Type, case:gen, gender:masc, number:pl, degree:pos])),
% dative masc pl
asserta(lex(FDatmfpl,adj,[pos:adj,txt:FDatmfpl,lex:FNmfsg, type:Type, case:dat, gender:masc, number:pl, degree:pos])),
% ablative masc pl
asserta(lex(FAblmfpl,adj,[pos:adj,txt:FAblmfpl,lex:FNmfsg, type:Type, case:abl, gender:masc, number:pl, degree:pos])),
% nominative fem pl
asserta(lex(FNmfpl,adj,[pos:adj,txt:FNmfpl,lex:FNmfsg, type:Type, case:nom, gender:fem, number:pl, degree:pos])),
% accusative fem pl
asserta(lex(FAcmfpl,adj,[pos:adj,txt:FAcmfpl,lex:FNmfsg, type:Type, case:acc, gender:fem, number:pl, degree:pos])),
% genitive fem pl
asserta(lex(FGenmfpl,adj,[pos:adj,txt:FGenmfpl,lex:FNmfsg, type:Type, case:gen, gender:fem, number:pl, degree:pos])),
% dative fem pl
asserta(lex(FDatmfpl,adj,[pos:adj,txt:FDatmfpl,lex:FNmfsg, type:Type, case:dat, gender:fem, number:pl, degree:pos])),
% ablative fem pl
asserta(lex(FAblmfpl,adj,[pos:adj,txt:FAblmfpl,lex:FNmfsg, type:Type, case:abl, gender:fem, number:pl, degree:pos])),
% nominative n pl
asserta(lex(FNnpl,adj,[pos:adj,txt:FNnpl,lex:FNmfsg, type:Type, case:nom, gender:neuter, number:pl, degree:pos])),
% accusative n pl
asserta(lex(FAcnpl,adj,[pos:adj,txt:FAcnpl,lex:FNmfsg, type:Type, case:acc, gender:neuter, number:pl, degree:pos])),
% genitive n pl
asserta(lex(FGennpl,adj,[pos:adj,txt:FGennpl,lex:FNmfsg, type:Type, case:gen, gender:neuter, number:pl, degree:pos])),
% dative n pl
asserta(lex(FDatnpl,adj,[pos:adj,txt:FDatnpl,lex:FNmfsg, type:Type, case:dat, gender:neuter, number:pl, degree:pos])),
% ablative n pl
asserta(lex(FAblnpl,adj,[pos:adj,txt:FAblnpl,lex:FNmfsg, type:Type, case:abl, gender:neuter, number:pl, degree:pos])),

fail.

macro_adj_2.

%






macro_adj_2a :-
adj(FNmfnsg,Root,a2,Type,[Comp,Sup]),
declension_endings(adj,a2,[_,Acmfsg,Genmfsg,Datmfsg,Ablmfsg,
                  _,_,Gennsg,Datnsg,Ablnsg,
                  Nmfpl,Acmfpl,Genmfpl,Datmfpl,Ablmfpl,
                  Nnpl,Acnpl,Gennpl,Datnpl,Ablnpl]),

atom_concat(Root,Acmfsg,FAcmfsg),
atom_concat(Root,Genmfsg,FGenmfsg),
atom_concat(Root,Datmfsg,FDatmfsg),
atom_concat(Root,Ablmfsg,FAblmfsg),
atom_concat(Root,Gennsg,FGennsg),
atom_concat(Root,Datnsg,FDatnsg),
atom_concat(Root,Ablnsg,FAblnsg),
atom_concat(Root,Nmfpl,FNmfpl),
atom_concat(Root,Acmfpl,FAcmfpl),
atom_concat(Root,Genmfpl,FGenmfpl),
atom_concat(Root,Datmfpl,FDatmfpl),
atom_concat(Root,Ablmfpl,FAblmfpl),
atom_concat(Root,Nnpl,FNnpl),
atom_concat(Root,Acnpl,FAcnpl),
atom_concat(Root,Gennpl,FGennpl),
atom_concat(Root,Datnpl,FDatnpl),
atom_concat(Root,Ablnpl,FAblnpl),

% SING

% nominative m f n
asserta(lex(FNmfnsg,adj,[pos:adj,txt:FNmfnsg,lex:FNmfnsg, type:Type, case:nom, gender:masc, number:sing, degree:pos])),
% accusative m
asserta(lex(FAcmfsg,adj,[pos:adj,txt:FAcmfsg,lex:FNmfnsg, type:Type, case:acc, gender:masc, number:sing, degree:pos])),
% genitive m
asserta(lex(FGenmfsg,adj,[pos:adj,txt:FGenmfsg,lex:FNmfnsg, type:Type, case:gen, gender:masc, number:sing, degree:pos])),
% dative m
asserta(lex(FDatmfsg,adj,[pos:adj,txt:FDatmfsg,lex:FNmfnsg, type:Type, case:dat, gender:masc, number:sing, degree:pos])),
% ablative m
asserta(lex(FAblmfsg,adj,[pos:adj,txt:FAblmfsg,lex:FNmfnsg, type:Type, case:abl, gender:masc, number:sing, degree:pos])),
% nominative f
asserta(lex(FNmfnsg,adj,[pos:adj,txt:FNmfsg,lex:FNmfnsg, type:Type, case:nom, gender:fem, number:sing, degree:pos])),
% accusative f
asserta(lex(FAcmfsg,adj,[pos:adj,txt:FAcmfsg,lex:FNmfnsg, type:Type, case:acc, gender:fem, number:sing, degree:pos])),
% genitive f
asserta(lex(FGenmfsg,adj,[pos:adj,txt:FGenmfsg,lex:FNmfnsg, type:Type, case:gen, gender:fem, number:sing, degree:pos])),
% dative f
asserta(lex(FDatmfsg,adj,[pos:adj,txt:FDatmfsg,lex:FNmfnsg, type:Type, case:dat, gender:fem, number:sing, degree:pos])),
% ablative f
asserta(lex(FAblmfsg,adj,[pos:adj,txt:FAblmfsg,lex:FNmfnsg, type:Type, case:abl, gender:fem, number:sing, degree:pos])),
% nominative n
asserta(lex(FNmfnsg,adj,[pos:adj,txt:FNmfnsg,lex:FNmfnsg, type:Type, case:nom, gender:neuter, number:sing, degree:pos])),
% accusative n
asserta(lex(FNmfnsg,adj,[pos:adj,txt:FNmfnsg,lex:FNmfnsg, type:Type, case:acc, gender:neuter, number:sing, degree:pos])),
% genitive n
asserta(lex(FGennsg,adj,[pos:adj,txt:FGennsg,lex:FNmfnsg, type:Type, case:gen, gender:neuter, number:sing, degree:pos])),
% dative n
asserta(lex(FDatnsg,adj,[pos:adj,txt:FDatnsg,lex:FNmfnsg, type:Type, case:dat, gender:neuter, number:sing, degree:pos])),
% ablative n
asserta(lex(FAblnsg,adj,[pos:adj,txt:FAblnsg,lex:FNmfnsg, type:Type, case:abl, gender:neuter, number:sing, degree:pos])),

% PLURAL

% nominative masc pl
asserta(lex(FNmfpl,adj,[pos:adj,txt:FNmfpl,lex:FNmfnsg, type:Type, case:nom, gender:masc, number:pl, degree:pos])),
% accusative masc pl
asserta(lex(FAcmfpl,adj,[pos:adj,txt:FAcmfpl,lex:FNmfnsg, type:Type, case:acc, gender:masc, number:pl, degree:pos])),
% genitive masc pl
asserta(lex(FGenmfpl,adj,[pos:adj,txt:FGenmfpl,lex:FNmfnsg, type:Type, case:gen, gender:masc, number:pl, degree:pos])),
% dative masc pl
asserta(lex(FDatmfpl,adj,[pos:adj,txt:FDatmfpl,lex:FNmfnsg, type:Type, case:dat, gender:masc, number:pl, degree:pos])),
% ablative masc pl
asserta(lex(FAblmfpl,adj,[pos:adj,txt:FAblmfpl,lex:FNmfnsg, type:Type, case:abl, gender:masc, number:pl, degree:pos])),
% nominative fem pl
asserta(lex(FNmfpl,adj,[pos:adj,txt:FNmfpl,lex:FNmfnsg, type:Type, case:nom, gender:fem, number:pl, degree:pos])),
% accusative fem pl
asserta(lex(FAcmfpl,adj,[pos:adj,txt:FAcmfpl,lex:FNmfnsg, type:Type, case:acc, gender:fem, number:pl, degree:pos])),
% genitive fem pl
asserta(lex(FGenmfpl,adj,[pos:adj,txt:FGenmfpl,lex:FNmfnsg, type:Type, case:gen, gender:fem, number:pl, degree:pos])),
% dative fem pl
asserta(lex(FDatmfpl,adj,[pos:adj,txt:FDatmfpl,lex:FNmfnsg, type:Type, case:dat, gender:fem, number:pl, degree:pos])),
% ablative fem pl
asserta(lex(FAblmfpl,adj,[pos:adj,txt:FAblmfpl,lex:FNmfnsg, type:Type, case:abl, gender:fem, number:pl, degree:pos])),
% nominative n pl
asserta(lex(FNnpl,adj,[pos:adj,txt:FNnpl,lex:FNmfnsg, type:Type, case:nom, gender:neuter, number:pl, degree:pos])),
% accusative n pl
asserta(lex(FAcnpl,adj,[pos:adj,txt:FAcnpl,lex:FNmfnsg, type:Type, case:acc, gender:neuter, number:pl, degree:pos])),
% genitive n pl
asserta(lex(FGennpl,adj,[pos:adj,txt:FGennpl,lex:FNmfnsg, type:Type, case:gen, gender:neuter, number:pl, degree:pos])),
% dative n pl
asserta(lex(FDatnpl,adj,[pos:adj,txt:FDatnpl,lex:FNmfnsg, type:Type, case:dat, gender:neuter, number:pl, degree:pos])),
% ablative n pl
asserta(lex(FAblnpl,adj,[pos:adj,txt:FAblnpl,lex:FNmfnsg, type:Type,case:abl, gender:neuter, number:pl, degree:pos])),

fail.

macro_adj_2a.





%%%%%%%%
% NOUNS
%%%%%%%%

macro_noun_1 :-

noun(1,Gender,Lex,Root,class:Class,Sem,Box),
declension_endings(n,1,fem,[Nfsg,Acfsg,Genfsg,Datfsg,Ablfsg,
                    Nfpl,Acfpl,Genfpl,Datfpl,Ablfpl]),


atom_concat(Root,Nfsg,FNfsg),
atom_concat(Root,Acfsg,FAcfsg),
atom_concat(Root,Genfsg,FGenfsg),
atom_concat(Root,Datfsg,FDatfsg),
atom_concat(Root,Ablfsg,FAblfsg),
atom_concat(Root,Nfpl,FNfpl),
atom_concat(Root,Acfpl,FAcfpl),
atom_concat(Root,Genfpl,FGenfpl),
atom_concat(Root,Datfpl,FDatfpl),
atom_concat(Root,Ablfpl,FAblfpl),

ifthenelse(constraint([nb:pl],Box),true, 
% nominative fem sing
(asserta(lex(FNfsg,noun,[pos:noun,txt:FNfsg,lex:Lex, case:nom, gender:Gender, class:Class,number:sing,Sem])),
% accusative fem sing
asserta(lex(FAcfsg,noun,[pos:noun,txt:FAcfsg,lex:Lex, case:acc, gender:Gender, class:Class,number:sing,Sem])),
% genitive fem sing
asserta(lex(FGenfsg,noun,[pos:noun,txt:FGenfsg,lex:Lex, case:gen, gender:Gender, class:Class,number:sing,Sem])),
% dative fem sing
asserta(lex(FDatfsg,noun,[pos:noun,txt:FDatfsg,lex:Lex, case:dat, gender:Gender, class:Class,number:sing,Sem])),
% ablative fem sing
asserta(lex(FAblfsg,noun,[pos:noun,txt:FAblfsg,lex:Lex, case:abl, gender:Gender, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 
% nominative fem pl
(asserta(lex(FNfpl,noun,[pos:noun,txt:FNfpl,lex:Lex, case:nom, gender:Gender, class:Class,number:pl,Sem])),
% accusative fem pl
asserta(lex(FAcfpl,noun,[pos:noun,txt:FAcfpl,lex:Lex, case:acc, gender:Gender, class:Class,number:pl,Sem])),
% genitive fem pl
asserta(lex(FGenfpl,noun,[pos:noun,txt:FGenfpl,lex:Lex, case:gen, gender:Gender, class:Class,number:pl,Sem])),
% dative fem pl
asserta(lex(FDatfpl,noun,[pos:noun,txt:FDatfpl,lex:Lex, case:dat, gender:Gender, class:Class,number:pl,Sem])),
% ablative fem pl
asserta(lex(FAblfpl,noun,[pos:noun,txt:FAblfpl,lex:Lex, case:abl, gender:Gender, class:Class,number:pl,Sem])) )),

fail.

macro_noun_1.

%

% declension_endings(n,3,mf,[nil,em,is,i,e,
%                  es,es,ibus,ibus]).

macro_noun_3_mf :-

noun(3,Gender,Lex,Root,Genmfpl,class:Class,Sem,Box),
(Gender=masc; Gender=fem ; Gender=or([masc,fem])),
declension_endings(n,3,mf,[ _,  Acmfsg,Genmfsg,Datmfsg,Ablmfsg,
                  Nmfpl,Acmfpl,Datmfpl,Ablmfpl]),


atom_concat(Root,Acmfsg,FAcmfsg),
atom_concat(Root,Genmfsg,FGenmfsg),
atom_concat(Root,Datmfsg,FDatmfsg),
atom_concat(Root,Ablmfsg,FAblmfsg),
atom_concat(Root,Nmfpl,FNmfpl),
atom_concat(Root,Acmfpl,FAcmfpl),
atom_concat(Root,Genmfpl,FGenmfpl),
atom_concat(Root,Datmfpl,FDatmfpl),
atom_concat(Root,Ablmfpl,FAblmfpl),

ifthenelse(constraint([nb:pl],Box),true, 
% nominative mf sing
(asserta(lex(Lex,noun,[pos:noun,txt:Lex,lex:Lex, case:nom, gender:Gender, class:Class,number:sing,Sem])),
% accusative mf sing
asserta(lex(FAcmfsg,noun,[pos:noun,txt:FAcmfsg,lex:Lex, case:acc, gender:Gender, class:Class,number:sing,Sem])),
% genitive mf sing
asserta(lex(FGenmfsg,noun,[pos:noun,txt:FGenmfsg,lex:Lex, case:gen, gender:Gender, class:Class,number:sing,Sem])),
% dative mf sing
asserta(lex(FDatmfsg,noun,[pos:noun,txt:FDatmfsg,lex:Lex, case:dat, gender:Gender, class:Class,number:sing,Sem])),
% ablative mf sing
asserta(lex(FAblmfsg,noun,[pos:noun,txt:FAblmfsg,lex:Lex, case:abl, gender:Gender, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 
% nominative mf pl
(asserta(lex(FNmfpl,noun,[pos:noun,txt:FNmfpl,lex:Lex, case:nom, gender:Gender, class:Class,number:pl,Sem])),
% accusative mf pl
asserta(lex(FAcmfpl,noun,[pos:noun,txt:FAcmfpl,lex:Lex, case:acc, gender:Gender, class:Class,number:pl,Sem])),
% genitive mf pl
asserta(lex(FGenmfpl,noun,[pos:noun,txt:FGenmfpl,lex:Lex, case:gen, gender:Gender, class:Class,number:pl,Sem])),
% dative mf pl
asserta(lex(FDatmfpl,noun,[pos:noun,txt:FDatmfpl,lex:Lex, case:dat, gender:Gender, class:Class,number:pl,Sem])),
% ablative mf pl
asserta(lex(FAblmfpl,noun,[pos:noun,txt:FAblmfpl,lex:Lex, case:abl, gender:Gender, class:Class,number:pl,Sem])) )),

fail.

macro_noun_3_mf.

lex(uis,noun,[pos:noun,txt:uis,lex:uis, case:nom, gender:fem, class:common,number:sing,sem:[abstract]]).
lex(uim,noun,[pos:noun,txt:uim,lex:uis, case:acc, gender:fem, class:common,number:sing,sem:[abstract]]).
lex(uis,noun,[pos:noun,txt:uis,lex:uis, case:gen, gender:fem, class:common,number:sing,sem:[abstract]]).
lex(ui,noun,[pos:noun,txt:ui,lex:uis, case:dat, gender:fem, class:common,number:sing,sem:[abstract]]).
lex(ui,noun, [pos:noun,txt:ui,lex:uis, case:abl, gender:fem, class:common,number:sing,sem:[abstract]]).

%

macro_noun_3_n :-

noun(3,neuter,Lex,Root,Gennpl,class:Class,Sem,Box),
declension_endings(n,3,neuter, [ _,_     ,Gennsg,Datnsg,Ablnsg,
                       Nnpl,Acnpl,Datnpl,Ablnpl]),


atom_concat(Root,Gennsg,FGennsg),
atom_concat(Root,Datnsg,FDatnsg),
atom_concat(Root,Ablnsg,FAblnsg),
atom_concat(Root,Nnpl,FNnpl),
atom_concat(Root,Acnpl,FAcnpl),
atom_concat(Root,Gennpl,FGennpl),
atom_concat(Root,Datnpl,FDatnpl),
atom_concat(Root,Ablnpl,FAblnpl),

ifthenelse(constraint([nb:pl],Box),true, 

% nominative neuter sing
(asserta(lex(Lex,noun,[pos:noun,txt:Lex,lex:Lex, case:nom, gender:neuter, class:Class,number:sing,Sem])),
% accusative neuter sing
asserta(lex(Lex,noun,[pos:noun,txt:Lex,lex:Lex, case:acc, gender:neuter, class:Class,number:sing,Sem])),
% genitive neuter sing
asserta(lex(FGennsg,noun,[pos:noun,txt:FGennsg,lex:Lex, case:gen, gender:neuter, class:Class,number:sing,Sem])),
% dative neuter sing
asserta(lex(FDatnsg,noun,[pos:noun,txt:FDatnsg,lex:Lex, case:dat, gender:neuter, class:Class,number:sing,Sem])),
% ablative neuter sing
asserta(lex(FAblnsg,noun,[pos:noun,txt:FAblnsg,lex:Lex, case:abl, gender:neuter, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 
% nominative neuter pl
(asserta(lex(FNnpl,noun,[pos:noun,txt:FNnpl,lex:Lex, case:nom, gender:neuter, class:Class,number:pl,Sem])),
% accusative neuter pl
asserta(lex(FAcnpl,noun,[pos:noun,txt:FAcnpl,lex:Lex, case:acc, gender:neuter, class:Class,number:pl,Sem])),
% genitive  neuter pl
asserta(lex(FGennpl,noun,[pos:noun,txt:FGennpl,lex:Lex, case:gen, gender:neuter, class:Class,number:pl,Sem])),
% dative neuter pl
asserta(lex(FDatnpl,noun,[pos:noun,txt:FDatnpl,lex:Lex, case:dat, gender:neuter, class:Class,number:pl,Sem])),
% ablative neuter pl
asserta(lex(FAblnpl,noun,[pos:noun,txt:FAblnpl,lex:Lex, case:abl, gender:neuter, class:Class,number:pl,Sem])) )),

fail.

macro_noun_3_n.

%

macro_noun_4_mf :-

noun(4,Gender,Lex,Root,class:Class,Sem,Box),
declension_endings(n,4,mf,[ _,  Acmfsg,Genmfsg,Datmfsg,Ablmfsg,
                  Nmfpl,Acmfpl,Genmfpl,Datmfpl,Ablmfpl]),


atom_concat(Root,Acmfsg,FAcmfsg),
atom_concat(Root,Genmfsg,FGenmfsg),
atom_concat(Root,Datmfsg,FDatmfsg),
atom_concat(Root,Ablmfsg,FAblmfsg),
atom_concat(Root,Nmfpl,FNmfpl),
atom_concat(Root,Acmfpl,FAcmfpl),
atom_concat(Root,Genmfpl,FGenmfpl),
atom_concat(Root,Datmfpl,FDatmfpl),
atom_concat(Root,Ablmfpl,FAblmfpl),

ifthenelse(constraint([nb:pl],Box),true, 

% nominative mf sing
(asserta(lex(Lex,noun,[pos:noun,txt:Lex,lex:Lex, case:nom, gender:Gender, class:Class,number:sing,Sem])),
% accusative mf sing
asserta(lex(FAcmfsg,noun,[pos:noun,txt:FAcmfsg,lex:Lex, case:acc, gender:Gender, class:Class,number:sing,Sem])),
% genitive mf sing
asserta(lex(FGenmfsg,noun,[pos:noun,txt:FGenmfsg,lex:Lex, case:gen, gender:Gender, class:Class,number:sing,Sem])),
% dative mf sing
asserta(lex(FDatmfsg,noun,[pos:noun,txt:FDatmfsg,lex:Lex, case:dat, gender:Gender, class:Class,number:sing,Sem])),
% ablative mf sing
asserta(lex(FAblmfsg,noun,[pos:noun,txt:FAblmfsg,lex:Lex, case:abl, gender:Gender, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 

% nominative mf pl
(asserta(lex(FNmfpl,noun,[pos:noun,txt:FNmfpl,lex:Lex, case:nom, gender:Gender, class:Class,number:pl,Sem])),
% accusative mf pl
asserta(lex(FAcmfpl,noun,[pos:noun,txt:FAcmfpl,lex:Lex, case:acc, gender:Gender, class:Class,number:pl,Sem])),
% genitive mf pl
asserta(lex(FGenmfpl,noun,[pos:noun,txt:FGenmfpl,lex:Lex, case:gen, gender:Gender, class:Class,number:pl,Sem])),
% dative mf pl
asserta(lex(FDatmfpl,noun,[pos:noun,txt:FDatmfpl,lex:Lex, case:dat, gender:Gender, class:Class,number:pl,Sem])),
% ablative mf pl
asserta(lex(FAblmfpl,noun,[pos:noun,txt:FAblmfpl,lex:Lex, case:abl, gender:Gender, class:Class,number:pl,Sem])) )),

fail.

macro_noun_4_mf.





%
% DOMUS
%

% The lex forms are entered here, not generated

lex(domus,noun,[pos:noun,txt:domus,lex:domus, case:nom, gender:fem, class:common,number:sing,sem:[abstract, thing]]).
lex(domum,noun,[pos:noun,txt:domum,lex:domus, case:acc, gender:fem, class:common,number:sing,sem:[abstract, thing]]).
lex(domus,noun,[pos:noun,txt:domus,lex:domus, case:gen, gender:fem, class:common,number:sing,sem:[abstract, thing]]).
lex(domui,noun,[pos:noun,txt:domui,lex:domus, case:dat, gender:fem, class:common,number:sing,sem:[abstract, thing]]).
lex(domo,noun, [pos:noun,txt:domo,lex:domus, case:abl, gender:fem, class:common,number:sing,sem:[abstract, thing]]).

lex(domus,noun, [pos:noun,txt:domus,lex:domus, case:nom, gender:fem, class:common,number:pl,sem:[abstract, thing]]).

lex(domus,noun, [pos:noun,txt:domus,lex:domus, case:acc, gender:fem, class:common,number:pl,sem:[abstract, thing]]).
lex(domos,noun, [pos:noun,txt:domos,lex:domus, case:acc, gender:fem, class:common,number:pl,sem:[abstract, thing]]).

lex(domuum,noun,[pos:noun,txt:domuum,lex:domus, case:gen, gender:fem, class:common,number:pl,sem:[abstract, thing]]).
lex(domum,noun, [pos:noun,txt:domum,lex:domus, case:gen, gender:fem, class:common,number:pl,sem:[abstract, thing]]).
lex(domorum,noun,[pos:noun,txt:domorum,lex:domus, case:gen, gender:fem, class:common,number:pl,sem:[abstract, thing]]).

lex(domibus,noun,[pos:noun,txt:domibus,lex:domus, case:or([dat,abl]), gender:fem, class:common,number:pl,sem:[abstract, thing]]).









macro_noun_5 :-

noun(5,Gender,Lex,Root,class:Class,Sem,Box),
declension_endings(n,5,mf,[Nmfsg,  Acmfsg,Genmfsg,Datmfsg,Ablmfsg,
                  Nmfpl,Acmfpl,Genmfpl,Datmfpl,Ablmfpl]),

atom_concat(Root,Nmfsg,FNmfsg),
atom_concat(Root,Acmfsg,FAcmfsg),
atom_concat(Root,Genmfsg,FGenmfsg),
atom_concat(Root,Datmfsg,FDatmfsg),
atom_concat(Root,Ablmfsg,FAblmfsg),
atom_concat(Root,Nmfpl,FNmfpl),
atom_concat(Root,Acmfpl,FAcmfpl),
atom_concat(Root,Genmfpl,FGenmfpl),
atom_concat(Root,Datmfpl,FDatmfpl),
atom_concat(Root,Ablmfpl,FAblmfpl),

ifthenelse(constraint([nb:pl],Box),true, 
% nominative mf sing
(asserta(lex(FNmfsg,noun,[pos:noun,txt:FNmfsg,lex:Lex, case:nom, gender:Gender, class:Class,number:sing,Sem])),
% accusative mf sing
asserta(lex(FAcmfsg,noun,[pos:noun,txt:FAcmfsg,lex:Lex, case:acc, gender:Gender, class:Class,number:sing,Sem])),
% genitive mf sing
asserta(lex(FGenmfsg,noun,[pos:noun,txt:FGenmfsg,lex:Lex, case:gen, gender:Gender, class:Class,number:sing,Sem])),
% dative mf sing
asserta(lex(FDatmfsg,noun,[pos:noun,txt:FDatmfsg,lex:Lex, case:dat, gender:Gender, class:Class,number:sing,Sem])),
% ablative mf sing
asserta(lex(FAblmfsg,noun,[pos:noun,txt:FAblmfsg,lex:Lex, case:abl, gender:Gender, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 
% nominative mf pl
(asserta(lex(FNmfpl,noun,[pos:noun,txt:FNmfpl,lex:Lex, case:nom, gender:Gender, class:Class,number:pl,Sem])),
% accusative mf pl
asserta(lex(FAcmfpl,noun,[pos:noun,txt:FAcmfpl,lex:Lex, case:acc, gender:Gender, class:Class,number:pl,Sem])),
% genitive mf pl
asserta(lex(FGenmfpl,noun,[pos:noun,txt:FGenmfpl,lex:Lex, case:gen, gender:Gender, class:Class,number:pl,Sem])),
% dative mf pl
asserta(lex(FDatmfpl,noun,[pos:noun,txt:FDatmfpl,lex:Lex, case:dat, gender:Gender, class:Class,number:pl,Sem])),
% ablative mf pl
asserta(lex(FAblmfpl,noun,[pos:noun,txt:FAblmfpl,lex:Lex, case:abl, gender:Gender, class:Class,number:pl,Sem])) )),

fail.

macro_noun_5.

%




macro_noun_2_masc :-

noun(2,masc,Lex,Root,class:Class,Sem,Box),
declension_endings(n,2,masc,[_Nmsg,Acmsg,Genmsg,Datmsg,Ablmsg,
                     Nmpl,Acmpl,Genmpl,Datmpl,Ablmpl]),

atom_concat(Root,Acmsg,FAcmsg),
atom_concat(Root,Genmsg,FGenmsg),
atom_concat(Root,Datmsg,FDatmsg),
atom_concat(Root,Ablmsg,FAblmsg),
atom_concat(Root,Nmpl,FNmpl),
atom_concat(Root,Acmpl,FAcmpl),
atom_concat(Root,Genmpl,FGenmpl),
atom_concat(Root,Datmpl,FDatmpl),
atom_concat(Root,Ablmpl,FAblmpl),

ifthenelse(constraint([nb:pl],Box),true, 
% nominative masc sing
(asserta(lex(Lex,noun,[pos:noun,txt:Lex,lex:Lex, case:nom, gender:masc, class:Class,number:sing,Sem])),
% accusative masc sing
asserta(lex(FAcmsg,noun,[pos:noun,txt:FAcmsg,lex:Lex, case:acc, gender:masc, class:Class,number:sing,Sem])),
% genitive masc sing
asserta(lex(FGenmsg,noun,[pos:noun,txt:FGenmsg,lex:Lex, case:gen, gender:masc, class:Class,number:sing,Sem])),
% dative masc sing
asserta(lex(FDatmsg,noun,[pos:noun,txt:FDatmsg,lex:Lex, case:dat, gender:masc, class:Class,number:sing,Sem])),
% ablative masc sing
asserta(lex(FAblmsg,noun,[pos:noun,txt:FAblmsg,lex:Lex, case:abl, gender:masc, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 

% nominative masc pl
(asserta(lex(FNmpl,noun,[pos:noun,txt:FNmpl,lex:Lex, case:nom, gender:masc, class:Class,number:pl,Sem])),
% accusative masc pl
asserta(lex(FAcmpl,noun,[pos:noun,txt:FAcmpl,lex:Lex, case:acc, gender:masc, class:Class,number:pl,Sem])),
% genitive  masc pl
asserta(lex(FGenmpl,noun,[pos:noun,txt:FGenmpl,lex:Lex, case:gen, gender:masc, class:Class,number:pl,Sem])),
% dative masc pl
asserta(lex(FDatmpl,noun,[pos:noun,txt:FDatmpl,lex:Lex, case:dat, gender:masc, class:Class,number:pl,Sem])),
% ablative masc pl
asserta(lex(FAblmpl,noun,[pos:noun,txt:FAblmpl,lex:Lex, case:abl, gender:masc, class:Class,number:pl,Sem])) )),

fail.

macro_noun_2_masc.




macro_noun_2_fem :-

noun(2,fem,Lex,Root,class:Class,Sem,Box),
declension_endings(n,2,masc,[_Nmsg,Acmsg,Genmsg,Datmsg,Ablmsg,
                     Nmpl,Acmpl,Genmpl,Datmpl,Ablmpl]),

atom_concat(Root,Acmsg,FAcmsg),
atom_concat(Root,Genmsg,FGenmsg),
atom_concat(Root,Datmsg,FDatmsg),
atom_concat(Root,Ablmsg,FAblmsg),
atom_concat(Root,Nmpl,FNmpl),
atom_concat(Root,Acmpl,FAcmpl),
atom_concat(Root,Genmpl,FGenmpl),
atom_concat(Root,Datmpl,FDatmpl),
atom_concat(Root,Ablmpl,FAblmpl),

ifthenelse(constraint([nb:pl],Box),true, 
% nominative masc sing
(asserta(lex(Lex,noun,[pos:noun,txt:Lex,lex:Lex, case:nom, gender:fem, class:Class,number:sing,Sem])),
% accusative masc sing
asserta(lex(FAcmsg,noun,[pos:noun,txt:FAcmsg,lex:Lex, case:acc, gender:fem, class:Class,number:sing,Sem])),
% genitive masc sing
asserta(lex(FGenmsg,noun,[pos:noun,txt:FGenmsg,lex:Lex, case:gen, gender:fem, class:Class,number:sing,Sem])),
% dative masc sing
asserta(lex(FDatmsg,noun,[pos:noun,txt:FDatmsg,lex:Lex, case:dat, gender:fem, class:Class,number:sing,Sem])),
% ablative masc sing
asserta(lex(FAblmsg,noun,[pos:noun,txt:FAblmsg,lex:Lex, case:abl, gender:fem, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 

% nominative masc pl
(asserta(lex(FNmpl,noun,[pos:noun,txt:FNmpl,lex:Lex, case:nom, gender:fem, class:Class,number:pl,Sem])),
% accusative masc pl
asserta(lex(FAcmpl,noun,[pos:noun,txt:FAcmpl,lex:Lex, case:acc, gender:fem, class:Class,number:pl,Sem])),
% genitive  masc pl
asserta(lex(FGenmpl,noun,[pos:noun,txt:FGenmpl,lex:Lex, case:gen, gender:fem, class:Class,number:pl,Sem])),
% dative masc pl
asserta(lex(FDatmpl,noun,[pos:noun,txt:FDatmpl,lex:Lex, case:dat, gender:fem, class:Class,number:pl,Sem])),
% ablative masc pl
asserta(lex(FAblmpl,noun,[pos:noun,txt:FAblmpl,lex:Lex, case:abl, gender:fem, class:Class,number:pl,Sem])) )),

fail.

macro_noun_2_fem.

%




macro_noun_2_n :-

noun(2,neuter,Lex,Root,class:Class,Sem,Box),
declension_endings(n,2,neuter,[Nnsg,Acnsg,Gennsg,Datnsg,Ablnsg,
                       Nnpl,Acnpl,Gennpl,Datnpl,Ablnpl]),



atom_concat(Root,Nnsg,FNnsg),
atom_concat(Root,Acnsg,FAcnsg),
atom_concat(Root,Gennsg,FGennsg),
atom_concat(Root,Datnsg,FDatnsg),
atom_concat(Root,Ablnsg,FAblnsg),
atom_concat(Root,Nnpl,FNnpl),
atom_concat(Root,Acnpl,FAcnpl),
atom_concat(Root,Gennpl,FGennpl),
atom_concat(Root,Datnpl,FDatnpl),
atom_concat(Root,Ablnpl,FAblnpl),

ifthenelse(constraint([nb:pl],Box),true, 
% nominative neuter sing
(asserta(lex(FNnsg,noun,[pos:noun,txt:FNnsg,lex:Lex, case:nom, gender:neuter, class:Class,number:sing,Sem])),
% accusative neuter sing
asserta(lex(FAcnsg,noun,[pos:noun,txt:FAcnsg,lex:Lex, case:acc, gender:neuter, class:Class,number:sing,Sem])),
% genitive neuter sing
asserta(lex(FGennsg,noun,[pos:noun,txt:FGennsg,lex:Lex, case:gen, gender:neuter, class:Class,number:sing,Sem])),
% dative neuter sing
asserta(lex(FDatnsg,noun,[pos:noun,txt:FDatnsg,lex:Lex, case:dat, gender:neuter, class:Class,number:sing,Sem])),
% ablative neuter sing
asserta(lex(FAblnsg,noun,[pos:noun,txt:FAblnsg,lex:Lex, case:abl, gender:neuter, class:Class,number:sing,Sem])) )),

ifthenelse(constraint([nb:sg],Box),true, 
% nominative neuter pl
(asserta(lex(FNnpl,noun,[pos:noun,txt:FNnpl,lex:Lex, case:nom, gender:neuter, class:Class,number:pl,Sem])),
% accusative neuter pl
asserta(lex(FAcnpl,noun,[pos:noun,txt:FAcnpl,lex:Lex, case:acc, gender:neuter, class:Class,number:pl,Sem])),
% genitive  neuter pl
asserta(lex(FGennpl,noun,[pos:noun,txt:FGennpl,lex:Lex, case:gen, gender:neuter, class:Class,number:pl,Sem])),
% dative neuter pl
asserta(lex(FDatnpl,noun,[pos:noun,txt:FDatnpl,lex:Lex, case:dat, gender:neuter, class:Class,number:pl,Sem])),
% ablative neuter pl
asserta(lex(FAblnpl,noun,[pos:noun,txt:FAblnpl,lex:Lex, case:abl, gender:neuter, class:Class,number:pl,Sem])) )),

fail.

macro_noun_2_n.






% SECOND PASS FOR COMPOUND IRREGULAR VERBS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ensures that we get all the forms we need for a verb such as abire, where ire is recognized
% and lends the necessary forms

mkmore :-

verb([v(Full_Lex,Conj,xxx,RootPft,RootSup)],Class,Kind), % The xxx as value for RootPr tags an irregular verb
lex(Form,v,FS),                                          % we have a form of the root verb (e.g. ferret...)
                                                         % all word forms will be recalled because of the fail
							 % building up a loop
                                                         % note that we do not know it is the right verb
							 % until atom_concat has managed to find out 
							 % on the basis of producing string divisions
                                                         % until the right one is found
							 % nil+abire,a+bire, *** ab+ire ***, abi+re, abir+e, abire+nil
constraint([lex:Lex],FS),
Lex \= Full_Lex,                   % the process should not apply to its own product (with an empty Pre)
atom_concat(Pre,Lex,Full_Lex),     % the verb we are looking at results from the concatenation 
                                   % of a prefix and an irregular verb (redire, praeferre,...)
pick(lex:Lex,FS,FS1),              % getting rid of the values for the core irregular verb
pick(txt:Form,FS1,FS2),
atom_concat(Pre,Form,Full_Form),
append([lex:Full_Lex],FS2,FS3),      % substituting the new ones
append([txt:Full_Form],FS3,FS_New),
asserta(lex(Full_Form,v,FS_New)),    % asserting the result
fail.

mkmore.


% propterea

lex(propterea,dummy,[]).

% prodesse


lex(prosunto, v, [txt:prosunto, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:imperative, number:pl, person:3]).
lex(prodesto, v, [txt:prodesto, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:imperative, number:sing, person:3]).
lex(prodestote, v, [txt:prodestote, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:imperative, number:pl, person:2]).
lex(prodesto, v, [txt:prodesto, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:imperative, number:sing, person:2]).
lex(prodeste, v, [txt:prodeste, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:imperative, number:pl, person:2]).
lex(prodes, v, [txt:prodes, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:imperative, number:sing, person:2]).
lex(prodesse, v, [txt:prodesse, lex:prodesse, pos:v, class:v_esse, type:nonfinite, voice:act, tense:present, kind:std, mood:infinitive, number:_, person:_]).
lex(profore, v, [txt:profore, lex:prodesse, pos:v, class:v_esse, type:nonfinite, voice:act, tense:future, kind:std, mood:infinitive, number:_, person:_]).
lex(prodessetis, v, [txt:prodessetis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:pl, person:2]).
lex(prodessemus, v, [txt:prodessemus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:pl, person:1]).
lex(profores, v, [txt:profores, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:2]).
lex(prodesses, v, [txt:prodesses, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:2]).
lex(proforem, v, [txt:proforem, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:1]).
lex(prodessem, v, [txt:prodessem, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:1]).
lex(proforent, v, [txt:proforent, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:pl, person:3]).
lex(prodessent, v, [txt:prodessent, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:pl, person:3]).
lex(proforet, v, [txt:proforet, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:3]).
lex(prodesset, v, [txt:prodesset, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:3]).
lex(proderatis, v, [txt:proderatis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:indicative, number:pl, person:2]).
lex(proderamus, v, [txt:proderamus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:indicative, number:pl, person:1]).
lex(proderas, v, [txt:proderas, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:indicative, number:sing, person:2]).
lex(proderam, v, [txt:proderam, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:indicative, number:sing, person:1]).
lex(proderant, v, [txt:proderant, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:indicative, number:pl, person:3]).
lex(proderat, v, [txt:proderat, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:imperfect, kind:std, mood:indicative, number:sing, person:3]).
lex(proderitis, v, [txt:proderitis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:indicative, number:pl, person:2]).
lex(proderimus, v, [txt:proderimus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:indicative, number:pl, person:1]).
lex(proderis, v, [txt:proderis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:indicative, number:sing, person:2]).
lex(prodero, v, [txt:prodero, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:indicative, number:sing, person:1]).
lex(proderunt, v, [txt:proderunt, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:indicative, number:pl, person:3]).
lex(proderit, v, [txt:proderit, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future, kind:std, mood:indicative, number:sing, person:3]).
lex(prositis, v, [txt:prositis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:subjunctive, number:pl, person:2]).
lex(prosimus, v, [txt:prosimus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:subjunctive, number:pl, person:1]).
lex(prosis, v, [txt:prosis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:subjunctive, number:sing, person:2]).
lex(prosim, v, [txt:prosim, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:subjunctive, number:sing, person:1]).
lex(prosint, v, [txt:prosint, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:subjunctive, number:pl, person:3]).
lex(prosit, v, [txt:prosit, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:subjunctive, number:sing, person:3]).
lex(prodestis, v, [txt:prodestis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:indicative, number:pl, person:2]).
lex(prosumus, v, [txt:prosumus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:indicative, number:pl, person:1]).
lex(prodes, v, [txt:prodes, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:indicative, number:sing, person:2]).
lex(prosum, v, [txt:prosum, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:indicative, number:sing, person:1]).
lex(prosunt, v, [txt:prosunt, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:indicative, number:pl, person:3]).
lex(prodest, v, [txt:prodest, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:present, kind:std, mood:indicative, number:sing, person:3]).
lex(profuit, v, [txt:profuit, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:indicative, number:sing, person:3]).
lex(profuerunt, v, [txt:profuerunt, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:indicative, number:pl, person:3]).
lex(profuere, v, [txt:profuere, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:indicative, number:pl, person:3]).
lex(profui, v, [txt:profui, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:indicative, number:sing, person:1]).
lex(profuisti, v, [txt:profuisti, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:indicative, number:sing, person:2]).
lex(profuimus, v, [txt:profuimus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:indicative, number:pl, person:1]).
lex(profuistis, v, [txt:profuistis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:indicative, number:pl, person:2]).
lex(profuerat, v, [txt:profuerat, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:indicative, number:sing, person:3]).
lex(profuerant, v, [txt:profuerant, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:indicative, number:pl, person:3]).
lex(profueram, v, [txt:profueram, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:indicative, number:sing, person:1]).
lex(profueras, v, [txt:profueras, lex:prodesse, pos:v, class:v_esse, type:finite, kind:_, mood:indicative, voice:act, tense:pluperfect, number:sing, person:2]).
lex(profueramus, v, [txt:profueramus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:indicative, number:pl, person:1]).
lex(profueratis, v, [txt:profueratis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:indicative, number:pl, person:2]).
lex(profuerit, v, [txt:profuerit, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future_perfect, kind:_, mood:indicative, number:sing, person:3]).
lex(profuerint, v, [txt:profuerint, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future_perfect, kind:_, mood:indicative, number:pl, person:3]).
lex(profuero, v, [txt:profuero, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future_perfect, kind:_, mood:indicative, number:sing, person:1]).
lex(profueris, v, [txt:profueris, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future_perfect, kind:_, mood:indicative, number:sing, person:2]).
lex(profuerimus, v, [txt:profuerimus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future_perfect, kind:_, mood:indicative, number:pl, person:1]).
lex(profueritis, v, [txt:profueritis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:future_perfect, kind:_, mood:indicative, number:pl, person:2]).
lex(profuerit, v, [txt:profuerit, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:subjunctive, number:sing, person:3]).
lex(profuerint, v, [txt:profuerint, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:subjunctive, number:pl, person:3]).
lex(profuerim, v, [txt:profuerim, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:subjunctive, number:sing, person:1]).
lex(profueris, v, [txt:profueris, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:subjunctive, number:sing, person:2]).
lex(profuerimus, v, [txt:profuerimus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:subjunctive, number:pl, person:1]).
lex(profueritis, v, [txt:profueritis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:perfect, kind:_, mood:subjunctive, number:pl, person:2]).
lex(profuisset, v, [txt:profuisset, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:subjunctive, number:sing, person:3]).
lex(profuissent, v, [txt:profuissent, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:subjunctive, number:pl, person:3]).
lex(profuissem, v, [txt:profuissem, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:subjunctive, number:sing, person:1]).
lex(profuisses, v, [txt:profuisses, lex:prodesse, pos:v, class:v_esse, type:finite, kind:_, mood:subjunctive, voice:act, tense:pluperfect, number:sing, person:2]).
lex(profuissemus, v, [txt:profuissemus, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:subjunctive, number:pl, person:1]).
lex(profuissetis, v, [txt:profuissetis, lex:prodesse, pos:v, class:v_esse, type:finite, voice:act, tense:pluperfect, kind:_, mood:subjunctive, number:pl, person:2]).
lex(profuisse, v, [txt:profuisse, lex:prodesse, pos:v, class:v_esse, type:nonfinite, voice:act, tense:past, kind:_, mood:infinitive, number:_, person:_]).


%
%  END OF MORPHOLOGICAL GENERATION PROCEDURES
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%
% PERSONAL PRONOUNS
%

lex(ego, prpers,[pos:prpers,txt:ego,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:nom, sem:[hum]]).
lex(egomet, prpers,[pos:prpers,txt:ego,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:nom, sem:[hum]]).
lex(me, prpers,[pos:prpers,txt:me,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).
lex(memet, prpers,[pos:prpers,txt:me,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).
lex(mei, prpers,[pos:prpers,txt:mei,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:gen, sem:[hum]]).
lex(mihi, prpers,[pos:prpers,txt:mihi,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:dat, sem:[hum]]).
lex(me, prpers,[pos:prpers,txt:me,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).
lex(memet, prpers,[pos:prpers,txt:me,lex:pp1sg,
                 number:sing, person:1, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).
lex(tu, prpers,[pos:prpers,txt:tu,lex:pp2sg,
                 number:sing, person:2, gender:or([masc,fem]),
                 case:nom, sem:[hum]]).
lex(te, prpers,[pos:prpers,txt:te,lex:pp2sg,
                 number:sing, person:2, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).
lex(tui, prpers,[pos:prpers,txt:tui,lex:pp2sg,
                 number:sing, person:2, gender:or([masc,fem]),
                 case:gen, sem:[hum]]).
lex(tibi, prpers,[pos:prpers,txt:tibi,lex:pp2sg,
                 number:sing, person:2, gender:or([masc,fem]),
                 case:dat, sem:[hum]]).
lex(te, prpers,[pos:prpers,txt:te,lex:pp2sg,
                 number:sing, person:2, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).
lex(nos, prpers,[pos:prpers,txt:nos,lex:pp1pl,
                 number:pl, person:1, gender:or([masc,fem]),
                 case:nom, sem:[hum]]).
lex(nos, prpers,[pos:prpers,txt:nos,lex:pp1pl,
                 number:pl, person:1, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).
lex(nostri, prpers,[pos:prpers,txt:nostri,lex:pp1pl,
                 number:pl, person:1, gender:or([masc,fem]),
                 case:gen, sem:[hum]]).
lex(nostrum, prpers,[pos:prpers,txt:nostrum,lex:pp1pl,
                 number:pl, person:1, gender:or([masc,fem]),
                 case:gen, sem:[hum]]).
lex(nobis, prpers,[pos:prpers,txt:nobis,lex:pp1pl,
                 number:pl, person:1, gender:or([masc,fem]),
                 case:dat, sem:[hum]]).
lex(nobis, prpers,[pos:prpers,txt:nobis,lex:pp1pl,
                 number:pl, person:1, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).
lex(uos, prpers,[pos:prpers,txt:uos,lex:pp2pl,
                 number:pl, person:2, gender:or([masc,fem]),
                 case:nom, sem:[hum]]).
lex(uos, prpers,[pos:prpers,txt:uos,lex:pp2pl,
                 number:pl, person:2, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).
lex(uestri, prpers,[pos:prpers,txt:uestri,lex:pp2pl,
                 number:pl, person:2, gender:or([masc,fem]),
                 case:gen, sem:[hum]]).
lex(uestrum, prpers,[pos:prpers,txt:uestrum,lex:pp2pl,
                 number:pl, person:2, gender:or([masc,fem]),
                 case:gen, sem:[hum]]).
lex(uobis, prpers,[pos:prpers,txt:uobis,lex:pp2pl,
                 number:pl, person:2, gender:or([masc,fem]),
                 case:dat, sem:[hum]]).
lex(uobis, prpers,[pos:prpers,txt:uobis,lex:pp2pl,
                 number:pl, person:2, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).

% the reflexive pronouns need to be segregated (see the lex value)
% they need to be accessed by the parser for the double interpretation 
% of SE in subordinate clauses : rex dixit se reginam amasse


lex(se, prpers,[pos:prpers,txt:se,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).

lex(sese, prpers,[pos:prpers,txt:sese,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).

lex(semet, prpers,[pos:prpers,txt:semet,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:acc, sem:[hum]]).


%%%%%

lex(sui, prpers,[pos:prpers,txt:sui,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:gen, sem:[hum]]).
lex(sibi, prpers,[pos:prpers,txt:sibi,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:dat, sem:[hum]]).
lex(se, prpers,[pos:prpers,txt:se,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).
lex(sese, prpers,[pos:prpers,txt:sese,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).
lex(semet, prpers,[pos:prpers,txt:semet,lex:pp3refl,
                 number:or([sing,pl]), person:3, gender:or([masc,fem]),
                 case:abl, sem:[hum]]).






% RELATIVE PRONOUNS
%

lex(qui, relative, [pos:relative, txt:qui, lex:relnommascsing,gender:masc,
                    case:nom, number:sing, function:[subject]]).
lex(qui, relative, [pos:relative, txt:qui, lex:relnommascsing,gender:masc,
                    case:nom, number:pl, function:[subject]]).
lex(quisquis, relative, [pos:relative, txt:quisquis, lex:relnommascsing,gender:masc,
                    case:nom, number:sing, function:[subject]]).

		% acc rel pros must also bear the 'subject' function on account 
                % of their use as subjects in nonfinite clauses :
                     % quem puto epistulam scripsisse

lex(quem, relative, [pos:relative, txt:quem, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[object]]).
lex(quem, relative, [pos:relative, txt:quem, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[object_i]]).
lex(quem, relative, [pos:relative, txt:quem, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[prep_cplt]]).
lex(quem, relative, [pos:relative, txt:quem, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[subject]]).

	% it should be kept in mind that object_i is like a direct object in that it requires an accusative
	% and like an indirect object as far as its semantic role is concerned

lex(cuius, relative, [pos:relative, txt:cuius, lex:relgen3gsing,gender:or([masc,fem,neuter]),
                    case:gen, number:sing, function:[noun_cplt]]).
lex(cui, relative, [pos:relative, txt:cui, lex:reldat3gsing,gender:or([masc,fem,neuter]),
                    case:dat, number:sing, function:[i_object]]).
lex(quo, relative, [pos:relative, txt:quo, lex:relablmascneutersing,gender:or([masc,neuter]),
                    case:abl, number:sing, function:[prep_cplt]]).
lex(quoquo, relative, [pos:relative, txt:quoquo, lex:relablmascneutersing,gender:or([masc,neuter]),
                    case:abl, number:sing, function:[prep_cplt]]).

  % QUAE, an overloaded relative pronoun:

lex(quae, relative, [pos:relative, txt:quae, lex:relnomfemsing,gender:fem,
                    case:nom, number:sing, function:[subject]]).

lex(quae, relative, [pos:relative, txt:quae, lex:relnomfempl,gender:fem,
                    case:nom, number:pl, function:[subject]]).
lex(quae, relative, [pos:relative, txt:quae, lex:relnomneuterpl,gender:neuter,
                    case:nom, number:pl, function:[subject]]).
lex(quae, relative, [pos:relative, txt:quae, lex:relaccneuterpl,gender:neuter,
                    case:acc, number:pl, function:[object]]).
lex(quae, relative, [pos:relative, txt:quae, lex:relaccneuterpl,gender:neuter,
                    case:acc, number:pl, function:[subject]]).
lex(quam, relative, [pos:relative, txt:quam, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[object]]).
lex(quam, relative, [pos:relative, txt:quam, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[object_i]]).
lex(quam, relative, [pos:relative, txt:quam, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[prep_cplt]]).
lex(quam, relative, [pos:relative, txt:quam, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[subject]]).
lex(qua, relative, [pos:relative, txt:qua, lex:relablfemsing,gender:fem,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(quod, relative, [pos:relative, txt:quod, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[subject]]).
lex(quod, relative, [pos:relative, txt:quod, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[object]]).
lex(quod, relative, [pos:relative, txt:quod, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[prep_cplt]]).

lex(quidquid, relative, [pos:relative, txt:quidquid, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[subject]]).
lex(quidquid, relative, [pos:relative, txt:quidquid, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[object]]).
lex(quidquid, relative, [pos:relative, txt:quidquid, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[prep_cplt]]).

lex(quos, relative, [pos:relative, txt:quos, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[object]]).
lex(quos, relative, [pos:relative, txt:quos, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[object_i]]).
lex(quos, relative, [pos:relative, txt:quos, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[prep_cplt]]).
lex(quos, relative, [pos:relative, txt:quos, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[subject]]).
lex(quorum, relative, [pos:relative, txt:quorum, lex:relgenmascneuterpl,gender:or([masc,neuter]),
                    case:gen, number:pl, function:[noun_cplt]]).
lex(quibus, relative, [pos:relative, txt:quibus, lex:reldatabl3gpl,gender:or([masc,fem,neuter]),
                    case:or([dat,abl]), number:pl, function:[i_object]]).
lex(quibus, relative, [pos:relative, txt:quibus, lex:reldatabl3gpl,gender:or([masc,fem,neuter]),
                    case:or([dat,abl]), number:pl, function:[prep_cplt]]).
lex(quas, relative, [pos:relative, txt:quas, lex:relaccfempl,gender:fem,
                    case:acc, number:pl, function:[object]]).
lex(quas, relative, [pos:relative, txt:quas, lex:relaccfempl,gender:fem,
                    case:acc, number:pl, function:[prep_cplt]]).
lex(quas, relative, [pos:relative, txt:quas, lex:relaccfempl,gender:fem,
                    case:acc, number:pl, function:[subject]]).
lex(quarum, relative, [pos:relative, txt:quarum, lex:relgenfempl,gender:fem,
                    case:gen, number:pl, function:[noun_cplt]]).








lex(quicumque, relative, [pos:relative, txt:quicumque, lex:relnommascsing,gender:masc,
                    case:nom, number:or([sing,pl]), function:[subject]]).
lex(quemcumque, relative, [pos:relative, txt:quemcumque, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[object]]).
lex(quemcumque, relative, [pos:relative, txt:quemcumque, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[object_i]]).
lex(quemcumque, relative, [pos:relative, txt:quemcumque, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[prep_cplt]]).
lex(quemcumque, relative, [pos:relative, txt:quemcumque, lex:relaccmascsing,gender:masc,
                    case:acc, number:sing, function:[subject]]).
lex(cuiuscumque, relative, [pos:relative, txt:cuiuscumque, lex:relgen3gsing,gender:or([masc,fem,neuter]),
                    case:gen, number:sing, function:[noun_cplt]]).
lex(cuicumque, relative, [pos:relative, txt:cuicumque, lex:reldat3gsing,gender:or([masc,fem,neuter]),
                    case:dat, number:sing, function:[i_object]]).
lex(quocumque, relative, [pos:relative, txt:quocumque, lex:relablmascneutersing,gender:or([masc,neuter]),
                    case:abl, number:sing, function:[prep_cplt]]).
lex(quaecumque, relative, [pos:relative, txt:quaecumque, lex:relnomfemsing,gender:fem,
                    case:nom, number:sing, function:[subject]]).
lex(quaecumque, relative, [pos:relative, txt:quaecumque, lex:relnomfempl,gender:fem,
                    case:nom, number:pl, function:[subject]]).
lex(quaecumque, relative, [pos:relative, txt:quaecumque, lex:relnomneuterpl,gender:neuter,
                    case:nom, number:pl, function:[subject]]).
lex(quaecumque, relative, [pos:relative, txt:quaecumque, lex:relaccneuterpl,gender:neuter,
                    case:acc, number:pl, function:[object]]).
lex(quaecumque, relative, [pos:relative, txt:quaecumque, lex:relaccneuterpl,gender:neuter,
                    case:acc, number:pl, function:[subject]]).
lex(quamcumque, relative, [pos:relative, txt:quamcumque, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[object]]).
lex(quamcumque, relative, [pos:relative, txt:quamcumque, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[object_i]]).
lex(quamcumque, relative, [pos:relative, txt:quamcumque, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[prep_cplt]]).
lex(quamcumque, relative, [pos:relative, txt:quamcumque, lex:relaccfemsing,gender:fem,
                    case:acc, number:sing, function:[subject]]).
lex(quacumque, relative, [pos:relative, txt:quacumque, lex:relablfemsing,gender:fem,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(quodcumque, relative, [pos:relative, txt:quodcumque, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[subject]]).
lex(quodcumque, relative, [pos:relative, txt:quodcumque, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[object]]).
lex(quodcumque, relative, [pos:relative, txt:quodcumque, lex:relneutersing,gender:neuter,
                    case:or([nom,acc]), number:sing, function:[prep_cplt]]).
lex(quoscumque, relative, [pos:relative, txt:quoscumque, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[object]]).
lex(quoscumque, relative, [pos:relative, txt:quoscumque, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[object_i]]).
lex(quoscumque, relative, [pos:relative, txt:quoscumque, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[prep_cplt]]).
lex(quoscumque, relative, [pos:relative, txt:quoscumque, lex:relaccmascpl,gender:masc,
                    case:acc, number:pl, function:[subject]]).
lex(quorumcumque, relative, [pos:relative, txt:quorumcumque, lex:relgenmascneuterpl,gender:or([masc,neuter]),
                    case:gen, number:pl, function:[noun_cplt]]).
lex(quibuscumque, relative, [pos:relative, txt:quibuscumque, lex:reldatabl3gpl,gender:or([masc,fem,neuter]),
                    case:or([dat,abl]), number:pl, function:[i_object]]).
lex(quibuscumque, relative, [pos:relative, txt:quibuscumque, lex:reldatabl3gpl,gender:or([masc,fem,neuter]),
                    case:or([dat,abl]), number:pl, function:[prep_cplt]]).
lex(quascumque, relative, [pos:relative, txt:quascumque, lex:relaccfempl,gender:fem,
                    case:acc, number:pl, function:[object]]).
lex(quascumque, relative, [pos:relative, txt:quascumque, lex:relaccfempl,gender:fem,
                    case:acc, number:pl, function:[prep_cplt]]).
lex(quascumque, relative, [pos:relative, txt:quascumque, lex:relaccfempl,gender:fem,
                    case:acc, number:pl, function:[subject]]).
lex(quarumcumque, relative, [pos:relative, txt:quarumcumque, lex:relgenfempl,gender:fem,
                    case:gen, number:pl, function:[noun_cplt]]).





% PERSONAL PRONOUNS THIRD PERSON NON-REFLEXIVE
%

lex(is, prpers, [pos:prpers, txt:is, lex:prpersnommascsing,gender:masc,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(ea, prpers, [pos:prpers, txt:ea, lex:prpersnomfemsing,gender:fem,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(id, prpers, [pos:prpers, txt:id, lex:prpersnomaccneutersing,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:sing, function:[subject, object]]).
% acc  pros must also bear the 'subject' function on account of their use as subjects in nonfinite clauses :
                     %  puto eum epistulam scripsisse
lex(eum, prpers, [pos:prpers, txt:eum, lex:prpersaccmascsing,gender:masc,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(eam, prpers, [pos:prpers, txt:eam, lex:prpersaccfemsing,gender:fem,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(eius, prpers, [pos:prpers, txt:eius, lex:prpersgen3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(ei, prpers, [pos:prpers, txt:ei, lex:prpersdat3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(eo, prpers, [pos:prpers, txt:eo, lex:prpersablmascneutersing,gender:or([masc,neuter]),person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(ea, prpers, [pos:prpers, txt:ea, lex:prpersablfemsing,gender:fem,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).

% PLURAL
%

lex(ii, prpers, [pos:prpers, txt:ii, lex:prpersnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(ei, prpers, [pos:prpers, txt:ei, lex:prpersnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(eae, prpers, [pos:prpers, txt:eae, lex:prpersnomfempl,gender:fem,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(ea, prpers, [pos:prpers, txt:ea, lex:prpersnomaccneuterpl,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:pl, function:[subject,object]]).
lex(eos, prpers, [pos:prpers, txt:eos, lex:prpersaccmascpl,gender:masc,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(eas, prpers, [pos:prpers, txt:eas, lex:prpersaccfemspl,gender:fem,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(eorum, prpers, [pos:prpers, txt:eorum, lex:prpersgenmascneuterpl,gender:or([masc,neuter]),person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(earum, prpers, [pos:prpers, txt:earum, lex:prpersgenfempl,gender:fem,person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).

lex(eis, prpers, [pos:prpers, txt:eis, lex:prpersdatabl3gpl,gender:masc,person:3,sem:_,
                    case:dat, number:pl, function:[i_object, prep_cplt]]).
lex(iis, prpers, [pos:prpers, txt:iis, lex:prpersdatabl3gpl,gender:masc,person:3,sem:_,
                    case:dat, number:pl, function:[i_object, prep_cplt]]).

lex(eis, prpers, [pos:prpers, txt:eis, lex:prpersdatabl3gpl,gender:masc,person:3,sem:_,
                    case:abl, number:pl, function:[i_object, prep_cplt]]).
lex(iis, prpers, [pos:prpers, txt:iis, lex:prpersdatabl3gpl,gender:masc,person:3,sem:_,
                    case:abl, number:pl, function:[i_object, prep_cplt]]).

lex(eis, prpers, [pos:prpers, txt:eis, lex:prpersdatabl3gpl,gender:fem,person:3,sem:_,
                    case:dat, number:pl, function:[i_object, prep_cplt]]).
lex(iis, prpers, [pos:prpers, txt:iis, lex:prpersdatabl3gpl,gender:fem,person:3,sem:_,
                    case:dat, number:pl, function:[i_object, prep_cplt]]).

lex(eis, prpers, [pos:prpers, txt:eis, lex:prpersdatabl3gpl,gender:fem,person:3,sem:_,
                    case:abl, number:pl, function:[i_object, prep_cplt]]).
lex(iis, prpers, [pos:prpers, txt:iis, lex:prpersdatabl3gpl,gender:fem,person:3,sem:_,
                    case:abl, number:pl, function:[i_object, prep_cplt]]).

lex(eis, prpers, [pos:prpers, txt:eis, lex:prpersdatabl3gpl,gender:neuter,person:3,sem:_,
                    case:dat, number:pl, function:[i_object, prep_cplt]]).
lex(iis, prpers, [pos:prpers, txt:iis, lex:prpersdatabl3gpl,gender:neuter,person:3,sem:_,
                    case:dat, number:pl, function:[i_object, prep_cplt]]).

lex(eis, prpers, [pos:prpers, txt:eis, lex:prpersdatabl3gpl,gender:neuter,person:3,sem:_,
                    case:abl, number:pl, function:[i_object, prep_cplt]]).
lex(iis, prpers, [pos:prpers, txt:iis, lex:prpersdatabl3gpl,gender:neuter,person:3,sem:_,
                    case:abl, number:pl, function:[i_object, prep_cplt]]).



% ADJ IS/EA/ID
%

lex(is, adj, [pos:adj, txt:is, lex:is,gender:masc,type:tool,
                    case:nom, number:sing]).
lex(ea, adj, [pos:adj, txt:ea, lex:is,gender:fem,type:tool,
                    case:nom, number:sing]).
lex(id, adj, [pos:adj, txt:id, lex:is,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).
lex(eum, adj, [pos:adj, txt:eum, lex:is,gender:masc,type:tool,
                    case:acc, number:sing]).
lex(eam, adj, [pos:adj, txt:eam, lex:is,gender:fem,type:tool,
                    case:acc, number:sing]).
lex(eius, adj, [pos:adj, txt:eius, lex:is,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:sing]).
lex(ei, adj, [pos:adj, txt:ei, lex:is,gender:or([masc,fem,neuter]),type:tool,
                    case:dat, number:sing]).
lex(eo, adj, [pos:adj, txt:eo, lex:is,gender:or([masc,neuter]),type:tool,
                    case:abl, number:sing]).
lex(ea, adj, [pos:adj, txt:ea, lex:is,gender:fem,type:tool,
                    case:abl, number:sing]).

% PLURAL
%

lex(ii, adj, [pos:adj, txt:ii, lex:is,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(ei, adj, [pos:adj, txt:ei, lex:is,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(eae, adj, [pos:adj, txt:eae, lex:is,gender:fem,type:tool,
                    case:nom, number:pl]).
lex(ea, adj, [pos:adj, txt:ea, lex:is,gender:neuter,type:tool,
                    case:or([nom,acc]), number:pl]).
lex(eos, adj, [pos:adj, txt:eos, lex:is,gender:masc,type:tool,
                    case:acc, number:pl]).
lex(eas, adj, [pos:adj, txt:eas, lex:is,gender:fem,type:tool,
                    case:acc, number:pl]).
lex(eorum, adj, [pos:adj, txt:eorum, lex:is,gender:or([masc,neuter]),type:tool,
                    case:gen, number:pl]).
lex(earum, adj, [pos:adj, txt:earum, lex:is,gender:fem,type:tool,
                    case:gen, number:pl]).
lex(eis, adj, [pos:adj, txt:eis, lex:is,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).
lex(iis, adj, [pos:adj, txt:iis, lex:is,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).






% IDEM EADEM IDEM
%

lex(idem, prpers, [pos:prpers, txt:idem, lex:prpersnommascsing,gender:masc,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(eadem, prpers, [pos:prpers, txt:eadem, lex:prpersnomfemsing,gender:fem,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(idem, prpers, [pos:prpers, txt:idem, lex:prpersnomaccneutersing,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:sing, function:[subject, object]]).
% acc  pros must also bear the 'subject' function on account of their use as subjects in nonfinite clauses :
                     %  puto eundem epistulam scripsisse
lex(eundem, prpers, [pos:prpers, txt:eundem, lex:prpersaccmascsing,gender:masc,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(eandem, prpers, [pos:prpers, txt:eandem, lex:prpersaccfemsing,gender:fem,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(eiusdem, prpers, [pos:prpers, txt:eiusdem, lex:prpersgen3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(eidem, prpers, [pos:prpers, txt:eidem, lex:prpersdat3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(eodem, prpers, [pos:prpers, txt:eodem, lex:prpersablmascneutersing,gender:or([masc,neuter]),person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(eadem, prpers, [pos:prpers, txt:eadem, lex:prpersablfemsing,gender:fem,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).

% PLURAL
%

lex(iidem, prpers, [pos:prpers, txt:iidem, lex:prpersnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(eidem, prpers, [pos:prpers, txt:eidem, lex:prpersnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(eaedem, prpers, [pos:prpers, txt:eaedem, lex:prpersnomfempl,gender:fem,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(eadem, prpers, [pos:prpers, txt:eadem, lex:prpersnomaccneuterpl,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:pl, function:[subject,object]]).
lex(eosdem, prpers, [pos:prpers, txt:eosdem, lex:prpersaccmascpl,gender:masc,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(easdem, prpers, [pos:prpers, txt:easdem, lex:prpersaccfemspl,gender:fem,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(eorundem, prpers, [pos:prpers, txt:eorundem, lex:prpersgenmascneuterpl,gender:or([masc,neuter]),person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(earundem, prpers, [pos:prpers, txt:earundem, lex:prpersgenfempl,gender:fem,person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(eisdem, prpers, [pos:prpers, txt:eisdem, lex:prpersdatabl3gpl,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:or([dat,abl]), number:pl, function:[i_object, prep_cplt]]).
lex(iisdem, prpers, [pos:prpers, txt:iisdem, lex:prpersdatabl3gpl,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:or([dat,abl]), number:pl, function:[i_object, prep_cplt]]).



% ADJ IDEM EADEM IDEM
%

lex(idem, adj, [pos:adj, txt:idem, lex:idem,gender:masc,type:tool,
                    case:nom, number:sing]).
lex(eadem, adj, [pos:adj, txt:eadem, lex:idem,gender:fem,type:tool,
                    case:nom, number:sing]).
lex(idem, adj, [pos:adj, txt:idem, lex:idem,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).
lex(eundem, adj, [pos:adj, txt:eundem, lex:idem,gender:masc,type:tool,
                    case:acc, number:sing]).
lex(eandem, adj, [pos:adj, txt:eandem, lex:idem,gender:fem,type:tool,
                    case:acc, number:sing]).
lex(eiusdem, adj, [pos:adj, txt:eiusdem, lex:idem,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:sing]).
lex(eidem, adj, [pos:adj, txt:eidem, lex:idem,gender:or([masc,fem,neuter]),type:tool,
                    case:dat, number:sing]).
lex(eodem, adj, [pos:adj, txt:eodem, lex:idem,gender:or([masc,neuter]),type:tool,
                    case:abl, number:sing]).
lex(eadem, adj, [pos:adj, txt:eadem, lex:idem,gender:fem,type:tool,
                    case:abl, number:sing]).

% PLURAL
%

lex(iidem, adj, [pos:adj, txt:iidem, lex:idem,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(eidem, adj, [pos:adj, txt:eidem, lex:idem,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(eaedem, adj, [pos:adj, txt:eaedem, lex:idem,gender:fem,type:tool,
                    case:nom, number:pl]).
lex(eadem, adj, [pos:adj, txt:eadem, lex:idem,gender:neuter,type:tool,
                    case:or([nom,acc]), number:pl]).
lex(eosdem, adj, [pos:adj, txt:eosdem, lex:idem,gender:masc,type:tool,
                    case:acc, number:pl]).
lex(easdem, adj, [pos:adj, txt:easdem, lex:idem,gender:fem,type:tool,
                    case:acc, number:pl]).
lex(eorundem, adj, [pos:adj, txt:eorundem, lex:idem,gender:or([masc,neuter]),type:tool,
                    case:gen, number:pl]).
lex(earundem, adj, [pos:adj, txt:earundem, lex:idem,gender:fem,type:tool,
                    case:gen, number:pl]).
lex(eisdem, adj, [pos:adj, txt:eisdem, lex:idem,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).
lex(iisdem, adj, [pos:adj, txt:iisdem, lex:idem,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).







% HIC HAEC HOC
%

lex(hic, prdem, [pos:prdem, txt:hic, lex:prdemnommascsing,gender:masc,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(haec, prdem, [pos:prdem, txt:haec, lex:prdemnomfemsing,gender:fem,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(hoc, prdem, [pos:prdem, txt:hoc, lex:prdemnomaccneutersing,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:sing, function:[subject, object]]).
lex(hunc, prdem, [pos:prdem, txt:hunc, lex:prdemaccmascsing,gender:masc,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(hanc, prdem, [pos:prdem, txt:hanc, lex:prdemaccfemsing,gender:fem,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(huius, prdem, [pos:prdem, txt:huius, lex:prdemgen3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(huic, prdem, [pos:prdem, txt:huic, lex:prdemdat3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(hoc, prdem, [pos:prdem, txt:hoc, lex:prdemablmascneutersing,gender:or([masc,neuter]),person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(hac, prdem, [pos:prdem, txt:hac, lex:prdemablfemsing,gender:fem,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).

% PLURAL
%

lex(hi, prdem, [pos:prdem, txt:hi, lex:prdemnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(hae, prdem, [pos:prdem, txt:hae, lex:prdemnomfempl,gender:fem,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(haec, prdem, [pos:prdem, txt:haec, lex:prdemnomaccneuterpl,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:pl, function:[subject,object]]).
lex(hos, prdem, [pos:prdem, txt:hos, lex:prdemaccmascpl,gender:masc,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(has, prdem, [pos:prdem, txt:has, lex:prdemaccfemspl,gender:fem,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(horum, prdem, [pos:prdem, txt:horum, lex:prdemgenmascneuterpl,gender:or([masc,neuter]),person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(harum, prdem, [pos:prdem, txt:harum, lex:prdemgenfempl,gender:fem,person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(his, prdem, [pos:prdem, txt:eis, lex:prdemdatabl3gpl,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:or([dat,abl]), number:pl, function:[i_object, prep_cplt]]).



% ADJ hic haec hoc
%

lex(hic, adj, [pos:adj, txt:hic, lex:hic,gender:masc,type:tool,
                    case:nom, number:sing]).
lex(haec, adj, [pos:adj, txt:haec, lex:hic,gender:fem,type:tool,
                    case:nom, number:sing]).
lex(hoc, adj, [pos:adj, txt:hoc, lex:hic,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).
lex(hunc, adj, [pos:adj, txt:hunc, lex:hic,gender:masc,type:tool,
                    case:acc, number:sing]).
lex(hanc, adj, [pos:adj, txt:hanc, lex:hic,gender:fem,type:tool,
                    case:acc, number:sing]).
lex(huius, adj, [pos:adj, txt:huius, lex:hic,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:sing]).
lex(huic, adj, [pos:adj, txt:huic, lex:hic,gender:or([masc,fem,neuter]),type:tool,
                    case:dat, number:sing]).
lex(hoc, adj, [pos:adj, txt:hoc, lex:hic,gender:or([masc,neuter]),type:tool,
                    case:abl, number:sing]).
lex(hac, adj, [pos:adj, txt:hac, lex:hic,gender:fem,type:tool,
                    case:abl, number:sing]).

% PLURAL
%

lex(hi, adj, [pos:adj, txt:hi, lex:hic,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(hae, adj, [pos:adj, txt:hae, lex:hic,gender:fem,type:tool,
                    case:nom, number:pl]).
lex(haec, adj, [pos:adj, txt:haec, lex:hic,gender:neuter,type:tool,
                    case:or([nom,acc]), number:pl]).
lex(hos, adj, [pos:adj, txt:hos, lex:hic,gender:masc,type:tool,
                    case:acc, number:pl]).
lex(has, adj, [pos:adj, txt:has, lex:hic,gender:fem,type:tool,
                    case:acc, number:pl]).
lex(horum, adj, [pos:adj, txt:horum, lex:hic,gender:or([masc,neuter]),type:tool,
                    case:gen, number:pl]).
lex(harum, adj, [pos:adj, txt:harum, lex:hic,gender:fem,type:tool,
                    case:gen, number:pl]).
lex(his, adj, [pos:adj, txt:his, lex:hic,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).





% ISTE ISTA ISTUD
%

lex(iste, prdem, [pos:prdem, txt:iste, lex:prdemnommascsing,gender:masc,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(ista, prdem, [pos:prdem, txt:ista, lex:prdemnomfemsing,gender:fem,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(istud, prdem, [pos:prdem, txt:istud, lex:prdemnomaccneutersing,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:sing, function:[subject, object]]).
lex(istum, prdem, [pos:prdem, txt:istum, lex:prdemaccmascsing,gender:masc,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(istam, prdem, [pos:prdem, txt:istam, lex:prdemaccfemsing,gender:fem,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(istius, prdem, [pos:prdem, txt:istius, lex:prdemgen3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(isti, prdem, [pos:prdem, txt:isti, lex:prdemdat3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(isto, prdem, [pos:prdem, txt:isto, lex:prdemablmascneutersing,gender:or([masc,neuter]),person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(ista, prdem, [pos:prdem, txt:ista, lex:prdemablfemsing,gender:fem,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).

% PLURAL
%

lex(isti, prdem, [pos:prdem, txt:isti, lex:prdemnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(istae, prdem, [pos:prdem, txt:istae, lex:prdemnomfempl,gender:fem,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(ista, prdem, [pos:prdem, txt:ista, lex:prdemnomaccneuterpl,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:pl, function:[subject,object]]).
lex(istos, prdem, [pos:prdem, txt:istos, lex:prdemaccmascpl,gender:masc,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(istas, prdem, [pos:prdem, txt:istas, lex:prdemaccfemspl,gender:fem,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(istorum, prdem, [pos:prdem, txt:istorum, lex:prdemgenmascneuterpl,gender:or([masc,neuter]),person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(istarum, prdem, [pos:prdem, txt:istarum, lex:prdemgenfempl,gender:fem,person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(istis, prdem, [pos:prdem, txt:istis, lex:prdemdatabl3gpl,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:or([dat,abl]), number:pl, function:[i_object, prep_cplt]]).



% ADJ iste ista istud
%

lex(iste, adj, [pos:adj, txt:iste, lex:iste,gender:masc,type:tool,
                    case:nom, number:sing]).
lex(ista, adj, [pos:adj, txt:ista, lex:iste,gender:fem,type:tool,
                    case:nom, number:sing]).
lex(istud, adj, [pos:adj, txt:istud, lex:iste,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).
lex(istum, adj, [pos:adj, txt:istum, lex:iste,gender:masc,type:tool,
                    case:acc, number:sing]).
lex(istam, adj, [pos:adj, txt:istam, lex:iste,gender:fem,type:tool,
                    case:acc, number:sing]).
lex(istius, adj, [pos:adj, txt:istius, lex:iste,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:sing]).
lex(isti, adj, [pos:adj, txt:isti, lex:iste,gender:or([masc,fem,neuter]),type:tool,
                    case:dat, number:sing]).
lex(isto, adj, [pos:adj, txt:isto, lex:iste,gender:or([masc,neuter]),type:tool,
                    case:abl, number:sing]).
lex(ista, adj, [pos:adj, txt:ista, lex:iste,gender:fem,type:tool,
                    case:abl, number:sing]).

% PLURAL
%

lex(isti, adj, [pos:adj, txt:isti, lex:iste,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(istae, adj, [pos:adj, txt:istae, lex:iste,gender:fem,type:tool,
                    case:nom, number:pl]).
lex(ista, adj, [pos:adj, txt:ista, lex:iste,gender:neuter,type:tool,
                    case:or([nom,acc]), number:pl]).
lex(istos, adj, [pos:adj, txt:istos, lex:iste,gender:masc,type:tool,
                    case:acc, number:pl]).
lex(istas, adj, [pos:adj, txt:istas, lex:iste,gender:fem,type:tool,
                    case:acc, number:pl]).
lex(istorum, adj, [pos:adj, txt:istorum, lex:iste,gender:or([masc,neuter]),type:tool,
                    case:gen, number:pl]).
lex(istarum, adj, [pos:adj, txt:istarum, lex:iste,gender:fem,type:tool,
                    case:gen, number:pl]).
lex(istis, adj, [pos:adj, txt:istis, lex:iste,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).






% ILLE ILLA ILLUD
%

lex(ille, prdem, [pos:prdem, txt:ille, lex:prdemnommascsing,gender:masc,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(illa, prdem, [pos:prdem, txt:illa, lex:prdemnomfemsing,gender:fem,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(illud, prdem, [pos:prdem, txt:illud, lex:prdemnomaccneutersing,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:sing, function:[subject, object]]).
lex(illum, prdem, [pos:prdem, txt:illum, lex:prdemaccmascsing,gender:masc,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(illam, prdem, [pos:prdem, txt:illam, lex:prdemaccfemsing,gender:fem,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(illius, prdem, [pos:prdem, txt:illius, lex:prdemgen3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(illi, prdem, [pos:prdem, txt:illi, lex:prdemdat3gsing,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(illo, prdem, [pos:prdem, txt:illo, lex:prdemablmascneutersing,gender:or([masc,neuter]),person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(illa, prdem, [pos:prdem, txt:illa, lex:prdemablfemsing,gender:fem,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).

% PLURAL
%

lex(illi, prdem, [pos:prdem, txt:illi, lex:prdemnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(illae, prdem, [pos:prdem, txt:illae, lex:prdemnomfempl,gender:fem,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(illa, prdem, [pos:prdem, txt:illa, lex:prdemnomaccneuterpl,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:pl, function:[subject,object]]).
lex(illos, prdem, [pos:prdem, txt:illos, lex:prdemaccmascpl,gender:masc,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(illas, prdem, [pos:prdem, txt:illas, lex:prdemaccfemspl,gender:fem,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(illorum, prdem, [pos:prdem, txt:illorum, lex:prdemgenmascneuterpl,gender:or([masc,neuter]),person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(illarum, prdem, [pos:prdem, txt:illarum, lex:prdemgenfempl,gender:fem,person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(illis, prdem, [pos:prdem, txt:illis, lex:prdemdatabl3gpl,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:or([dat,abl]), number:pl, function:[i_object, prep_cplt]]).



% ADJ ille illa illud
%

lex(ille, adj, [pos:adj, txt:ille, lex:ille,gender:masc,type:tool,
                    case:nom, number:sing]).
lex(illa, adj, [pos:adj, txt:illa, lex:ille,gender:fem,type:tool,
                    case:nom, number:sing]).
lex(illud, adj, [pos:adj, txt:illud, lex:ille,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).
lex(illum, adj, [pos:adj, txt:illum, lex:ille,gender:masc,type:tool,
                    case:acc, number:sing]).
lex(illam, adj, [pos:adj, txt:illam, lex:ille,gender:fem,type:tool,
                    case:acc, number:sing]).
lex(illius, adj, [pos:adj, txt:illius, lex:ille,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:sing]).
lex(illi, adj, [pos:adj, txt:illi, lex:ille,gender:or([masc,fem,neuter]),type:tool,
                    case:dat, number:sing]).
lex(illo, adj, [pos:adj, txt:illo, lex:ille,gender:or([masc,neuter]),type:tool,
                    case:abl, number:sing]).
lex(illa, adj, [pos:adj, txt:illa, lex:ille,gender:fem,type:tool,
                    case:abl, number:sing]).

% PLURAL
%

lex(illi, adj, [pos:adj, txt:illi, lex:ille,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(illae, adj, [pos:adj, txt:illae, lex:ille,gender:fem,type:tool,
                    case:nom, number:pl]).
lex(illa, adj, [pos:adj, txt:illa, lex:ille,gender:neuter,type:tool,
                    case:or([nom,acc]), number:pl]).
lex(illos, adj, [pos:adj, txt:illos, lex:ille,gender:masc,type:tool,
                    case:acc, number:pl]).
lex(illas, adj, [pos:adj, txt:illas, lex:ille,gender:fem,type:tool,
                    case:acc, number:pl]).
lex(illorum, adj, [pos:adj, txt:illorum, lex:ille,gender:or([masc,neuter]),type:tool,
                    case:gen, number:pl]).
lex(illarum, adj, [pos:adj, txt:illarum, lex:ille,gender:fem,type:tool,
                    case:gen, number:pl]).
lex(illis, adj, [pos:adj, txt:illis, lex:ille,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).






% IPSE IPSA IPSUM
%

lex(ipse, prdem, [pos:prdem, txt:ipse, lex:prdemnommascsing,gender:masc,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(ipsa, prdem, [pos:prdem, txt:ipsa, lex:prdemnomfemsing,gender:fem,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(ipsum, prdem, [pos:prdem, txt:ipsum, lex:prdemnomaccneutersing,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:sing, function:[subject, object]]).
lex(ipsum, prdem, [pos:prdem, txt:ipsum, lex:prdemaccmascsing,gender:masc,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(ipsam, prdem, [pos:prdem, txt:ipsam, lex:prdemaccfemsing,gender:fem,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(ipsius, prdem, [pos:prdem, txt:ipsius, lex:prdemgen3gsing,gender:masc,person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(ipsius, prdem, [pos:prdem, txt:ipsius, lex:prdemgen3gsing,gender:fem,person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(ipsius, prdem, [pos:prdem, txt:ipsius, lex:prdemgen3gsing,gender:neuter,person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(ipsi, prdem, [pos:prdem, txt:ipsi, lex:prdemdat3gsing,gender:masc,person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(ipsi, prdem, [pos:prdem, txt:ipsi, lex:prdemdat3gsing,gender:fem,person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(ipsi, prdem, [pos:prdem, txt:ipsi, lex:prdemdat3gsing,gender:neuter,person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(ipso, prdem, [pos:prdem, txt:ipso, lex:prdemablmascneutersing,gender:masc,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(ipso, prdem, [pos:prdem, txt:ipso, lex:prdemablmascneutersing,gender:neuter,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(ipsa, prdem, [pos:prdem, txt:ipsa, lex:prdemablfemsing,gender:fem,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).

% PLURAL
%

lex(ipsi, prdem, [pos:prdem, txt:ipsi, lex:prdemnommascpl,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(ipsae, prdem, [pos:prdem, txt:ipsae, lex:prdemnomfempl,gender:fem,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(ipsa, prdem, [pos:prdem, txt:ipsa, lex:prdemnomaccneuterpl,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:pl, function:[subject,object]]).
lex(ipsos, prdem, [pos:prdem, txt:ipsos, lex:prdemaccmascpl,gender:masc,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(ipsas, prdem, [pos:prdem, txt:ipsas, lex:prdemaccfemspl,gender:fem,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(ipsorum, prdem, [pos:prdem, txt:ipsorum, lex:prdemgenmascneuterpl,gender:or([masc,neuter]),person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(ipsarum, prdem, [pos:prdem, txt:ipsarum, lex:prdemgenfempl,gender:fem,person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).

lex(ipsis, prdem, [pos:prdem, txt:ipsis, lex:prdemdatabl3gpl,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:or([dat,abl]), number:pl, function:[i_object, prep_cplt]]).



% ADJ ipse ipsa ipsum
%

lex(ipse, adj, [pos:adj, txt:ipse, lex:ipse,gender:masc,type:tool,
                    case:nom, number:sing]).
lex(ipsa, adj, [pos:adj, txt:ipsa, lex:ipse,gender:fem,type:tool,
                    case:nom, number:sing]).
lex(ipsum, adj, [pos:adj, txt:ipsum, lex:ipse,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).
lex(ipsum, adj, [pos:adj, txt:ipsum, lex:ipse,gender:masc,type:tool,
                    case:acc, number:sing]).
lex(ipsam, adj, [pos:adj, txt:ipsam, lex:ipse,gender:fem,type:tool,
                    case:acc, number:sing]).
lex(ipsius, adj, [pos:adj, txt:ipsius, lex:ipse,gender:masc,type:tool,
                    case:gen, number:sing]).
lex(ipsius, adj, [pos:adj, txt:ipsius, lex:ipse,gender:fem,type:tool,
                    case:gen, number:sing]).
lex(ipsius, adj, [pos:adj, txt:ipsius, lex:ipse,gender:neuter,type:tool,
                    case:gen, number:sing]).
lex(ipsi, adj, [pos:adj, txt:ipsi, lex:ipse,gender:masc,type:tool,
                    case:dat, number:sing]).
lex(ipsi, adj, [pos:adj, txt:ipsi, lex:ipse,gender:fem,type:tool,
                    case:dat, number:sing]).
lex(ipsi, adj, [pos:adj, txt:ipsi, lex:ipse,gender:neuter,type:tool,
                    case:dat, number:sing]).
lex(ipso, adj, [pos:adj, txt:ipso, lex:ipse,gender:masc,type:tool,
                    case:abl, number:sing]).
lex(ipso, adj, [pos:adj, txt:ipso, lex:ipse,gender:neuter,type:tool,
                    case:abl, number:sing]).
lex(ipsa, adj, [pos:adj, txt:ipsa, lex:ipse,gender:fem,type:tool,
                    case:abl, number:sing]).

% PLURAL
%

lex(ipsi, adj, [pos:adj, txt:ipsi, lex:ipse,gender:masc,type:tool,
                    case:nom, number:pl]).
lex(ipsae, adj, [pos:adj, txt:ipsae, lex:ipse,gender:fem,type:tool,
                    case:nom, number:pl]).
lex(ipsa, adj, [pos:adj, txt:ipsa, lex:ipse,gender:neuter,type:tool,
                    case:or([nom,acc]), number:pl]).
lex(ipsos, adj, [pos:adj, txt:ipsos, lex:ipse,gender:masc,type:tool,
                    case:acc, number:pl]).
lex(ipsas, adj, [pos:adj, txt:ipsas, lex:ipse,gender:fem,type:tool,
                    case:acc, number:pl]).
lex(ipsorum, adj, [pos:adj, txt:ipsorum, lex:ipse,gender:masc,type:tool,
                    case:gen, number:pl]).
lex(ipsorum, adj, [pos:adj, txt:ipsorum, lex:ipse,gender:neuter,type:tool,
                    case:gen, number:pl]).
lex(ipsarum, adj, [pos:adj, txt:ipsarum, lex:ipse,gender:fem,type:tool,
                    case:gen, number:pl]).
lex(ipsis, adj, [pos:adj, txt:ipsis, lex:ipse,gender:masc,type:tool,
                    case:or([dat,abl]), number:pl]).
lex(ipsis, adj, [pos:adj, txt:ipsis, lex:ipse,gender:fem,type:tool,
                    case:or([dat,abl]), number:pl]).
lex(ipsis, adj, [pos:adj, txt:ipsis, lex:ipse,gender:neuter,type:tool,
                    case:or([dat,abl]), number:pl]).






% ALIUS ALIA ALIUD
%

lex(alius, prindef, [pos:prindef, txt:alius, lex:alius,gender:masc,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(alia, prindef, [pos:prindef, txt:alia, lex:alius,gender:fem,person:3,sem:_,
                    case:nom, number:sing, function:[subject]]).
lex(aliud, prindef, [pos:prindef, txt:aliud, lex:alius,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:sing, function:[subject, object]]).
lex(alium, prindef, [pos:prindef, txt:alium, lex:alius,gender:masc,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(aliam, prindef, [pos:prindef, txt:aliam, lex:alius,gender:fem,person:3,sem:_,
                    case:acc, number:sing, function:[object,prep_cplt,subject]]).
lex(alius, prindef, [pos:prindef, txt:alius, lex:alius,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:gen, number:sing, function:[noun_cplt]]).
lex(alii, prindef, [pos:prindef, txt:alii, lex:alius,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:dat, number:sing, function:[i_object]]).
lex(alio, prindef, [pos:prindef, txt:alio, lex:alius,gender:or([masc,neuter]),person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).
lex(alia, prindef, [pos:prindef, txt:alia, lex:alius,gender:fem,person:3,sem:_,
                    case:abl, number:sing, function:[prep_cplt]]).

% PLURAL
%

lex(alii, prindef, [pos:prindef, txt:alii, lex:alius,gender:masc,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(aliae, prindef, [pos:prindef, txt:aliae, lex:alius,gender:fem,person:3,sem:_,
                    case:nom, number:pl, function:[subject]]).
lex(alia, prindef, [pos:prindef, txt:alia, lex:alius,gender:neuter,person:3,sem:_,
                    case:or([nom,acc]), number:pl, function:[subject,object]]).
lex(alios, prindef, [pos:prindef, txt:alios, lex:alius,gender:masc,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(alias, prindef, [pos:prindef, txt:alias, lex:alius,gender:fem,person:3,sem:_,
                    case:acc, number:pl, function:[object,prep_cplt,subject]]).
lex(aliorum, prindef, [pos:prindef, txt:aliorum, lex:alius,gender:or([masc,neuter]),person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(aliarum, prindef, [pos:prindef, txt:aliarum, lex:alius,gender:fem,person:3,sem:_,
                    case:gen, number:pl, function:[noun_cplt]]).
lex(aliis, prindef, [pos:prindef, txt:aliis, lex:alius,gender:or([masc,fem,neuter]),person:3,sem:_,
                    case:or([dat,abl]), number:pl, function:[i_object, prep_cplt]]).


% ADJ aliud
%

lex(aliud, adj, [pos:adj, txt:aliud, lex:alius,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).

lex(alius, adj, [pos:adj, txt:alius, lex:alius,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:sing]).





% OMNIS
%

lex(omnis, adj, [pos:adj, txt:omnis, lex:omnis,gender:or([masc,fem]),type:tool,
                    case:nom, number:sing]).
lex(omne, adj, [pos:adj, txt:omne, lex:omnis,gender:neuter,type:tool,
                    case:or([nom,acc]), number:sing]).
lex(omnem, adj, [pos:adj, txt:omnem, lex:omnis,gender:or([masc,fem]),type:tool,
                    case:acc, number:sing]).
lex(omnis, adj, [pos:adj, txt:omnis, lex:omnis,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:sing]).
lex(omni, adj, [pos:adj, txt:ipsi, lex:omnis,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:sing]).
lex(omnes, adj, [pos:adj, txt:omnes, lex:omnis,gender:or([masc,fem]),type:tool,
                    case:or([nom,acc]), number:pl]).
lex(omnia, adj, [pos:adj, txt:omnia, lex:omnis,gender:neuter,type:tool,
                    case:nom, number:pl]).
lex(omnia, adj, [pos:adj, txt:omnia, lex:omnis,gender:neuter,type:tool,
                    case:acc, number:pl]).
lex(omnium, adj, [pos:adj, txt:omnium, lex:omnis,gender:or([masc,fem,neuter]),type:tool,
                    case:gen, number:pl]).
lex(omnibus, adj, [pos:adj, txt:omnibus, lex:omnis,gender:or([masc,fem,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).

% Pronoun (only in the plural)
%

lex(omnes, prindef, [pos:prindef, txt:omnes, lex:omnis,gender:masc,
                    case:or([nom,acc]), number:pl,person:3,sem:[hum],
                    function:[subject, object]]).
lex(omnia, prindef, [pos:prindef, txt:omnia, lex:omnis,gender:neuter,
                    case:acc, number:pl,person:3,sem:[thing, abstract],
                    function:[object]]).
lex(omnia, prindef, [pos:prindef, txt:omnia, lex:omnis,gender:neuter,
                    case:nom, number:pl,person:3,sem:[thing, abstract],
                    function:[subject]]).
lex(omnium, prindef, [pos:prindef, txt:omnium, lex:omnis,gender:or([masc,fem,neuter]),
                    case:gen, number:pl,person:3,sem:_,
                    function:[noun_cplt]]).
lex(omnibus, prindef, [pos:prindef, txt:omnibus, lex:omnis,gender:or([masc,fem,neuter]),
                    case:or([dat,abl]), number:pl,person:3,sem:_,
                    function:[i_object, prep_cplt]]).





% NEMO
%

lex(nemo, prindef, [pos:prindef, txt:nemo, lex:nemo,gender:masc,
                    case:nom, number:sing,person:3,sem:[hum],
                    function:[subject]]).
lex(neminem, prindef, [pos:prindef, txt:neminem, lex:nemo,gender:masc,
                    case:acc, number:sing,person:3,sem:[hum],
                    function:[subject,object]]).
lex(nullius, prindef, [pos:prindef, txt:nullius, lex:nemo,gender:masc,
                    case:gen, number:sing,person:3,sem:[hum],
                    function:[noun_cplt]]).
lex(nemini, prindef, [pos:prindef, txt:nemini, lex:nemo,gender:masc,
                    case:dat, number:sing,person:3,sem:[hum],
                    function:[i_object]]).
lex(nullo, prindef, [pos:prindef, txt:nullo, lex:nemo,gender:masc,
                    case:abl, number:sing,person:3,sem:[hum],
                    function:[prep_cplt]]).



% NIHIL
%

lex(nihil,prindef, [pos:prindef, txt:nihil, lex:nihil,gender:neuter,
                    case:or([nom,acc]), number:sing,person:3,sem:[thing,abstract],
                    function:[subject,object]]).

lex(nil,prindef, [pos:prindef, txt:nihil, lex:nihil,gender:neuter,
                    case:or([nom,acc]), number:sing,person:3,sem:[thing,abstract],
                    function:[subject,object]]).



% DUO
%

lex(duo, adj, [pos:adj, txt:duo, lex:duo,gender:or([masc,neuter]),type:tool,
                    case:or([nom,acc]), number:pl]).
lex(duae, adj, [pos:adj, txt:duae, lex:duo,gender:fem,type:tool,
                    case:nom, number:pl]).
lex(duos, adj, [pos:adj, txt:duos, lex:duo,gender:masc,type:tool,
                    case:acc, number:pl]).
lex(duas, adj, [pos:adj, txt:duas, lex:duo,gender:fem,type:tool,
                    case:acc, number:pl]).
lex(duorum, adj, [pos:adj, txt:duorum, lex:duo,gender:or([masc,neuter]),type:tool,
                    case:gen, number:pl]).
lex(duarum, adj, [pos:adj, txt:duarum, lex:duo,gender:fem,type:tool,
                    case:gen, number:pl]).
lex(duobus, adj, [pos:adj, txt:duobus, lex:duo,gender:or([masc,neuter]),type:tool,
                    case:or([dat,abl]), number:pl]).
lex(duabus, adj, [pos:adj, txt:duabus, lex:duo,gender:fem,type:tool,
                    case:or([dat,abl]), number:pl]).

% TRES
%

lex(tres, adj, [pos:adj, txt:tres, lex:tres,gender:or([masc,fem]),type:tool,
                    case:or([nom,acc]), number:pl]).
lex(tria, adj, [pos:adj, txt:tria, lex:tres,gender:neuter,type:tool,
                    case:or([nom,acc]), number:pl]).
lex(trium, adj, [pos:adj, txt:trium, lex:tres,gender:_,type:tool,
                    case:gen, number:pl]).
lex(tribus, adj, [pos:adj, txt:tribus, lex:tres,gender:_,type:tool,
                    case:or([dat,abl]), number:pl]).

% QUATTUOR etc.
%

lex(quattuor, adj, [pos:adj, txt:quattuor, lex:quattuor,gender:_,type:tool,
                    case:_, number:pl]).
lex(quinque, adj, [pos:adj, txt:quinque, lex:quinque,gender:_,type:tool,
                    case:_, number:pl]).
lex(sex, adj, [pos:adj, txt:sex, lex:sex,gender:_,type:tool,
                    case:_, number:pl]).
lex(septem, adj, [pos:adj, txt:septem, lex:septem,gender:_,type:tool,
                    case:_, number:pl]).
lex(octo, adj, [pos:adj, txt:octo, lex:octo,gender:_,type:tool,
                    case:_, number:pl]).
lex(nouem, adj, [pos:adj, txt:nouem, lex:nouem,gender:_,type:tool,
                    case:_, number:pl]).
lex(decem, adj, [pos:adj, txt:decem, lex:decem,gender:_,type:tool,
                    case:_, number:pl]).
lex(undecim, adj, [pos:adj, txt:undecim, lex:undecim,gender:_,type:tool,
                    case:_, number:pl]).
lex(duodecim, adj, [pos:adj, txt:duodecim, lex:duodecim,gender:_,type:tool,
                    case:_, number:pl]).
lex(tredecim, adj, [pos:adj, txt:tredecim, lex:tredecim,gender:_,type:tool,
                    case:_, number:pl]).
lex(centum, adj, [pos:adj, txt:centum, lex:centum,gender:_,type:tool,
                    case:_, number:pl]).
lex(mille, adj, [pos:adj, txt:mille, lex:mille,gender:_,type:tool,
                    case:_, number:pl]).





% QUIS QUID
%

% indefinite pronouns
%

lex(quis, prindef, [pos:prindef, txt:quis, lex:quis,gender:masc,
                    case:nom, number:sing,person:3,sem:[hum],
                    function:[subject]]).

lex(quid, prindef, [pos:prindef, txt:quid, lex:quis,gender:neuter,
                    case:or([nom,acc]), number:sing,person:3,sem:[thing, abstract],
                    function:[object,subject]]).
lex(aliquis, prindef, [pos:prindef, txt:aliquis, lex:aliquis,gender:masc,
                    case:nom, number:sing,person:3,sem:[hum],
                    function:[subject]]).

lex(aliquid, prindef, [pos:prindef, txt:aliquid, lex:aliquis,gender:neuter,
                    case:or([nom,acc]), number:sing,person:3,sem:[thing, abstract],
                    function:[object,subject]]).

% quis quid interrogative
%

lex(quis, print, [pos:print, txt:quis, lex:quis,gender:or([masc,fem]),
                    case:nom, number:sing,person:3,sem:[hum],
                    function:[subject]]).

lex(quid, print, [pos:print, txt:quid, lex:quis,gender:neuter,
                    case:or([nom,acc]), number:sing,person:3,sem:[thing, abstract],
                    function:[object,subject]]).






% OTHER INTERROGATIVE PRONOUNS
%

% acc int pros must also bear the 'subject' function on account of their use as subjects in nonfinite clauses :
                     % rogo quem putas epistulam scripsisse

lex(quem, print, [pos:print, txt:quem, lex:quis,gender:masc,
                    case:acc, number:sing,person:3,sem:[hum], 
                    function:[object,object_i,prep_cplt,subject]]).

% it should be kept in mind that object_i is like a direct object in that it requires an accusative
% and like an indirect object as far as its semantic role is concerned

lex(cuius, print, [pos:print, txt:cuius, lex:quis,gender:or([masc,fem,neuter]),
                    case:gen, number:sing,person:3,sem:_, function:[noun_cplt]]).
lex(cui, print, [pos:print, txt:cui, lex:quis,gender:or([masc,fem]),
                    case:dat, number:sing,person:3,sem:[hum], function:[i_object]]).
lex(quo, print, [pos:print, txt:quo, lex:quis,gender:or([masc,neuter]),
                    case:abl, number:sing,person:3,sem:_, function:[prep_cplt]]).

lex(quae, print, [pos:print, txt:quae, lex:quis,gender:neuter,
                    case:or([nom,acc]), number:pl,person:3,sem:[abstract, thing], 
                    function:[subject,object]]).

lex(quam,print, [pos:print, txt:quam, lex:quis,gender:fem,
                    case:acc, number:sing,person:3,sem:[hum], 
                    function:[object,object_i,prep_cplt,subject]]).
lex(qua, print, [pos:print, txt:qua, lex:quis,gender:fem,
                    case:abl, number:sing,person:3,sem:[hum], 
                    function:[prep_cplt]]).
lex(quos, print, [pos:print, txt:quos, lex:quis,gender:masc,
                    case:acc, number:pl,person:3,sem:[hum], 
                    function:[object,object_i,prep_cplt,subject]]).
lex(quorum, print, [pos:print, txt:quorum, lex:quis,gender:or([masc,neuter]),
                    case:gen, number:pl,person:3,sem:_, 
                    function:[noun_cplt]]).
lex(quibus, print, [pos:print, txt:quibus, lex:quis,gender:or([masc,fem,neuter]),
                    case:or([dat,abl]), number:pl,person:3,sem:_, 
                    function:[i_object, prep_cplt]]).
lex(quas, print, [pos:print, txt:quas, lex:quis,gender:fem,
                    case:acc, number:pl, person:3,sem:[hum],
                    function:[object,prep_cplt,subject]]).
lex(quarum, print, [pos:print, txt:quarum, lex:quis,gender:fem,
                    case:gen, number:pl,person:3,sem:[hum], 
                    function:[noun_cplt]]).




% INTERROGATIVE ADJECTIVES
%

lex(quis,  adj, [pos:adj, type:int, txt:quis, lex:quis,gender:masc,case:nom, number:sing]).
lex(qui,   adj, [pos:adj, type:int,txt:qui, lex:quis,gender:masc,case:nom, number:sing]).
lex(quae,  adj, [pos:adj, type:int,txt:quae, lex:quis,gender:fem,case:nom, number:sing]).
lex(quod,  adj, [pos:adj, type:int,txt:quod, lex:quis,gender:neuter,case:or([nom,acc]), number:sing]). 
lex(quem,  adj, [pos:adj, type:int,txt:quem, lex:quis,gender:masc,case:acc, number:sing]).
lex(cuius, adj, [pos:adj, type:int,txt:cuius, lex:quis,gender:or([masc,fem,neuter]),case:gen, number:sing]).
lex(cui,   adj, [pos:adj, type:int,txt:cui, lex:quis,gender:_,case:dat, number:sing]).
lex(quo,   adj, [pos:adj, type:int,txt:quo, lex:quis,gender:or([masc,neuter]),case:abl, number:sing]).
lex(quae,  adj, [pos:adj, type:int,txt:quae, lex:quis,gender:neuter,case:or([nom,acc]), number:pl]).
lex(quam,  adj, [pos:adj, type:int,txt:quam, lex:quis,gender:fem,case:acc, number:sing]).
lex(qua,   adj, [pos:adj, type:int,txt:qua, lex:quis,gender:fem,case:abl, number:sing]).
lex(quos,  adj, [pos:adj, type:int,txt:quos, lex:quis,gender:masc,case:acc, number:pl]).
lex(quorum,adj, [pos:adj, type:int,txt:quorum, lex:quis,gender:or([masc,neuter]),case:gen, number:pl]).
lex(quibus,adj, [pos:adj, type:int,txt:quibus, lex:quis,gender:or([masc,fem,neuter]),case:or([dat,abl]), number:pl]).
lex(quas,  adj, [pos:adj, type:int,txt:quas, lex:quis,gender:fem,case:acc, number:pl]).
lex(quarum,adj, [pos:adj, type:int,txt:quarum, lex:quis,gender:fem,case:gen, number:pl]).




% QUISQUAM
%

lex(quisquam, prindef, [pos:prindef, txt:quisquam, lex:quisquam,gender:masc,
                    case:nom, number:sing,person:3,sem:[hum],
                    function:[subject]]).
lex(quicquam, prindef, [pos:prindef, txt:quicquam, lex:quisquam,gender:neuter,
                    case:or([nom,acc]), number:sing,person:3,sem:[abstract, thing],
                    function:[subject, object]]).
lex(quemquam, prindef, [pos:prindef, txt:quemquam, lex:quisquam,gender:masc,
                    case:acc, number:sing,person:3,sem:[hum],
                    function:[subject,object]]).
lex(cuiusquam, prindef, [pos:prindef, txt:cuiusquam, lex:quisquam,gender:or([masc,neuter]),
                    case:gen, number:sing,person:3,sem:_,
                    function:[noun_cplt]]).
lex(cuiquam, prindef, [pos:prindef, txt:cuiquam, lex:quiquam,gender:or([masc,neuter]),
                    case:dat, number:sing,person:3,sem:_,
                    function:[i_object]]).
lex(nullo, prindef, [pos:prindef, txt:nullo, lex:nemo,gender:masc,
                    case:abl, number:sing,person:3,sem:[hum],
                    function:[prep_cplt]]).






%
% PREP
%

% ad+acc, in+abl, propter+acc, sub+abl, causa+gen (special!)
% the gerund flag records the property -or lack of- of gerund bearing
% ad urbem capiendam, in urbe capienda, urbis capiendae causa

% most preps, as their name indicates, come before the np they govern
% some are pre and post, such as causa, which can appear both before and after the np
% (of course, causa is not really a prep, but behaves as one)

lex(a,         prep,[lex:ab,      pos:prep, requires:abl,           gerund:no, type:pre]).
lex(ab,        prep,[lex:ab,      pos:prep, requires:abl,           gerund:no, type:pre]).
lex(abs,       prep,[lex:ab,      pos:prep, requires:abl,           gerund:no, type:pre]).
lex(ad,        prep,[lex:ad,      pos:prep, requires:acc,           gerund:yes,type:pre]).
lex(ante,      prep,[lex:ante,    pos:prep, requires:acc,           gerund:no, type:pre]).
lex(apud,      prep,[lex:apud,    pos:prep, requires:acc,           gerund:no, type:pre]).
lex(circum,      prep,[lex:circum,    pos:prep, requires:acc,           gerund:no, type:pre]).
lex(contra,    prep,[lex:contra,  pos:prep, requires:acc,           gerund:no, type:pre]).
lex(cum,       prep,[lex:cum,     pos:prep, requires:abl,           gerund:no, type:pre]).
lex(de,        prep,[lex:de,      pos:prep, requires:abl,           gerund:yes,type:pre]).
lex(e,         prep,[lex:ex,      pos:prep, requires:abl,           gerund:no, type:pre]).
lex(ex,        prep,[lex:ex,      pos:prep, requires:abl,           gerund:no, type:pre]).
lex(in,        prep,[lex:in,      pos:prep, requires:abl,           gerund:yes,type:pre]).
lex(in,        prep,[lex:in,      pos:prep, requires:acc,           gerund:no, type:pre]).
lex(inter,     prep,[lex:inter,   pos:prep, requires:acc,           gerund:no, type:pre]).
lex(intra,     prep,[lex:intra,   pos:prep, requires:acc,           gerund:no, type:pre]).
lex(ob,        prep,[lex:ob,      pos:prep, requires:acc,           gerund:no, type:pre]).
lex(per,       prep,[lex:per,     pos:prep, requires:acc,           gerund:no, type:pre]).
lex(post,       prep,[lex:post,     pos:prep, requires:acc,           gerund:no, type:pre]).
lex(praeter,   prep,[lex:praeter, pos:prep, requires:acc,           gerund:no, type:pre]).
lex(pro,       prep,[lex:pro,     pos:prep, requires:abl,           gerund:no, type:pre]).
lex(propter,   prep,[lex:propter, pos:prep, requires:acc,           gerund:no, type:pre]).
lex(sine,      prep,[lex:sine,    pos:prep, requires:abl,           gerund:no, type:pre]).
lex(sub,       prep,[lex:sub,     pos:prep, requires:or([abl,acc]), gerund:no, type:pre]).
lex(trans,     prep,[lex:trans,   pos:prep, requires:acc,           gerund:no, type:pre]).


% CAUSA

lex(causa,     prep,[lex:causa,   pos:prep,requires:gen,            gerund:yes,type:or([pre,post])]).


% INT and REL : ubi
%

% the rel value needs not be registered
% where needed in relative clauses, the lex feature is used

lex(ubi,adv,[lex:ubi,pos:adv,type:int,sem:position]).


% REL only : cum
%

lex(cum,adv,[lex:cum,pos:adv,type:rel,sem:when]).


% INT only : quo, unde, quando, quamdiu, quare, cur, quid, quomodo
%

lex(quo,         adv,[lex:quo,      pos:adv, type:int, sem:direction_to]).
lex(unde,        adv,[lex:unde,     pos:adv, type:int, sem:direction_from]).
lex(quando,      adv,[lex:quando,   pos:adv, type:int, sem:when]).
lex(quamdiu,     adv,[lex:quamdiu,  pos:adv, type:int, sem:duration]).
lex(cur,         adv,[lex:cur,      pos:adv, type:int, sem:reason]).
lex(quare,       adv,[lex:quare,    pos:adv, type:int, sem:reason]).
lex(quid,        adv,[lex:quid,     pos:adv, type:int, sem:reason]).
lex(quomodo,     adv,[lex:quomodo,  pos:adv, type:int, sem:manner_means]).



% STANDARD ADVERBS
%

lex(autem,    adv,[lex:autem,      pos:adv,type:clausal, sem:discourse]).
lex(bene,     adv,[lex:bene,       pos:adv,type:vpbound, sem:eval]).
lex(dein,     adv,[lex:dein,       pos:adv,type:vpbound, sem:time]).
lex(deinde,   adv,[lex:deinde,     pos:adv,type:vpbound, sem:time]).
lex(enim,     adv,[lex:enim,       pos:adv,type:clausal, sem:discourse]).
lex(ergo,     adv,[lex:ergo,       pos:adv,type:clausal, sem:discourse]).
lex(etiam,    adv,[lex:etiam,      pos:adv,type:clausal, sem:discourse]).
lex(forte,    adv,[lex:forte,      pos:adv,type:vpbound, sem:discourse]).
lex(grauiter, adv,[lex:grauiter,   pos:adv,type:vpbound,  sem:eval]).
lex(hic,      adv,[lex:hic,        pos:adv,type:vpbound, sem:place]).
lex(iam,      adv,[lex:iam,        pos:adv,type:vpbound, sem:time]).
lex(igitur,      adv,[lex:igitur,        pos:adv,type:clausal, sem:discourse]).
lex(ita,      adv,[lex:ita,        pos:adv,type:clausal, sem:discourse]).
lex(magis,      adv,[lex:magis,        pos:adv,type:clausal, sem:discourse]).
lex(modo,     adv,[lex:modo,       pos:adv,type:vpbound, sem:time]).
lex(mox,      adv,[lex:mox,        pos:adv,type:vpbound, sem:time]).
lex(nam,      adv,[lex:nam,        pos:adv,type:clausal, sem:discourse]).
lex(namque,   adv,[lex:namque,     pos:adv,type:clausal, sem:discourse]).
lex(nimium,     adv,[lex:nimium,       pos:adv,type:adjbound, sem:eval]).
lex(nunc,     adv,[lex:nunc,       pos:adv,type:vpbound, sem:time]).
lex(plerumque,adv,[lex:plerumque,  pos:adv,type:vpbound, sem:time]).
lex(profecto, adv,[lex:profecto,   pos:adv,type:clausal, sem:discourse]).
lex(quidem,   adv,[lex:quidem,     pos:adv,type:clausal, sem:discourse]).
lex(quoque,   adv,[lex:quoque,     pos:adv,type:clausal, sem:eval]).
lex(saepe,    adv,[lex:saepe,      pos:adv,type:vpbound, sem:time]).
lex(semel,    adv,[lex:semel,      pos:adv,type:vpbound, sem:time]).
lex(semper,   adv,[lex:semper,     pos:adv,type:vpbound, sem:time]).
lex(sic,      adv,[lex:sic,        pos:adv,type:clausal, sem:discourse]).
lex(tam,   adv,[lex:tam,     pos:adv,type:clausal, sem:discourse]).
lex(tamen,    adv,[lex:tamen,      pos:adv,type:clausal, sem:discourse]).
lex(tum,      adv,[lex:tum,        pos:adv,type:vpbound, sem:time]).
lex(tunc,     adv,[lex:tunc,       pos:adv,type:vpbound, sem:time]).

% lex(et,adv,[lex:et,pos:adv,type:vpbound,sem:discourse]).
  % timeo Danaos et dona ferentes
% too expensive !!!! ET conjunction is a nightmare on its own, no need to add adverb ET !!


%
% COORDINATORS
%

lex(que,   coord,   [lex:que,   pos:coord]).
lex(uel,   coord,   [lex:uel,   pos:coord]).
lex(et,    coord,   [lex:et,    pos:coord]).
lex(aut,   coord,   [lex:aut,   pos:coord]).
lex(sed,   coord,   [lex:sed,   pos:coord]).
lex(atque, coord,   [lex:atque, pos:coord]).
lex(ac,    coord,   [lex:ac,    pos:coord]).
lex(at,    coord,   [lex:at,    pos:coord]).
lex(an,    coord,   [lex:an,    pos:coord]).            % de facto restricted to interrogative clauses
lex(quam,  coord,   [lex:quam,  pos:coord,type:correlative]).                 % QUAM is an awful rompecabezas for your decent parser 

%
% SUBORDINATORS
%

lex(antequam,   sub,[lex:antequam,  pos:sub, argbound:no,   mood:_,           value:time]).
lex(priusquam,   sub,[lex:priusquam,  pos:sub, argbound:no,   mood:_,           value:time]).

lex(postquam,   sub,[lex:postquam,  pos:sub, argbound:no,   mood:indicative,           value:time]).
lex(posquam,   sub,[lex:postquam,  pos:sub, argbound:no,   mood:indicative,           value:time]).
lex(posteaquam,   sub,[lex:postquam,  pos:sub, argbound:no,   mood:indicative,           value:time]).

lex(si,        sub,[lex:si,       pos:sub, argbound:no,   mood:_]).
lex(nisi,      sub,[lex:nisi,     pos:sub, argbound:no,   mood:_]).
lex(ni,        sub,[lex:nisi,     pos:sub, argbound:no,   mood:_]).

lex(cum,       sub,[lex:cum,      pos:sub, argbound:no,   mood:_]).
lex(dum,       sub,[lex:dum,      pos:sub, argbound:no,   mood:_]).

lex(ut,        sub,[lex:ut,       pos:sub, argbound:no,   mood:subjunctive, value:or([purpose,consequence])]).
lex(uti,        sub,[lex:uti,       pos:sub, argbound:no,   mood:subjunctive, value:or([purpose,consequence])]).
lex(ne,        sub,[lex:ne,       pos:sub, argbound:no,   mood:subjunctive, value:purpose]).

lex(quasi,   sub,[lex:quasi,  pos:sub, argbound:no,   mood:subjunctive,           value:reason]).
lex(quoniam,   sub,[lex:quoniam,  pos:sub, argbound:no,   mood:_,           value:reason]).
lex(quia,      sub,[lex:quia,     pos:sub, argbound:no,   mood:_,           value:reason]).
lex(quod,      sub,[lex:quod,     pos:sub, argbound:no,   mood:_,           value:reason]).

lex(tamquam,   sub,[lex:tamquam,  pos:sub, argbound:no,   mood:subjunctive,           value:reason]).
lex(tanquam,   sub,[lex:tamquam,  pos:sub, argbound:no,   mood:subjunctive,           value:reason]).

lex(quia,      sub,[lex:quia,     pos:sub, argbound:yes,   mood:_]).
lex(quod,      sub,[lex:quod,     pos:sub, argbound:yes,   mood:_]).
lex(quoniam,   sub,[lex:quoniam,  pos:sub, argbound:yes,   mood:_]).

lex(quin,      sub,[lex:quin,     pos:sub, argbound:yes,   mood:subjunctive]).

lex(quamuis,   sub,[lex:quamuis,  pos:sub, argbound:no,   mood:subjunctive, value:concession]).
lex(quamquam,  sub,[lex:quamquam, pos:sub, argbound:no,   mood:indicative,  value:concession]).
lex(etsi,      sub,[lex:etsi,     pos:sub, argbound:no,   mood:indicative,  value:concession]).

lex(ut,        sub,[lex:ut,       pos:sub, argbound:yes,  mood:subjunctive]).
lex(ne,        sub,[lex:ne,       pos:sub, argbound:yes,  mood:subjunctive]).


%
% NEGATION
%

lex(non, neg, [lex:non, pos:neg, type:decl]).
lex(haud, neg, [lex:haud, pos:neg, type:decl]).
lex(ne,  neg, [lex:ne,  pos:neg, type:imp]).

%
% PARTICLES
%

lex(ne_int, part,   [lex:ne_int,  type:int, value:open_orientation,     clausetype:_]).
lex(num,    part,   [lex:num,     type:int, value:open_orientation,     clausetype:sub]). % in indirect questions
lex(num,    part,   [lex:num,     type:int, value:negative_orientation, clausetype:main]). % in direct questions
lex(nonne,  part,   [lex:num,     type:int, value:positive_orientation, clausetype:_]). % in direct questions

lex(utinam, part,   [lex:utinam]).

%
% PUNCTUATION
%

lex(',',punct,[lex:comma]).
lex(':',punct,[lex:colon]).
lex('?',punct,[lex:question_mark]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






% FEATURE LIST UNIFICATION
%

%
% feature unification in verification mode : CONSTRAINT
%

% adapted from GAL et al. "Prolog for NLP" Appendix 12 p.244-245

/* note that 'constraint' should not be called
   with a feature whose ***name*** is uninstantiated:

  !!!! constraint([Featurename:Featurevalue,sem:[thing]],
       [sem:[hum], gender:masc,Cat:N])

will succeed, instantiating Featurename to sem, and Featurevalue to [hum];
sem:[thing] will match Cat:N, and the whole predicate call will succeed,
in spite of the incompatibility between the two sem features */

% constraint : 2 args
%

constraint(F,F1) :- nonvar(F1),F=F1,!.
% identity of the two sets to be unified

constraint([],_) :-!.
% first set is empty - end of recursion clause

constraint([F:V|Tail1],[F:V|Second]) :-
    !, constraint(Tail1,Second).

% identical feature-value pair
% we still have to constraint the tails


constraint([sem:Set1|Tail1],[sem:Set2|Second]) :-
is_list(Set1),
is_list(Set2),
sset(Set1,Set2),
constraint(Tail1,Second),!.

% semantic features : the values are instantiated lists;
% the first must be a subset of the second
% the first arg should house the restrictions
% the second the sem values of the object meant to satisfy these restrictions

constraint([F:or(Set1)|Tail1],[F:or(Set2)|Second]) :-
is_list(Set1),
is_list(Set2),
checkinter(Set1,Set2),
constraint(Tail1,Second),!.

% OR-lists : the values are instantiated OR-lists;
% they must have a non-empty intersection

constraint([F:or(Set)|Tail1],[F:V|Second]) :-
is_list(Set),
atomic(V),
member(V,Set),
constraint(Tail1,Second),!.

constraint([F:V|Tail1],[F:or(Set)|Second]) :-
is_list(Set),
atomic(V),
member(V,Set),
constraint(Tail1,Second),!.

% OR-lists against atomic value : the atomic value is a member of the OR-list

% BEGIN EXCEPT BLOCK
%

constraint([F:except(Set)|Tail1],[F:V|Second]) :-
is_list(Set),
atomic(V),
\+ member(V,Set),
constraint(Tail1,Second),!.

constraint([F:V|Tail1],[F:except(Set)|Second]) :-
atomic(V),
is_list(Set),
\+ member(V,Set),
constraint(Tail1,Second),!.

constraint([F:or(Or_Set)|Tail1],[F:except(Except_Set)|Second]) :-
is_list(Or_Set),
is_list(Except_Set),
member(M,Or_Set),
\+ member(M,Except_Set),
constraint(Tail1,Second),!.

constraint([F:except(Except_Set)|Tail1],[F:or(Or_Set)|Second]) :-
is_list(Or_Set),
is_list(Except_Set),
member(M,Or_Set),
\+ member(M,Except_Set),
constraint(Tail1,Second),!.

% END EXCEPT BLOCK
%

constraint([F:Set1|Tail1],[F:Set2|Second]) :-
not(atomic(Set1)),
not(atomic(Set2)),
constraint(Set1,Set2),
constraint(Tail1,Second),!.

% set-valued features : we unify the values with constraint

constraint([F:V|_],[F:V2|_]) :-
   V\=V2,                        % test  not necessary because of all the cuts here above but included for the sake of logic
   !,
   fail.

% we have the same feature name, but a different value: we have to fail...


constraint([F1:V1|Tail1],[F2:V2|Tail2]) :-
   constraint([F1:V1],Tail2),
   constraint(Tail1,[F2:V2|Tail2]),!.

%
% in construction mode : FUNIFY : 3 args
%

% FUNIFY IS ARITY-3 CONSTRAINT !!!

% in construction mode we are building in arg3 the result of joining the conditions set by arg1 and arg2
% construction mode is used only in very specific cases, where we must wait before we can check the conditions set
% in the first two args
% whenever possible, we use constraint with arity 2

funify(F,F,F) :- !.

% identity of the two sets to be unified

funify(X,[],X) :-!.
funify([],X,X) :-!.

% either set is empty - end of recursion clause

funify([F:V|Tail1],[F:V|Second],[F:V|NewTail]) :-
    !, funify(Tail1,Second,NewTail).

% identical feature-value pair is just copied over
% we still have to funify the tails

funify([sem:Set1|Tail1],[sem:Set2|Second],[sem:ResF|NewTail]) :-
is_list(Set1),
is_list(Set2),
union(Set1,Set2,ResF),
funify(Tail1,Second,NewTail),!.

% semantic features: we return the union of the two lists

funify([F:or(Set1)|Tail1],[F:or(Set2)|Second],[F:ResF|NewTail]) :-
is_list(Set1),
is_list(Set2),
intersec(Set1,Set2,ResF),
funify(Tail1,Second,NewTail),!.

% OR-lists: we return the intersection of the two lists

funify([F:or(Set)|Tail1],[F:V|Second],[F:V|NewTail]) :-
is_list(Set),
atomic(V),
member(V,Set),
funify(Tail1,Second,NewTail),!.

funify([F:V|Tail1],[F:or(Set)|Second],[F:V|NewTail]) :-
is_list(Set),
atomic(V),
member(V,Set),
funify(Tail1,Second,NewTail),!.

% OR-lists and atomic value: we return the atomic value

% BEGIN EXCEPT CASES
%

% OR-SET // EXCEPT-SET
%---------------------

funify([F:or(Or_Set)|Tail1],[F:except(Except_Set)|Second],[F:or(NSet)|NewTail]) :-
is_list(Or_Set),
is_list(Except_Set),
exclude(Except_Set,Or_Set,NSet),
NSet \= [],
funify(Tail1,Second,NewTail),!.

funify([F:except(Except_Set)|Tail1],[F:or(Or_Set)|Second],[F:or(NSet)|NewTail]) :-
is_list(Or_Set),
is_list(Except_Set),
exclude(Except_Set,Or_Set,NSet),
NSet \= [],
funify(Tail1,Second,NewTail),!.

% ATOM // EXCEPT-SET
%--------------------

funify([F:V|Tail1],[F:except(Except_Set)|Second],[F:V|NewTail]) :-
atomic(V),
is_list(Except_Set),
\+ member(V,Except_Set),
funify(Tail1,Second,NewTail),!.

funify([F:except(Except_Set)|Tail1],[F:V|Second],[F:V|NewTail]) :-
atomic(V),
is_list(Except_Set),
\+ member(V,Except_Set),
funify(Tail1,Second,NewTail),!.

% EXCEPT-SET // EXCEPT-SET
%--------------------------

funify([F:except(Except_Set1)|Tail1],[F:except(Except_Set2)|Second],[F:except(UnionSet)|NewTail]) :-
is_list(Except_Set1),
is_list(Except_Set2),
union(Except_Set1,Except_Set2,UnionSet),
funify(Tail1,Second,NewTail),!.

% END EXCEPT CASES
%

funify([F:Set1|Tail1],[F:Set2|Second],[F:ResF|NewTail]) :-
not(atomic(Set1)),
not(atomic(Set2)),
funify(Set1,Set2,ResF),
funify(Tail1,Second,NewTail),!.

% set-valued features : we unify the values with funify

funify([F:V|_],[F:V2|_],_) :-
   V \=V2,
   !,
   fail.

% we have the same feature name, but a different value: we have to fail...

funify([F1:V1|Tail1],[F2:V2|Tail2],Result) :-
   funify([F1:V1],Tail2,NewTail2),
   funify(Tail1,[F2:V2|NewTail2],Result),!.

% different features:
% we funify the first with the remainder of the second feature set
% and then we funify the remainder of the first set
% with the result of the first unification
% tacked on to the second feature

%

exclude([H|T],V,T1) :- \+ member(H,V),!, exclude(T,V,T1).
exclude([H|T],V,T1) :- pick(H,V,V1), exclude(T,V1,T1).
exclude([],X,X).

% exclude eliminates a certain set from a Set
% case of : funify([one:except([a,b]), two:b],[one:or([b,c,a,d]), two:b],S).
% S = [one:or([c,d]), two:b]

%
% pick : non-deterministic selection in a list
%

% often called 'select'

pick(A,[A|B],B).
pick(A,[B|C],[B|D]) :- pick(A,C,D).

%
% sets : subset and union
%

% for semantic features
% we extend set membership to include inheritance

% the left semantic set expresses the restriction,
% the right semantic set the values the restriction is matched against

sset([First|Others],Set) :- sfok(First,Set), sset(Others,Set).
sset([],_).

sfok(Sem,Semlist) :- inlist(Sem,Semlist),!.

sfok(Sem,Semlist) :- subclass(Hypo,Sem), inlist(Hypo,Semlist),!.

% we have sth more specific than what we are looking for
% we are looking for an animal and we've got a poodle
% recall that the left arg houses sem restrictions
% the right arg the sem potential of the object meant to satisfy
% these restrictions

inlist(Sem,[Sem|_]) :- !.
inlist(Sem,[_|Remainder]) :- inlist(Sem,Remainder).

subclass(X,Y) :- ako(X,Y).
subclass(X,Y) :- ako(X,Z) , subclass(Z,Y).

% EXAMPLE:

ako(city,loc).
ako(abstract,thing). % this is wrong but helpful
                     % it will get fixed once we have developped an adequate taxonomy
                     % here the taxonomy is inexistent and pure toy

% checkinter(+L1,+L2)
% checks that the intersection of the two sets is not empty

checkinter(L1,L2) :- member(Member,L1),member(Member,L2).

% intersec(tion)(+L1,+L2,-Inter)
% returns the intersection of two sets
% fails if the intersection is empty

intersec(L1,L2,Inter) :-
    setof(Member,(member(Member,L1),member(Member,L2)),Inter).

% set union : PREDEFINED in SWI Prolog

% union(+L1,+L2,-Union)
% union(L1,L2,Union) :-
%        setof(Member,(member(Member,L1);member(Member,L2)),Union).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

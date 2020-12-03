
% ALP, A Latin Parser	v 101
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% more specifically :
% A data-driven feature-unification parser for a micro-subset of classical Latin
%

% Latest Update : November 2020
% © Archibald Michiels
% amichiels@uliege.be


% Directions for use
%%%%%%%%%%%%%%%%%%%%

% To start the program running type "go." when it has finished loading.
% Do not type in the quotes but make sure you do type in the dot (: go.)
% The Latin sentences can come from a file or from stdin, the standard input, i.e. the keyboard
% Type "stdin." or the name of a file, followed by a dot, e.g. "testalp." (do not type in the quotes: stdin. or filename. (e.g. testalp.)
% The files must end on a line that is made up of the single word 'stop' followed by a dot, i.e. "stop.", without the quotes (stop.)


:- style_check(-singleton).
:- style_check(-discontiguous).

:- [vocfile]. % vocfile is produced by makelex.pl
              % the vocfile should be in the same dir as alp itself


%%%%%%%%%%%%%%%
% DECLARATIONS
%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes). % only necessary if home-made 'getsentence' is used (equivalent of readln)
				          % 'getsentence' IS made use of in the present version !!!!!!!!!!!!!!!!!!!


% the 'lex' predicate is fed by macro-expansion clauses executed by makelex
% and must therefore be declared dynamic

% operator  declaration:

:- op(800,xfx,--->).

% used in rules : rule_name ---> rule_body (=list of 'actions' to be taken)

% empty check point for quick debugging; call : spy(checkpoint)
% after inserting a call or several calls to 'checkpoint(1)'... 'checkpoint(n)' wherever appropriate
% Then start stepping through the goals

checkpoint(_).

% better readability in the programmer's eye ;-)

ifthen(Condition,Goal) :- Condition -> Goal ; true.
ifthenelse(Condition, ThenGoal, ElseGoal) :- Condition -> ThenGoal ; ElseGoal.


%%%%%%%%%%
% GO STEP
%%%%%%%%%%

go :- nl,

     protocola(alp),               % protocol file stored in the current directory
     set_default,
     write('ALP, a data-driven feature-unification parser for a micro-subset of classical Latin' ),nl,
     write('-----------------------------------------------------------------------------------'),
     nl,nl,
     write('A.Michiels, amichiels@uliege.be'),nl,nl,
     nl,nl,
     recorded(stg,[current,Current],_),                           % settings concern depth of analysis, nber of parses, structures displayed
								  % and text type
        write('Current settings are: '),
        write(Current),
        write('. Modify ? (y/n) '),
        flush_output,
        get_single_char(X),
        ifthenelse((X is 89;X is 121),
                    customize,(nl,nl)),

     write('Input file?  [stdin. or file_name.] --> '),
     read(Input),
     dealwith(Input,HandleIn),
     write('Output file? [file_name.] --> '),
     read(Output),
     concat(Output,'.lst',Outlist),



     !,

     start(Outlist,HandleIn,Input).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET_DEFAULT: activates default settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_default :-
          eraseall(stg),
         recorda(stg,[current,'DEFAULT'],_),

         recorda(stg,[type_of_structures,1]),  % parses are displayed for full sentences only
         recorda(stg,[depth,1]),               % shallow
         recorda(stg,[number_of_parses,1]),    % one parse, and a second one if as good as the first
         recorda(stg,[texttype,1]).            % prose


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CUSTOMIZE : user's choice of settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

customize :-
        nl,
        nl,
        flush_output,
        recorded(stg,[current,Current],Key),
        write(' Customizing settings (current is '),
        write(Current),
        write('): choose one the following items...'),
        nl,
        nl,
        write(' 0. - Depth Level for Finite Passes'), nl,
        write(' 1. - Type of Structures Displayed'), nl,
        write(' 2. - Number of Parses Displayed'),nl,
        write(' 3. - Text Type'),nl,
        write(' 4. - Back to default'), nl,nl,
        write(' Enter quit. to quit'), nl, nl,
        flush_output,

        read(X),
        custom(X),
        ifthenelse( X\=quit,
                    (ifthen(recorded(stg,[current,'DEFAULT'],Key), erase(Key)),
                    recorda(stg,[current,'CUSTOM']),
                    customize),
                    true).

custom(quit) :- nl.
custom(quit) :- nl.

custom(0) :-
        write('Customizing Depth Level for Finite Pass '), nl,
        write('Enter 1. for Shallow, 2. for Intermediate, 3. for Deep: '),
        getvalue(depth).


custom(1) :-
        write('Customizing Type of Parses Displayed '), nl,
        write('Enter 1. for Whole Sentences Only, 2. for Phrases and Clauses: '),
        getvalue(type_of_structures).


custom(2) :-
        write('Customizing Number of Parses Displayed '), nl,
        write('Enter 1. for one parse only, or two if the second parse has the same ranking as the first '), nl,
        write('Enter 2. for two parses, whatever the ranking of the second '), nl,
        write('Enter 3. for all available parses: '),nl,
        getvalue(number_of_parses).


custom(3) :-
        write('Customizing Type of Text '), nl,
        write('Enter 1. for Prose, 2. for Poetry: '),
        getvalue(texttype).


custom(4) :-
        set_default.


getvalue(X) :-
        read(Value),
        recorded(stg,[X,_],Key),
        erase(Key),
        recorda(stg,[X,Value]).

writesettings(Hout) :-
        recorded(stg,X,_),
        write(Hout,X), tab(Hout,1),
        fail.

writesettings(Hout) :-
        nl(Hout),
        nl(Hout).







% Input is from stdin(user's terminal) or from file
%

   dealwith(stdin,_):- !. % nothing to be done, no file to be opened -
                          % standard input (from the user's terminal)

   dealwith(FileIn,HandleIn):-
                         open(FileIn,read,HandleIn).
                                        % open(File_Name,Mode,Stream)


   start(Outlist,_,stdin) :-
                 open(Outlist,write,Lists),
                  write(Lists,'Settings: '),
                  writesettings(Lists),
		  recorda(out,Lists,_),
                 recorda(alltime,0,_),
                 repeat,
                 clear,				% clearing the data base
                 statistics(cputime,TB),
                 recorda(time,TB),              % record starting time
                 nl,
                 nl,
                 write('Key in your sentence or stop. to quit'),
                 nl,
                 getsentence(Sentence,     % list of words corresponding to string
                      user,         % where the string is to be found
                      ".?!",         % sentence delimiters : final dot / question mark / exclamation mark
                      ",:",         % word delimiters : comma / colon
                      ",:?",         % returned punctuation signs : comma / colon / question mark
                      nocaps,       % decapitalize or leave alone (nocaps - caps)
                      _),           % end-of-file capture
                                    % getsentence is also responsible for the v/u distinction to be dropped
                                    % it turns all v's into u's (graviter -> grauiter, etc.)
                 eraseall(pos),     % clearing database
                 eraseall(fin),
                 deque(Sentence, Clean),   % getting rid of enclitic -que (and other minor text manipulations)
                 process(Clean,Lists).

   start(Outlist,HandleIn,Input) :-
                 Input \= stdin,
                 open(Outlist,write,Lists),
                 % open(File_Name,Mode,Stream)
                 write(Lists,'Settings: '),
                  writesettings(Lists),
                 recorda(out,Lists,_),
                 nl,
                 recorda(alltime,0,_),
                 repeat,
                 clear,
                 statistics(cputime,TB),
                 recorda(time,TB),
                 nl,
                 getsentence(Sentence,HandleIn,
                               ".?!",
                               ",:",
                               ",:",
                               nocaps,
                               _),				  % see above
		 Sentence\=['%'|_],				  % comment line
						                  % these lines silently fail
								  % this failure sends us back to the beginning of
								  % the repeat-fail loop
                 write(Sentence),nl,
                 eraseall(pos),
                 eraseall(fin),
                 deque(Sentence, Clean),
                 process(Clean,Lists).









% start the whole thing
%%%%%%%%%%%%%%%%%%%%%%%%

process(Sentence,Stream) :- positions(Sentence,0,L),
                            nl,nl(Stream),write(L),writeq(Stream,L),nl(Stream),nl,
                            runs.

/* 'positions' computes word positions starting from zero;
the sentence with the word positions fed in (the L list) is then
printed on screen and in the results file

'runs' calls a quartet of run(Pass), where Pass is " stop ",
then " lex "(icon), then (passes making up) " gr "(ammar), then " output "

Each run is supposed to be independent - we do not come back to it (except at the very end of the parsing process); so when it has done its job
we have to make sure that a run *** never *** requires information that one of the next runs provides

'run(Pass)' recursively calls itself, going through all
the 'rule_name ---> rule_body' pairs for that pass

when no new info is found, it fails,
because all the clauses for '--->' have a condition
(a 'not recorded' check included in the definition of
the predicate 'map') which will lead to failure

before failing, the last pass, run(output), gets a chance to retry everything and then print
(once each) one or more parses spanning the whole S

the failing of 'runs' causes backtracking to the repeat goal in 'start';
the db is cleared and the process relaunched with a new S from the user

if the S happens to be 'stop.' the process halts by abortion (abort). */




% computing string positions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% each word in the list is assigned a starting and an ending position
% not that the first word spans from position 0 to position 1

positions([Head|Tail],Pos,Listofwords) :-
     ifthen((Head=se;Head=sibi;Head=sui), recorda(flags,active(se)) ),  % trying to reduce time spent on reflexive pronoun binding
									% use recorda rather than map because there may be more than one SE in the string


     phrase_words(Phrase_words),   % list of phrasewords, i.e. words which belong only to phrases such as 'pili'
                                   % this list is to be carefully updated whenever a new phrase is entered

     ifthenelse( ((lex(Head,_,_), NewHead=Head);
				% the word is known and is to be entered as such

                  (member(Head,Phrase_words), NewHead=Head) ;
                            % OR the word is a member of the list of phrase words

                   (atom_concat(First1,ne,Head),  % OR we have 'ne' attached to a word registered as such e.g. 'romamne'
                    lex(First1,_,_), First1\=non,  % we keep particle 'nonne' as a single lex
                    NewHead=First1,Part=yes); % we keep track of the fact that there WAS a particle attached


                  % amauisti > amasti   /   deleueram > deleram  /  audiueram > audieram
                  % the contracted form gets divided, ui/ue/u are restored, and we check that the proposed expanded variant is
                  % a licit variant, i.e. generated by a 'makelex' macro-clause as the first arg of a lex clause

                  % e.g. amasti : a u masti  NO 	a ui masti NO 	a ue masti NO
                  %               am u asti  NO 	am ui asti NO	am ue asti NO
                  %               ama u sti  NO		ama ui sti YES > amauisti 
                  % lex(amauisti, v, [pos:v, class:tr_cod, type:finite, lex:amare, voice:act, txt:amauisti, tense:perfect, kind:_, 
                  %      mood:indicative, number:sing, person:2]).			 
                   
                  (atom_concat(First,Second,Head),            % only Head is known - atom divisions are put forward by atom_concat
                      (atom_concat(First,u,FirstExpand) ;
                       atom_concat(First,ui,FirstExpand);
                       atom_concat(First,ue,FirstExpand) ),  % contracted pft
                   atom_concat(FirstExpand,Second,Known),
                   lex(Known,v,_),                          % the full form is registered as a verb form
                   NewHead=Known);
                            % OR the word is a contracted form of a verb; we register the non-contracted form







                            % OR the word bears a suffix with established meaning and function:
                            % we register the suffix in a suffix box which keeps track of the position of the word
                            % to which the suffix is attached (Pos,Posand1)
                            % the suffix must be attached to a registered word - otherwise we let it go
                            % the suffixes concerned are:
                            % cumque / nam / piam / dam / uis / libet / quisque / que  ?????????
                            % code to be added here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







                   (Head=stop,NewHead=Head)
                             % OR the word is the word Stop
                ),   % IF
                       % we register only known words (lex items or phrase members) or the stop. command

                               (Posand1 is Pos + 1,                                 % THEN
                                ifthenelse(var(Part),		% no particle detected (no attached 'ne')

                                             (recorda(pos,position(Pos,Posand1,NewHead),_),
                                             Next=Posand1,Listofwords=[Pos/NewHead|Tails]),  % THEN only one word added to the word list

                                            (Next is Posand1 +1,			% OR ELSE
                                            recorda(pos,position(Pos,Posand1,ne_int),_),    % record the particle
                                            recorda(pos,position(Posand1,Next,NewHead),_), % record the word
                                            Listofwords=[Pos/ne_int,Posand1/NewHead|Tails]     % add both particle and word to the word list
                                            )),

                                            % we record the word, expanded if necessary


                                positions(Tail,Next,Tails)),                             % we start from Next, which is either Posand1 or
											 % succ(Posand1)

                                (positions(Tail,Pos,Tails),recorda(unmapped,Head,_), nl,write('Unknown word: '), write(Head),nl)
				% ELSE word is unmapped and is recorded as such in the 'unmapped' box
                          ).


    % position of the last word indicates string end

     positions([],X,[endpos(X)]) :-
                 recorda(fin,fin(X),_).  % the 'fin' box records the end positition of the string,
					% which will be used by the 'path' procedure to ensure
					% that the proposed parse covers the whole string









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta-interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Stopping...
%
runs :- run(stop).

% vocabulary
%
runs :- run(lex).     % words and fixed phrases (in ius, in dubium)

% GRAMMAR RUNS
%

% adjective phrases and verbs

runs :- run(adv).
runs :- run(adj).
runs :- run(verb).

% nps

runs :- run(core).  % core nps
                    % full nps, i.e. with args or genitives attached or nps with relative clauses
                    % must be dealt with later, in the 'finite' run
                    % the information is simply not available yet...


% clauses and complex nps and pps (which can hold nps with finite rel clauses...)
% sentence level : nonfinite and  finite
% most of the grammatical parsing occurs at this level

runs :- ifthen( recorded(stg,[depth,1]),
                                        run(finite,shallow)),
        ifthen( recorded(stg,[depth,2]),
                                        run(finite,inbetween)),
        ifthen( recorded(stg,[depth,3]),
					 run(finite,deep)).



% Outputting results

runs :- run(output).



run(Pass) :- Pass \= output, Pass \= finite, 
                            % THIS CHECK TO BE REMOVED BEFORE USING THE DEBUGGING OUTPUT PROCEDURES !!!
             [Pass,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(Pass).

run(finite,deep) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite,deep).


% the finite runs are limited in nber - as few as possible !!!


run(finite,shallow) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             runstop(finite2).

runstop(finite2) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             fail.



run(finite,inbetween) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite2).

run(finite2) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite3).


run(finite3) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite4).

run(finite4) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite5).

run(finite5) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             fail.


% OUTPUT %
%

run(output) :- [_,_] ---> Condition_Action,        % one last pass through everything !!!!!!!!!!!!!!!!!!!!!
             testexec(Condition_Action),
             fail.           % FAIL GOAL TO BE REMOVED BEFORE USING THE DEBUGGING OUTPUT PROCEDURES !!!


% the output needs to be executed only once, and then fail to allow
% the processing of the following S

% the [Pass,Rule_name] part does not give rise to any action ;
% it is there only for guiding the process (through Pass)
% and for documentation and debugging purposes

testexec([]).
testexec([First|Rest]) :-
                       call(First),
                       testexec(Rest).

% testexec is simply a list traversal with execution
% of each element as a Prolog goal



%%%%%%%%%%
% LEXICON
%%%%%%%%%%

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

% The entries below feed the lex predicate.

% The word form (e.g. ambularent) is first arg within the lex structure to facilitate indexing.
% The pos (Part of Speech - v etc.) is arg for the lex predicate
% (and will be the name of the record box where the info
% will be stored when the form is found in the text to be parsed).

% The list of features is copied over from lex to the Box.

% Example:

% the lexical entry:
% verb([v(ambulare,1,ambul,ambulau,ambulat)],intr,std).

% will, thanks to the relevant macro clause, generate forms such as:

/*
lex(ambularent,    % word form
      v,             % box
      [pos:v, class:intr, type:finite, lex:ambulare, voice:act, txt:ambularent,
       tense:imperfect, kind:std, mood:subjunctive, number:pl, person:3]).      % list of features
*/

% The info here will be associated with a wordform found in text (such as 'ambularent' as third word in the string)
% by the lexical pass:

/*
[lex,words] --->[recorded(pos,position(A,B,Word),_), % we have the word form Word in the string, e.g. ambularent
                 lex(Word,Box,FS),                   % we have a lex for the Word (ambularent) to put in the relevant Box (the v Box)
                 map(Box,[from:A,to:B|FS])].       % we store its position there accompanied by the feature list FS (third arg of lex)
						     % the feature list will have features for both word variant and lemma:
						     %	[lex:ambulare, txt:ambularent] is part of the FS
*/




%%%%%%%%
% VERBS
%%%%%%%%

/*
The lexarg structure associates the arg pattern of the verb with its lemma form.
A verb may include a number of different arglists, sometimes corresponding to different
readings of the verb - it all depends on the semantic granularity we want to achieve.
There is in the arglist a slot ('clause:List') reserved for storing constraints set on the clause
at the level of the whole clause, for instance polarity requirements set by multi-word units
(haud magni facere / non pili facere, etc).

Each arg is declared obligatory or optional ; only optional args can be skipped by the 'match' procedure
applied to each arg, whose task is to work out which bit of the input string can be parsed as a textual embodiment
of the argument.

The type of the arg indicates the nature of the string we are looking for to fill the arg position (np etc.).

The 'constraints' can store any constraint we wish to impose on the arg filler.

The 'mwuw' feature records a bonus value associated with the multi-word unit reading of a syntactic configuration.

Note that the value of the sem feature must be a list, even if the list is to contain one element only
(the most usual case).

In the morphological info slot, a row of three xs (xxx) means that the form is missing - no word forms will be
produced that need the missing root.

We make use of this facility, accompanied by the distinction between standard and deponent verbs, to account for the behaviour
of 'coepi' or 'audere':

verb([v(coepi,3,xxx,coept)],tr_cod,dep).    % treated as a deponent - semantically preserves 'active' voice
                                            % generates the 'coeptus' of 'coeptus sum' and friends, with 'active' meaning

verb([v(coepi,3,xxx,coep,xxx)],tr_cod,std). % treated as lacking everything except perfect tense and derived variants
                                            % enables the generation of 'coeperam', 'coepisse', etc.

To claim that 'coepi' is used in the passive voice with a passive infinitive does not seem to make much sense,
pace Jacques Michel, Grammaire de base du latin,
$ 257 : "Coepi prend la voix de l'infinitif avec lequel il se construit".

Marius Lavency (VSVS, $ 285)  is more cautious:
"Les verbes coepisse, 'commencer', desisse, 'cesser', prennent des formes passives
lorsque l'infinitif qui les complète est lui-même passif."

The treatment proposed here seems adequate for PARSING purposes (in a generator it would overgenerate, as a parsing component
it allows the 'active' reading of coepi to be maintained). I don't know that the following is interpretable if a passive VOICE is
insisted on: 'Cum procinctae igitur classes erant et instructa acies coeptumque in hostem progredi,
tibicines inter exercitum positi canere inceptabant.' Aulus Gellius, Noctes Atticae 1.11.pr.1.

The point is that inchoatives such as 'coepi' simply do not take part in the assignment of voice.
Their aspectual value is transparent with respect to diathesis.

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
verb([v(maerere,2,maere,xxx,xxx)],intr,std).
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
verb([v(occidere,3,occid,occid,occis)],tr_cod,std).
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


/*

%%%%%%%%%%%%%%%%%%%
VERB TEMPLATES
%%%%%%%%%%%%%%%%%%%

only some of the most frequent arg patterns are catered for in the following list

Intransitives with human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

Intransitives no constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

Intransitive with infinitive cplt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).


Intransitives with prep cplt (prep specified) and human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:PREP]]])]).

prep unspecified
%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[]]])]).



Simple transitive verbs with human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[
             ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

human OBJECT
%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]])]).


with human dative object and human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:dat]]])]).


Transitive with prep cplt and human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:PREP]] ])]).


ab/ex alternative
%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ])]).



tr with adjunct with specified CASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      adjunct:[type:np,oblig:no,constraints:[case:CASE]]])]).




verbs with nonfinite_i object (and human subject)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).


transitive with NP object or indirect question
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(v_v,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,mood:subjunctive, flagint:or([yes_no_question,wh_question])]]])]).


with indirect object
%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]]])]).


with indirect object alone (human)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]]])]).

with double acc
%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object_i:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[abstract]]]])]).


object and object cplt
%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[
         % adj as object_cplt
                ws(v_v,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std]]]),

         % np as object_cplt
                 ws(v_v,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:np,oblig:yes,constraints:[class:common,case:acc]]])]).


alternance
%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]]]),

                 ws(v_v,tr_cod_prep_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]])]).



acc cum infinitive
%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).


with indirect object and finite clause object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lexarg(Xre,
       arglist:[ws(v_v,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,constraints:[case:acc]],
                      object:[type:pred,oblig:no,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]])]).

with finite clause arg
%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:no,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]])]).

*/




% ADESSE
lexarg(adesse,
       arglist:[ws(adsum_be_present,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      cplt:[type:np,oblig:no,constraints:[case:dat]]])]).


% AMBULARE
% Ambulo in horto.
lexarg(ambulare,
       arglist:[ws(ambulo_walk,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% CADERE
lexarg(cadere,
       arglist:[ws(cado_fall,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).
% CEDERE
lexarg(cedere,
       arglist:[ws(cedo_go,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                       prep_cplt:[type:pp,oblig:no,constraints:[]]]),
                 ws(cedo_yield,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat]]]) ]).

% CENARE
% Cenabis bene.
lexarg(cenare,
       arglist:[ws(ceno_dine,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% DUBITARE
lexarg(dubitare,
       arglist:[ws(dubito_doubt,tr,clause:[[polarity:neg]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:quin]]]),
                ws(dubito_hesitate,tr,clause:[[polarity:neg]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(dubito_doubt,tr,clause:[[illocutionary_force:question]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:quin]]]),
                ws(dubito_hesitate,tr,clause:[[illocutionary_force:question]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])



]).

% ERRARE
% Errare humanum est.
lexarg(errare,
       arglist:[ws(erro_be_mistaken,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% GAUDERE
lexarg(gaudere,
       arglist:[ws(gaudeo_rejoice,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% IACERE
lexarg(iacere,
       arglist:[ws(iaceo_lie_down,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% INSANIRE
% Insanit rex.
lexarg(insanire,
       arglist:[ws(insanio_be_mad,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).




% IRE
% Eo Romam.
% Eo auxilium rogatum.
% Itum est in templum.
lexarg(ire,
       arglist:[ws(eo_go,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]],
                      cplt:[type:np,oblig:no,constraints:[case:abl,sem:[loc]]],
                      cplt:[type:np,oblig:no,constraints:[case:acc,sem:[city]]],
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]] ])]).
% ABIRE
lexarg(abire,
       arglist:[ws(abeo_leave,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex]), case:abl]],
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]] ])]).
% REDIRE
lexarg(redire,
       arglist:[ws(redeo_return,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]]  ])]).

% LOQUI
lexarg(loqui,
       arglist:[ws(loquor_speak,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:de, case:abl]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:cum, case:abl]]

                      ])]).

% LUDERE
% Eant lusum.
lexarg(ludere,
       arglist:[ws(ludo_play,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% MAERERE
lexarg(maerere,
       arglist:[ws(maereo_be_afflicted,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      cplt:[type:np,oblig:no,constraints:[case:abl]]])]).

% MANERE
lexarg(manere,
       arglist:[ws(maneo_stay,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

% MORI
lexarg(mori,
       arglist:[ws(mori_die,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).



% MOVERE
lexarg(mouere,
       arglist:[ws(moueo_move,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]]),

                ws(moueo_move,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% MUTARE
lexarg(mutare,
       arglist:[ws(muto_change,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]]),

                ws(muto_change,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% NASCI
lexarg(nasci,
       arglist:[ws(nasci_be_born,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).


% PERIRE
lexarg(perire,
       arglist:[ws(perire_perish,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% PRAESTARE
lexarg(praestare,
       arglist:[ws(praesto_excel,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% PRODESSE
lexarg(prodesse,
       arglist:[ws(prosum_help,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]]])]).


% REGNARE
% Tres annos regnavit.
lexarg(regnare,
       arglist:[ws(regno_reign,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% SEQUI
% Sequuntur caedes.
lexarg(sequi_1,
       arglist:[ws(sequor_follow,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]]])]).
% STARE
lexarg(stare,
       arglist:[ws(sto_stand,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

% TACERE
% Tacebant omnes senatores.
lexarg(tacere,
       arglist:[ws(taceo_keep_silent,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% VENIRE
% Timeo ne veniant.
lexarg(uenire,
       arglist:[ws(uenio_come,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]],
                      cplt:[type:np,oblig:no,constraints:[case:acc,sem:[city]]],
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]] ])]).
% PERUENIRE
lexarg(peruenire,
       arglist:[ws(peruenio_arrive,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:or([ad,in]), case:acc]]] )  ]).

% UIUERE
% Vixerunt.
% Vixit vitam longam beatamque.
lexarg(uiuere,
       arglist:[ws(uiuo_live,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[lex:uita,case:acc]]])]).





% INTRANSITIVE + PP as arg
%

% ABHORRERE
% Imperatores timeo qui a pace abhorrent.
lexarg(abhorrere,
       arglist:[ws(abhorreo_detest,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:ab]]])]).
% FUGERE
lexarg(fugere,
       arglist:[ws(fugio_flee,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]]),

                ws(fugio_flee,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes]]) ]).

% PUGNARE
% Pugnavere Romani cum Germanis.
lexarg(pugnare,
       arglist:[ws(pugno_fight,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:cum]]])]).

% RECUBARE
% Rege sub tegmine fagi recubante scribit regina epistulam ad servum Marci.
lexarg(recubare,
       arglist:[ws(recubo_lie_down,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[]]])]).

% SERPERE
% specifically handcrafted for Virgil's "Hanc sine tempora circum inter victrices hederam tibi serpere lauros"
lexarg(serpere,
       arglist:[ws(serpo_crawl,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     prep_cplt:[type:pp,oblig:yes,constraints:[prep:inter]],
                     prep_cplt:[type:pp,oblig:yes,constraints:[prep:circum]]])]).





% SIMPLE V TR
%

% ACCIPERE
% Rex accepit dona ab hostibus.
lexarg(accipere,
       arglist:[ws(accipio_receive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ])]).

% AGERE
% Rem age.
% Ausus est in ecclesia tua agere negotium procurandi fructus mortis.
lexarg(agere,
       arglist:[ ws(negotium_ago_take_care_of,tr_cod,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, lex:negotium]],
                      object_cplt:[type:pred, oblig:yes,constraints:[local_case:gen,type:gerund]] ]),

                ws(ago_do,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]] ])]).

% AMARE
% Regina deam amat.
lexarg(amare,
       arglist:[
             ws(amo_love,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% AMITTERE
% Angebat Hamilcarem amissa Sicilia.
lexarg(amittere,
       arglist:[ws(amitto_let_go,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% AUDERE / SOLERE
% verb([v(solere,2,sole,xxx,xxx)],tr_cod,std).
% verb([v(solere,2,xxx,solit)],tr_cod,dep).
lexarg(solere,
       arglist:[ws(soleo_be_used_to,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% verb([v(audere,2,aude,xxx,xxx)],tr_cod,std).
% verb([v(audere,2,xxx,aus)],tr_cod,dep).
lexarg(audere,
       arglist:[ws(audeo_dare,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc]),sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% AUDIRE
lexarg(audire,
       arglist:[
             ws(audio_hear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

             ws(audio_hear_that,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,argbound:yes,subordinator:or([quod, quia, quoniam])]]])]).





% ANGERE
% Angebat Hamilcarem amissa Sicilia.
lexarg(angere,
       arglist:[ws(ango_torment,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]])]).
% CANERE
lexarg(canere,
       arglist:[
             ws(cano_sing,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).



% CAPERE
% Caesar mittit legiones legato urbis capiendae causa.
lexarg(capere,
       arglist:[ws(capio_take,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% CLAMARE
% Magna voce clamat.
lexarg(clamare,
       arglist:[ws(clamo_call,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      adjunct:[type:np,oblig:no,constraints:[case:abl]]])]).
% CLAUDERE
lexarg(claudere,
       arglist:[
             ws(claudo_close,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% COEPI
% Patrem amare coepi.
% Coeptus sum a patre amari.

% verb([v(coepi,3,xxx,coept)],tr_cod,dep).    % treated as a deponent - semantically preserves 'active' voice
% verb([v(coepi,3,xxx,coep,xxx)],tr_cod,std). % treated as lacking everything except perfect tense and derived variants

lexarg(coepi,
       arglist:[ws(coepi_begin,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

              ws(coepi_begin,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc]),sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% COGERE
lexarg(cogere,
       arglist:[ws(cogo_compel,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:ut]]]),


                ws(cogo_compel,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:ad]] ]),


                ws(cogo_compel,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])
]).

% COGNOSCERE
% Cognouerat virtutem tuam.
lexarg(cognoscere,
       arglist:[ws(cognosco_learn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% NOSCERE
% Nouerat virtutem tuam.
lexarg(noscere,
       arglist:[ws(novi_know,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% COLERE
% Mihi colenda est uirtus.
lexarg(colere,
       arglist:[ws(colo_inhabit,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[loc]]]]),

                ws(colo_honor,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[abstract]]]])]).
% CONCUPISCERE
lexarg(concupiscere,
       arglist:[ws(concupisco_desire,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% DEBERE
lexarg(debere,
       arglist:[ws(debeo_owe,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:dat]]]),

                ws(debeo_must,tr_inf_no_subj,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% DELERE
% Karthago delenda est.
lexarg(delere,
       arglist:[ws(deleo_destroy,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% DEMENTARE
% Quos vult perdere dementat.
lexarg(dementare,
       arglist:[ws(demento_madden,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]]])]).
% DIRIPERE
lexarg(diripere,
       arglist:[ws(diripio_sack_destroy,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% DUCERE
% Imperator legiones ad proelium duxit.
lexarg(ducere,
       arglist:[ws(duco_lead,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]],
                     prep_cplt:[type:pp,oblig:no,constraints:[]]])]).
% ADDUCERE
% Cupiditate regni adductus nouis rebus studebat. (Caesar, De Bello Gallico)
lexarg(adducere,
       arglist:[ws(adduco_drive,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                     prep_cplt:[type:pp,oblig:no,constraints:[]]])]).


% ELIGERE
% Elige cui dicas : tu mihi sola places.
lexarg(eligere,
       arglist:[ws(eligo_choose,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% ERIPERE
lexarg(eripere,
       arglist:[
             ws(eripio_snatch,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ])]).
% EXCIPERE
lexarg(excipere,
       arglist:[
             ws(excipio_take_out,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% EXHAURIRE
lexarg(exhaurire,
       arglist:[ws(exhaurio_endure,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% FERRE
% Timeo Danaos et dona ferentes.
lexarg(ferre,
       arglist:[ws(fero_bear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% REFERRE
% De re refertur.
lexarg(referre,
       arglist:[ws(refero_relate,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[]]])]).
% FRANGERE
lexarg(frangere,
       arglist:[
             ws(frango_break,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% GERERE
lexarg(gerere,
       arglist:[
             ws(gero_manage,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% HORTARI
% Caesari erat eundum Romam ad senatores hortandos.
lexarg(hortari,
       arglist:[ws(hortor_exhort,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).
% IGNOSCERE
% Orat te mater ut filio ignoscas suo.
lexarg(ignoscere,
       arglist:[ws(ignosco_pardon,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      i_object:[type:np,oblig:yes,constraints:[sem:[hum],case:dat]]])]).
% INCIPERE
lexarg(incipere,
       arglist:[ws(incipio_begin,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

               ws(incipio_begin,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]) ]).

% INDICARE
% Me tabula sacer votiva paries indicat uvida suspendisse potenti vestimenta maris deo.
lexarg(indicare,
       arglist:[ws(indico_show_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]],
                      adjunct:[type:np,oblig:no,constraints:[case:abl]]])]).
% INTERFICERE
% Rex, cum interfecisset Marcum, magnitudinem sceleris sui perspexit.
lexarg(interficere,
       arglist:[ws(interficio_kill,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]]])]).
% INVENIRE
lexarg(inuenire,
       arglist:[ws(inuenio_find_that_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),


                ws(inuenio_find,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% IUUARE
lexarg(iuuare,
       arglist:[
             ws(iuuo_help,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% LAUDARE
% Laudamus te.
lexarg(laudare,
       arglist:[ws(laudo_praise,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% LEGERE
% Rex vult legere libellos impudicos quos serva Marci scripsit.
lexarg(legere,
       arglist:[ws(lego_read,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% MEMINI
% conjugated forms too few to be linked to a pattern
% entered separately as lex clauses
% Memento documenti patientiae nostrae.
lexarg(memini,
       arglist:[ws(memini_remember,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:no,constraints:[case:or([acc,gen])]]])]).

% MISCERE
lexarg(miscere,
       arglist:[
             ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% NARRARE
lexarg(narrare,
       arglist:[ws(narro_tell,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[abstract],case:acc]]])]).

% OBLIUISCI
% Non obliviscar sermones tuos - Pascal, Mémorial
% Oblita est periculi ancilla fortior dominis multis
% Obliuiscitur rex reginam epistulas longas scripsisse ancillae Marci.
lexarg(obliuisci,
       arglist:[ws(obliuiscor_forget,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:no,constraints:[case:or([acc,gen])]]]),

                ws(obliuiscor_forget_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).
% OBTEMPERARE
lexarg(obtemperare,
       arglist:[ws(obtempero_obey,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat]]])]).
% OCCIDERE
lexarg(occidere,
       arglist:[ws(occido_kill,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[sem:[hum],case:acc]]])]).
% OPPUGNARE
% Decem annos urbem oppugnaverunt.
lexarg(oppugnare,
       arglist:[ws(oppugno_attack,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% OPTARE
lexarg(optare,
       arglist:[ws(opto_choose,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:no,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]]),

                ws(opto_choose,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% OSTENDERE
lexarg(ostendere,
       arglist:[ws(ostendo_show_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                 ws(ostendo_show,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,mood:subjunctive, flagint:wh_question]]]),

                ws(ostendo_show,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]) ]).

% PARARE
lexarg(parare,
       arglist:[
             ws(paro_prepare,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ad]] ])]).
% PARCERE
lexarg(parcere,
       arglist:[ws(parco_spare,tr_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat]]])]).

% PATI
lexarg(pati,
       arglist:[ws(patior_bear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% PERDERE
% Quos uult perdere.
lexarg(perdere,
       arglist:[ws(perdo_ruin_or_lose,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% PERSPICERE
% Rex, cum interfecisset Marcum, magnitudinem sceleris sui perspexit.
lexarg(perspicere,
       arglist:[ws(perspicio_see,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% PETERE
% Petiuit consulatum magna cum cura.
% Thessaliam ex negotio petebam.
lexarg(petere,
       arglist:[ws(peto_try_to_reach_or_get,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[thing],case:acc]]]),

                ws(peto_consulatum_run_for_consul,tr_cod,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,lex:consulatus]]])]).
% PONERE
% Italiam peto causa videndae Romae in montibus positae.
lexarg(ponere,
       arglist:[ws(pono_put,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[]]])]).
% SE PRAEBERE
lexarg(praebere,
       arglist:[
         % adj as object_cplt
                ws(se_praebere_prove_oneself_to_be,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,lex:or([pp3refl,pp1sg,pp2sg,pp1pl,pp2pl])]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std]]]),

         % np as object_cplt
                 ws(v_v,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc,lex:or([pp3refl,pp1sg,pp2sg,pp1pl,pp2pl])]],
                       object_cplt:[type:np,oblig:yes,constraints:[class:common,case:acc]]])

        ]).

% PROCURARE
lexarg(procurare,
       arglist:[ws(procuro_procure,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% PREMERE
lexarg(premere,
       arglist:[
             ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% QUAERERE
% Rex quaerebat uitam beatam in natura.
% Quaero num pater tuus uenerit.
% Quaero ueneritne pater tuus.
lexarg(quaerere,
       arglist:[ws(quaero_seek,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(quaero_ask,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,mood:subjunctive, flagint:or([yes_no_question,wh_question])]]])]).
% RAPERE
lexarg(rapere,
       arglist:[ws(rapio_seize,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ad]] ])]).

% RECIPERE
lexarg(recipere,
       arglist:[
             ws(recipio_receive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% REDDERE
lexarg(reddere,
       arglist:[ws(reddo_give_back,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% RELINQUERE
lexarg(relinquere,
       arglist:[ws(relinquo_leave,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% LINQUERE
lexarg(linquere,
       arglist:[ws(linquo_leave,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% RENOVARE
% quies renovavit corpora animosque
lexarg(renouare,
       arglist:[ws(renouo_renew,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% ROGARE
% Eo auxilium rogatum.
% Rogebant quae fortuna exercitus esset.
lexarg(rogare,
       arglist:[ws(rogo_ask_for,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

               ws(rogo_ask,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,
                       constraints:[type:finite,mood:subjunctive, flagint:or([yes_no_question,wh_question])]]])]).
% SEQUI
% Crediderunt me te secutum fuisse.
lexarg(sequi_2,
       arglist:[ws(sequor_follow,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% SERUARE
% Aestiuo serues ubi piscem tempore quaeris? (Martial, 2.78)
lexarg(seruare,
       arglist:[ws(servo_keep_safe,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% SOLUERE
lexarg(soluere,
       arglist:[ws(soluo_release,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% STUDERE
% nouis rebus studebat.
lexarg(studere,
       arglist:[ws(studeo_desire,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:dat]]]),

               ws(studeo_want_to,tr_inf_no_subj,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).
% TEGERE
lexarg(tegere,
       arglist:[
             ws(tego_cover,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% TENERE
lexarg(tenere,
       arglist:[ws(teneo_hold,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% TIMERE
% Timeo Danaos et dona ferentes.
% Timeo amicis meis.
% Timeo ne veniant ad urbem capiendam.
lexarg(timere,
       arglist:[ws(timeo_fear,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

               ws(timeo_fear_for,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:dat]]]),

               ws(timeo_fear_that,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,
                              constraints:[type:finite,mood:subjunctive, argbound:yes,subordinator:or([ne,ut])]]])]).
% TOLLERE
lexarg(tollere,
       arglist:[
             ws(tollo_raise_up,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[]] ] )] ).

% TRAHERE
lexarg(trahere,
       arglist:[
             ws(traho_drag,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).


% UTI
% Cicerone magistro usi sunt multi magistri.
lexarg(uti,
       arglist:[ws(utor_use,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:abl]],
                     object_cplt:[type:np,oblig:no,constraints:[case:abl,lextype:full]]])]).
% UERTERE
lexarg(uertere,
       arglist:[
             ws(uerto_turn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in])]] ])]).

% VIDERE
% Italiam peto causa videndae Romae in montibus positae.
lexarg(uidere,
       arglist:[ws(uideo_see,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% VINCERE
% Vincere scis.
lexarg(uincere,
       arglist:[ws(uinco_win,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[sem:[hum],case:acc]]])]).
% VITARE
% Parsimonia est ars uitandi sumptus superuacuos.
lexarg(uitare,
       arglist:[ws(uito_avoid,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% UOCARE
% Nauta rationes puellae in dubium uocat.
% Vocavit matrem eius et non uenit.
lexarg(uocare,
       arglist:[ws(uoco_call,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]]),

         % mwu IN IUS VOCARE
             ws(mwu_in_ius_uoco_bring_to_court,tr_cod_cplt,clause:[],mwuw:2,
             args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                   object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                   object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:in_ius]]]),

         % mwu IN DUBIUM VOCARE
              ws(mwu_in_dubium_uoco_call_into_doubt,tr_cod_cplt,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[sem:[abstract],case:acc]],
                    object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:in_dubium]]])]).






% WITH INDIRECT OBJECT
%

% DARE
% Dedimus profecto grande documentum patientiae.
lexarg(dare,
       arglist:[ws(do_give,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]]])]).
% PLACERE
% Elige cui dicas : tu mihi sola places.
lexarg(placere,
       arglist:[ws(placeo_please,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]]])]).
% SUSPENDERE
% Me tabula sacer votiva paries indicat uvida suspendisse potenti vestimenta maris deo.
lexarg(suspendere,
       arglist:[ws(suspendo_dedicate,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]]])]).




% DOUBLE ACC
%

% DOCERE
% Credo ancillam grammaticam pueros docere.
lexarg(docere,
       arglist:[ws(doceo_teach,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],            % person teaching
                      object_i:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]],   % person being taught
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[abstract]]]])]). % topic being taught
% object_i is like a direct object in that it requires the accusative case,
% but like an indirect object (i.e. an i_object, standard dative case indirect object) in the semantic role it fulfils






% OTHER MULTI-WORD UNITS
%

% ALICUI AQUA ET IGNI INTERDICERE
% Dicunt militibus malis aqua et igni praetorem interdixisse.
lexarg(interdicere,
       arglist:[ws(mwu_aqua_et_igni_interdico_exile,tr_cod_cplt,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:dat]],
                      object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:aqua_et_igni]]])]).

% ALIQUEM/QUOD (NON) PILI FACERE
% Praetor non amabat milites nec faciebat pili cohortem.
lexarg(facere,
        arglist:[ws(mwu_non_pili_facio_not_give_a_damn,tr_cod_cplt,clause:[[polarity:neg]],mwuw:2,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:pili]]]),
                 ws(mwu_tanti_facio_appreciate,tr_cod_cplt,clause:[],mwuw:2,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:tanti]]]),


% Verba facere
% Partibus factis, fecit verba leo.
                 ws(uerba_facio_speak,tr_cod,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc, lex:uerbum, number:pl]]]),
% Iter facere
% Iter feci per Galliam
                ws(iter_facio_travel,tr_cod,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc, lex:iter, number:sing]],
                    prep_cplt:[type:pp,oblig:no,constraints:[]]]),
% Standard facio
% Faciamus bonum vinum.
               ws(facio_make,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]])]).






% MULTIPLE CONSTRUCTIONS

%

% HABERE
% Libellos Marci habet rex impudicos.
% Libellos impudicos habet regina documenta ingenii humani.
% Catilina nihil pensi neque sancti habere dicitur.
lexarg(habere,
       arglist:[ ws(habeo_have,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

         % adj as object_cplt
                ws(habeo_consider_as,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std]]]),

         % np as object_cplt
                 ws(habeo_consider_as,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:np,oblig:yes,constraints:[class:common,case:acc]]]),

         % mwu  NIHIL PENSI NEQUE SANCTI HABERE
                 ws(mwu_nihil_pensi_neque_sancti_habeo_fear_neither_god_nor_man,tr_cod_cplt,clause:[],mwuw:2,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:nihil_pensi_neque_sancti]]])]).






% ALTERNANCE
%

% DONARE
% Marco donat ciuitas immortalitatem.
% Marcum donat ciuitas immortalitate.
lexarg(donare,
       arglist:[ws(dono_donate,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]],
                      i_object:[type:np,oblig:yes,case:dat,constraints:[case:dat, sem:[hum]]]]),

                 ws(dono_donate,tr_cod_coi,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:abl, sem:[thing]]],
                       i_object:[type:np,oblig:yes,case:acc,constraints:[case:acc, sem:[hum]]]])]).
% MITTERE
% Regina putat regem epistulas longas ad ancillam misisse.
% Dixit rex reginam librum pulchrum misisse Marco.
lexarg(mittere,
       arglist:[ws(mitto_send,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]]]),

                 ws(mitto_send,tr_cod_prep_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]])]).
% SCRIBERE
% Si rex amasset servas, scripsisset libellos impudicos.
lexarg(scribere,
       arglist:[ws(scribo_write,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]]]),

                
                 ws(scribo_write,tr_cod_prep_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]])]).




% ACC CUM INFINITIVE
%

% CREDERE (+acc_cum_inf)
% Crede hoc mihi.
% Crediderunt Ciceronem oratorem optimum.
% Credunt Ciceronem oratorem optimum esse.
lexarg(credere,
       arglist:[ws(credo_believe,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

                ws(credo_believe_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                 % adj as object_cplt
                ws(credo_believe_to_be,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc]]]),

                % np as object_cplt
                 ws(credo_believe_to_be,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:np,oblig:yes,case:acc,constraints:[case:acc]]])]).

% DICERE (+acc_cum_inf)
% Elige cui dicas : tu mihi sola places.
% Dicunt militibus malis aqua et igni praetorem interdixisse.
% Marcus dixit regi magno salutem longam.
lexarg(dicere,
       arglist:[ws(dico_say_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

         % + finite clause as object
              ws(dico_say_that,tr_f,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                    object:[type:pred,oblig:yes,constraints:[type:finite,add:colon]]]),          % the add:colon feature is used to
												% capture direct discourse inserted in a sentence
												% elige cui dicas : tu mihi sola places

         % + unspecified np object
                ws(dico_say,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]]),

         % mwu ALICUI SALUTEM DICERE
              ws(mwu_salutem_dico_greet,tr_cod_coi,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    i_object:[type:np,oblig:yes,case:dat,constraints:[case:dat, sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc, lex:salus]]])]).



% EXISTIMARE (+acc_cum_inf)
% Cicero epistulas optimas scripsisse existimabatur.
lexarg(existimare,
       arglist:[ws(existimo_deem_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% IUBERE (+acc_cum_inf)
% Iussit omnes tacere.
lexarg(iubere,
       arglist:[ws(iubeo_order_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% IUDICARE (+acc_cum_inf)
% Iudico te optimum praetorem esse.
lexarg(iudicare,
       arglist:[ws(iudico_judge_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).



% NEGARE (+acc_cum_inf)
% Negat se libellos impudicos scribere.

% Cicero,  Epistulae ad Familiares 9.14.1.1:
% ***negant*** enim [se dubitare ***quin*** tu meis praeceptis et consiliis obtemperans
% praestantissimum te civem et singularem consulem praebeas].

% if the object arg is of type:pred, we add a feature inherited_pol:neg to the object pred
% when a constraint on neg polarity is checked, both the pol feature and the inherited_pol feature are queried
% if one of them matches, the constraint is deemed to be satisfied
% not clear yet how to make it work... !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

%  object:[type:pred,oblig:yes,inherited_pol:neg,constraints:[type:nonfinite]]])]). NEGARE Object Specs
% arglist:[ws(dubito_doubt,tr,clause:[[polarity:neg]],mwuw:0,                       DUBITARE Clause Constraints


lexarg(negare,
       arglist:[ws(nego_deny_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,inherited_pol:neg,constraints:[type:nonfinite]]])]).




% NUNTIARE (+acc_cum_inf)
% Nuntiatum est hostes vinum amare.
lexarg(nuntiare,
       arglist:[ws(nuntio_announce_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% PUTARE (+acc_cum_inf)
% Rex quem putas reginam amare amat ancillam reginae.
% Humani nihil a me alienum puto
lexarg(putare,
       arglist:[ws(puto_think_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

               ws(puto_deem,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc,number:N,gender:G]],
                       object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, number:N,gender:G]]]),


                ws(puto_deem,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:np,oblig:yes,constraints:[case:acc]]])


]).

% REOR (+acc_cum_inf)
% Omnes hostes rogaturos esse auxilium ratus est.
lexarg(reri,
       arglist:[ws(reor_think_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% SCIRE (+acc_cum_inf)
% Vincere scis.
% Caesar se Germanos vicisse sciebat.
lexarg(scire,
       arglist:[ws(scio_know_that,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

         % + subjectless nonfinite clause - vincere scis
              ws(scio_know_how_to,tr_inf_no_subj,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% NESCIRE (+acc_cum_inf)
% Victoria uti nescis.
lexarg(nescire,
       arglist:[ws(nescio_not_know_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

         % + subjectless nonfinite clause - petere consulatum nescis
                ws(nescio_not_know_how_to,tr_inf_no_subj,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% SINERE
lexarg(sinere,
arglist:[ws(sino_allow,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% SENTIRE
lexarg(sentire,
arglist:[ws(sentio_perceive,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

        ws(sentio_perceive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

        ws(sentio_perceive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,
                              constraints:[type:finite,mood:subjunctive, flagint:or([yes_no_question,wh_question])]]])]).




% TRADERE (+acc_cum_inf)
% Cicero litteras longas scribere traditur.
lexarg(tradere,
       arglist:[ws(trado_report_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                ws(trado_yield,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      i_object:[type:np,oblig:no,constraints:[case:dat,sem:[hum]]]])]).





% WITH FINITE CLAUSE ARG
%

% ORARE
% Orat te mater ut filio ignoscas suo.
lexarg(orare,
       arglist:[ws(oro_ask,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,constraints:[case:acc]],
                      object:[type:pred,oblig:no,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]])]).



% VERERI
% Ne hostis vincat vereor.
% Vereor Italiam petere.
lexarg(uereri,
       arglist:[ws(uereor_fear,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(uereor_fear,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,
                              argbound:yes, subordinator:or([ne,ut])]]])]).




% SUBJECTLESS NONFINITE CPLT
%

% POSSE (+ subjectless nonfinite clause)
% Legere possunt.
verb([v(aux,posse,possum,potes,potest,possumus,potestis,possunt,
            possim,possis,possit,possimus,possitis,possint,
            poteram,poteras,poterat,poteramus,poteratis,poterant,
            potero,poteris,poterit,poterimus,poteritis,poterunt,
            possem,posses,posset,possemus,possetis,possent,
            posse,potu)],tr_inf_no_subj,std).
lexarg(posse,
       arglist:[ws(possum_be_able_to,tr_inf_no_subj,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% VELLE (+ subjectless nonfinite clause)
%  Epistulas tuas legere uolo.
verb([v(aux,uelle,uolo,uis,uult,uolumus,uultis,uolunt,
        uelim,uelis,uelit,uelimus,uelitis,uelint,
        uolebam,uolebas,uolebat,uolebamus,uolebatis,uolebant,
        uolam,uoles,uolet,uolemus,uoletis,uolent,
        uellem,uelles,uellet,uollemus,uolletis,uollent,
        uelle,uolu)],tr_inf_no_subj,std).
lexarg(uelle,
       arglist:[ws(uolo_want_to,tr_inf_no_subj,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),
               
                ws(uolo_want_sbd_to,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive]]]),

                ws(uolo_want_sbd_to,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% NOLLE (+ subjectless nonfinite clause)
% Nolite Lugdunum ire.
verb([v(aux,nolle,nolo,xxx,xxx,nolumus,xxx,nolunt,
        nolim,nolis,nolit,nolimus,nolitis,nolint,
        nolebam,nolebas,nolebat,nolebamus,nolebatis,nolebant,
        xxx,xxx,xxx,xxx,xxx,xxx,
        nollem,nolles,nollet,nollemus,nolletis,nollent,
        nolle,nolu)],tr_inf_no_subj,std).
lexarg(nolle,
       arglist:[ws(nolo_refuse_to,tr_inf_no_subj,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(nolo_refuse_to,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% MALLE (+ subjectless nonfinite clause)
%  Rex Italiam petere mavult.
verb([v(aux,malle,malo,mauis,mauult,malumus,mauultis,malunt,
        malim,malis,malit,malimus,malitis,malint,
        malebam,malebas,malebat,malebamus,malebatis,malebant,
        xxx,xxx,xxx,xxx,xxx,xxx,
        mallem,malles,mallet,mallemus,malletis,mallent,
        malle,malu)],tr_inf_no_subj,std).
lexarg(malle,
       arglist:[ws(malo_prefer_to,tr_inf_no_subj,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(malo_prefer_to,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% FIERI
% Fit ut omnes me  libellos impudicos legere sciant.
% Fiat lux et facta est lux.
verb([v(aux,fieri,fio, fis,fit,fimus, fitis, fiunt,
        fiam, fias, fiat, fiamus, fiatis, fiant,
        fiebam,fiebas,fiebat,fiebamus,fiebatis,fiebant,
        fiam,fies,fiet,fiemus,fietis,fient,
        fierem,fieres,fieret,fieremus,fieretis,fierent,
        fieri,xxx)],vpred,std).

lexarg(fieri,
       arglist:[  ws(fio_become,vpred,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                 predicative:[type:np,oblig:yes, constraints:[case:or([nom,acc]), class:common]]]),

               ws(fio_become,vpred,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]]]),

               ws(fio_become,vpred,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes,subordinator:ut]]])]).






% Impersonal verbs
%

% DECET
verb([v(imp,decet,decet,deceat,decebat,decebit,deceret,decere,decu)],vimp,std).
lexarg(decet,
       arglist:[ws(decet_it_behoves,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).

% PAENITET
verb([v(imp,paenitet,paenitet,paeniteat,paenitebat,paenitebit,paeniteret,paenitere,paenitu)],vimp,std).
lexarg(paenitet,
       arglist:[ws(paenitet_regret_doing,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]]),

                  % eos peccatorum paenitet
                ws(paenitet_regret,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:dummy],                  % the arglist needs a subject, a dummy one is provided
                     arg:[type:np,oblig:yes,constraints:[case:gen]],
                     cplt:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).

% PLACET
verb([v(imp,placet,placet,placeat,placebat,placebit,placeret,placere,placu)],vimp,std).
lexarg(placet,
       arglist:[ws(placet_it_be_decided_to,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
		arg:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]]]),

               ws(placet_it_be_decided_to,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,argbound:yes,subordinator:ut]],
		arg:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]]])]).

% LICET
verb([v(imp,licet,licet,liceat,licebat,licebit,liceret,licere,licu)],vimp,std).
lexarg(licet,
       arglist:[ws(licet_it_is_allowed_to,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).
% IUUAT
verb([v(imp,iuuat,iuuat,iuuet,iuuabat,iuuabit,iuuaret,iuuare,iuu)],vimp,std).
lexarg(iuuat,
       arglist:[ws(iuuat_it_pleases,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).
% FUGIT
verb([v(imp,fugit,fugit,fugiat,fugiebat,fugiet,fugeret,fugere,fug)],vimp,std).
lexarg(fugit,
       arglist:[ws(fugit_it_escapes,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                    arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).
% FALLIT
verb([v(imp,fallit,fallit,fallat,fallebat,fallet,falleret,fallere,fefell)],vimp,std).
lexarg(fugit,
       arglist:[ws(fallit_it_escapes,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).

% CONSTAT
verb([v(imp,constat,constat,constet,constabat,constabit,constaret,constare,constit)],vimp,std).
lexarg(constat,
       arglist:[ws(constat_it_is_established_that,intr,clause:[],mwuw:0,
               args:[arg:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).


% PERTINET
verb([v(imp,pertinet,pertinet,pertineat,pertinebat,pertinebit,pertineret,pertinere,pertinu)],vimp,std).
lexarg(pertinet,
       arglist:[ws(pertinet_it_is_important_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite]],
                     prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]]),

                ws(pertinet_it_is_important_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive, flagint:or([yes_no_question,wh_question])]],
                     prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]])]).



% OPORTET
verb([v(imp,oportet,oportet,oporteat,oportebat,oportebit,oporteret,oportere,oportu)],vimp,std).
lexarg(oportet,
       arglist:[ws(oportet_it_is_necessary_for_X_to,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),
                     
                ws(oportet_it_is_necessary_to,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(oportet_it_is_necessary_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive]]])]).

% INTEREST
verb([v(imp,interest,interest,intersit,intererat,intererit,interesset,interesse,interfu)],vimp,std).
lexarg(interest,
       arglist:[ws(interest_it_matters_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite]],
                     arg:[type:np,oblig:no,constraints:[case:gen]],
                     arg:[type:adjp,oblig:no,constraints:[case:abl, type:poss]],
                      prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]]),

                ws(interest_it_matters_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive, flagint:or([yes_no_question,wh_question])]],
                      arg:[type:np,oblig:no,constraints:[case:gen]],
                     arg:[type:adjp,oblig:no,constraints:[case:abl, type:poss]],
                      prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]] ),

                 ws(interest_it_matters_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,argbound:yes, subordinator:or([ut,ne])]],
                     arg:[type:np,oblig:no,constraints:[case:gen]],
                     arg:[type:adjp,oblig:no,constraints:[case:abl, type:poss]],
                      prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]] )]).





% SUM
%%%%%%

% the constraints would have to be revised along with a revision of the grammatical framework 
% they are inserted in !!!!!

% ESSE is MUCH TOO expensive in the present framework
% sth will have to be done to improve efficiency

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% ESSE, coordination and clause-level adjuncts are responsible for 90 % of the inefficiency plaguing ALP
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


verb([v(esse,3,xxx,fu,xxx)],v_esse,std).
lexarg(esse,
       arglist:[     % Marcus legatus est.
		     % Pecunia magna documentum est avaritiae.

         /*    ws(sum_be,v_esse,clause:[],mwuw:0,
             args:[subject:[type:np,oblig:yes,constraints:[case:nom]],
                   predicative:[type:np,oblig:yes, constraints:[case:nom, class:common]]]),

             ws(sum_be,v_esse,clause:[],mwuw:0,
             args:[subject:[type:np,oblig:yes,constraints:[case:acc]],
                   predicative:[type:np,oblig:yes, constraints:[case:acc, class:common]]]),
          */
             ws(sum_be,v_esse,clause:[],mwuw:0,
             args:[subject:[type:np,oblig:yes,constraints:[]],
                   predicative:[type:np,oblig:yes, constraints:[class:common]]]),
 
                  % number and gender constraints on the predicative np would be too hard -
                  % the case constraint is the only one to be really binding
                  % but see below to understand why it is not imposed 


                   % Marcus bonus est.

              ws(sum_be,v_esse,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[]],
                    % constraints:[number:Nb2,gender:Gender2,case:Case2]],
                    predicative:[type:adjp,oblig:yes, constraints:[]]]),
                   % constraints:[number:Nb2,gender:Gender2,case:Case2]]]),
                  % here the triple constraint is necessary

                  % however, constraints are remmed to allow 
                  % 'amo qui amat me', 'amo quem amat me', instead of 'amo eum qui amat me'
                  % all this by a round-about way which it would take me too much time to go into


                  % Karthago delenda est.

              ws(sum_be,v_esse,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[number:Nb2,gender:Gender2,case:Case2]],
                    predicative:[type:gerundive,oblig:yes, constraints:[number:Nb2,gender:Gender2,case:Case2]]]),

              % if an arg is opened (dative np) for the subject of the gerund,
              % we have a case of 'est_alicui_have_to', for which see below

                    % Malum est insanire.

              ws(sum_be,v_esse,clause:[],mwuw:0,
              args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                    predicative:[type:adjp,oblig:yes, constraints:[number:sing,gender:neuter,case:nom,type:std]]]),

                  % Obliuisci est in nostra potestate.

                ws(sum_be,v_esse,clause:[],mwuw:0,
                 args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                       predicative:[type:pp,oblig:yes, constraints:[sem:[abstract]]]]),

                   % Marcus erat in templo.

              ws(sum_be,v_esse,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[]],
                       predicative:[type:pp,oblig:yes, constraints:[]]]),

                   % Marcus erat Lugduni, domi

              ws(sum_be_in,v_esse,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       adjunct:[type:adjunct,oblig:yes, constraints:[value:place]]]),

                   % ubi, quando, cur, quare, etc. with esse

               ws(sum_be_in,v_esse,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                       predicative:[type:advp,oblig:yes, constraints:[type:int]]]),


                   % note below the semantic values assigned, perceptible in the translations:
                   % note also the bonuses assigned in the mwuw feature value
                   % for multi-word unit readings of configurations involving sum and extra
                   % syntactic and lexical material

                   % Opus est mihi Marci patientia.

              ws(opus_est_need,v_esse,clause:[],mwuw:2,
                 args:[subject:[type:np,oblig:yes,constraints:[lex:opus,txt:opus,pathlist:[One_Word]]],
                       i_object:[type:np,oblig:no,constraints:[case:dat,sem:[hum]]],
                       object:[type:np,oblig:yes, constraints:[case:abl]]]),

		% note the constraints on the subject:
		% it must be a wordform whose lex is opus (used to avoid a context-retrievable subject being posited)
		% it must be the wordform opus itself (opera, etc are not ok here)
		% it must be on its own (pathlist value is made up of a single p(x,y) structure
                % (the word opus cannot be qualified: * opus populorum)

		% Est imperatoris boni capere urbes.

                ws(est_alicuius_be_the_mark_of,v_esse,clause:[],mwuw:2,
                   args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                         i_object:[type:np,oblig:yes,constraints:[case:gen,sem:[hum]]]]),

		% Caesari urbs erat capienda. (2)
                % Caesari eundum Romam erat.  (1)
                % the i_object slot houses the subject of the verb derived from the gerund

                % with true gerund (1):

                ws(est_alicui_have_to,v_esse,clause:[],mwuw:2,
                   args:[subject:[type:pred,oblig:yes,constraints:[type:gerund]],
                         i_object:[type:np,oblig:yes,constraints:[case:dat,sem:[hum]]]]),

                % with gerundive  (2):

                ws(est_alicui_have_to,v_esse,clause:[],mwuw:2,
                    args:[subject:[type:np,oblig:yes,constraints:[number:Nb2,gender:Gender2,case:Case2]],
                    predicative:[type:adjp,oblig:yes, constraints:[type:gdiv,number:Nb2,gender:Gender2,case:Case2]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat,sem:[hum]]]]),

                % necesse est

                ws(necesse_est_be_necessary,v_esse,clause:[],mwuw:2,
                    args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                    predicative:[type:adjp,oblig:yes, constraints:[lex:necesse]],
                     arg:[type:np,oblig:no,constraints:[case:dat,sem:[hum]]]]),

                ws(necesse_est_be_necessary,v_esse,clause:[],mwuw:2,
                    args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite]],
                    predicative:[type:adjp,oblig:yes, constraints:[lex:necesse]]]),

                 ws(necesse_est_be_necessary,v_esse,clause:[],mwuw:2,
                    args:[subject:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive,
                                                              argbound:yes, subordinator:ut]],
                    predicative:[type:adjp,oblig:yes, constraints:[lex:necesse]]]),

                  ws(necesse_est_be_necessary,v_esse,clause:[],mwuw:2,
                    args:[arg:[type:pred,oblig:yes,constraints:[type:finite,mood:subjunctive, argbound:no]],  % note arg, not subject
                    predicative:[type:adjp,oblig:yes, constraints:[lex:necesse]]]),


                 % Caesari est pecunia magna.

                 ws(est_alicui_possess,v_esse,clause:[],mwuw:0,
                    args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                          i_object:[type:np,oblig:yes,constraints:[case:dat,sem:[hum]]]])]).





% COMPLETE VOCABULARY IN MAKELEX.PL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%
% ADJ
%%%%%%

% 1
% bonus, formosus, impudicus, liber, longus, magnus, malus, nouus, paruus, patulus, pulcher, Romanus, uerus
% meus, tuus, suus, noster, uester, etc.

adj(solus,sol,a1,std,[xxx,xxx]).   % no comparative or superlative
adj(totus,tot,a1,std,[xxx,xxx]).
adj(uter,utr,a1,tool,[xxx,xxx]).
adj(alter,alter,a1,tool,[xxx,xxx]).
adj(unus,un,a1,tool,[xxx,xxx]).
adj(alius,ali,a1,tool,[xxx,xxx]).

adj(quantus,quant,1,int,[xxx,xxx]).




adj(altus,alt,1,std,[alt,altissim]).
adj(beatus,beat,1,std,[beat,beatissim]).
adj(bonus,bon,1,std,[mel,optim]).
adj(caecus,caec,1,std,[caec,caecissim]).
adj(cupidus,cupid,1,std,[cupid,cupidissim]).
adj(dignus,dign,1,std,[dign,dignissim]).
adj(doctus,doct,1,std,[doct,doctissim]).
adj(extremus,extrem,1,std,[xxx,xxx]).
adj(formosus,formos,1,std,[formos,formosissim]).
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
adj(mortuus,mortu,1,std,[xxx,xxx]).
adj(multus,mult,1,std,[xxx,xxx]).
adj(nouus,nou,1,std,[recent,recentissim]).
adj(paruus,paru,1,std,[min,minim]).
adj(patulus,patul,1,std,[patul,patulissim]).
adj(peritus,perit,1,std,[perit,peritissim]).
adj(primus,prim,1,std,[xxx,xxx]).
adj(propinquus,propinqu,1,std,[prop,proxim]).
adj(publicus,public,1,std,[public,publicissim]).
adj(pulcher,pulchr,1,std,[pulchr,pulcherrim]).
adj(romanus,roman,1,std,[xxx,xxx]).
adj(sacer,sacr,1,std,[sanct,sanctissim]).
adj(saluus,salu,1,std,[xxx,xxx]).
adj(superuacuus,superuacu,1,std,[xxx,xxx]).
adj(supremus,suprem,1,std,[xxx,xxx]).
adj(uerus,uer,1,std,[uer,uerissim]).
adj(uiuus,uiu,1,std,[xxx,xxx]).
adj(ultimus,ultim,1,std,[xxx,xxx]).
adj(uotiuus,uotiu,1,std,[xxx,xxx]).
adj(uuidus,uuid,1,std,[uuid,uuidissim]).


% examples of  arg-bearing adjectives
%

% having two lexarg structures rather than a single one with optional arg seems to prove more efficient

lexarg(cupidus,
      args:[object:[type:np,oblig:no,constraints:[case:gen]]]).
lexarg(cupidus,
      args:[object:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]]).
lexarg(peritus,
      args:[object:[type:np,oblig:no,constraints:[case:gen]]]).
lexarg(peritus,
      args:[object:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]]).

lexarg(alienus,
      args:[prep_cplt:[type:pp,oblig:no,constraints:[case:abl,sem:[hum], prep:ab]]]).

%

adj(meus,me,1,poss,[xxx,xxx]).
adj(tuus,tu,1,poss,[xxx,xxx]).
adj(suus,su,1,poss,[xxx,xxx]).
adj(noster,nostr,1,poss,[xxx,xxx]).
adj(uester,uestr,1,poss,[xxx,xxx]).

% 2
% amabilis, difficilis, facilis,felix, fortis, grandis, ingens, etc.

adj(qualis,qual,2,int,[xxx,xxx]).


adj(amabilis,amabil,2,std,[amabil,amabilissim]).
adj(breuis,breu,2,std,[breu,breuissim]).
adj(difficilis,difficil,2,std,[difficil,difficilim]).
adj(facilis,facil,2,std,[facil,facillim]).
adj(fortis,fort,2,std,[fort,fortissim]).
adj(grandis,grand,2,std,[grand,grandissim]).
adj(grauis,grau,2,std,[grau,grauissim]).


adj(diues,diuit,a2,std,[diuit,ditissim]).
adj(felix,felic,a2,std,[felic,felicissim]).
adj(ingens,ingent,a2,std,[ingent,ingentissim]).
adj(potens,potent,a2,std,[potent,potentissim]).






%%%%%%%
% NOUNS
%%%%%%%

% acqua, agricola, ancilla, corona, dea, epistula, grammatica,
% iustitia, nauta, patientia, pecunia, puella, regina, sapientia,serva, etc.

noun(1,fem,amica,amic,class:common,sem:[hum],[]).
noun(1,fem,ancilla,ancill,class:common,sem:[hum],[]).
noun(1,fem,aqua,aqu,class:common,sem:[thing],[]).
noun(1,fem,auaritia,auariti,class:common,sem:[abstract],[]).
noun(1,fem,cannae,cann,class:common,sem:[city, thing],[nb:pl]). % pluralia tantum
noun(1,fem,corona,coron,class:common,sem:[thing],[]).
noun(1,fem,cura,cur,class:common,sem:[abstract],[]).
noun(1,fem,dea,de,class:common,sem:[hum],[]).
noun(1,fem,divitiae,diviti,class:common,sem:[thing],[nb:pl]).  % pluralia tantum
noun(1,fem,ecclesia,ecclesi,class:common,sem:[hum, thing,loc],[]).
noun(1,fem,epistula,epistul,class:common,sem:[thing, abstract],[]).
noun(1,fem,fortuna,fortun,class:common,sem:[abstract,hum],[]).
noun(1,fem,gallia,galli,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,gloria,glori,class:common,sem:[abstract],[]).
noun(1,fem,grammatica,grammatic,class:common,sem:[abstract],[]).
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
noun(1,fem,puella,puell,class:common,sem:[hum],[]).
noun(1,fem,regina,regin,class:common,sem:[hum],[]).
noun(1,fem,roma,rom,class:common,sem:[city,thing],[nb:sg]).
noun(1,fem,sapientia,sapienti,class:common,sem:[abstract],[]).
noun(1,fem,sardinia,sardini,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,scientia,scienti,class:common,sem:[abstract],[]).
noun(1,fem,serua,seru,class:common,sem:[hum, thing],[]).
noun(1,fem,sicilia,sicili,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,tabula,tabul,class:common,sem:[thing],[]).
noun(1,fem,terra,terr,class:common,sem:[thing,loc],[]).
noun(1,fem,thessalia,thessali,class:common,sem:[loc,thing],[nb:sg]).
noun(1,fem,troia,troi,class:common,sem:[city,thing],[nb:sg]).
noun(1,fem,uia,ui,class:common,sem:[abstract,thing,loc],[]).
noun(1,fem,uictoria,uictori,class:common,sem:[abstract],[]).
noun(1,fem,uita,uit,class:common,sem:[thing, abstract],[]).


noun(1,masc,agricola,agricol,class:common,sem:[hum],[]).
noun(1,masc,belga,belg,class:common,sem:[hum],[]).
noun(1,masc,nauta,naut,class:common,sem:[hum],[]).



% discipulus,dominus, equus, libellus,populus, seruus, etc.

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
noun(2, masc, populus, popul,class:common, sem:[hum],[]).
noun(2, masc, romanus, roman,class:common, sem:[hum],[nb:pl]).
noun(2, masc, seruus, seru,class:common, sem:[hum, thing],[]).
noun(2, masc, troianus, troian,class:common, sem:[hum],[nb:pl]).


% ager, faber, liber, puer, uir, etc.

noun(2, masc, ager, agr,class:common, sem:[thing],[]).
noun(2, masc, faber, fabr,class:common, sem:[hum],[]).
noun(2, masc, liber, libr,class:common, sem:[thing,abstract],[]).
noun(2, masc, magister, magistr,class:common, sem:[hum],[]).
noun(2, masc, puer, puer,class:common, sem:[hum],[]).
noun(2, masc, uir, uir,class:common, sem:[hum],[]).

% fagus

noun(2, fem, fagus, fag,class:common, sem:[thing],[]).

% bellum, documentum, periculum, templum, etc.

noun(2, neuter, auxilium, auxili,class:common, sem:[thing, abstract],[]).
noun(2, neuter, bellum, bell,class:common, sem:[thing],[]).
noun(2, neuter, beneficium, benefici,class:common, sem:[abstract, thing],[]).
noun(2, neuter, bonum, bon,class:common, sem:[thing,abstract],[]).
noun(2, neuter, caelum, cael,class:common, sem:[thing],[]).
noun(2, neuter, documentum, document,class:common, sem:[thing,abstract],[]).
noun(2, neuter, donum, don,class:common, sem:[thing],[]).
noun(2, neuter, fatum, fat,class:common, sem:[abstract],[]).
noun(2, neuter, incommodum, incommod,class:common, sem:[abstract],[]).
noun(2, neuter, ingenium, ingeni,class:common, sem:[abstract],[]).
noun(2, neuter, lugdunum, lugdun,class:common, sem:[city,thing, abstract],[nb:sg]). % only sing
noun(2, neuter, negotium, negoti,class:common, sem:[abstract],[]).
noun(2, neuter, peccatum, peccat,class:common, sem:[abstract],[]).
noun(2, neuter, periculum, pericul,class:common, sem:[thing,abstract],[]).
noun(2, neuter, praemium, praemi,class:common, sem:[thing, abstract],[]).
noun(2, neuter, proelium, proeli,class:common, sem:[thing, abstract],[]).
noun(2, neuter, regnum, regn,class:common, sem:[thing, abstract],[]).
noun(2, neuter, silentium, silenti,class:common, sem:[abstract],[]).
noun(2, neuter, templum, templ,class:common, sem:[thing, loc],[]).
noun(2, neuter, uerbum, uerb,class:common, sem:[abstract],[]).
noun(2, neuter, uestimentum,uestiment,class:common, sem:[thing],[]).
noun(2, neuter, uinum, uin,class:common, sem:[thing],[]).

noun(2, neuter, arma, arm,class:common, sem:[thing],[nb:pl]).  % pluralia tantum
noun(2, neuter, castra, castr,class:common, sem:[thing, loc],[nb:pl]).  % pluralia tantum
noun(2, neuter, milia, mili,class:common, sem:_,[nb:pl]).  % pluralia tantum

% imperator, leo, miles, praetor, rex, etc.

noun(3,masc,amor,amor,um,class:common,sem:[abstract],[]).
noun(3,masc,ciuis,ciu,ium,class:common,sem:[hum],[]).
noun(3,masc,consul,consul,um,class:common,sem:[hum],[]).
noun(3,masc,dolor,dolor,um,class:common,sem:[abstract],[]).
noun(3,masc,dux,duc,um,class:common,sem:[hum],[]).
noun(3,masc,error,error,um,class:common,sem:[abstract],[]).
noun(3,masc,frater,fratr,um,class:common,sem:[hum],[]).
noun(3,masc,homo,homin,um,class:common,sem:[hum],[]).
noun(3,masc,hostis,host,ium,class:common,sem:[hum],[]).
noun(3,masc,ignis,ign,ium,class:common,sem:[thing],[]).
noun(3,masc,imperator,imperator,um,class:common,sem:[hum],[]).
noun(3,masc,leo,leon,um,class:common,sem:[thing, hum],[]).
noun(3,masc,maiores,maior,um,class:common,sem:[hum],[nb:pl]).    % pluralia tantum
noun(3,masc,miles,milit,um,class:common,sem:[hum],[]).
noun(3,masc,mons,mont,ium,class:common,sem:[loc,thing],[]).
noun(3,masc,mos,mor,um,class:common,sem:[abstract],[]).
noun(3,masc,orator,orator,um,class:common,sem:[hum],[]).
noun(3,masc,paries,pariet,um,class:common,sem:[thing],[]).
noun(3,masc,pater,patr,um,class:common,sem:[hum],[]).
noun(3,masc,praetor,praetor,um,class:common,sem:[hum],[]).
noun(3,masc,rex,reg,um,class:common,sem:[hum],[]).
noun(3,masc,senator,senator,um,class:common,sem:[hum],[]).
noun(3,masc,sermo,sermon,um,class:common,sem:[thing, abstract],[]).


% ciuitas,cohors, immortalitas, nux, ratio, salus,urbs, etc.

noun(3,fem,ars,art,ium,class:common,sem:[abstract],[]).
noun(3,fem,caedes,caed,ium,class:common,sem:[abstract],[]).
noun(3,fem,carthago,carthagin,um,class:common, sem:[city,thing, abstract],[nb:sg]).
noun(3,fem,celebritas,celebritat,um,class:common,sem:[abstract],[]).
noun(3,fem,ciuitas,ciuitat,um,class:common,sem:[hum],[]).
noun(3,fem,cohors,cohort,ium,class:common,sem:[hum],[]).
noun(3,fem,cupiditas,cupiditat,um,class:common,sem:[abstract],[]).
noun(3,fem,fines,fin,ium,class:common,sem:[thing, loc],[nb:pl]). % pluralia tantum
noun(3,fem,gens,gent,ium,class:common,sem:[hum],[]).
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
noun(3,fem,ratio,ration,um,class:common,sem:[abstract],[]).
noun(3,fem,salus,salut,um,class:common,sem:[abstract],[]).
noun(3,fem,sollemnitas,sollemnitat,um,class:common,sem:[abstract],[]).
noun(3,fem,uires,uir,ium,class:common,sem:[thing,abstract],[nb:pl]). % pluralia tantum
noun(3,fem,uirtus,uirtut,um,class:common,sem:[abstract],[]).
noun(3,fem,uox,uoc,um,class:common,sem:[abstract],[]).
noun(3,fem,urbs,urb,ium,class:common,sem:[hum,thing,loc],[]).
noun(3,fem,uxor,uxor,um,class:common,sem:[hum],[]).
noun(3,or([masc,fem]),fem,parens,parent,um,class:common,sem:[hum],[]).

% nomen, tegmen, etc.

noun(3,neuter,caput,capit,um,class:common,sem:[thing,hum,abstract],[]).
noun(3,neuter,corpus,corpor,um,class:common,sem:[thing],[]).
noun(3,neuter,facinus,facinor,um,class:common,sem:[abstract],[]).
noun(3,neuter,flumen,flumin,um,class:common,sem:[thing, loc],[]).
noun(3,neuter,iter,itiner,um,class:common,sem:[abstract, loc],[]).
noun(3,neuter,mare,mar,um,class:common,sem:[thing, loc],[nb:sg]).
noun(3,neuter,nomen,nomin,um,class:common,sem:[thing],[]).
noun(3,neuter,opus,oper,um,class:common,sem:[abstract,thing],[]).
noun(3,neuter,os,or,um,class:common,sem:[thing],[]).
noun(3,neuter,pectus,pector,um,class:common,sem:[thing],[]).
noun(3,neuter,scelus,sceler,um,class:common,sem:[abstract],[]).
noun(3,neuter,tegmen,tegmin,um,class:common,sem:[thing, loc],[]).
noun(3,neuter,tempus,tempor,um,class:common,sem:[abstract,thing, time],[]).


% special wordforms for MARE

% lex(mari,noun,[pos:noun,txt:mari,lex:mare, case:abl, gender:neuter, class:common,number:sing,sem:[thing, loc]]).
% lex(maria,noun,[pos:noun,txt:maria,lex:mare, case:or([nom,acc]), gender:neuter, class:common,number:pl,sem:[thing, loc]]).

% exercitus, iuuentus, manus, etc.

noun(4,fem,iuuentus,iuuent,class:common,sem:[abstract, time],[]).
noun(4,fem,manus,man,class:common,sem:[thing],[]).

noun(4,masc,consulatus,consulat,class:common,sem:[abstract, time],[]).
noun(4,masc,exercitus,exercit,class:common,sem:[thing,hum, loc],[]).
noun(4,masc,fructus,fruct,class:common,sem:[thing,abstract],[]).
noun(4,masc,spiritus,spirit,class:common,sem:[abstract],[]).
noun(4,masc,sumptus,sumpt,class:common,sem:[thing,abstract],[]).


% dies, res, spes, etc.

noun(5,or([masc,fem]),dies,di,class:common,sem:[time, thing, abstract],[]).
noun(5,fem,fides,fid,class:common,sem:[abstract],[]).
noun(5,fem,res,r,class:common,sem:[thing, abstract],[]).
noun(5,fem,spes,sp,class:common,sem:[abstract],[]).

% examples of arg-bearing noun
%

% cf args for adjectives

% either we extend the args of scribere with ad+HUM or we associate the AD+NP as prep_cplt of 
% an arg-bearing noun - not BOTH

/*
lexarg(epistula,
      args:[prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]]).

lexarg(litterae,
      args:[prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]]).
*/

lexarg(scientia,
       args:[object:[type:np,oblig:no,constraints:[case:gen]]]).
lexarg(scientia,
        args:[object:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]]).

lexarg(ars,
       args:[object:[type:np,oblig:no,constraints:[case:gen]]]).
lexarg(ars,
        args:[object:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]]).

% euentus eius : his death
lexarg(euentus,
       args:[object:[type:np,oblig:no,constraints:[case:gen]]]).

/*
% included in the phrase negotium agere + gerund

lexarg(negotium,
        args:[object:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]]).
*/

%

%
% NAMES
%

% Gaia, Marcus, etc.  used as proper names for individuals
% therefore no plural generated

noun(1,fem,gaia,gai,class:proper,sem:[hum],[nb:sg]).
noun(1,masc,catilina,catilin,class:proper,sem:[hum],[nb:sg]).

noun(2,masc,augustinus,augustin,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,catullus,catull,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,homerus,homer,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,marcus,marc,class:proper,sem:[hum],[nb:sg]).
noun(2,masc,petrus,petr,class:proper,sem:[hum],[nb:sg]).

noun(3,fem,uenus,uener,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,caesar,caesar,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,cicero,ciceron,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,hamilcar,hamilcar,um,class:proper,sem:[hum],[nb:sg]).
noun(3,masc,hannibal,hannibal,um,class:proper,sem:[hum],[nb:sg]).






%%%%%%%%%
% STRINGS
%%%%%%%%%

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% the phrase_words list should be carefully kept uptodate
% it should contain all items that do not have a lex entry
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

phrase_words([bellique,domi,dubium,
              humi,igni,ius,integro,
              militiaeque,nihil,nil,neque,
              pensi,pili,ruri,sancti,tanti]).

string(phrase,[aqua,et,igni],[lex:aqua_et_igni,w:1]).
string(phrase,[belli,domique],[lex:domi_bellique,w:1, value:time]).
string(phrase,[domi,bellique],[lex:domi_bellique,w:1, value:time]).
string(phrase,[domi,militiaeque],[lex:domi_militiaeque,w:1, value:time]).
string(phrase,[domi],[lex:domi,w:1,value:loc]).
string(phrase,[humi],[lex:humi,w:1,value:loc]).
string(phrase,[in,dubium],[lex:in_dubium,w:1]).
string(phrase,[in,ius],[lex:in_ius,w:1]).
string(phrase,[de,integro],[lex:de_integro,w:1,value:manner_means]).
string(phrase,[militiae,domique],[lex:domi_militiaeque,w:1, value:time]).
string(phrase,[nihil,pensi,neque,sancti],[lex:nihil_pensi_neque_sancti,w:1]).
string(phrase,[nil,pensi,neque,sancti],[lex:nihil_pensi_neque_sancti,w:1]).
string(phrase,[pili],[lex:pili,w:1]).
string(phrase,[tanti],[lex:tanti,w:1]).
string(phrase,[ruri],[lex:ruri,w:1,value:loc]).


%%%%%%%%%%
% DB UTILS
%%%%%%%%%%

% The internal Prolog db (accessed by 'record', 'recorded' and 'erase') can be used as blackboard
% recording the information pertaining to the sentence being parsed,
% as opposed to the 'assert/retract' accessible db, which has the same lifespan as the program itself.


map(Box,FS) :- not(recorded(Box,FS,_)), recorda(Box,FS,_).  % the not recorded is essential !!!!!
                                                              % for keeping track of a 'no new solution' situation
							      % it enables the various runs to come to an end

mapped(Box,X) :- recorded(Box,X,_).




%%%%%%%%%%%%%%%
% Stopping ...
%%%%%%%%%%%%%%%

[stop,halting] ---> [recorded(pos,position(0,1,stop),_),   % this happens when the word stop. is entered
                     recorded(out,Lists,Ref),
                     recorded(alltime,Time,Reftime),
                     nl(Lists), write(Lists,'TOTAL TIME : '), write(Lists,Time), nl(Lists),
                     nl, write('TOTAL TIME : '), write(Time), nl,
                     close(Lists),
                     erase(Ref),
                     erase(Reftime),
                     noprotocol,
                     abort].

% abort returns to Prolog...



% LEXICAL PREDICATES
%%%%%%%%%%%%%%%%%%%%


% specific entries

[lex,necesse] --->[recorded(pos,position(A,B,necesse),_),
                   map(adj,[from:A,to:B,lex:necesse,txt:necesse,type:std,pos:adj,case:or([nom,acc]), 
                             gender:neuter, number:sing, degree:pos])].

[lex,propterea_quod] --->[recorded(pos,position(A,B,propterea),_),
                          recorded(pos,position(B,C,quod),_),
                          map(sub,[from:A,to:C,lex:propterea_quod,pos:sub,argbound:no,mood:_,value:reason])].


[lex,eo_ipso_quod] --->[recorded(pos,position(A,B,eo),_),
                          recorded(pos,position(B,C,ipso),_),
                          recorded(pos,position(C,D,quod),_),
                          map(sub,[from:A,to:D,lex:eo_ipso_quod,pos:sub,argbound:no,mood:_,value:reason])].




% the comma is read as a possible coordinator if it is followed by ET as the next word but one or the next word but two
% filius, mater et pater
% filius bonus, mater bona et pater
% VERY SKETCHY 
% COORDINATION EXPENSIVE AND TO BE MAINTAINED WITHIN STRICT BOUNDS

[lex,comma_as_coord] --->[recorded(pos,position(A,B,','),_),
                          (Next is B+1; Next is B+2),
                          recorded(pos,position(Next,_,et),_),
                          map(coord,[from:A,to:B,lex:comma,pos:coord])].





[lex,words] --->[recorded(pos,position(A,B,Word),_),
                 lex(Word,Box,FS),
                 map(Box,[from:A,to:B|FS])].

/* a record (a feature bundle) is added to the db in the box corresponding to the lexical item's POS.
   The start and end positions of the item in the S are recorded in the first two features; the remaining features
   are read off the lexicon */



[lex,strings] --->[recorded(pos,position(A,B,Word),_),
					% we have the first word of the string
                    string(Box,[Word|OtherWords],FeatureList),
                    match(B,C,OtherWords),
					% we attempt to match all the others
                    append([pathlist:[p(A,C)],txt:[Word|OtherWords]],FeatureList,FullFeatureList),
                    map(Box,FullFeatureList)].
					% we pass the whole list as value for the txt feature

match(BeginPos,EndPos,[W|Ws]) :-
        recorded(pos,position(BeginPos,NextPos,W),_),
        match(NextPos,EndPos,Ws).

match(EndPos,EndPos,[]).






% PARSE FLAG SETTINGS
%%%%%%%%%%%%%%%%%%%%%%

% These flags are meant to speed up the parsing process -
% we flag the rules dealing with gaps (in relative clauses),
% coordination and nonfinite clauses.

% another flag is set for the SE family right at input (in the process turning string into wordlist)

[lex,flags] ---> [mapped(relative,_),			% we have a relative pronoun somewhere in the S
                  map(flags,active(gap))].

[lex,flags] ---> [mapped(coord,_),			% we have a coordinator somewhere in the S
                  map(flags,active(coord))].

[lex,flags] ---> [mapped(v,FSverb),			% we have a nonfinite verb form (infinitive) somewhere in the S
                  constraint([type:nonfinite],FSverb),
                  map(flags,active(nonfinite))].

% the following clauses are necessary because ESSE can be understood : amatum, dicturum, delendam,...

[lex,flags] ---> [mapped(p_p,_),			% we have a nonfinite verb form (past participle) somewhere in the S
                  map(flags,active(nonfinite))].

[lex,flags] ---> [mapped(p_f,_),			% we have a nonfinite verb form (future participle) somewhere in the S
                  map(flags,active(nonfinite))].


[lex,flags] ---> [mapped(gdiv,_),			% we have a nonfinite verb form (gerundive) somewhere in the S
                  map(flags,active(nonfinite))].




% correlatives

corr(magis_quam, magis, quam).
corr(tam_quam, tam, quam).

% Note the following lex clause, generated by makelex:
                           % lex(quam,  coord,   [lex:quam,  pos:coord,type:correlative]).      





%%%%%%%%%%%%%%%%%%%%%%
% GRAMMAR PREDICATES
%%%%%%%%%%%%%%%%%%%%%%


% CLAUSE EXPANSIONS
%%%%%%%%%%%%%%%%%%%


% *****VERY EXPENSIVE**** because not linked to anything in particular
% every time a clause is built, it is ready for expansion
% and so is a clause that has already been expanded

% but we need to allow for pps , adjuncts not further specified, adverbials relating to parts of the predication (graviter angi)
% and advps of the clausal type such as autem or etiam
% and ablative absolute, of course


% The full expansion set should be reserved for finite clauses (the ablative absolute should not be thought as a normal expansion
% for a gerundive clause or another ablative absolute), but now we use a single expand process, so the aa IS included

% The expansions are meant to cover grammatical structures that are not arguments,
% i.e. not linked to specific predicates in the lexicon.

% It is therefore not surprising that all these elements should be optional.



full_expansions([ clause_level_adjunct_1:[type:adjunct,    constraints:[value:purpose]], % videndae urbis in montibus positae, pacis petendae, etc.
                  clause_level_adjunct_2:[type:adjunct,    constraints:[value:time]],      % tres annos, ...
                  clause_level_adjunct_3:[type:adjunct,    constraints:[value:place]],     % Romae
                  clause_level_adjunct_4:[type:adjunct,    constraints:[value:manner_means]],  % legendo libros
                  clause_level_adjunct_5:[type:advp,       constraints:[type:or([vpbound,clausal]),sem:or([eval,time,place])]],   %  graviter
		  clause_level_adjunct_6:[type:advp,       constraints:[type:int]],
		  clause_level_adjunct_7:[type:pp,         constraints:[prep:or([ante,apud,causa,circum,contra,cum,de,ex,inter,intra,
				                              ob,praeter,pro,propter,sine,sub,trans])]],         % cum magna voce
                  clause_level_adjunct_8:[type:pp,         constraints:[prep:in, case:abl]],  % in horto, in ecclesia tua

				% the idea is to move the preps towards the type of adjuncts they occur in
				% this is not readily feasible
                                % for the time being PER has been removed to join the time adjuncts
                                % some prepositions are rather unlikely in expansions, namely in+acc and ad+acc
                                % although the latter is found in 'consules victi ad Cannas' (where ad=apud)
                                % such a case is better left out to avoid spurious parses

                  linker:[type:advp,                       constraints:[type:clausal,sem:discourse]],
                  dativus_ethicus:[type:np,                constraints:[case:dat,lex:or([pp1sg,pp2sg,pp1pl,pp2pl])]],
                  ablative_absolute:[type:pred,            constraints:[type:aa]]    ]). % rege sub tegmine fagi recubante





partial_expansions([]). % dummy




% A FEW REMARKS BEFORE PLUNGING...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Let's make clear, at the outset of this grammar section, what the 'pathlist', 'distance' and 'w(eight)'
% features are meant to convey.

% 'pathlist' has as value a list of 'p(X,Y)' structures, where X and Y are string positions,
% each one of these structures covering a portion of the input string.
% The pathlist will be used to ensure that the whole string is covered by the proposed parse (when all local pathlists are
% appended into one long one); it is also used to establish adjacency, contiguity, and other path properties that the
% parser needs to know about, partly in order to work out the value of the next feature discussed here,

% namely 'distance' - distance is computed on the basis of the distance between the members of a grammatical structure,
% if such a distance is felt to be a possible cause of straining, i.e. the stylistically felt non-contiguity that might
% lead to the downscaling of the likelihood of a given parse being the correct one.
% Distances are cumulated and yield the 'straining factor' for the input string under a given parse.

% The 'weight' feature works in the opposite direction - it makes a parse more likely to be adopted, by specifying the
% coherence factor that the belonging of a structure to a whole brings to that whole. It is easy to understand that
% the final weight given to a parse (supposed to be the degree of confidence it inspires of being right) will be computed on
% the basis of subtracting the straining factor from the weight accumulated by the individual weights of the constituents.

% Let's also look at the 'constraint' and 'map' predicates, which are used throughout the parsing process.
% 'constraint' is a two-place predicate that takes as arguments feature lists -
% it can be used to retrieve feature values or to impose them.

% The very first time we meet it here, it is used to retrieve feature values such as that for the feature 'lex';
% such features are supposed to be part of the lexical entry for the adj, which appears here as the FS variable
% (feature structure).

% We check the availability of the FS structure by means of the 'mapped' predicate, which is just sugar for 'recorded',
% the Prolog predicate that retrieves info stored on the fly, here the info associated with a wordform occurring in
% the string at a certain position (between A and B we are looking for an adj - note that A and B are free variables at
% this stage - it is the call to 'mapped' that is going to instantiate them and yield (if it is available!) the feature bundle
% associated with the adj. Note that 'adj', the part of speech, is the name of the record 'box' or 'folder' where we store
% information pertaining to adjectives.

% These boxes can also be created by the grammar - they are not necessarily lexical.
% As a matter of fact, each of the grammar 'rules' of the parser ends up by storing a new structure in its own box,
% so that this parser pass, or a following one, has access to it.

% This creating of a new structure to be stored in its own box is the responsibility of the 'map' predicate -
% 'map' is essentially sugar for 'recorda' (used in Prolog to store info on the fly, info to be retrieved
% by the 'recorded' predicate mentioned just above) but with an important proviso -
% the info is NOT stored if it is already there in the box.

% This procedure ensures control of the parsing process, because for each pass, the parsing process is restarted,
% until it fails because it cannot add anything new -
% the control on this 'not adding anything new' is precisely the one built in in 'map', which does not
% succeed if it doesn't have anything new to store.


% Let's start





%%%%%%%%%%%%%%%%%%%
% ADJECTIVE PHRASES
%%%%%%%%%%%%%%%%%%%

% standard adj (bonae)
%%%%%%%%%%%%%%%%%%%%%%


% lex(bonae, adj, [pos:adj, txt:bonae, lex:bonus, type:std, case:dat, gender:fem, number:sing, degree:pos]).


[adj,adjp1] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([pos:adj, case:C, number:N, gender:G, type:Type, lex:Lex],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Lex,type:Type,w:1])].


% fortunatos nimium
%%%%%%%%%%%%%%%%%%%

 [adj,adjp1aa] --->
 [mapped(adjp,ADJP),

 constraint([pathlist:PADJ,distance:[0],hp:HP,case:C,number:N,gender:G,
             lex:LexADJ,type:Type,w:1], ADJP),
 mapped(advp,ADVP),
 constraint([pathlist:PADV,type:adjbound,lex:LexADV],ADVP),
 append(PADJ,PADV,Path),
 contiguous(Path),							% before or after

 map(adjp,[cat:adjp,pathlist:Path,distance:[0],hp:HP,
             case:C,number:N, gender:G,lex:LexADJ,type:Type,w:2,c_str:[LexADV,LexADJ]])
].



% comparative adj with ablative comp_cplt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ... doctior Petro
% lex(doctior, adj, [pos:adj, txt:doctior, lex:doctus, type:std, case:nom, gender:masc, number:sing, degree:comp]).

[finite,adjp1a] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([pos:adj, case:Case, number:N, gender:G, type:Type, lex:Lex, degree:comp],FS),
						% comparative degree needed
 constraint([case:or([nom,acc])],[case:Case]),
						% ablative after comparative restricted to nominative or accusative adjective
 mapped(np,FSnp),
 constraint([pathlist:PL1,hp:HL1,distance:Distance,w:W,case:abl],FSnp),  % we need an ablative np
  HL1 \= [],                      % avoid dummy nps as antecedents of relative clauses
                                  % this is one way of catching them - they do not cover head string position
 append([p(A,B)],PL1,Path),
 msort(Path,Sorted),             % a useful duo : sorting the path and making sure it does not feature duplicates
 \+ dup(Sorted),
 % Weight is W+1,			% weight is derived from np and increased to account for the adjective
 myplus(W,1,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,distance:Distance,hp:[p(A,B)],
             case:Case,number:N, gender:G,lex:Lex,type:Type,w:Weight,
             c_str:[Lex,comp_cplt:FSnp]])].

 % here we see the introduction of a c_str(constituent structure) feature to record info to be displayed in the parse tree
 % we note that adjective phrases (and also noun phrases) are sometimes assigned a c_str feature, and sometimes not
 % if we wish to retrieve info from the value of that feature, we will have to take care of the possibility of the feature
 % not being present in the info box associated with the phrase



% comparative adj with comp_cplt introduced by quam 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - note how the presence of QUAM slows down the parsing process
% Quam is very ambiguous, and can also be a coordinator as the second member of a correlative pair (tam ... quam, magis ... quam))

% ... doctior quam Petrus

[finite,adjp1a] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([pos:adj, case:Case, number:N, gender:G, type:Type, lex:Lex, degree:comp],FS),
 mapped(coord,[from:C,to:D|FScoord]),  % quam is registered as a coordinator
 constraint([lex:quam],FScoord),      % we insist on having 'quam', nothing else
 mapped(np,FSnp),
 constraint([pathlist:PL1,hp:HL1,distance:Distance,w:W,case:Case],FSnp),
 HL1 \= [],                      % avoid dummy nps as antecedents of relative clauses
 start(PL1,D),			 % np directly follows quam
				 % for such checks we use the 'start' and 'extremity' predicates
 append([[p(A,B)],[p(C,D)]],PL1,Path),  % the quam needs to enter the path, too
 msort(Path,Sorted),
 \+ dup(Sorted),
 % Weight is W+1,
 myplus(W,1,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,distance:Distance,hp:[p(A,B)],
             case:Case,number:N, gender:G,lex:Lex,type:Type,w:Weight,
             c_str:[Lex,comp_cplt:FSnp]])].


% superlative with genitive sup_cplt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Omnium fortissimi sunt Belgae.
% lex(fortissimi, adj, [pos:adj, txt:fortissimi, lex:fortis, type:std, case:gen, gender:masc, number:sing, degree:sup]).


[finite,adjp1asup] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([pos:adj, case:Case, number:N, gender:G, type:Type, lex:Lex, degree:sup],FS), % we have a superlative
 mapped(np,FSnp),
 constraint([pathlist:PL1,hp:HL1,distance:Distance,w:W,case:gen],FSnp),  % genitive case needed
 HL1 \= [],                      % avoid dummy nps as antecedents of relative clauses
 append([p(A,B)],PL1,Path),
 msort(Path,Sorted),
 \+ dup(Sorted),
 % Weight is W+1,
 myplus(W,1,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,distance:Distance,hp:[p(A,B)],
             case:Case,number:N, gender:G,lex:Lex,type:Type,w:Weight,
             c_str:[Lex,sup_cplt:FSnp]])].




% the three categories below were meant to apply to gerundives, present and future participles used alone,
% without their arg structure  (and therefore reducible to adjective status ?????? - a hoary issue...)

% gerundives
%%%%%%%%%%%%

% delenda
% lex(delenda, gdiv, [pos:gdiv, txt:delenda, case:nom, gender:fem, number:sing,
%                     lex:delere, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).



[adj,gdiv] --->
[ mapped(gdiv,[from:A,to:B|FS]),
				% gdiv gerundives (NOT gerund)
 constraint([case:C, number:N, gender:G, txt:Txt,lex:Lex],FS),
 \+constraint([c_str:_],FS), % the gdiv is not coordinate !

% map(gerundive,[cat:gerundive,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
           %  case:C,number:N, gender:G,type:gdiv,lex:Lex,w:1]),

 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,type:gdiv,lex:Txt,w:1])
 ].



% present participle
%%%%%%%%%%%%%%%%%%%%

% recubans
% lex(recubans, p_pr, [pos:p_pr, txt:recubans, case:nom, gender:_, number:sing,
%                      lex:recubare, class:intr, type:p_pr, kind:std, mood:participle, person:3]).


/*
[adj,adjp1ab] --->
[ mapped(p_pr,[from:A,to:B|FS]),
				% present participle p_pr
 constraint([case:C, number:N, gender:G, lex:Lex],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Lex,type:ppr,w:1,
             c_str:[head:FS]])].
*/

% Register those, where needed, as simple adjectives, not present participles.
% If they are apt to take arguments, they are dealt with in the 'participial clause' section below.


% future participle
%%%%%%%%%%%%%%%%%%%

% amaturus
% lex(amaturus, p_f, [pos:p_f, txt:amaturus, case:nom, gender:masc, number:sing,
%                     lex:amare, class:tr_cod, type:p_f, kind:std, mood:participle, person:3]).


[adj,adjp1ac] --->
[ mapped(p_f,[from:A,to:B|FS]),
				% future participle p_f
 constraint([case:C, number:N, gender:G, lex:Lex],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Lex,type:ppf,morph:ppf,w:1])].

% the type and morph features carry the same info, namely that we have to do with a future participle
% the difference being that reference to the type feature is made elsewhere in the grammar,
% but the type feature does not get printed, whereas the morph feature does (but is not referred to elsewhere)


% past participle
%%%%%%%%%%%%%%%%%%

% The past participle needs to be recorded as an adjective - it would not do
% to have to specify the past participles that can be used as adjectives
% as if they were special cases
% but we need to prioritize passive voice when the p_p occurs with a form of esse

% amatus
% lex(amatus, p_p, [pos:p_p, txt:amatus, case:nom, gender:masc, number:sing,
%                   lex:amare, class:tr_cod, type:p_p, kind:std, mood:participle, person:3]).


[adj,adjp1acx] --->
[ mapped(p_p,[from:A,to:B|FS]),
				% past participle p_p
 constraint([case:C, number:N, gender:G, txt:Txt,lex:Lex],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Txt,type:ppt,morph:ppt,w:0])].

% type and morph : see above
% 0 weight as against true passives - which, included in the verb phrase, get their weight there



% arg-bearing adj
%%%%%%%%%%%%%%%%%%


% true adj with arg structure
% ... cupidus pecuniae

% lex(cupidus, adj, [pos:adj, txt:cupidus, lex:cupidus, type:std, case:nom, gender:masc, number:sing, degree:pos]).

% lexarg(cupidus,
%      args:[object:[type:np,oblig:no,constraints:[case:gen]]]).
% lexarg(cupidus,
%      args:[object:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]]).


[finite,adj] --->
[mapped(adjp,Fadjp),
 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],
            number:Nb,gender:G,type:std,case:C,lex:Lex], Fadjp),
 lexarg(Lex,args:Args),
		   % connecting with the arglist via lexarg/2
 expand(Args,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
								   % expand is looking for the args
							           % like match_list, the other predicate meant to check arg
								   % satisfaction (for verbal predicates), it assigns trees, paths,
								   % distances and weights
 flatten(ExpandPaths,Flat),           % the flatten predicate is used as a safety measure - we need to avoid lists with lists
				      % as list elements
 msort(Flat,Sorted),
 adjacent(PL1,Sorted),			% args before or after adj, but contiguous
                                        % this is what the adjacency predicate is for
 \+dup(Sorted),

 contiguous(Sorted),                    % contiguity of the arguments - they cannot be dispersed all over the clause
                                         % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


 insort(ExpandTrees,ST),                % insort puts args in canonical order - the parse is then much simpler to work with
					% for procedures which would have to start from it; it also helps the human reader
					% who should not be worried about the word order of the input string not being respected
					% the string is still there !!!!!!
 flatten(ExpandDistances,ED),
 sum(ED,Distance),                      % distances are integers in a list, easy to get the total of
 % Dis is Distnp1+Distance,               % they come from two sources
  myplus(Distnp1,Distance,Dis),       % myplus guards against uninstantiated variables, which throw up an error
                                      % if standard 'is' is used
 append(PL1,Sorted,Path),
 % W is Weight+3,
 myplus(Weight,3,W),                     % bonus given to args as opposed to info ending up in non-args, i.e. adjuncts
 map(adjp,[pathlist:Path,hp:HL1,distance:[Dis],
           cat:adjp,w:W,type:complex,			% complex type to make sure we do not expect it any place an adjective can fit
           number:Nb,gender:G, lex:Lex,
           case:C,c_str:[Lex,ST]])].              % the constituent structure is the head adj followed by its arguments






% present participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mapped as BOTH participle clause and NP
%----------------------------------------

% ... sub tegmine fagi recubante

[finite,adj1] --->
[mapped(p_pr,[from:A,to:B|FS]),				% present participle
 constraint([number:Nb,gender:G,case:C,txt:Txt,lex:Clex], FS),
 lexarg(Clex,arglist:ArgList),
					 % connecting with the arglist via lexarg/2

 % selecting the subject to remove it from the arglist:              % the subject is not be found in the np
								     % the participial clause gets attached to
   pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),  % finding the arglist
   pick(subject:_,Args,NewArgs),                             % exploring it


 expand(NewArgs,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),

 % Sorted \= [],               % not reduced to the present participle alone, which is then best viewed as an adjective
			       % see the stance taken above :
                               % although we could simply register as adj, not present participles,
                               % those participles that do not need any arg position filled

 % adjacent([p(A,B)],Sorted),  % args before or after adj, but contiguous
 \+dup(Sorted),
% contiguous(Sorted),        % too strict - we may also try 'quasicontiguous'
			     %  which allows for one word to get inserted
			     %  if we wish to parse Virgil's patulae recubans sub tegmine fagi...
                             % see below contiguity test when the expansions are added
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Dis),
 append([p(A,B)],Sorted,Path),
 % W is Weight+MW+1,
 myplus(Weight,MW,T1),
 myplus(T1,1,W),

 % bonus weight for clause over np
 myplus(W,2,WeightClause),


 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts

  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Dis,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(WeightClause,WExpandN,WWC),
  myplus(W,WExpandN,WW),



map(participle_clause,[pathlist:NSorted,hp:[p(A,B)],distance:[NDistance],
           cat:ppclause,w:WWC,type:pprcentered,
           number:Nb,gender:G, lex:Lex,
           case:C,c_str:[Txt,NST]]),


	ifthenelse(G=masc,NPSem=[hum],NPSem=[thing]), % if it's not a neuter, treat as +HUM

map(np,[pathlist:NSorted,hp:[p(A,B)],
           index:i(p(A,B)),distance:[NDistance],
           cat:np,class:_,sem:NPSem,
           number:Nb,person:3,gender:G, type:_,lex:Lex,lextype:full,
           case:C,w:WW,
           c_str:[present_participle,Lex,NST]])    ].




% deponent past participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mapped as participle clause only
%---------------------------------

% Rex imperatorem urbem cepisse ratus insanivit.

% in the lexicon we have entries such as:
% lex(ratus,
%      p_p,
%      [pos:p_p, txt:ratus, case:nom, gender:masc, number:sing, lex:reri, class:tr_inf,
%      type:p_p, kind:dep, mood:participle, person:3]).

[finite,adj1] --->
[mapped(p_p,[from:X,to:Y|FS]),                           % past participle of a deponent verb

 constraint([case:C, number:Nb, gender:G, txt:Txt,lex:Clex, kind:dep],FS),   % we insist on a deponent verb
								     % this is NOT passive voice !

 lexarg(Clex,arglist:ArgList),  % connecting with the arglist via lexarg/2

 % selecting the subject to remove it from the arglist:
   pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
   pick(subject:_,Args,NewArgs),

 expand(NewArgs,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
 Sorted \= [],               % not reduced to the past participle alone, which is then best viewed as an adjective
 adjacent([p(X,Y)],Sorted),  % args before or after pp, but contiguous
 \+dup(Sorted),
% contiguous(Sorted),
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Distance),
 append([p(X,Y)],Sorted,Path),


 % W is Weight+MW+1,
 myplus(Weight,MW,T1),
 myplus(T1,1,W),

 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts

 ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Distance,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
   contiguous(NSorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(W,WExpandN,WW),


 map(participle_clause,[pathlist:NSorted,hp:[p(X,Y)],distance:[NDistance],
           cat:ppclause,w:WW,type:depppcentered,
           number:Nb,gender:G, lex:Lex,
           case:C,c_str:[Txt,NST]])].




% standard past participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mapped as both participle clause and NP
%----------------------------------------

% Rex a regina amatus epistulas longas ad servam Marci scribit.

% we have entries such as:
% lex(amatus,
%      p_p,
%      [pos:p_p, txt:amatus, case:nom, gender:masc, number:sing, lex:amare, class:tr_cod, type:p_p,
%       kind:std, mood:participle, person:3]).

[finite,adj2] --->
[mapped(p_p,[from:X,to:Y|FS]),                            % a true past participle involved in passive voice
 constraint([case:C, number:Nb, gender:G, txt:Txt,lex:Clex, kind:std],FS),

 lexarg(Clex,arglist:ArgList),
 pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),

 % tinkering with the arglist:

 % selecting the object to remove it from the arglist:

 pick(object:_,Args,Args1),  % note that there must be an object if a pp was produced !!

 % turning the subject into an optional (a+) abl pp arg

 pick(subject:SubjectSpecs,Args1,Args2),
 pick(constraints:Sconstraints,SubjectSpecs,_),

 % in the lines below we insist on ab+ablative for people, and simple ablative for non-people

 ifthenelse( Sconstraints=[],                 % no constraint on subj: both types of agent are OK  IF-CLAUSE
             ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE   this is an OR : both are OK
              NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,

                                                                                   % ELSE-CLAUSE:
              ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                            NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                            NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause

 append([agent:NewArg],Args2,Args3),   % adding it to the args

 expand(Args3,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
 Sorted \= [],               % not reduced to the past participle alone, which is then best viewed as an adjective
 adjacent([p(X,Y)],Sorted),  % args before or after adj, but contiguous
 \+dup(Sorted),
 % contiguous(Sorted),
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Distance),
 append([p(X,Y)],Sorted,Path),
 % W is Weight+MW+1,
 myplus(Weight,MW,T1),
 myplus(T1,1,W),

 % bonus weight for clause over np
 myplus(W,5,WeightClause),


 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts

  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Distance,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(WeightClause,WExpandN,WWC),
  myplus(W,WExpandN,WW),


map(participle_clause,[pathlist:NSorted,hp:[p(X,Y)],distance:[NDistance],
           cat:ppclause,w:WWC,type:ppcentered,
           number:Nb,gender:G, lex:Lex,
           case:C,c_str:[Txt,NST]]),

	ifthenelse((G=masc;G=fem),NPSem=[hum],NPSem=[thing]), % if it's not a neuter, treat as +HUM

map(np,[pathlist:NSorted,hp:[p(X,Y)],
           index:i(p(X,Y)),distance:[NDistance],
           cat:np,class:_,sem:NPSem,
           number:Nb,person:3,gender:G, type:_,lex:Lex,lextype:full,
           case:C,w:WW,
           c_str:[past_participle,Lex,NST]])    ].




% future participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mapped as participle clause only
%----------------------------------


% Regem epistulas longas ad servam Marci scripturum regina amare coepit.

[finite,adj3] --->
[mapped(adjp,Fadjp),                                    % the future participle IS registered as an adjective
 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],
            number:Nb,gender:G,case:C,txt:Txt,lex:Clex,type:ppf], Fadjp),

 lexarg(Clex,arglist:ArgList), % connecting with the arglist via lexarg/2

 % selecting the subject to remove it from the arglist:
   pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
   pick(subject:_,Args,NewArgs),


 expand(NewArgs,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
 Sorted \= [],               % not reduced to the future participle alone,
			% which is then best viewed as an adjective pure and simple (which it is...)

 % adjacent(PL1,Sorted),  % args before or after adj, but contiguous
 \+dup(Sorted),
% contiguous(Sorted),
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Distance),
 % Dis is Distnp1+Distance,
 myplus(Distnp1,Distance,Dis),
 append(PL1,Sorted,Path),
 % W is Weight+MW+1,
 myplus(Weight,MW,T1),
 myplus(T1,1,W),

 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts

  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Dis,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(W,WExpandN,WW),


 map(participle_clause,[pathlist:NSorted,hp:HL1,distance:[NDistance],
           cat:ppclause,w:WW,type:fpcentered,
           number:Nb,gender:G, lex:Lex,
           case:C,c_str:[Txt,NST]])].




%%%%%%%%%%%%%%%%%%%%
% ADVERBIAL PHRASES
%%%%%%%%%%%%%%%%%%%%

% Adverbial phrases are elementary in this parser -
% they are simply derived from adverbs 'tout court'.

% The semantic value and type are likely to be the most useful features to attach the adverb where it belongs
% and specify its contribution to adjuncts.

% adverbial phrases built out of a single adverb such as semper
% lex(semper,	adv,	[lex:semper,	pos:adv,	type:vpbound,		sem:time]).
% lex(modo,     adv,	[lex:modo,      pos:adv,	type:vpbound,		sem:time]).
% lex(grauiter, adv,	[lex:grauiter,  pos:adv,	type:vpbound,	        sem:manner_means]).

[core,adv1] --->
[mapped(adv,[from:A,to:B|FS]),
 constraint([type:Type, lex:Lex, sem:Sem],FS),
 map(advp,[cat:advp,pathlist:[p(A,B)],distance:[0],
             type:Type,lex:Lex,sem:Sem,w:1, c_str:[head:Lex]])].




%%%%%%%%%%%%%%%
% NOUN PHRASES
%%%%%%%%%%%%%%%


% the core NPS are assembled before the other NPs, for which they can serve as building blocks
% there are indeed two passes for nps : core and finite
% the core NPs are simple nps that do not involve predications
% therefore no relatives, no arg-bearing nouns, just the simple buiding blocks:
% nouns as nps, names as nps, adj+n as np, and so on...

% Each np is associated with an index which refers to the positions it spans in the input string
% The index is useful to make sense of gaps, i.e. traces (t or e in syntactic parlance) 'left' by elements
% 'moved out of place' by 'transformations'. The quotes are meant to show distance with respect to the syntactic
% theory underlining such treatment.

% But undoubtedly a similar treatment is needed. If the trace cannot be associated with the relative pronoun, and,
% via the relative, and more importantly, the antecedent, all the controls we wish to perform, such as semantic controls
% on arg bearers, will prove impossible in relative clauses, to give one example.





% simple NP
%%%%%%%%%%%

% rex
% lex(rex, noun, [pos:noun, txt:rex, lex:rex, case:nom, gender:masc, class:common, number:sing, sem:[hum]]).


[core,np1] --->
[mapped(noun,[from:A,to:B|FS]),
 constraint([pos:noun,lex:Lex,class:common,sem:Sem,txt:Text,
            number:Nb,gender:G,case:C],FS),

 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:common,lextype:full,
           number:Nb,person:3,gender:G, type:core,lex:Lex,txt:Text,
           case:C,w:1])].


% np opening the sentence with relative adjective
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Quas urbes hostis diripere coepit.
% lex(quas, relative, [pos:relative, txt:quas, lex:relaccfempl,gender:fem,
%                      case:acc, number:pl, function:[object,prep_cplt,subject]]).

[core,np1a] --->
 [mapped(relative,[from:0,to:1,pos:relative, txt:_, lex:_,gender:G,
                    case:C, number:Nb, function:_]),
							% positions 0 to 1 - it must be the very first word

 mapped(noun,[from:1,to:2|FS]),                          % immediately followed by a noun
							% this is likely to prove too strict - we may use a more relaxed
						        % type of adjacency (2 to 3 as well as 1 to 2, for instance)
 constraint([pos:noun,lex:Lex,class:common,sem:Sem,
            number:Nb,gender:G,case:C],FS),             % case number and gender agreement : the usual agreement triplet

 map(np,[pathlist:[p(0,2)],hp:[p(1,2)],index:i(p(1,2)),distance:[0],
           cat:np,sem:Sem,class:common,lextype:full,			% lextype:full is meant to prevent further modification
           number:Nb,person:3,gender:G, type:core,lex:Lex,
           case:C,w:1,c_str:[relative_adj,head:FS]])].


% Adjectives as NPS : people (boni etc...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% much better to deal with them as straightforward nouns, to avoid overgeneration
% the list should be established on the basis of what we find in classical texts

/*
[core,adjpasnp] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([pos:adj, case:C, number:pl, gender:masc, lex:Lex],FS),   % plural and masc
 map(np,[cat:np,pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
             index:i(p(A,B)),sem:[hum],person:3,lextype:full,
             case:C,number:pl, gender:masc,lex:Lex,class:common,type:core,w:1,
             c_str:[head:FS]])].
*/

% Adjectives as NPS : abstract (bonum, uilia, etc...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% here too a restricted class of adjectives seems to fit the part;
% probably better to enter them as nouns, too

/*
[core,adjpasnp1] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([pos:adj, case:C, number:Nb, gender:neuter, lex:Lex],FS),  % neuter
 map(np,[cat:np,pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
             index:i(p(A,B)),sem:[abstract],person:3,lextype:full,
             case:C,number:Nb, gender:neuter,lex:Lex,class:common,type:core,w:1,
             c_str:[head:FS]])].
*/




% MWUs
%%%%%%

% MULTIPLE WORD UNITS are a very important component of the lexicon of any language
% The lexicographical ressources for Latin may not yet be fully adequate
% the mwu's must not only be registered but their degree of syntactic frozenness and their
% reluctance to open themselves to lexical variation must be carefully investigated and
% properly recorded - the task is particularly hard in the case of dead languages,
% but also made easier by the fact that we may very well deem ourselves satisfied if we
% can account for what is found in text (not, of course, by means of the totally adhoc
% listing of elements)


% Example compound entries : RES PUBLICA and RES NOVAE

% we can increase the weight (3 instead of 1+1)
% if we wish to privilege the compound reading as against the adj-noun pair
% we need to register the weight when filling in the arg slot of the arg-bearer

% one should not wonder at the mix between lexis and grammar here
% all non-frozen mwus, be they np compounds or more elaborate structures,
% need to have an entry in the grammar, at exactly the right level,
% to account for the amount of variation they are ready to accept
% while retaining their mwu reading (the whole problem in a nutshell...)

% there should be (hundreds of) thousands of such entries
% no joke !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

% RES PUBLICA
[core,np2ai] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adj,[from:B, to:C|FSadj]),    % not publica res

 constraint([number:Nb,gender:fem,case:Case,lex:res],FSnoun),
 constraint([number:Nb,gender:fem,case:Case,lex:publicus],FSadj),
 map(np,[pathlist:[p(A,C)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],cat:np,class:common,sem:[abstract,hum],
          number:Nb,person:3,gender:fem,type:core,lex:res_publica,lextype:full,
          case:Case,w:3])].


% RES NOVAE (revolution)
% cupiditate regni adductus novis rebus studebat (Caesar, De Bello Gallico, 1.9.3)
[core,np2aii] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adj,[from:X, to:Y|FSadj]),
 constraint([number:pl,gender:fem,case:Case,lex:res],FSnoun),  % plural needed, of course
 constraint([number:pl,gender:fem,case:Case,lex:nouus],FSadj),
 adjacent([p(A,B)],[p(X,Y)]), % adjacency required res nouae or nouae res
 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),
 map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[0],cat:np,class:common,sem:[abstract],
          number:pl,person:3,gender:fem,type:core,lex:res_novae,lextype:full,
          case:Case,w:3])].


% VIA SACRA, SACRA VIA
% on account of the two word orders, we only require adjacency

[core,np2aiii] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),
 constraint([number:sing,gender:fem,case:Case,lex:uia],FSnoun),    % sing and fem, of course
 constraint([number:sing,gender:fem,case:Case,lex:sacer],FSadj),
 adjacent([p(A,B)],[p(X,Y)]),
 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),
 map(np,[pathlist:Sorted,hp:[p(X,Y)],index:i(p(X,Y)),distance:[0],cat:np,class:common,sem:[loc],
          number:sing,person:3,gender:fem,type:core,lex:sacra_uia,lextype:full,
          case:Case,w:3])].



% Much more open: various cases of appositions (but not FREE appositions - the word order is a good clue
% to the restricted character of such groupings

% URBS ROMA etc.
[core,np2aiv] --->
[mapped(noun,[from:A, to:B|FSnoun1]),
 mapped(noun,[from:B, to:C|FSnoun2]),

 constraint([number:sing,case:Case,lex:urbs],FSnoun1),				% urbs in the sing

 constraint([number:sing,gender:fem,case:Case,sem:[city],lex:Lex],FSnoun2),     % any city
                                                                                % we can use the sem feature
										% remember that the value of the sem feature
										% should always be a list

 map(np,[pathlist:[p(A,C)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],cat:np,class:common,sem:[city],
          number:sing,person:3,gender:fem,type:core,lex:Lex,lextype:full,
          case:Case,w:2])].

% in the lex we do not bother with the word urbs



% WITH INTERROGATIVE ADJ
%%%%%%%%%%%%%%%%%%%%%%%%%

[core,np2adjint] --->
[mapped(noun,[from:B, to:C|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),

 constraint([number:Nb,gender:Gender,case:Case,class:Class,sem:Sem,lex:LexNoun],FSnoun),
 constraint([number:Nb,gender:Gender,case:Case,type:int,lex:_],FSadj),                % interrogative

 map(np,[pathlist:[p(A,C)],hp:[p(B,C)],index:i(p(B,C)),distance:[0],cat:np,class:Class,sem:Sem,
          number:Nb,person:3,gender:Gender,type:int,lex:LexNoun,lextype:full,
          case:Case,w:4,c_str:[head:FSnoun,adjp:FSadj]])].

% needs heavy weight to compete, as it carries the burden of signalling a wh_question
% and wh-forms have a knack of doing all sorts of things
% (relatives without antecedent, 'relatif de liaison', ...)



% STANDARD
%%%%%%%%%%

% with adjective phrase - the np cannot be core on account of the args that the adjective can bear,
% e.g. gerunds with adj such as cupidus



% adj phrase following the noun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np2a] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adjp,FSadj),

 constraint([number:Nb,gender:Gender,case:Case,class:Class,sem:Sem,lex:LexNoun],FSnoun),
 ifthen(mapped(prep,[from:X,to:A|_]),(Case=acc;Case=gen;Case=abl)),                       % for efficiency reasons - see above
 constraint([number:Nb,gender:Gender,case:Case,lex:LexAdj,type:Type,w:W],FSadj),
 Type \= int,					% interrogative adjectives dealt with separately - they need heavy weight
 constraint([pathlist:Padj],FSadj),

 append([p(A,B)],Padj,Pnp),
 extremity(Padj,Ext),
 Ext > B,                             % here the adj phrase follows the noun
 distance([p(A,B)],Padj,Distance),    % the distance between the noun and the path of the adjective
                                      % determines the straining factor as well as helping to decide
				      % whether noun and adj DO belong together

 ifthen(LexAdj=is, Distance=0),    % is/ea/id adjacent - this requirement is too strong

 msort(Pnp, Sorted),
 \+dup(Sorted),

 ifthenelse( recorded(stg,[texttype,1]),  % texttype 1 is prose
                                        Distance < 4, Distance < 6) ,        % 3 is thus the maximum distance
				                                             % between adj and noun in prose

 % we still have to exclude the occurrence, within the gap, of nouns to which the adjective could be
 % attached with priority, because they agree in the well-mapped agreement triplet
 ifthen(Distance=5, relaxedadjacent5_cgn([p(A,B)],Padj,Case,Gender,Nb)),
                               % five in between, neither of them a noun with relevant triplet
 ifthen(Distance=4, relaxedadjacent4_cgn([p(A,B)],Padj,Case,Gender,Nb)),
                               % four in between, neither of them a noun with relevant triplet

 ifthen(Distance=3, relaxedadjacent3_cgn([p(A,B)],Padj,Case,Gender,Nb)),
                               % three in between, neither of them a noun with relevant triplet
 ifthen(Distance=2, relaxedadjacent2_cgn([p(A,B)],Padj,Case,Gender,Nb)),
                               % two in between, neither of them a noun with relevant triplet
 ifthen(Distance=1, relaxedadjacent1_cgn([p(A,B)],Padj,Case,Gender,Nb)),
                               % one in between, not a noun with same [gender,number,case] triplet

  % Weight is W+1,
  myplus(W,1,Weight),
append([pathlist:[p(A,B)]],FSnoun,FSnounp), % so that the noun should appear when the pathlists get pretty-printed
  map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[Distance],   % distance is recorded as
									       % straining factor
            cat:np,class:Class,sem:Sem,
            number:Nb,person:3,gender:Gender,type:core,lex:LexNoun,lextype:full,
            case:Case,w:Weight,
            c_str:[head:FSnounp,adjp:FSadj]])].



% adj phrase preceding the noun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cf above - here the adjective precedes

[finite,np2b] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adjp,FSadj),
 constraint([number:Nb,gender:Gender,case:Case,class:Class,sem:Sem,lex:LexNoun],FSnoun),
ifthen(mapped(prep,[from:X,to:A|_]),(Case=acc;Case=gen;Case=abl)),                        % for efficiency resons - see above
 constraint([number:Nb,gender:Gender,case:Case,lex:LexAdj,type:Type,w:W1],FSadj),
 Type \= int,						% interrogative adjectives dealt with separately
 constraint([pathlist:Padj],FSadj),
 append([p(A,B)],Padj,Pnp),
 extremity(Padj,Ext),

 Ext =< A,
	% the adj phrase precedes the noun
 distance([p(A,B)],Padj,Distance),
 ifthen(LexAdj=is, Distance=0),
 msort(Pnp, Sorted),
  \+dup(Sorted),
 ifthenelse( recorded(stg,[texttype,1]),
                                        Distance < 4, Distance < 6)  ,       % 3 is thus the maximum distance
				                                             % between adj and noun in prose

 % we still have to exclude the occurrence, within the gap, of nouns to which the adjective could be
 % attached with priority, because they agree in the well-mapped agreement triplet
 ifthen(Distance=5, relaxedadjacent5_cgn(Padj,[p(A,B)],Case,Gender,Nb)),
                               % five in between, neither of them a noun with relevant triplet
 ifthen(Distance=4, relaxedadjacent4_cgn(Padj,[p(A,B)],Case,Gender,Nb)),
                               % four in between, neither of them a noun with relevant triplet

 ifthen(Distance=3, relaxedadjacent3_cgn(Padj,[p(A,B)],Case,Gender,Nb)),
                               % three in between, neither of them a noun with relevant triplet
 ifthen(Distance=2, relaxedadjacent2_cgn(Padj,[p(A,B)],Case,Gender,Nb)),
                               % two in between, neither of them a noun with relevant triplet
 ifthen(Distance=1, relaxedadjacent1_cgn(Padj,[p(A,B)],Case,Gender,Nb)),
                               % one in between, not a noun with same [gender,number,case] triplet

  % Weight is W1+1,
  myplus(W1,1,Weight),
  append([pathlist:[p(A,B)]],FSnoun,FSnounp), % so that the noun should appear when the pathlists get pretty-printed
  map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[Distance],
            cat:np,class:Class,sem:Sem,
            number:Nb,person:3,gender:Gender,type:core,lex:LexNoun,lextype:full,
            case:Case,w:Weight,c_str:[head:FSnounp,adjp:FSadj]])].









% with participle clause (mostly following the noun they get attached to)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rex recubans sub tegmine
% vir imperator futurus
% rex amatus a regina

% status of other participle clauses (outside of absolute ablatives) still to be settled  !!!!!!!!!

% note that the clauses are attached to nps, not nouns

[finite,np2a] --->
[mapped(np,FSnp),
 mapped(participle_clause,FSpp),
                               % the latter are built around the participle
			       % and deal with the args that the underlying verb features
                               % present, past and future participles are involved

 constraint([pathlist:PLnp,hp:HPnp,index:Index,distance:Distance,w:Weightnp,

             number:Nb,gender:Gender,case:Case,class:Class,person:Person,
             sem:Sem,lex:LexNoun],FSnp),

 constraint([number:Nb,gender:Gender,case:Case,lex:_,w:W],FSpp),
			% all these features are duly recorded in the participle_clause

 constraint([pathlist:PLpp],FSpp),
 append(PLnp,PLpp,PLbignp),

 extremity(PLpp,Extpp),       % extremity of clause
 length(PLpp,Length),
 extremity(PLnp,Extnp),       % extremity of np
% ifthen(Length > 1, Extpp > Extnp),          % the participle clause, if longer than one word, follows the noun ???????
 relaxadjacent(PLnp,PLpp),    % a type of adjacency that allows for one element to interrupt the sequence
                              % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
%  adjacent(PLnp,PLpp),
 
msort(PLbignp, Sorted),
 \+dup(Sorted),
 % Weight is W+Weightnp+1,     % we try to give them a good weight by adding the little bonus of 1
 myplus(W,Weightnp,T1),
 myplus(T1,1,Weight),
 map(np,[pathlist:Sorted,hp:HPnp,index:Index,distance:Distance,
           cat:np,class:Class,sem:Sem,
           number:Nb,person:Person,gender:Gender,type:core,lex:LexNoun,lextype:full,   % person to be left open or read from np
           case:Case,w:Weight,
           c_str:[head:LexNoun,participle_clause:FSpp]])].




% arg-bearing nouns
%%%%%%%%%%%%%%%%%%%

% cannot be assembled before the 'finite' run on account of args

% the arg-bearing noun is retrieved from within the NP it is included in

[finite,np2c] --->
[mapped(np,Fnp),
 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],sem:Sem,index:Index,
            number:Nb,person:3,gender:G,case:C,class:common,lex:Lex], Fnp),
 lexarg(Lex,args:Args),						% connecting with the arglist via lexarg/2

 extremity(PL1,BeginArgPos),	% the end position of the core np should be the start position of the args
				% no : epistulas scribit ad ancillam Marci
                                % unless we associate ad+NP with scribere (a possible treament)
                                % DO NOT KEEP BOTH
 expand(Args,ExpandTrees,ExpandPaths,ExpandDistances,w:W1),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
 start(Sorted,BeginArgPos),  % cf above on positions
 \+dup(Sorted),
 contiguous(Sorted),         % the args or arg parts are close together   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Distance),
 % Dis is Distnp1+Distance,
 myplus(Distnp1,Distance,Dis),
 append(PL1,Sorted,Path),
 % Weight is W1+3,
  myplus(W1,3,Weight),         % important bonus for arg info as opposed to adjuncts
 map(np,[pathlist:Path,hp:HL1,index:Index,distance:[Dis],
           cat:np,sem:Sem,class:common,lextype:full,              % 'lextype:full' means we do not want to expand it further
           number:Nb,person:3,gender:G, type:full,lex:Lex,
           case:C,w:Weight,c_str:[Fnp,ST]])].




% names
%%%%%%%

[core,np3] --->
[mapped(noun,[from:A,to:B|FS1]),
 constraint([class:proper,number:Nb,gender:G, case:C,lex:LexNoun,    % proper as class value
              sem:Sem],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:proper,lex:LexNoun,lextype:full,
           number:Nb,person:3,gender:G,type:core,case:C,w:1])].




% PRONOUNS
%%%%%%%%%%


% interrogative pronouns
%%%%%%%%%%%%%%%%%%%%%%%%

[core,np3bint] --->
[mapped(print,[from:A,to:B|FS1]),		% PRINT: interrogative pronouns as a lexical class
 constraint([number:Nb,gender:G, case:C,person:Person,sem:Sem,lex:Lex],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:int,lex:Lex,
           number:Nb,person:3,gender:G,type:int,case:C,w:1])].   % we register the interrogative nature
										       % as type and class value : overkill ?


% demonstrative pronouns
%%%%%%%%%%%%%%%%%%%%%%%%

[core,np3ba] --->
[mapped(prdem,[from:A,to:B|FS1]),              % PRDEM : ditto for demonstrative pronouns
 constraint([number:Nb,gender:G, case:C,lex:Lex],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:_,class:dem,lex:Lex,						% class dem
           number:Nb,person:3,gender:G,type:core,case:C,w:1])].

% indefinite pronouns
%%%%%%%%%%%%%%%%%%%%%

[core,np3baa] --->
[mapped(prindef,[from:A,to:B|FS1]),            % PRINDEF : guess what...
 constraint([number:Nb,gender:G, case:C,sem:Sem,lex:Lex],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:indef,lex:Lex,						% class indef
           number:Nb,person:3,gender:G,type:core,case:C,w:1])].





% nihil humani, nil novi
%%%%%%%%%%%%%%%%%%%%%%%%

[core,np3baaa] --->

[(Typepro=prdem;Typepro=prindef;Typepro=print), % demonstrative, indefinite or interrogative pronoun
 mapped(Typepro,[from:A,to:B|FS1]),
 constraint([number:sing,gender:neuter, case:C,sem:Sem,lex:Lex],FS1),
 mapped(adjp,FSadj),
 constraint([pathlist:Padj,number:sing,gender:neuter,w:Weight,case:gen,lex:LexAdj,w:W1],FSadj),
 adj(LexAdj,_,1,_,_), % 1 indicates second declension adj
 adjacent([p(A,B)],Padj),
 append([p(A,B)],Padj,Path),
 myplus(Weight,1,Weight1),
 ifthenelse(Typepro=print,Type=int,Type=core),
 map(np,[pathlist:Path,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,lex:Lex,class:Typepro,type:Type,						% class inherited from pronoun
           number:sing,person:3,gender:neuter,case:C,w:Weight1,c_str:[FS1,FSadj]])
].

% quid praemii
%%%%%%%%%%%%%%

[core,np3baaaa] --->	

[(Typepro=prdem;Typepro=prindef;Typepro=print), % demonstrative, indefinite or interrogative pronoun
 mapped(Typepro,[from:A,to:B|FS1]),
 constraint([number:sing,gender:neuter, case:C,sem:Sem,lex:Lex],FS1),
 mapped(np,FSnp),
 constraint([pathlist:Pnp,number:N,gender:G,w:Weight,case:gen,lex:LexN,w:W1],FSnp),
 adjacent([p(A,B)],Pnp),
 append([p(A,B)],Pnp,Path),
 myplus(Weight,1,Weight1),
 ifthenelse(Typepro=print,Type=int,Type=core),
 map(np,[pathlist:Path,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,lex:Lex,class:Typepro,type:Type,						% class inherited from pronoun
           number:N,person:3,gender:G,case:C,w:Weight1,c_str:[FS1,FSnp]])
].



% nihil alienum a te, quid facilius
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np3baab] --->
[(Typepro=prdem;Typepro=prindef;Typepro=print), % demonstrative, indefinite or interrogative pronoun
 mapped(Typepro,[from:A,to:B|FS1]),
 constraint([number:sing,gender:neuter, case:C,sem:Sem,lex:Lex],FS1),
 mapped(adjp,FSadj),
 constraint([pathlist:Padj,number:sing,gender:neuter,w:Weight,case:C,lex:LexAdj,w:W1],FSadj),
 adjacent([p(A,B)],Padj),
 append([p(A,B)],Padj,Path),
 myplus(Weight,1,Weight1),
 ifthenelse(Typepro=print,Type=int,Type=core),
 map(np,[pathlist:Path,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,lex:Lex,class:Typepro,type:Type,						% class inherited from pronoun
           number:sing,person:3,gender:neuter,type:core,case:C,w:Weight1,c_str:[FS1,FSadj]])
].






% personal pronouns
%%%%%%%%%%%%%%%%%%%

[core,np3b] --->
[mapped(prpers,[from:A,to:B|FS1]),	     %	PRPERS
 constraint([number:Nb,gender:G, case:C,person:Person,sem:Sem,lex:Lex],FS1),
 Lex\=pp3refl,									% reflexives are worth special attention
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:proper,lex:Lex,						% class proper
											% as if they were proper names
           number:Nb,person:Person,gender:G,type:core,case:C,w:1])].


% reinforced by solus
% to be revised !!!!

% just to be able to deal with the splendid line : elige cui dicas : tu mihi sola places
%                                        splendid on account of the unstressed (there lies the genius !)
%					 contrast between 'elige' and 'sola'....

[core,np3b] --->
[mapped(prpers,[from:A,to:B|FS1]),
 mapped(adjp,FSadj),
 constraint([pathlist:PLadj,number:Nb,gender:G,case:C,lex:solus],FSadj),      % solus only : shows how ad hoc the thing is
 constraint([number:Nb,gender:G, case:C,person:Person,sem:Sem,lex:Lex],FS1),
 Lex\=pp3refl,
 append([p(A,B)],PLadj,PL),
 msort(PL,Sorted),
 map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:proper,lex:Lex,
           number:Nb,person:Person,gender:G,type:core,case:C,w:1,c_str:[head:FS1,adjp:FSadj]])].


% THIRD PERSON REFLEXIVE PRONOUNS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3rd person reflexive pronouns are given a variable as index -
% the index will be instantiated with a value inside or outside the clause
% Scit rex se epistulam Marco misisse / Scit rex se reginam amare :
% think of the possible references for SE


% both non-emphatic and emphatic (se / se ipsum)

[core,np3c] --->
[mapped(prpers,[from:A,to:B|FS1]),
 constraint([number:Nb,gender:G, case:C,person:Person,sem:Sem, lex:pp3refl],FS1), % the lex value catches the reflexive pronouns
										  % of the third person
ifthenelse( (mapped(adjp,FSadj),
	     constraint([pathlist:[p(B,X)],number:Nbi,gender:Gi,case:C,lex:ipse],FSadj),

             funify([number:Nb],[number:Nbi],[number:Nbres]), % the reflex have Or-sets for gender and number; ipse is more precise
             funify([gender:G],[gender:Gi],[gender:Gres])),


											 % ipse follows : se ipsum not ipsum se
								% IF CLAUSE

             (PathList=[p(A,X)],Nbout=Nbres,Gout=Gres,E=yes),			% THEN CLAUSE  E is used to capture emphasis (i.e. when IPSE is used)
             (PathList=[p(A,B)],Nbout=_,Gout=_,E=no)),		% ELSE CLAUSE

map(np,[pathlist:PathList,hp:[p(A,B)],index:_,distance:[0],         % the index will be instantiated when the parse is ready for output
           cat:np,sem:Sem,class:proper,lex:pp3refl,emphasis:E,
           number:Nbout,person:3,gender:Gout,type:core,case:C,w:1])].




% so-called 'relatif de liaison'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% must always be in front, in positions 0 to 1

[core,np3c] --->
[mapped(relative,[from:0,to:1,pos:relative, txt:_, lex:_,gender:Gender,
                    case:Case, number:Number, function:_]),

map(np,[pathlist:[p(0,1)],hp:[p(0,1)],index:i(p(0,1)),distance:[0],
           cat:np,sem:_,class:proper,lex:relatif_de_liaison,
           number:Number,person:3,gender:Gender,type:core,w:1,case:Case])].




% GENITIVES IN NPS
%%%%%%%%%%%%%%%%%%


% core np + genitive np = full np
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the two heads (of the genitive phrase and the NP it is attached to) must be adjacent
% (within a two-word flexibility range, without any noun in between)
% tu patulae recubans sub tegmine fagi : OK
% rationes bonas bonae deae : OK
% the head cannot be a proper name : * marcus pulchrae deae

% we must wait for the finite run on account of the fact that the genitive np might contain a rel clause

[finite,np5] --->
[mapped(np,Fnp1), % the head
 mapped(np,Fnp2), % the noun cplt

 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],sem:Sem,
            number:Nb,person:3,gender:G,case:C,class:common,lex:LexNoun], Fnp1),
 constraint([pathlist:PL2, hp:HL2,distance:[Distnp2],case:gen],Fnp2),  % case must be genitive

 PL1 \= PL2, % not the same NP

  % relaxedadjacentX_n allows for an interruption by X words, non of them a noun

 ifthenelse(adjacent(HL1,HL2), Malus is 0, true),
 ifthenelse(relaxedadjacent1_n(HL1,HL2,n),Malus is 2, true),		% we deprioritize ....
 ifthenelse(relaxedadjacent2_n(HL1,HL2,n),Malus is 3, true),            % further...
  nonvar(Malus),			% one of the three conditions above must have fired !!!!!!!!!!!!!!!!!!!!!




% ifthenelse(precedes(HL1,HL2), Weight is 2, Weight is 1),  % HL1 is head, HL2 genitive

 % doubtful, even if gen phrase more often follows than precedes
 % the bonus is not likely to be useful for classical Latin, and counterproductive for poetry



append(PL1,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 myplus(Distnp1,Distnp2,T1),
 myplus(T1,Malus,Distance),
 % Distance is Distnp1 + Distnp2 + Malus,

                                        % the Malus increases the straining factor
 map(np,[pathlist:Sorted, hp:HL1,index:i(HL1),distance:[Distance],
           sem:Sem,number:Nb, person:3,cat:np,lex:LexNoun,lextype:full,
           gender:G, type:full,case:C,class:common,w:1,
           c_str:[head:Fnp1,noun_cplt:Fnp2]])].




% APPOSITION
%%%%%%%%%%%%%


% NP abstract + Pred in apposition
% mihi solacium est quod bona teneo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np6] --->
[mapped(np,HeadNP), % the head NP
 mapped(apposed_sub,ApposedPred), % the Pred in apposition

 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],sem:[abstract],
            number:Nb,person:3,gender:G,case:C,lex:LexNoun], HeadNP),

 ( constraint([pathlist:PL2, distance:[Distpred],mood:indicative, w:Wsub,subordinator:quod],ApposedPred);
   constraint([pathlist:PL2, distance:[Distpred],mood:subjunctive, w:Wsub,subordinator:ut],ApposedPred)
 ),

precedes(HL1,PL2), % apposition follows

append(PL1,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 myplus(Distnp1,Distpred,T1),
 myplus(T1,Malus,Distance),
Weight is Wsub+2,

 map(np,[pathlist:Sorted, hp:HL1,index:i(HL1),distance:[Distance],
           sem:[abstract],number:Nb, person:3,cat:np,lex:LexNoun,lextype:full,
           gender:G, type:full,case:C,class:common,w:Weight,
           c_str:[head:HeadNP,apposition:ApposedPred]])].



% id .... ut/quod ....

% id habeo solacium quod bona teneo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np6a] --->
[mapped(prpers,[from:A,to:B|FS1]),
 constraint([txt:or([id,eo]),case:C],FS1), % id or eo, nothing else
 mapped(apposed_sub,ApposedPred), % the Pred in apposition

  ( constraint([pathlist:PL2, distance:[Distpred],mood:indicative, w:Wsub,subordinator:quod],ApposedPred);
   constraint([pathlist:PL2, distance:[Distpred],mood:subjunctive, w:Wsub,subordinator:ut],ApposedPred)
 ),

precedes([p(A,B)],PL2), % apposition follows

append([p(A,B)],PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),

					                           % the Malus increases the straining factor
 map(np,[pathlist:Sorted, hp:[p(A,B)],index:i([p(A,B)]),distance:[Distpred],
           sem:[abstract],number:sing, person:3,cat:np,lex:id,lextype:full,
           gender:neuter, type:full,case:C,class:common,w:Wsub,
           c_str:[head:id,apposition:ApposedPred]])].




% NP WITH RELATIVE CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np6] --->

[ % the NP
 mapped(np,FS1),
 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1]],FS1),
 constraint([cat:np,index:Index,number:Nbnp,gender:Gnp,
             sem:SemNP,lex:Lex, case:Case],FS1),

 ifthenelse(constraint([c_str:C_str1],FS1),Head=C_str1,Head=Lex),

 % the relative clause
 mapped(relative_clause,FS2),
 constraint([number:Nb,gender:G,pathlist:PL2,distance:[Distrel], constraints:Constraints,w:W],FS2),

 constraint([number:Nbnp],[number:Nb]), constraint([gender:Gnp],[gender:G]),

 constraint([index:Index],FS2), % index sharing with the NP - essential to link relative and antecedent

 cleanc(Constraints,CC),        % the Constraints should not include case:Case (to be removed - other constraints to be kept)
 ifthen(CC\=[],constraint(CC,FS1)),   % apply the constraints, e.g. semantic constraints passed on to the antecedent noun

 msort(PL1,PLnpSorted),
 extremity(PLnpSorted,X), % contiguity test np and rel clause
 Xplus is X+1,
 Xplusplus is X+2,

 % room for only one word to fit between antecedent and relative clause, two in poetry
 % should introduce penalties !!!

  ifthenelse( recorded(stg,[texttype,2]), (start(PL2,X), Mali is 0; 
                                           start(PL2,Xplus), Mali is 1; 
                                           start(PL2,Xplusplus),Mali is 2), 
                                          (start(PL2,X), Mali is 0; start(PL2,Xplus), Mali is 1) ),

 append(PLnpSorted,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 % contiguous(Sorted),           % not applicable on account of possible non-contiguity in the NP constituents
				% although at first sight the restriction looks reasonable... but:
                                % imperatores timeo qui a pace abhorrent
 % Distance is Distnp1+Distrel,
 % Weight is W+1,
 myplus(Distnp1,Distrel,DistanceA),
 myplus(DistanceA, Mali, Distance),
 myplus(W,1,Weight),
 map(np,[pathlist:Sorted, hp:HL1,distance:[Distance],
           cat:np,type:full,class:common,lextype:full,
           index:Index,number:Nbnp,gender:Gnp,sem:SemNP,person:3,case:Case,
           lex:Lex,w:Weight,c_str:[head:Head,rel_clause:FS2]])
].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% code below to be revised: 

% rel clause precedes antecedent ; antecedent has hic... is... ipse... idem.... 
% as adj or boils down to one of these pronouns

% pronouns : prpers or prdem accessible through pos

% adjectives : lex value within the c_str of the adjp within the np
% this lex value to be one of member(LexAdj,[hic,(is),idem,iste,ille,ipse])

% qui a pace abhorrent eos timeo
% qui a pace abhorrent eos imperatores timeo

% end of relative clause not too far from beginning of np : 
% use the same requirement as above : one word in prose, two in poetry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np6] --->

[ % the NP
 mapped(np,FS1),
 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1]],FS1),
 constraint([cat:np,index:Index,number:Nbnp,gender:Gnp,
             sem:SemNP,lex:Lex, case:Case],FS1),

  ifthenelse(constraint([c_str:C_str1],FS1),Head=C_str1,Head=Lex),

 % the relative clause
 mapped(relative_clause,FS2),
 constraint([number:Nb,gender:G,pathlist:PL2,distance:[Distrel], constraints:Constraints,w:W],FS2),

 constraint([number:Nbnp],[number:Nb]), constraint([gender:Gnp],[gender:G]),

 constraint([index:Index],FS2), % index sharing with the NP - essential to link relative and antecedent

 cleanc(Constraints,CC),        % the Constraints should not include case:Case (to be removed - other constraints to be kept)
 ifthen(CC\=[],constraint(CC,FS1)),   % apply the constraints, e.g. semantic constraints passed on to the antecedent noun

 msort(PL1,PLnpSorted),
 msort(PL2,PLrelSorted),




 extremity(PLrelSorted,X), % contiguity test np and rel clause
 Xplus is X+1,
 Xplusplus is X+2,

 % room for only one word to fit between antecedent and relative clause, two in poetry

 % we need a general penalty for preceding, plus specific penalties for distance !!!!!

 
  ifthenelse( recorded(stg,[texttype,2]), (start(PLnpSorted,X), Mali is 2; 
                                           start(PLnpSorted,Xplus), Mali is 3; 
                                           start(PLnpSorted,Xplusplus), Mali is 4), 
                                          (start(PLnpSorted,X), Mali is 2; start(PLnpSorted,Xplus), Mali is 3) ),

 append(PLnpSorted,PLrelSorted,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 % contiguous(Sorted),           % not applicable on account of possible non-contiguity in the NP constituents
				% although at first sight the restriction looks reasonable... but:
                                % imperatores timeo qui a pace abhorrent
 % Distance is Distnp1+Distrel,
 % Weight is W+1,
 myplus(Distnp1,Distrel,DistanceA),
 myplus(DistanceA, Mali, Distance),
 myplus(W,1,Weight),
 map(np,[pathlist:Sorted, hp:HL1,distance:[Distance],
           cat:np,type:full,class:common,lextype:full,
           index:Index,number:Nbnp,gender:Gnp,sem:SemNP,person:3,case:Case,
           lex:Lex,w:Weight,c_str:[head:Head,rel_clause:FS2]])
].






% CLEANC
%%%%%%%%%

% doing away with the case constraint, which concerns the relative pronoun, not the antecedent !

cleanc([],[]).
cleanc([case:_|OtherC],OtherC) :- !. % case constraint occurs only once, therefore the cut can be used
cleanc([H|T],[H|T1]) :- cleanc(T,T1).




% with dummy np derivable from relative clause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% restricted to subject, direct object and indirect object:
% qui amat reginam amat ancillam
% amo quem amat regina

% case sharing does not seem to be required:
% 'do aquam cui ancilla dat pecuniam' but also:

%                                      elige cui dicas        : tu mihi sola places
%                                      haec tibi non tenues   veniet delapsa per auras
%				       Ovid, Ars Amatoria, I, 42-43
% let's pay tribute :
%	déca			       choisis à qui dire          : toi seule me plais
%       alexandrin                     la vois-tu tout d'en haut   te tomber dans les bras ?

% case sharing would be ensured by the use of a single variable Case in the rel clause constraints
% and the np being built - a very reasonable thing to do, but goodbye Ovid - can't bear the thought ;-)


[finite,np7] --->

[ mapped(relative_clause,FS2),
  constraint([number:Nb,gender:G,case:Case_in_Rel,pathlist:PL2,distance:[Distrel],gap:Gap,
             constraints:Constraints,w:W],FS2),

  start(PL2,Y),
  Gap \= [],         % exclude ubi and cum-relatives  which feature no gap

 % we have a relative clause only, NO NP; therefore
 % we check that the relative is not immediately preceded by a noun which could serve as antecedent

 
 \+((mapped(np,NP), constraint([pathlist:PL,number:Nb,gender:G],NP), extremity(PL,Y)            )),


 % nor by a comma (used to separate non-restrictive relatives from their antecedents)

  \+((mapped(punct,[from:X,to:Y, lex:comma|_]))),


   
   constraint([case:Case_in_Rel],[case:or([nom,acc,dat])]),    % restrictions on case

   constraint([index:dummy_np],FS2),			       % index sharing with the NP to be built

   constraint([case:Case_NP],[case:or([nom,acc,dat])]),        % same case restrictions -
							       % but NOT necessarily same case as in the relative

   cleanc(Constraints,CC),        % the Constraints should not include case:Case (to be removed - other constraints to be kept)
   ifthen(CC\=[],constraint(CC,[cat:np,type:full,class:common,index:_,
                                number:Nb,gender:G,sem:SemNP,person:3])),
   % register the constraints, e.g. semantic constraints passed on to the antecedent noun


 % mapping the NP:
   map(np,[pathlist:PL2,hp:[],distance:[Distrel],
             cat:np,type:full,class:common,
             index:dummy_np,number:Nb,gender:G,sem:SemNP,person:3,case:Case_NP,     % lex value enables the parser
										  % to spot this fabricated NP
             lex:dummy_np,w:W,
             c_str:[head:[lex:dummy_np, number:Nb,gender:G,person:3,index:_,
                          constraints_to_be_met:Constraints],         % specified for the sake of the reader of the parse tree
                    rel_clause:FS2]])].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PPS  Prepositional Phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% restriction : the preposition precedes the ***head*** of the np
% magno sub tegmine OK
% sub deae bonae tegmine OK

% all pps must wait for the 'finite' run on account of the np they include


% prep precedes NP
%%%%%%%%%%%%%%%%%%

% the prep sports 'pre' as value for the 'type' feature

% lex(in,prep,[lex:in,pos:prep, requires:acc,gerund:no,type:pre]).

[finite,pp1] --->
[mapped(prep,[from:A, to:B|Fprep]),
 mapped(np,Fnp),

 constraint([sem:Sem,lex:LexNoun, index:Inp,w:W],Fnp),

 constraint([requires:Case, lex:Lexprep,type:pre],Fprep),  % case constraint on np

 constraint([hp:[p(Begin,End)],pathlist:P,distance:Dist,case:Case],Fnp), % it is the HEAD that we worry about 'magna cum cura'
 constraint([type:Type],Fnp),
 % start(P,B),

 ifthenelse( recorded(stg,[texttype,1]),  % PROSE
             (Begin=B;Begin is B+1;Begin is B+2 ), % PROSE
             (start(P,B); A=End)  % POETRY
            ),		% prep before NP but only in prose; in poetry allowed to immediately
							% follow NP head 'tempora circum'
 append([p(A,B)],P,PP),
 msort(PP, Sorted),

 map(pp,[pathlist:Sorted,hp:[p(Begin,End)],distance:Dist,index:Inp,
           case:Case,prep:Lexprep, sem:Sem,lex:LexNoun,
           w:W,type:Type,			% the type is the NP type, not the prep's
							% it will be available for checks on the NP within the PP
							% for instance in adjuncts
							% (Type registers whether the NP is interrogative)
           cat:pp,c_str:[prep:Lexprep,head:Fnp]])].






% prep can follow NP : causa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the 'post' value need not be assigned to the prep
% it can be satisfied by feature unification with an or-value which has 'post' as a component
% as in the case of 'causa' (qv.)

% lex(causa,prep,[lex:causa,pos:prep,requires:gen,gerund:yes,type:or([pre,post])]).


[finite,pp1a] --->
[mapped(prep,[from:A, to:B|Fprep]),
 mapped(np,Fnp),
 constraint([sem:Sem,lex:LexNoun, index:Inp,w:W],Fnp),
 constraint([requires:Case, lex:Lexprep,type:post],Fprep),
 constraint([hp:[p(Begin,End)],pathlist:P,distance:Dist,case:Case,index:Inp],Fnp),
 extremity(P,A),						% preposition follows NP
 append(P,[p(A,B)],PP),
 msort(PP, Sorted),
 map(pp,[pathlist:Sorted,hp:[p(Begin,End)],distance:Dist,index:Inp,
           case:Case,prep:Lexprep, sem:Sem,lex:LexNoun,
           w:W,
           cat:pp,c_str:[prep:Lexprep,head:Fnp]])].

% we do not bother about interrogatives - they are not likely to occur within an NP attached to causa - or are they?





% special case : urbis capiendae causa : gerund with post-NP causa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,pp2] --->
[mapped(prep,[from:A, to:B|Fprep]),
 constraint([type:post,requires:Case,lex:Lexprep],Fprep),

 mapped(pred,Pred),		% gerunds (and gerunds substituted by gerundives) are PREDS

 constraint([pathlist:PathlistPred,distance:Dist,w:W, type:gerund, case:gen],Pred),  % case is recorded in such preds
										     % and must be genitive here
 extremity(PathlistPred,A),    % prep follows
 append(PathlistPred,[p(A,B)],PP),
 msort(PP, Sorted),
 map(pp,[pathlist:Sorted,hp:[p(A,B)],distance:Dist,index:_,
           case:Case,prep:Lexprep, sem:_,lex:Lexprep,
           w:W,type:gerund,		% the type of the pp is relevant in arg structure specification
           cat:pp,c_str:[prep:Lexprep,head:Pred]])].






% preps with gerund(ive) : in, ad, pre-NP causa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,pp3] --->
[mapped(prep,[from:A, to:B|Fprep]),
 constraint([requires:Case, lex:Lexprep, gerund:yes, type:pre],Fprep),  % prep precedes

 mapped(pred,Pred),
 constraint([pathlist:PathlistPred,distance:Dist,w:W, type:gerund, case:Case],Pred),  % case as required by prep

 start(PathlistPred,B),			% preD follows
 append([p(A,B)],PathlistPred,PP),
 msort(PP, Sorted),
 map(pp,[pathlist:Sorted,hp:[p(A,B)],distance:Dist,index:_,
           case:Case,prep:Lexprep, sem:_,lex:Lexprep,
           w:W,type:gerund,
           cat:pp,c_str:[prep:Lexprep,head:Pred]])].






% ADJUNCTS
%%%%%%%%%%%

% These structures are housed in the np part of the grammar because they are often built out of an np.
% But what is of interest is the role they play in clause extensions, i.e. as structures belonging to the clause
% without being part of the arg structure of a predicate

% Note that PPs also cover a good deal of the ground assignable to adjuncts
% we should try to avoid the double-reading of pps as both pps AND adjuncts


% TIME DURATION
%%%%%%%%%%%%%%%%

% Tres annos regnavit.
% Quantos annos regnavit ?

[core,adjunct_time_duration] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),
 ifthen(constraint([type:int],FSadj), Type=int),  % we use the type feature here to register the
						  % declarative or interrogative nature of the structure
                                                  % Type is left as an uninstantiated variable in declaratives

 constraint([number:Nb,gender:Gender,case:acc,sem:[time],lex:Lexnoun],FSnoun),  % a time noun + accusative case
 constraint([number:Nb,gender:Gender,case:acc, lex:LexAdj],FSadj),              % agreement check
 adjacent([p(A,B)],[p(X,Y)]),
 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),

 map(adjunct,[pathlist:Sorted,hp:[p(X,Y)],distance:[0],
               cat:np,class:adjunct,value:time,					% the 'value' feature can be used in clause expansions
               number:Nb,person:3,gender:Gender,lex:Lexnoun,lextype:full,
               type:Type,
               case:acc,w:2,
               c_str:[head:[lex:Lexnoun, sem:time_duration,cat:np,number:Nb,gender:Gender,case:acc,index:i(p(X,Y))]],
                             adj:LexAdj])].


% Quamdiu regnavit?

[core,adjunct_time_duration_adv] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:duration, lex:LexAdv],FSadv),      % both interrogative and duration-oriented

  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                cat:advp,class:adjunct,value:time,
                lex:LexAdv,lextype:full,type:int,             % we know this is an interrogative adjunct
                w:2,c_str:[lex:LexAdv, sem:time_duration,cat:advp]])].

% per totum tempus

[finite,adjunct_time_duration_pp] --->
[mapped(pp,FSpp),
 constraint([pathlist:PL,distance:Dist,
           case:acc,prep:per,sem:[time],
           c_str:C_str], FSpp),

map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,sem:time_duration,value:time,type:_,
                w:2,c_str:C_str])].



% TIME WHEN
%%%%%%%%%%%%

% Hac hora veniet.  Aestivo.........tempore : no adjacency required !!!

[core,adjunct_time_when] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),
 constraint([number:Nb,gender:Gender,case:abl,sem:[time],lex:Lexnoun],FSnoun),  % time noun + ablative
 constraint([number:Nb,gender:Gender,case:abl, lex:LexAdj],FSadj),
 ifthen(constraint([type:int],FSadj), Type=int),
 % adjacent([p(A,B)],[p(X,Y)]),

 distance([p(A,B)],[p(X,Y)],Distance),    % the distance between the noun and the path of the adjective
                                      % determines the straining factor as well as helping to decide
				      % whether noun and adj DO belong together


 Distance < 4,                      % 3 is thus the maximum distance
				    % between adj and noun

 % we still have to exclude the occurrence, within the gap, of nouns to which the adjective could be
 % attached with priority, because they agree in the well-mapped agreement triplet

 ifthen(Distance=3, relaxedadjacent3_cgn([p(X,Y)],[p(A,B)],abl,Gender,Nb)),
                               % three in between, neither of them a noun with relevant triplet
 ifthen(Distance=2, relaxedadjacent2_cgn([p(X,Y)],[p(A,B)],abl,Gender,Nb)),
                               % two in between, neither of them a noun with relevant triplet
 ifthen(Distance=1, relaxedadjacent1_cgn([p(X,Y)],[p(A,B)],abl,Gender,Nb)),
                               % one in between, not a noun with same [gender,number,case] triplet



 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),
 map(adjunct,[pathlist:Sorted,hp:[p(X,Y)],distance:[0],
                cat:np,class:adjunct,value:time,
                number:Nb,person:3,gender:Gender,lex:Lexnoun,lextype:full,type:Type,
                case:abl,w:2,
                c_str:[head:[lex:Lexnoun, sem:time_when,cat:np,number:Nb,gender:Gender,case:acc,index:i(p(X,Y))]],
                                 adj:LexAdj])].


% Quando veniet ?

[core,adjunct_time_when_adv] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:when, lex:LexAdv],FSadv),     % interrogative and time-when
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:time,
                 lex:LexAdv,lextype:full,type:int,w:2,
                 c_str:[lex:LexAdv, sem:time_when,cat:advp]])].


% Domi militiaeque diripere domos nolebat.

% Phrases with the right value for the 'value' feature
% e.g. string(phrase,[domi,militaeque],[lex:domi_militaeque,w:1, value:time]).

[core,adjunct_time_phrase] --->
[mapped(phrase,FSphrase),
 constraint([value:time, pathlist:PL],FSphrase),
 map(adjunct,[pathlist:PL,distance:[0],class:adjunct,value:time,type:_,
                w:2,c_str:FSphrase])].



% PLACE ADJUNCTS
%%%%%%%%%%%%%%%%

% Locatives : sum Romae.

[core,adjunct_place_city] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 constraint([number:Nb,gender:Gender,case:gen,sem:[city],lex:Lexnoun],FSnoun),
                                           % genitive as locative for city names

map(adjunct,[pathlist:[p(X,Y)],hp:[p(X,Y)],distance:[0],
               cat:np,class:adjunct,value:place,
               number:Nb,person:3,gender:Gender,lex:Lexnoun,lextype:full,type:_,
               case:gen,w:2,
               c_str:[head:[lex:Lexnoun, sem:location,cat:np,number:Nb,gender:Gender,case:gen,index:i(p(X,Y))]]])].

% Ubi sunt ?

[core,adjunct_position_adv] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:position, lex:LexAdv],FSadv),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:place,
                 lex:LexAdv,lextype:full,type:int,w:2,
                 c_str:[lex:LexAdv, sem:location,cat:advp]])].

% Sunt domi.

[core,adjunct_place_phrase] --->
[mapped(phrase,FSphrase),
 constraint([value:loc, pathlist:PL],FSphrase),
 map(adjunct,[pathlist:PL,distance:[0],class:adjunct,value:place,type:_,
                w:2,c_str:FSphrase])].

/*

Covered by PP

% Sunt in Italia

[finite,adjunct_place_pp] --->
[mapped(pp,FSpp),
 constraint([pathlist:PL,distance:Dist,
           prep:or([in,ex,ab,per,inter,intra,sub,trans,apud,circum]),sem:Sem,
           c_str:C_str], FSpp),
(member(loc,Sem); member(city,Sem)), % semantic pair associated with locations

map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,sem:place,value:place,type:_,
                w:2,c_str:C_str])].

*/

% PURPOSE AND REASON
%%%%%%%%%%%%%%%%%%%%

% Misit legatum ad milites hortandos.
% pps belong to the finite pass

[finite,adjunct_purpose] --->
[mapped(pp,FSpp),
 constraint([pathlist:PL,distance:Dist,
           case:acc,prep:ad,
           type:gerund,                  % this is the prepositional phrase treatment
                                         % it might duplicate a pred treatment
           c_str:C_str], FSpp),

map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,value:purpose,type:_,
                w:2,c_str:C_str])].



% genitive gerundives (without 'causa')  (and gerunds???)
%%%%%%%%%%%%%%%%%%%%%

% 'Pacis petendae oratores ad consulem miserunt' (T.L) / 
% 'Germanicus Aegyptum proficiscitur cognoscendae antiquitatis' (Tacitus)
% the procedure is costly as it detaches genitive gerundives from their anchor points
% their very existence is debatable (see Benveniste)

[finite,adjunct_purpose2] --->
[mapped(pred,FSpred),
 constraint([pathlist:PL,distance:Dist,
           local_case:gen, w:WeightPred,
           type:gerund,subtype:gerundive,               %  there are also subordinate clauses of purpose dealt with on their own  !
           c_str:C_str], FSpred),     %  here we deal only with gerunds

 NW is WeightPred - 2,

 map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,value:purpose,type:_,
                w:NW,c_str:C_str])].

  % since we have a pred, we copy over the pred's weight (but somewhat downgraded) rather than assign weight 2 without further ado



% Cur misit epistulam ?

[core,adjunct_purpose_reason_adv] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:reason, lex:LexAdv],FSadv),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],cat:advp,class:adjunct,value:purpose,
          lex:LexAdv,lextype:full,type:int,
          w:2,c_str:[lex:LexAdv, sem:reason,cat:advp]])].


% MANNER AND MEANS
%%%%%%%%%%%%%%%%%%

% Libros legendo licet insanire.
% needs to wait for the finite pass on account of the pred

[finite,adjunct_manner_means] --->
[mapped(pred,FSpred),
 constraint([cat:pred,type:gerund,local_case:abl,mood:gerund,
             number:sing,person:3,gender:neuter,
             pathlist:PL, distance:Dist,w:WeightPred,
             c_str:C_str], FSpred),

map(adjunct,[pathlist:PL,distance:Dist,
               class:adjunct,value:manner_means,
                type:Type,
                w:WeightPred,c_str:C_str])].      % the weight accumulated in the pred is copied over rather than assign standard weight for adjuncts, to wit 2


% Quomodo vicit hostes ?

[core,adjunct_manner_means_adv] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:manner_means, lex:LexAdv],FSadv),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:manner_means,
                 lex:LexAdv,lextype:full,type:int,w:2,
                 c_str:[lex:LexAdv, sem:manner_means,cat:advp]])].


% grauiter angi

%  advp,[cat:advp,pathlist:[p(A,B)],distance:[0],type:Type,lex:Lex,sem:Sem,w:1])].

[core,adjunct_manner_means_adv] --->
[ mapped(advp,[from:A, to:B|FSadvp]),
  constraint([type:vpbound,sem:manner_means, lex:LexAdv],FSadvp),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:manner_means,
                 lex:LexAdv,lextype:full,type:clausal,w:2,
                 c_str:[lex:LexAdv, sem:manner_means,cat:advp]])].


% 'de integro'
% as an example of frozen mwu

[core,adjunct_manner_means_phrase] --->
[mapped(phrase,FSphrase),
 constraint([value:manner_means, pathlist:PL],FSphrase),
 map(adjunct,[pathlist:PL,distance:[0],class:adjunct,value:manner_means,type:_,
                w:2,c_str:FSphrase])].





%%%%%%%%%%%%
% VERB GROUP
%%%%%%%%%%%%

% we keep track of polarity (also for clausal constraints - e.g. non pili facere)
% we only take 'non' as a vg negation (an oversimplification)
% the other negations are deemed to be clause-type-bound, i.e. can affect any clause constituent


% positive polarity
%

% we copy the info from the verb form
% to the verb group

% lex(amabunt, v, [pos:v, class:tr_cod, type:finite, lex:amare, voice:act, txt:amabunt,
%                  tense:future, kind:std, mood:indicative, number:pl, person:3]).


% standard, one-word units
%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg1] --->
[ mapped(v,[from:B, to:C|FS1]),
  ifthen(constraint([gender:Gender],FS1),G=Gender), % we have a gender, we record it
					            % if we don't, we leave the G var a free var
						    % this prevents failure in the case of a gender check
   map(vgpos,[cat:vg,pathlist:[p(B,C)],hp:[p(B,C)], gender:G,w:0|FS1])].

  % computing weight is necessary to prioritize verb groups with esse as compared with esse + predicative
  % passive voice, for instance: amatus est





% two-word units
%%%%%%%%%%%%%%%%%

% deponents : active voice
%%%%%%%%%%%%%%%%%%%%%%%%%%

% hortatus sum eram ero sim essem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% lex(sumus, v, [pos:v, class:v_esse, type:finite, lex:esse, txt:sumus,
%                tense:present, kind:std, mood:indicative, number:pl, person:1]).

% lex(hortatus, p_p, [pos:p_p, txt:hortatus, case:nom, gender:masc, number:sing,
%     lex:hortari, class:tr_cod, type:p_p, kind:dep, mood:participle, person:3]).


[verb,vg4] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([type:finite, lex:esse,tense:Tense,
              mood:Mood,number:Nb,person:P],FSverb),
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),      % both orders : sum hortatus, hortatus sum
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:nom,number:Nb,gender:G,lex:Lex,kind:dep],FSpp),  % making sure we have a deponent verb
  ifthen(Tense=present,Tenseout=perfect),
  ifthen(Tense=imperfect,Tenseout=pluperfect),
  ifthen(Tense=future,Tenseout=future_perfect),

  map(vgpos,[cat:vg,type:finite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:P,mood:Mood,tense:Tenseout, voice:act,           % active voice
               number:Nb,gender:G,w:3])].


% secutum esse
%%%%%%%%%%%%%%

[verb,vg5] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:esse],FSverb),
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,number:Nb,gender:G,lex:Lex,kind:dep],FSpp),   % the -UM etc forms

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:_,mood:infinitive,tense:perfect,
               voice:act,number:Nb,gender:G,w:3])].		% active voice


% true passives
%%%%%%%%%%%%%%%

% amatus sum eram ero sim essem

[verb,vg4a] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([type:finite, lex:esse,tense:Tense, mood:Mood,number:Nb,person:P],FSverb),
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:nom,number:Nb,gender:G,lex:Lex,kind:std],FSpp),   % std as opposed to dep (standard versus deponent)
  ifthen(Tense=present,Tenseout=perfect),
  ifthen(Tense=imperfect,Tenseout=pluperfect),
  ifthen(Tense=future,Tenseout=future_perfect),

  map(vgpos,[cat:vg,type:finite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
            person:P,mood:Mood,tense:Tenseout, voice:pass,number:Nb,gender:G,w:5])].   % passive voice


% with 'est' or 'sunt' understood
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg4ab] --->
[ mapped(p_p,[from:C, to:D|FSpp]),
  constraint([case:nom,number:Nb,gender:G,lex:Lex,kind:std],FSpp),   % std as opposed to dep (standard versus deponent)
  map(vgpos,[cat:vg,type:finite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:indicative,tense:perfect, voice:pass,number:Nb,gender:G,w:5])].   % passive voice


% amatum esse
%%%%%%%%%%%%%%

[verb,vg5a] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:esse],FSverb),
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,number:Nb,gender:G,lex:Lex,kind:std],FSpp),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
            person:_,mood:infinitive,tense:perfect, voice:pass,number:Nb,gender:G,w:5])].   % passive voice


% with 'esse' understood
%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg5ab] --->
[ mapped(p_p,[from:C, to:D|FSpp]),
  constraint([case:acc,number:Nb,gender:G,lex:Lex,kind:std],FSpp),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:infinitive,tense:perfect, voice:pass,number:Nb,gender:G,w:5])].   % passive voice


% middle voice
%%%%%%%%%%%%%%

% we introduce middle voice to account for passive forms of intransitive verbs
% and a special use of the passive with transitives

% pugnatum est
%%%%%%%%%%%%%%

[verb,vg5amiddle] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([lex:esse,type:Type, person:3, number:sing, tense:Tense,mood:Mood],FSverb),       % esse
  mapped(v,[from:C, to:D|Supine]),					% supine verb form
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  ( constraint([type:supine,lex:Lex,kind:std],Supine);
    constraint([txt:coeptum],Supine) ),                % does not apply to deponent verbs except coepi
  ifthen(Tense=present,Tenseout=perfect),
  ifthen(Tense=imperfect,Tenseout=pluperfect),
  ifthen(Tense=future,Tenseout=future_perfect),

  map(vgpos,[cat:vg,type:Type,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:3,mood:Mood,tense:Tenseout,
               voice:middle,number:sing,gender:neuter,w:3])].           % middle voice


% with gerund
%%%%%%%%%%%%%

% insaniendum est (also with dep : hortandum est)

[verb,vg5amiddle1] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([lex:esse,type:Type, person:3, number:sing,tense:Tense,mood:Mood],FSverb),       % esse
  mapped(v,[from:C, to:D|Gerund]),					% gerund
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,type:gerund,lex:Lex,kind:_],Gerund),          % accusative form of the gerund
  map(vgpos,[cat:vg,type:Type,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:3,mood:Mood,tense:Tense,
               voice:middle,number:sing,gender:neuter,                  % middle voice
               value:obligation,w:3])].                                 % semantic force : obligation



% infinitive future : hortaturum esse, amaturum esse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% active voice : dealt with the same way for both deponent and standard verbs

[verb,vg5a] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:esse],FSverb),        % esse
  mapped(p_f,[from:C, to:D|FSpf]),       % future participle
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,number:Nb,gender:G,lex:Lex],FSpf),  % accusative

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:_,mood:infinitive,tense:future,
               voice:active,number:Nb,gender:G,w:15])].                 % heavy weight to counteract interpretation as a participial clause


% with esse understood
%%%%%%%%%%%%%%%%%%%%%%

[verb,vg5aa] --->
[ mapped(p_f,[from:B, to:C|FSpf]),
  constraint([case:acc,number:Nb,gender:G,lex:Lex],FSpf),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:[p(B,C)],hp:[p(B,C)],
               lex:Lex,
               person:_,mood:infinitive,tense:future,
               voice:active,number:Nb,gender:G,w:15])].                % same heavy weight, the ESSE being very frequently omitted


% FORE is entered as such in the lexicon:
% lex(fore, v,
%      [pos:v, class:v_esse, type:nonfinite, lex:esse, voice:act, txt:fore,
%       tense:future, kind:std, mood:infinitive, number:_, person:_]).

% and will be turned into a verb group by the general procedure described at the top of this section


% future passive
%%%%%%%%%%%%%%%%

% same procedure for iri


[verb,vg5aa] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:iri],FSverb),        % iri
  mapped(p_p,[from:C, to:D|FSpp]),       % past participle
  constraint([case:acc,number:sing,gender:neuter,lex:Lex],FSpp),  % accusative neuter sg (amatum)

  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:_,mood:infinitive,tense:future,
               voice:pass,number:_,gender:_,w:3])].



% subjunctive future
%%%%%%%%%%%%%%%%%%%%

% periphrastic / used in indirect questions
% ... quid dicturus sit /esset

[verb,vg5b] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([lex:esse, number:Nb, person:Person,mood:subjunctive, tense:or([present,imperfect])],FSverb),
  mapped(p_f,[from:C, to:D|FSpf]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:nom,number:Nb,gender:G,lex:Lex],FSpf),

  map(vgpos,[cat:vg,type:finite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
            person:Person,mood:subjunctive,tense:future, voice:active,number:Nb,gender:G,w:3])].


% gerundive with est or sunt understood
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,gdiv] --->
[ mapped(gdiv,[from:C,to:D|FS]),
				% gdiv gerundives (NOT gerund)
 constraint([case:nom, number:N, gender:G, txt:Txt,lex:Lex],FS),

map(vgpos,[cat:vg,type:finite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:indicative,tense:present, voice:active,number:N,gender:G,w:3])
 ].

% gerundive with esse understood
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,gdiv_a] --->
[ mapped(gdiv,[from:C,to:D|FS]),
				% gdiv gerundives (NOT gerund)
 constraint([case:acc, number:N, gender:G, txt:Txt,lex:Lex],FS),

map(vgpos,[cat:vg,type:nonfinite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:infinitive,tense:present, voice:active,number:N,gender:G,w:3])
 ].



% FULL VERB GROUPS
%%%%%%%%%%%%%%%%%%

[verb, vgpos] --->
[ mapped(vgpos,FS1),
  append([polarity:pos], FS1, FSVG),
  map(vg,FSVG)].

% to each vgpos group we assign the polarity:pos feature pair and turn it into a full verb group (vg)



% negative polarity
%%%%%%%%%%%%%%%%%%%

% neg necessarily in front of verb
% this amounts to an oversimplification (one more...)

[verb, vgneg] --->
[ mapped(neg,[from:A, to:B|Fneg]),
  constraint([lex:non],Fneg),
  mapped(vgpos,FS1),
  pick(pathlist:Path,FS1,FSnew),
  start(Path,B),                 % negation immediately precedes verb
  append([p(A,B)],Path,PL),      % adding the negation to the path
  msort(PL,PLsorted),
  contiguous(PLsorted),
  \+ dup(PLsorted),
  append([polarity:neg, pathlist:PLsorted],FSnew,FSVG), % the vgpos does not yet bear any polarity feature
   map(vg,FSVG)].






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% We start from the verb group in which we isolate the main verb.
% We fill in all the non-optional arguments of its argument list, taking all the relevant constraints
% into account.
% We also allow for pp adjuncts and other type of adjuncts which do not belong to the arglist,
% but can be attached to the clause as a whole: 'rex reginae aquam dat in templo magno'.

% The non-finite clauses are built first, but do not have a pre-emptive right on the constituents -
% the distribution of constituents as between main and complement clause allows for ambiguity.




%%%%%%%%%%%%
% nonfinite
%%%%%%%%%%%%

% (scio) regem epistulas legere / regem credere reginam se laudavisse.
%

[finite,prednfc] --->
[ mapped(flags,active(nonfinite)),  % we use this flag to jump the bits concerning non-finite clauses
                                   % if we do not have any nonfinite form in the input string
   mapped(vg,FSverb),
  constraint([type:nonfinite,mood:Mood,voice:Voice,tense:Tense,        % nonfinite verb form
              pathlist:PathlistVerb,lex:Clex, w:WVerb],FSverb),
  ifthen(constraint([gender:Gendersubj],FSverb),true),                  % if we have gender in the verb form, it must agree with the one
									% specified in the subject slot used in arg filling
  ifthen(constraint([number:Numbersubj],FSverb),true),                  % if we have number in the verb form, it must agree with the one
									% specified in the subject slot used in arg filling


  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),      % retrieving polarity

  lexarg(Clex,arglist:ArgList),						% connection with the args via lexarg
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),   % picking a word sense to see if it is appropriate ...
									% remember that in general there will be more than one
									% ws (i.e. word sense) for a given arg-bearer



ifthenelse(Voice=pass,							% PASSIVE
              % selecting the object to remove it from the arglist and turn it into a subject

                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:acc],Oconstraints1,NewOconstraints), % subjects are acc in nonfinite clauses !!
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),           % there must be a subject if a passive is used !!!


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE note the OR operator (;)
                      NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,
                                                                                     % ELSE-CLAUSE:
                 % ab+hum vs simple abl for non-hum

                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),           % ELSE2-clause

                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
             Argstomatch=Args),                                           % ACTIVE : leave the args as they are in the arg specs


  match_list(Argstomatch,TreeArgs,PathlistArgs, DistanceArgs,                 % attempting to match the arglist
             sujet([number:Numbersubj,gender:Gendersubj,person:_]),


             nonfinite,gap:Gap,w:Weight,Int),           % the nonfinite flag is important to determine the case of the subject

% we keep track of the fact that we are dealing with a nonfinite clause (accusative subject !!!)
% the weight is meant to reflect the nber and nature of the arguments satisfied - the more the better


 % we have explained above why we once decided to rem the partial expansions -
 % the attachment point of adjuncts is too often undecidable so that too many solutions are generated
 % we need a sophisticated pragmatic discourse-oriented module to deal adequately with this overgeneration

  full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
  adjacent(EFlat,CFlat),                                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(ExpandPaths,CPathlist,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  \+dup(Sorted),
  ifthenelse(contiguous(Sorted),BonusContiguous=3, BonusContiguous=0),  % bonus for strict contiguity


  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
   quasicontiguous(Sorted),	% insisting on straightforward contiguity is too strict
									%  Me tabula sacer.... EXPENSIVE
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),                                          % standard order imposed on args IN THE PARSE TREE
								 % NOT in the string, of course




   clause_constraints(Clause_Constraints,FSverb),   % they concern polarity (so far...) 

						% prioritize normal subject-object order (following a suggestion made by Dominique Longrée, ULg)

  ifthenelse(constraint([subject:[hp:Pathsubj]],ST),  % we have a non-gapped subject, we record its head path
	     true,				      % and abstain from doing anything else
             Pathsubj=[p(0,0)]),		      % otherwise the subject is higher up and therefore necessarily precedes

  ifthenelse(constraint([object:[hp:Pathobj]],ST),				        % IF-CLAUSE we have an object
             ifthenelse(precedes(Pathsubj,Pathobj), NW is Weight+1, NW=Weight),         % THEN-CLAUSE
             NW=Weight),								% ELSE-CLAUSE

  ifthenelse(constraint([i_object:[hp:Pathiobj1]],ST),					%  we have an indirect object (first type: dat)
             ifthenelse(precedes(Pathsubj,Pathiobj1), NW1 is NW+1, NW1=NW),
             NW1=NW),
  ifthenelse(constraint([object_i:[hp:Pathiobj2]],ST),					%  we have an indirect object (second type: acc)
             ifthenelse(precedes(Pathsubj,Pathiobj2), NW2 is NW1+1, NW2=NW1),           % as with doceo, for instance
             NW2=NW1),
  ifthenelse(var(Int), IntFlag=no, IntFlag=wh_question),       % the int flag on an arg indicates a wh_question

  % Wtot is NW2+WExpand+WVerb+MW,
				% summing the weights : for the args, the expansions and the verb group
  myplus(NW2,WExpandN,T1),
  myplus(T1,WVerb,T2),
  myplus(T2,MW,Wtot),
  myplus(Wtot,BonusContiguous,Wtottot),
  append([selected_reading:Lex],FSverb,FSverbfull),
  map(pred,[cat:pred,type:nonfinite,           % type of the whole pred is nonfinite
              mood:Mood,tense:Tense,class:m,
              pathlist:Sorted,distance:[Distance],
              number:sing,gender:neuter,case:or([nom,acc]),     % of little relevance for pred, but might be useful
              person:3,polarity:Pol,
              argbound:no,
              gap:Gap,
              w:Wtottot,
              flagint:IntFlag,
              c_str:[vg:FSverbfull|ST]])].










% (scis) vincere
%%%%%%%%%%%%%%%%%

[finite,prednfp] --->
[ mapped(flags,active(nonfinite)),
    mapped(vg,FSverb),
  constraint([type:nonfinite,mood:Mood,tense:Tense,voice:Voice,lex:Clex,pathlist:PathlistVerb, w:WVerb],FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist
                ( pick(object:_,Args,Args1),  % note that there must be an object if a passive was produced !!

               % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


                 append([agent:NewArg],Args2,At),
                 append([subject:_],At,Argstomatch) ), % adding dummy subject for match_list to drop in mode nonfinite_i !!!


                 Argstomatch=Args),

   match_list(Argstomatch,TreeArgs,PathlistArgs, DistanceArgs,
               sujet([number:Nsubj,gender:Gendersubj,person:Psubj]),
              nonfinite_i,gap:Gap,w:Weight,Int),

             % nonfinite_i specifies that no subject should be looked for
             % contrast 'scis vincere' et 'scis te vicisse'



  full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
  adjacent(EFlat,CFlat),                              % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(ExpandPaths,CPathlist,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  \+dup(Sorted),
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),
  clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WExpand+WVerb+MW,
  myplus(Weight,WExpandN,T1),
  myplus(T1,WVerb,T2),
  myplus(T2,MW,Wtot),
   ifthenelse(var(Int), IntFlag=no, IntFlag=wh_question),
   append([selected_reading:Lex],FSverb,FSverbfull),

  map(pred,[cat:pred,type:nonfinite_i,
              mood:Mood, tense:Tense,class:m,
              pathlist:Sorted, distance:[Distance],
              number:Nsubj,gender:Gendersubj,case:or([nom,acc]),
              person:Psubj,
              polarity:Pol,
              gap:Gap,w:Wtot,
              flagint:IntFlag,
              c_str:[vg:FSverbfull|ST]])].







% rogatum auxilium : supine
%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,prednfsupine] --->
[  mapped(vg,FSverb),
  constraint([mood:supine,lex:Clex,pathlist:PathlistVerb, w:WVerb],FSverb),  % needs a supine
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

  match_list(Args,TreeArgs,PathlistArgs, DistanceArgs,
               sujet([number:_,gender:_,person:_]),
              nonfinite_i,gap:[],w:Weight,_),

             % nonfinite_i specifies that no subject should be looked for


%  partial_expansions(PE),
%  expandkernel(PE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),
  append(PathlistVerb,PathlistArgs,AllPaths),
%  append(ExpandPaths,CPathlist,AllPaths),
%  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(DistanceArgs, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  contiguous(Sorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
   \+dup(Sorted),
  % append(TreeArgs,ExpandTrees,AllTrees),
  insort(TreeArgs, ST),
  clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WExpand+WVerb+MW,
  % myplus(Weight,WExpand,T1),
  myplus(Weight,WVerb,T2),
  myplus(T2,MW,Wtot),
  append([selected_reading:Lex],FSverb,FSverbfull),
  map(pred,[cat:pred,type:nonfinite_i,
              mood:supine, tense:present,class:m,
              pathlist:Sorted, distance:[Distance],
              number:sing,gender:neuter,case:or([nom,acc]),
              person:3,
              gap:[],w:Wtot,
              c_str:[vg:FSverbfull|ST]])].






% urbem capiendi (causa)
%%%%%%%%%%%%%%%%%%%%%%%%%

% we have a gerund (not a gerundive) - the args get satisfied the usual way

% PROBLEMS with contiguity checks but they need to be maintained

[finite,prednfp1] --->
[
  mapped(v,[from:A, to:B|FSverb]),
  constraint([type:gerund,case:Case,lex:Clex],FSverb),      % true gerund form of the verb

  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
  match_list(Args,TreeArgs,PathlistArgs, DistanceArgs,sujet(_), nonfinite_i,gap:[],w:Weight,Int),

     % nonfinite_i specifies that no subject should be looked for;
     % contrast 'urbem capiendi' et 'caesar cepit urbem'

  append([p(A,B)],PathlistArgs,CPathlist),
  flatten(CPathlist,Flat),
  flatten(DistanceArgs,Flatdis),
   msort(Flat, Sorted),
   \+dup(Sorted),
   % contiguous(Sorted),                                % see below contiguity test
   sum(Flatdis,Distance),
    append([selected_reading:Lex],FSverb,FSverbfull),
   % TW is Weight+MW,
   myplus(Weight,MW,TW),
    full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandPaths,EFlat),

  append(EFlat,Sorted,AllPaths),
  flatten(ExpandDistances,ExDisFlat),
   sum(ExDisFlat, ExDistance),
   TotDis is Distance + ExDistance,
  flatten(AllPaths, AllFlat),
  msort(AllFlat, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                  % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),
  myplus(TW,WExpandN,TotWeight),
   map(pred,[cat:pred,type:gerund,			% type:gerund is how we specify them in args and adjuncts
               case:_,
               mood:gerund,
               local_case:Case,                         % case is used for subject agreement,
		class:m,					% local_case to store the case of the gerund
               number:sing,person:3,
               gender:neuter,
               pathlist:NSorted, distance:[TotDis],
               gap:[],w:TotWeight,c_str:[vg:FSverbfull|ST]])].








% urbis capiendae (causa)
%%%%%%%%%%%%%%%%%%%%%%%%%


% PROBLEMS with contiguity checks: Caesari urbs ERAT capienda / Mihi colenda EST virtus
% we have to change the analysis of such strings - we opt for gerundive as adj
% and we keep the contiguity checks


/*
Augustinus : "agere negotium procurandi fructus mortis":

procurandi gerund gen *    *     + fructus acc masc pl + gen sg mortis //
procurandi gdiv   gen masc sg    + fructus gen masc sg + gen sg mortis

Lexical entries:
lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:gen,
                       gender:masc, number:sing, lex:procurare, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).
lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:gen,
                       gender:neuter, number:sing, lex:procurare, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).
lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:nom,
                       gender:masc, number:pl, lex:procurare, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).

lex(procurandi, v, [pos:v, class:tr_cod, type:gerund, lex:procurare, txt:procurandi, kind:std, mood:gerund, person:3, case:gen]).
*/






% we have a gerundive
% the verb must be transitive and the object satisfied by an np
% with the same [case,gender,number] as that of the gerundive

[finite,prednfp2] --->
[ mapped(gdiv,[from:A, to:B|FSverb]),                 % gerundives are gdiv
  constraint([type:gdiv,
              case:Case,gender:Gender, number:Number,  % agreement triplet
              lex:Clex],FSverb),
  lexarg(Clex,arglist:ArgList),

  pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
  pick(object:ObjectSpecs,Args,Args_temp),
									% removing the arg with its Specs
  pick(constraints:Constraints, ObjectSpecs,ObjectSpecs_temp),		% removing the Specs Constraints
  pick(case:acc,Constraints,Constraints_temp),				% removing the case constraint from the Constraints
  Case \= nom,  % nominative banned here
  append([case:Case,gender:Gender,number:Number],Constraints_temp, New_Constraints),			% rebuilding the Constraints
  append([constraints:New_Constraints],ObjectSpecs_temp,New_ObjectSpecs),				% rebuilding the Specs
  append([object:New_ObjectSpecs],Args_temp,NewArgs),							% rebuilding the Args


  match_list(NewArgs,TreeArgs,PathlistArgs, DistanceArgs,sujet(_), nonfinite_i,gap:[],w:Weight,Int),

             % nonfinite_i specifies that no subject should be looked for
             % contrast 'urbis capiendae' et 'caesar cepit urbem'

  PathlistArgs \= [],  % we must have at least one arg filled,
                       % the gerundive adj cannot be used on its own as a gerund substitute

  append([p(A,B)],PathlistArgs,CPathlist),
  flatten(CPathlist,Flat),
   msort(Flat, Sorted),
   flatten(DistanceArgs,Flatdis),
   \+dup(Sorted),
    sum(Flatdis,Distance),

   full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandPaths,EFlat),
  % adjacent(EFlat,Sorted),
  append(EFlat,Sorted,AllPaths),
  flatten(ExpandDistances,ExDisFlat),
   sum(ExDisFlat, ExDistance),
   TotDis is Distance + ExDistance,

  flatten(AllPaths, AllFlat),
  msort(AllFlat, NSorted),
  \+dup(NSorted),
 contiguous(NSorted),                       % NEEDS TO BE MAINTAINED        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),



   myplus(Weight,MW,T1),
   myplus(T1,2,NewWeight),
   % NewWeight is Weight+MW+2,                                % giving priority to gerundive construction as opposed to simple
							 % noun+adj nexus (in case gerundives are also registered as adj)

    myplus(NewWeight,WExpandN,TotW),
    append([selected_reading:Lex],FSverb,FSverbfull),
   map(pred,[cat:pred,type:gerund,subtype:gerundive,			% type:gerund is how we specify them in args and adjuncts
               case:_,mood:gerund,class:m,
               local_case:Case,  % case is used to determine subject agreement, local_case to retrieve the case of the gerund
               number:sing,person:3,gender:neuter,
               pathlist:NSorted, distance:[TotDis],
               gap:[],w:TotW,c_str:[vg:FSverbfull|ST]])].







%%%%%%%%%%%%%%%%%%%%%
% ABLATIVE ABSOLUTE
%%%%%%%%%%%%%%%%%%%%%

% type CICERONE CONSULE
%%%%%%%%%%%%%%%%%%%%%%%

[finite,prednfp3ab] --->
[ mapped(np,FSnp1),
  mapped(np,FSnp2),
  FSnp1 \= FSnp2,
  constraint([pathlist:PL1,case:abl, number:Number],FSnp1),

  constraint([pathlist:PL2,case:abl, number:Number,
              lex:or([consul, dux, rex, imperator])], FSnp2),
                      % best dealt with as a list, even if a long one;
                      % toy list here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      % the key quality seems to be built-in non-permanence
  adjacent(PL1,PL2),
  append(PL1,PL2,PL),
  map(pred,[cat:pred,
              class:m,
              type:aa,       % the aa type is how we pick up the ablative absolute in clause expansions
              case:abl,
              number:sing,    % nothing to do with the number of the construction's components
              person:3,
              mood:aa, 
              pathlist:PL, distance:[0],
              gap:[],w:0,
              c_str:[FSnp1,FSnp2]])
 ].

% type HAMILCARE UIUO
%%%%%%%%%%%%%%%%%%%%%

[finite,prednfp3ab] --->
[ mapped(np,FSnp1),
  mapped(adjp,FSadj),
  constraint([pathlist:PL1,case:abl, number:Number, gender:Gender],FSnp1),
  constraint([pathlist:PL2,case:abl, number:Number, gender:Gender,
              lex:or([uiuus,mortuus,grauis])], FSadj),
                      % the list to be established on the basis of genuine examples !!!!!!!!!!!!!!!!!!!!!
                      % best dealt as a list, even if a long one;
                      % toy list here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      % the key quality seems to be *****  built-in non-permanence  *******
  adjacent(PL1,PL2),
  append(PL1,PL2,PL),
  map(pred,[cat:pred,type:aa,class:m,
              case:abl,mood:aa,
              number:sing,person:3,
              pathlist:PL, distance:[0],
              gap:[],w:0,c_str:[FSnp1,FSadj]])
 ].




% type URBE CAPTA
%%%%%%%%%%%%%%%%%%

[finite,prednfp3] --->
[  mapped(np,FSnp),
   constraint([number:Number, gender:Gender, case:abl, pathlist:Pathlist, distance:[Distance]],FSnp),

   mapped(p_p,[from:X,to:Y|FSpp]),  % a past participle

   constraint([case:abl,gender:Gender, number:Number,lex:Clex],FSpp), % agreement checks case gender number
   lexarg(Clex,arglist:ArgList),

                 % selecting the object constraints:
   pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
   pick(object:ObjectSpecs,Args,Args1),            % note that there must be an object if a pp was produced !!
   pick(constraints:Constraints, ObjectSpecs,_),
   pick(case:acc,Constraints,NPConstraints),       % deleting the case constraint on the object
   constraint(NPConstraints,FSnp),                 % applying the object constraints on the ablative NP

   % turning the subject into an optional (a+) abl pp arg

   pick(subject:SubjectSpecs,Args1,Args2),
   pick(constraints:Sconstraints,SubjectSpecs,_),

   ifthenelse( Sconstraints=[],                 % no constraint on subj: both types of agent are OK  IF-CLAUSE
             ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
              NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,

                                                                                   % ELSE-CLAUSE:
              ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                            NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                            NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


   append([agent:NewArg],Args2,Args3),

   expand(Args3,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand should prove sufficient
   flatten(ExpandPaths,Flat),
   msort(Flat,Sorted),
   ifthen(Sorted\=[],adjacent([p(X,Y)],Sorted)),  % args before or after pp, but contiguous
   \+dup(Sorted),
   % contiguous(Sorted),		% too strict ? --> see below
   insort(ExpandTrees,ST),
   flatten(ExpandDistances,ED),
   sum(ED,DistExpand),
   append([p(X,Y)],Sorted,Path),
   % NewDis is Distance+DistExpand,
   myplus(Distance,DistExpand,NewDis),

   append(Path,Pathlist,CPathlist),
   flatten(CPathlist,Flat2),
   msort(Flat2, Sorted2),


 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Sorted2),
  append(EFlat,Sorted2,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,NewDis,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

  append(ST,ExpandT,Trees),
  insort(Trees, NST),


   extremity(NSorted,End),
   ifthenelse( mapped(punct,[from:End,to:Endpunct, lex:comma|_]),
             Adtopath=[p(End,Endpunct)],
             Adtopath=[]),						% comma ?
   append(NSorted,Adtopath,Path_aa),



   % TotWeight is Weight+MW,
   myplus(Weight,MW,TotWeight),
   myplus(TotWeight,WExpandN,TW),
   map(pred,[cat:pred,type:aa,class:m,
               case:abl,mood:aa,
               number:sing,person:3,
               pathlist:Path_aa, distance:[NDistance],
               gap:[],w:TW,
               c_str:[lex:Lex,FSpp,NST,object:FSnp]])
 ].
 
% type DICTIS DICENDIS
%%%%%%%%%%%%%%%%%%%%%%%

% dictis dicendis

[finite,prednfp3a] --->
[ mapped(p_p,[from:A,to:B|FSpp]),   % the past participle
  constraint([case:abl,gender:neuter, number:pl,lex:Clex],FSpp),  % neuter plural on top of ablative case
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:_,mwuw:MW,args:_), ArgList,_),  % getting at the Lex
  not(atom_concat(mwu,_,Lex)),						% Lex is not a multi-word unit

  mapped(gdiv,[from:B,to:C|FSgdiv]),                                  % the gerundive
  constraint([number:pl, gender:neuter, case:abl, lex:Clex],FSgdiv), % agreement checks
   ifthenelse( mapped(punct,[from:C,to:D, lex:comma|_]),
             Adtopath=[p(C,D)],
             Adtopath=[]),						% comma ?
   append([p(A,C)],Adtopath,Path_aa),
  map(pred,[cat:pred,type:aa,class:m,
              case:abl,mood:aa,
              number:sing,person:3,
              pathlist:Path_aa, distance:[0],
              gap:[],w:MW,c_str:[lex:Lex,pp:FSpp,gdiv:FSgdiv]])
 ].


% dicendis dictis
%%%%%%%%%%%%%%%%%

[finite,prednfp3b] --->
[ mapped(p_p,[from:B,to:C|FSpp]),
  constraint([case:abl,gender:neuter, number:pl,lex:Clex],FSpp),
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:_,mwuw:MW,args:_), ArgList,_),  % getting at the Lex
  not(atom_concat(mwu,_,Lex)),
  mapped(gdiv,[from:A,to:B|FSgdiv]),
  constraint([number:pl, gender:neuter, case:abl, lex:Clex],FSgdiv),
  ifthenelse( mapped(punct,[from:C,to:D, lex:comma|_]),
             Adtopath=[p(C,D)],
             Adtopath=[]),						% comma ?
   append([p(A,C)],Adtopath,Path_aa),

  map(pred,[cat:pred,type:aa,case:abl,mood:aa,class:m,
               number:sing,person:3,
              pathlist:Path_aa, distance:[0],
              gap:[],w:MW,c_str:[lex:Lex,pp:FSpp,gdiv:FSgdiv]])].


% type REGINA SUB TEGMINE FAGI RECUBANTE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,prednfp3c] --->
[  mapped(np,FSnp),
   constraint([number:Nb, gender:G, case:abl, pathlist:Pathlistnp, distance:[Distnp]],FSnp),

   mapped(participle_clause,FSpp),  % a whole clause

   constraint([pathlist:Pathlistpp,distance:[Distpp],
           w:Weight,type:pprcentered,               % makes it a participial clause built around a present participle
           number:Nb,gender:G, lex:Lex,
           case:abl,c_str:[Txt,ST]], FSpp),

   append(Pathlistnp,Pathlistpp,P1),
   msort(P1,P1sorted),
   extremity(P1sorted,End),
   ifthenelse( mapped(punct,[from:End,to:Y, lex:comma|_]),
             Adtopath=[p(End,Y)],
             Adtopath=[]),						% comma ?
   append(P1sorted,Adtopath,P2),
   msort(P2,Sorted),
   \+dup(Sorted),
   contiguous(Sorted),                                        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
   % Dis is Distnp+Distpp,
   myplus(Distnp,Distpp,Dis),


 map(pred,[cat:pred,type:aa,
               case:abl,mood:aa,class:m,
               number:sing,person:3,
               pathlist:Sorted, distance:[Dis],
               gap:[],w:Weight,c_str:[lex:Lex,arg:ST,subject:FSnp]])
 ].







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% finite clauses of various types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% neg imp  person
%%%%%%%%%%%%%%%%%%

% ne hoc feceris

[finite, negimp] --->
[mapped(neg,[from:A, to:B|Neg]),
constraint([type:imp],Neg),    % the neg must be of the right type, i.e. for imperatives (ne)
mapped(pred,FSpred),
constraint([type:finite,class:m,				% finite
            pathlist:PL,distance:Distance,
            number:N,
            person:_,                                   % second person but person here applies to the whole clause; 
                                                        % we need a new feature
            mood:subjunctive,                           % subjunctive
            tense:perfect,				% perfect
            argbound:no,
            gap:[],w:W,
            add:Add,					% the finite preds feature a feature... that tells whether
							% they come naked of with sth added in front

            flagint:no,					% no interrogative
            c_str:Cstr],FSpred),
start(PL,B),
append([p(A,B)],PL,PLtot),

map(pred,[cat:pred,type:finite,pathlist:PLtot,distance:Distance,
            illocutionary_force:command,
            number:N,person:3, mood:subjunctive,class:m,
            tense:perfect,argbound:no,
            polarity:neg,
            gap:[],w:W,
            add:Add,
            flagint:no,
            c_str:[illocutionary_force:neg_imperative,Cstr]])  % we mention the illocutionary force in the parse tree
].


% neg imp 1 person
%%%%%%%%%%%%%%%%%%

% ne hoc faciamus

[finite, negimp] --->
[mapped(neg,[from:A, to:B|Neg]),
constraint([type:imp],Neg),
mapped(pred,FSpred),
constraint([type:finite,class:m,pathlist:PL,distance:Distance,number:N,person:_, % person ought to be 1 - see above
            mood:subjunctive,
            tense:present,
            argbound:no,
            gap:[],w:W,add:Add,flagint:no,c_str:Cstr],FSpred),
start(PL,B),
append([p(A,B)],PL,PLtot),
map(pred,[cat:pred,type:finite,pathlist:PLtot,distance:Distance,
            illocutionary_force:command,
            number:N,person:3,class:m,
            mood:subjunctive,
            tense:present,
            argbound:no,
            polarity:neg,
            gap:[],w:W,add:Add,
            flagint:no,
            c_str:[illocutionary_force:neg_imperative,Cstr]])
].


% wishes
%%%%%%%%

% utinam + subj present or perfect

[finite, wish] --->
[mapped(part,[from:A, to:B|Utinam]),
constraint([lex:utinam],Utinam),     % only utinam !!!

mapped(pred,FSpred),
constraint([type:finite,class:m,pathlist:PL,distance:Distance,number:N,person:Person,
            mood:subjunctive,
            tense:Tense,
            argbound:no,
            polarity:Pol,
            gap:[],w:W,add:Add,
            flagint:no,
            c_str:Cstr],FSpred),

constraint([tense:or([present,perfect])],[tense:Tense]),       % not how the constraint is checked
							       % in order not to damage the Tense value
							       % assigned to the predicate

start(PL,B),
append([p(A,B)],PL,PLtot),
map(pred,[cat:pred,type:finite,pathlist:PLtot,distance:Distance,
            illocutionary_force:wish,
            number:N,person:Person, mood:subjunctive,class:m,
            tense:Tense,argbound:no,polarity:Pol,
            gap:[],w:W,add:Add,flagint:no,
            c_str:[illocutionary_force:wish,Cstr]])
].


% regrets
%%%%%%%%%

% utinam + imperfect or pluperfect subjunctive


[finite, regret] --->
[mapped(part,[from:A, to:B|Utinam]),
constraint([lex:utinam],Utinam),
mapped(pred,FSpred),
constraint([type:finite,class:m,pathlist:PL,distance:Distance,number:N,person:Person,
            mood:subjunctive,
            tense:Tense,
            argbound:no,polarity:Pol,
            gap:[],w:W,
            add:Add,
            flagint:no,
            c_str:Cstr],FSpred),

constraint([tense:or([imperfect, pluperfect])],[tense:Tense]),

start(PL,B),
append([p(A,B)],PL,PLtot),

map(pred,[cat:pred,type:finite,
            pathlist:PLtot,distance:Distance,
            illocutionary_force:regret,class:m,
            number:N,person:Person,
            mood:subjunctive,
            tense:Tense,
            argbound:no,polarity:Pol,
            gap:[],w:W,
            add:Add,
            flagint:no,
            c_str:[illocutionary_force:regret,Cstr]])
].






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BASIC DECLARATIVE CLAUSES

% start here to get a flavour of the structural part of the parsing process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Rex scribit epistulas
%%%%%%%%%%%%%%%%%%%%%%%



[finite,predfc] --->
[ 
   mapped(vg,FSverb),
   constraint([number:Nsubj,
              person:Psubj,
              type:finite,
              voice:Voice,
              mood:Mood,
              tense:Tense,
              pathlist:PathlistVerb,
              lex:Clex,
              w:WVerb],FSverb),

   ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos), % polarity is read off from the verb group


   Voice \= middle,                             % middle voice is given a specific treatment of the args
					        % and gets its own clause pattern

   lexarg(Clex,arglist:ArgList),
   pick(ws(Lex,Class,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

   ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist and turn it into a subject
                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:nom],Oconstraints1,NewOconstraints),
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause

                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
               Argstomatch=Args),                                    % active voice : leave arglist as it is


  
  match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:Nsubj,gender:_,person:Psubj]),
              finite,
              gap:Gap,
              w:Weight,
              Int),

 

 full_expansions(FE),
   expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),



   flatten(ExpandPaths,EFlat),
   adjacent(EFlat,CFlat),
   append(ExpandPaths,CPathlist,AllPaths),
   flatten(AllPaths,APF),

   recorded(fin,fin(Fin),_),
   ifthenelse(mapped(punct,[from:Lastbutone,to:Fin,lex:question_mark|_]),
                ifthenelse(member(p(Lastbutone,Fin),APF),Int=int,(append([p(Lastbutone,Fin)],APF,AP), Int=int)),
              AP=APF),





   myappend(DistanceArgs,ExpandDistances,Distances),
   flatten(Distances, FlatDist),
   sum(FlatDist, Distance),

  flatten(AP, Flat),
  msort(Flat, NSorted),
   \+dup(NSorted),

  % flatten(OtherPL,OtherPLflat),					% see above
  %  msort(OtherPLflat,OtherPLflatsorted),

  % contiguous(OtherPLflatsorted),


  start(NSorted,Start),

  ifthenelse(      mapped(punct,[from:Z,to:Start,lex:colon|_]),           % we have a colon in front of the finite clause
                   (Add=colon, append([p(Z,Start)],NSorted,NSortedF)),     % we add the colon to the path and register the property
                   (Add=no, NSortedF=NSorted)                              % we keep the old values
             ),


                               % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
ifthenelse( Add=colon,                                        % the colon indicates that the clause is a bit of direct speech
                                         contiguous(NSorted), quasicontiguous(NSorted)),

   
 ifthen(Add=colon,Gap=[]), % no gap in piece of direct speech used as arg


  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),




  % the proper treatment of gaps in cplt clauses involves percolating them up:

    ifthen((constraint([object:OBJ],ST),constraint([cat:pred,gap:Gapobj],OBJ)), % IF-clause
        ifthen(Gapobj\=[],Gap=Gapobj)),  % THEN-clause

/*
  % idem for weight:

  ifthenelse(  (constraint([object:OBJ],ST),constraint([cat:pred,w:LocalWeight],OBJ)), % IF-clause
                                           TotalWeight is LocalWeight+Weight,         % THEN-clause
                                          TotalWeight=Weight),			      % ELSE-clause

% more doubtful : it's only ONE arg, and should not be weightier than the others - or should it????
*/


   % priority should be given to the subject-predicative order : parsimonia(subject) est scientia (predicative)
								% vitandi sumptus supervacuos

  ifthenelse(constraint([subject:[hp:Pathsubj]],ST),  % we have a non-gapped subject, we record its head path
	     true,				      % and abstain from doing anything else
             Pathsubj=[p(0,0)]),		      % otherwise the subject is higher up and therefore necessarily precedes

  ifthenelse(constraint([predicative:[hp:Pathpredicative]],ST),				    % IF-CLAUSE we have a predicative
             ifthenelse(precedes(Pathsubj,Pathpredicative), NW is Weight+1, NW=Weight),     % THEN-CLAUSE
             NW=Weight),								% ELSE-CLAUSE

  ifthenelse( (constraint([object:[hp:Pathobject]],ST),	constraint([object_cplt:[hp:Pathobjectcplt]],ST)),
                              % IF-CLAUSE we have both obj and obj cplt
             ifthenelse(precedes(Pathobject,Pathobjectcplt), NW2 is NW+1, NW2=NW),         % THEN-CLAUSE
             NW2=NW),								% ELSE-CLAUSE


  % applying the clausal constraints (polarity):
  ifthen(Clause_Constraints=[[polarity:neg]], clause_constraints(Clause_Constraints,FSverb)),

  ifthenelse(var(Int),Force=statement,Force=question),

   % applying the clausal constraints (non-affirmative context)
   ifthen(Clause_Constraints=[[illocutionary_force:question]],Force=question),




  % Wtot is NW2+WExpand+WVerb+MW,
   myplus(NW2,(round(WExpand/4)),T1),
   myplus(T1,WVerb,T2),
   myplus(T2,MW,Wtot),


 % myplus(NW2,WVerb,T2),
 % myplus(T2,MW,Wtot),


  append([selected_reading:Lex],FSverb,FSverbfull),

  map(pred,[cat:pred,
              type:finite,
              pathlist:NSortedF,
              distance:[Distance],
              illocutionary_force:Force,class:m,
              number:sing,                      % nber, gender, and person of the CLAUSE, not its subject			
              person:3,
              gender:neuter,
              mood:Mood,
              tense:Tense,
              polarity:Pol,
              argbound:no,
              gap:Gap,
              w:Wtot,
              add:Add,
              flagint:IntFlag,
              c_str:[illocutionary_force:Force,vg:FSverbfull|ST]])  % ST is the sorted list of arg fillers
 ].



 



% scribit epistulas
%%%%%%%%%%%%%%%%%%%

% subject to be found elsewhere ; works as a building block in conjoined structures


[finite,predfp] --->
[ mapped(vg,FSverb),
  constraint([number:Nsubj,person:Psubj,type:finite,voice:Voice,
              mood:Mood, tense:Tense,pathlist:PathlistVerb,lex:Clex, w:WVerb],FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),
  Voice \= middle,
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),
  pick(subject:SubjectSpecs,Args,Args1),
  pick(constraints:Sconstraints,SubjectSpecs,_), % we need access to the constraints on the subject for two purposes
						% a) passivization b) making sure that there is no LEXICAL constraint on the subject
						% since it is not available in this type of incomplete, subjectless clause
  ifthenelse(Voice=pass,
              % selecting the object to remove it from the arglist
                (pick(object:_,Args1,Args2),  % note that there must be an object if a passive was produced !!


              % turning the subject into an optional (a+) abl pp arg
                 ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause

                 append([agent:NewArg],Args2,At),
                 append([subject:_],At,Argstomatch) ),  % dummy subject to be dropped by match_list in mode finite_i !!!

               Argstomatch=Args),

  % we should look for constraints on the subject - if there is a lexical constraint we need the subject in the clause and cannot drop it
  % we do not build the non-finite incomplete clause

            \+member([lex:_],Sconstraints),

  match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:Nsubj,gender:_,person:Psubj]),

              finite_i,     % indicates that we should not look for a subject !!!!!!!!!!!!!!!!!!!

              gap:Gap,
              w:Weight,
              Int),



 full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, full),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
  adjacent(EFlat,CFlat),
  append(EFlat,CFlat,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  \+dup(Sorted),



  append(TreeArgs,ExpandTrees,AllTrees),
  contiguous(Sorted),                                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  insort(AllTrees, ST),
  clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WVerb+MW+WExpand
  myplus(Weight,WVerb,T1),
  myplus(T1,WExpand,T2),
  myplus(T2,MW,Wtot),
  append([selected_reading:Lex],FSverb,FSverbfull),

  map(pred,[cat:pred,
              type:finite_i,					% records its subjectless nature
              pathlist:Sorted,distance:[Distance],
              number:Nsubj,person:Psubj,
              class:m,
              mood:Mood, tense:Tense,
              polarity:Pol,
              gap:Gap,
              w:Wtot,
              c_str:[vg:FSverbfull|ST]])
 ].







%%%%%%%%%%%%%%%
% MIDDLE VOICE
%%%%%%%%%%%%%%%

% Middle Voice in this parser is a mere technical term - nothing to do with middle voice as diathesis
% it is meant to account for 'impersonal passives', which are not really passives either

% insanitur etc...

[finite,predfp] --->
[ mapped(vg,FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),
  constraint([number:sing,
              person:3,
              type:finite,
              voice:middle,    % conditions are strict : third person sg middle voice

              mood:Mood,
              tense:Tense,
              pathlist:PathlistVerb,
              lex:Clex,
              w:WVerb],FSverb),
  lexarg(Clex,arglist:ArgList),

  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

  ifthenelse( (pick(object:OSpecs,Args,Args1), constraint([type:np],OSpecs)),
                                                     % NP as objects excluded with middles
             Nargs=Args1, Nargs=Args),

  match_list(Nargs,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:sing,gender:neuter,person:3]),
                     finite_i,				% no specified subject : do not look for one...
                     gap:[],
                     w:Weight, Int),

  full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, full),
  append(PathlistVerb,PathlistArgs,CPathlist),
    flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
adjacent(EFlat,CFlat),
  append(ExpandPaths,CPathlist,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
   \+dup(Sorted),
  contiguous(Sorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),
  clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WExpand+WVerb+MW,
  myplus(Weight,(WExpand/4),T1),
  myplus(T1,WVerb,T2),
  myplus(T2,MW,Wtot),
  append([selected_reading:Lex],FSverb,FSverbfull),

  map(pred,[cat:pred,
              type:finite,class:m,
              pathlist:Sorted,distance:[Distance],
              number:sing,person:3,mood:Mood, tense:Tense,
              voice:middle,
              argbound:no,
              illocutionary_force:statement,
              gap:[],
              w:Wtot,
              polarity:Pol,
              c_str:[vg:FSverbfull|ST]])
 ].



% NOMINATIVUS CUM INFINITIVO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rex seruam Marci amare dicitur
%

[finite,personalpass] --->
[ mapped(vg,FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),

  constraint([number:Nsubj,person:Psubj,
              type:finite,
              voice:pass,
              mood:indicative,     % (over)restricted to finite, pass, and indicative (subjunctive OK ?)
              tense:Tense,
              pathlist:Pathlistverb,
              lex:Lex,
              w:WVerb],FSverb),

 constraint([tense:or([present,imperfect, future])],[tense:Tense]),  % restrictions on tense, too

 constraint([lex:or([credere,dicere,existimare,
                     ferre,iudicare,
                     negare, nuntiare, putare,
                     tradere,uidere])],[lex:Lex]),   % restriction on the verb
						     % here too we are using a list
						     % should be incremented as vocabulary coverage increases
						     % but a list remains the best tool
 mapped(np,FSnp),
 constraint([number:Nsubj, person:Psubj, gender:Gsubj,
             case:nom,				    % we are in finite clauses
             pathlist:Pathlistnp, w:WNP,distance:[Distnp]],FSnp),

 mapped(pred,FSpred),

 constraint([type:nonfinite_i,				% second part : the pred assigned to the subject
             mood:infinitive,				% infinitive without its own subject
             pathlist:Pathlistpred, distance:[Distpred],
             number:Nsubj,gender:Gsubj,case:nom,person:Psubj,
             gap:[],w:Wpred,
             c_str:C_str_pred], FSpred),

 append([Pathlistverb,Pathlistnp,Pathlistpred],PL),
 \+dup(PL),
 msort(PL,Sorted),

 % Distance is Distnp+Distpred,
 % Weight is WNP+Wpred+WVerb+1,
 myplus(Distnp,Distpred,Distance),
 myplus(WNP,Wpred,T1),
 myplus(T1,WVerb,T2),
 myplus(T2,1,Weight),
 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,class:m,
             distance:[Distance],
             number:Nsubj,person:Psubj,
             mood:indicative,

             tense:Tense,polarity:Pol,
             illocutionary_force:statement,
             argbound:no,
             gap:[],
             w:Weight,
             add:no,           % just the clause
             c_str:[vg:FSverb,
                    % subject:C_str_NP,
                    pred:[subject:FSnp,pred:C_str_pred]]])               % quite a specific tree !
].






%%%%%%%%%%%%%%%%%%%%%%
% SUBORDINATE AS ARG
%%%%%%%%%%%%%%%%%%%%%%


% lex(ut, sub,        [lex:ut, pos:sub, argbound:yes, mood:subjunctive]).
% lex(ne, sub,        [lex:ne, pos:sub, argbound:yes, mood:subjunctive]).

[finite,predfsubarg] --->
[ mapped(sub,[from:A,to:B|Fsubordinator]),                 % we have a subordinator
 mapped(pred,Fp1),                                         % and a clause

 constraint([argbound:yes, mood:Mood, lex:Lex],Fsubordinator),    % the subordinator is arg-bound
                                                                  % and can specify mood
 constraint([type:finite,class:m,
             pathlist:PL1,distance:Distance,                      % we have a finite clause with the right mood
             gap:[],						 % and gapless
             mood:Mood,
             tense:Tense,
             w:Weight,
             c_str:Head],Fp1),




% if the string boasts a member of the SE family we have to undo the constraints on nber and gender set locally, i.e. when
% the clause was built : regina laudat se (SE is made to reflect nber and gender of regina)
% rex veretur ne regina se laudet (rex must be able to bind SE, gender masc and not fem, so we leave gender and number to be set when SE is bound


% we need an OR clause, so that NewHead remains unbound until the SE is met, and then stays bound


% OBJECT
(ifthenelse(mapped(flags,active(se)),  % IF1

			 ifthenelse(member(object:ObjectIn,Head),
                                         (
                                           pick(object:ObjectIn,Head,H1),
					                                           ifthenelse(  (member(lex:pp3refl,ObjectIn) ,member(emphasis:no,ObjectIn)),
                                                                                                ( pick(gender:GIN,ObjectIn, OI1),
                                                                                                  pick(number:NIN,OI1,OI2),
                                                                                                  append([gender:_,number:_],OI2,ObjectOut),
                                                                                                  append(H1,[object:ObjectOut],NewHead)

                                                                                                 ),
                                                                                    NewHead=Head)),
                                     NewHead=Head),


 NewHead=Head);

% I_OBJECT
ifthenelse(mapped(flags,active(se)),  % IF1

			 ifthenelse(member(i_object:ObjectIn,Head),
                                         (
                                           pick(i_object:ObjectIn,Head,H1),
					                                           ifthenelse(  (member(lex:pp3refl,ObjectIn) ,member(emphasis:no,ObjectIn)),
                                                                                                ( pick(gender:GIN,ObjectIn, OI1),
                                                                                                  pick(number:NIN,OI1,OI2),
                                                                                                  append([gender:_,number:_],OI2,ObjectOut),
                                                                                                  append(H1,[i_object:ObjectOut],NewHead)

                                                                                                 ),
                                                                                    NewHead=Head)),
                                     NewHead=Head),


 NewHead=Head);


% PREP_CPLT
% the structure is more complex, and need to be entered into


ifthenelse(mapped(flags,active(se)),  % IF1

			 ifthenelse(member(prep_cplt:PPIn,Head),
                                         (

                                           pick(prep_cplt:PPIn,Head,H1),
                                           pick(c_str:CSTR,PPIn, PP1),
                                           pick(head:CHead,CSTR,SSTR1),
					                                           ifthenelse(  (member(lex:pp3refl,CHead) ,member(emphasis:no,CHead)),
                                                                                                ( pick(gender:GIN,CHead, CH1),
                                                                                                  pick(number:NIN,CH1,CH2),
                                                                                                  append([gender:_,number:_],CH2,HeadOut),
                                                                                                  append(SSTR1,[head:HeadOut],NSSTR),
                                                                                                  append([c_str:NSSTR],PP1,PCNew),
                                                                                                  append(H1,[prep_cplt:PCNew],NewHead)

                                                                                                 ),
                                                                                    NewHead=Head)),
                                     NewHead=Head),


 NewHead=Head)),

% same should be done for object_i (special indirect object) and agent and more !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



 append([p(A,B)], PL1,NPL1),                     % add subordinator to subordinate clause
 msort(NPL1,Sorted),
 quasicontiguous(Sorted),                       % room for one or two words to stand out        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  \+dup(Sorted),

 % the pred we build should bear all the features that an arg expects,
 % including number, gender, case and index
 % weight is increased because it is an argument
 myplus(Weight,3,NW),

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:Distance,
             gap:[],class:m,
             number:_,person:_,gender:_,case:_,
             mood:Mood,
             tense:Tense,
             w:NW,
             subordinator:Lex,                 % we keep track of which subordinator is used
             argbound:yes,                     % and declare the clause arg-bound, not free-standing
             c_str:NewHead])
].








% YES-NO QUESTION AS ARG
%%%%%%%%%%%%%%%%%%%%%%%%%

% lex(num,part,[lex:num,type:int,value:open_orientation,clausetype:sub]). % in indirect questions

[finite,predfargint] --->
[ mapped(part,[from:A,to:B|Fint]),
  constraint([type:int, clausetype:sub], Fint),     % interrogative particle for subordinate clause

 mapped(pred,Fp1),

 constraint([type:finite,class:m,
             pathlist:PL1,distance:Distance,
             gap:[],
             mood:subjunctive,                     % subjunctive mood in indirect yes-no questions
             tense:Tense,
             w:Weight,
             flagint:no,                            % the clause does not feature an interrogative word (case then of a wh-question)
             c_str:Head],Fp1),
 append([p(A,B)], PL1,NPL1),                     % add interrogative particle to subordinate clause
 msort(NPL1,Sorted),
 contiguous(Sorted),                               % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  \+dup(Sorted),
 % TotalWeight is Weight+3,                        % a bonus for such constructions
 myplus(Weight,3,TotalWeight),
 % the pred should bear all the features that an arg expects,
 % including number, gender, case and index

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:Distance,
             gap:[],class:m,
             number:sing,person:3,gender:neuter,case:_,
             mood:subjunctive,
             tense:Tense,
             w:TotalWeight,
             argbound:yes,
             flagint:yes_no_question,    % flagint records the type of question (yes-no here, wh-based elsewhere)
             c_str:Head])
].






% FREE YES-NO QUESTION
%%%%%%%%%%%%%%%%%%%%%%%

% i.e. not as subordinate clauses

% lex(ne_int, part,   [lex:ne_int,  type:int, value:open_orientation,     clausetype:_]).
% lex(num,    part,   [lex:num,     type:int, value:negative_orientation, clausetype:main]). % in direct questions
% lex(nonne,  part,   [lex:num,     type:int, value:positive_orientation, clausetype:_]). % in direct questions

% ne_int is produced on the basis of enclitic ne


[finite,predfint] --->
[ mapped(part,[from:A,to:B|Fint]),
  constraint([type:int, clausetype:main, value:Value], Fint),
                                         % the type of interrogative particle we find in main clauses

 mapped(pred,Fp1),
 constraint([type:finite,class:m,
             pathlist:PL1,distance:Distance, gap:[],
             mood:Mood, tense:Tense,w:Weight,
             % flagint:no,
             c_str:Head],Fp1),
 (Mood=indicative; Mood=subjunctive),            % no imperative, for instance
                                              % the subjunctive carries various values
						 % and should be recorded in the parse


 append([p(A,B)], PL1,NPL1),                     % add interrogative particle
 msort(NPL1,Sorted),
 contiguous(Sorted),                             % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  \+dup(Sorted),
 % TotalWeight is Weight+3,
  myplus(Weight,3,TotalWeight),
 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:Distance, gap:[],
             illocutionary_force:question,class:m,
             number:sing,person:3,gender:neuter,polarity:Value,
             argbound:no,
             mood:Mood,				% the various discourse values associated with subjunctive mood
						% can be specified later, on the basis of more than just syntactic parsing
             tense:Tense,w:TotalWeight,
             flagint:yes_no_question,
             c_str:[yes_no_question,Value,Head]])
].





% WITH FREE SUBORDINATE CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the free subordinate clauses can be associated with various types of main clauses
% it is on the basis of subordinator, mood and tense that we can retrieve semantic values and discourse functions


% lex(si, sub,        [lex:si, pos:sub, argbound:no, mood:_]).
% lex(nisi, sub,      [lex:nisi, pos:sub, argbound:no, mood:_]).
% lex(ni, sub,        [lex:nisi, pos:sub, argbound:no, mood:_]).
% lex(cum, sub,       [lex:cum, pos:sub, argbound:no, mood:_]).
% lex(dum, sub,       [lex:dum, pos:sub, argbound:no, mood:_]).
% lex(ut, sub,        [lex:ut, pos:sub, argbound:no, mood:subjunctive, value:or([purpose,consequence])]).
% lex(ne, sub,        [lex:ne, pos:sub, argbound:no, mood:subjunctive, value:purpose]).
% lex(quoniam, sub,   [lex:quoniam, pos:sub, argbound:no, mood:_]).
% lex(quia, sub,      [lex:quia, pos:sub, argbound:no, mood:_]).
% lex(quod, sub,      [lex:quod, pos:sub, argbound:no, mood:_]).



% comma-joined or simply juxtaposed
%

% The comma should simply NOT be taken into account, being a modern invention...
% but, when present, it DOES help speeding the parsing process...
% we therefore make use of it if present, but we must be ready to do without


% free-standing subordinate clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,predfsub] --->
[ mapped(sub,[from:A,to:B|Fsubordinator]),
 mapped(pred,Fp1),
 constraint([type:finite,class:m,
             pathlist:PL1,distance:[Distp1], gap:[],
             mood:MoodSub,
             tense:TenseSub,
             w:W1,c_str:Head1],Fp1),
 constraint([argbound:no,lex:LexSub,mood:MoodSub],Fsubordinator),

 append([p(A,B)],PL1,PL2),
 msort(PL2,Sorted),
 contiguous(Sorted),    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 map(free_sub,[type:finite, class:m,pathlist:Sorted, distance:[Distp1],gap:[],subordinator:LexSub,
               mood:MoodSub,tense:TenseSub, w:W1,c_str:[free_standing_sub,Fsubordinator,Head1]])].



% subordinates in apposition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,predfsuba] --->
[ mapped(sub,[from:A,to:B|Fsubordinator]),
 mapped(pred,Fp1),
 constraint([type:finite,class:m,
             pathlist:PL1,distance:[Distp1], gap:[],
             mood:MoodSub,
             tense:TenseSub,
             w:W1,c_str:Head1],Fp1),
 constraint([argbound:no,lex:LexSub],Fsubordinator),

 start(PL1,B),

 append([p(A,B)],PL1,PL2),
 msort(PL2,Sorted),
 contiguous(Sorted),     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 map(apposed_sub,[type:finite, pathlist:PL2, distance:[Distp1],gap:[],subordinator:LexSub,
               mood:MoodSub,tense:TenseSub, w:W1,c_str:[apposed_sub,Head1]])].





% exclamations (accusative : me miserum etc.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,exclamation] --->

[ mapped(np,FSnp),
 constraint([number:N,gender:G,
             case:acc,				    % accusative only
             pathlist:Pathlistnp, w:WNP,distance:[Distnp]],FSnp),
 mapped(adjp,FSadjp),
 constraint([pathlist:Pathlistadjp,distance:[Distadjp],
             case:acc,number:N, gender:G,w:WADJP],FSadjp),

 append(Pathlistnp,Pathlistadjp,Pathlist),
 msort(Pathlist,Sorted),
 contiguous(Sorted),   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 Dis is Distnp+Distadjp,
 Weight is WNP+WADJP,

 map(exclamation,[type:accusative,pathlist:Sorted,distance:[Dis],gap:[],w:Weight,
    c_str:[exclamation,FSnp,FSadjp]])].




% Virgilian exclamation : fortunatos nimium sua si bona norint agricolas...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  'Fortunatos' indeed, not having to bother parsing what Dante's Guide said about them...

% to test :  'Fortunatos nimium sua si bona norint agricolas.'

% do not use commas or exclamation point !
% and be sure to set 'Type of structures shown' to 'All phrases and clauses' in the settings

% AND ...
% this bit of the grammar is a ***joke***, in case the point got missed!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,exclamation2] --->

[mapped(free_sub,FSSub),
 constraint([pathlist:Pathlistfreesub, distance:[Distfreesub],
               w:WFS,c_str:C_str_FreeSub], FSSub),

 mapped(np,FSnp),
 constraint([number:N,gender:G,
             case:acc,				    % accusative only
             pathlist:Pathlistnp, w:WNP,distance:[Distnp]],FSnp),

 mapped(adjp,FSadjp),
 constraint([pathlist:Pathlistadjp,distance:[Distadjp],
             case:acc,number:N, gender:G,w:WADJP],FSadjp),

 append(Pathlistadjp,Pathlistnp,PL1),
 append(PL1,Pathlistfreesub,PL),
 msort(PL,Sorted),
 contiguous(Sorted),    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 Dis is Distfreesub+Distnp+Distadjp,
 Weight is WFS+WNP+WADJP,

 map(exclamation,[type:accusative,pathlist:Sorted,distance:[Dis],gap:[],w:Weight,
    c_str:[exclamation,FSnp,FSadjp,C_str_FreeSub]])].







% cum, si etc  + subordinate/ (comma) / main clause

% cum rex scripsit libellum impudicum (,) regina felix erat.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,predfsub] --->
[ 
 mapped(pred,FSub),
 mapped(pred,FMain),
 FSub \= FMain,				%  2 clauses

 constraint([class:s,type:finite,punct:Punct,
             pathlist:PLSub,distance:[Distp1], gap:[],argbound:no,
             mood:MoodSub,
             tense:TenseSub,subordinator:Fsubordinator,
             w:W1,c_str:Head1],FSub),

 extremity(PLSub,StartMain),	

 constraint([type:finite,class:m,
             pathlist:PLMain,distance:[Distp2], gap:[],argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:W2,c_str:Head2],FMain),

 start(PLMain,StartMain),
							
 append(PLSub, PLMain,PL),                    
 msort(PL,Sorted),
 \+dup(Sorted),
 quasicontiguous(Sorted),                       % still room for one or two words out
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight),

% Clause semantics should be developed further
% we can use the nature of the subordinator as well as both mood and tense in both clauses


 ifthen( (constraint([lex:si],Fsubordinator),			% OPEN condition
          MoodMain=indicative,
          MoodSub=indicative),
          Clausesem=open),

 ifthen( (constraint([lex:si],Fsubordinator),                   % POTENTIALIS
          MoodSub=subjunctive,
          (TenseSub=perfect;TenseSub=present),
          MoodMain=subjunctive,
          TenseMain=present),
          Clausesem=potentialis),

 ifthen( (constraint([lex:si],Fsubordinator),			% IRREALIS (present)
          MoodSub=subjunctive,
          TenseSub=imperfect,
          MoodMain=subjunctive,
          TenseMain=imperfect),
          Clausesem=irrealis_present),

 ifthen( (constraint([lex:si],Fsubordinator),			% IRREALIS (past)
          MoodSub=subjunctive,
          (TenseSub=pluperfect;TenseSub=imperfect),
          MoodMain=subjunctive,
          TenseMain=pluperfect),
          Clausesem=irrealis_past),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),		       % CUM DUM for time full stop
          MoodSub=indicative,
          MoodMain=indicative),
          Clausesem=modality_free_time_clause),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),			% CUM DUM for time+ and so on...
          MoodSub=subjunctive,
          MoodMain=indicative),
          Clausesem=modality_laden_time_clause),

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             gap:[],class:m,
             argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:Weight,
             c_str:[clause_semantics:Clausesem, subordinator:Fsubordinator,subordinate_clause:Head1, main_clause:Head2]])
].


% main clause / (comma) / cum, si  etc + subordinate
% regina felix erat (,)  cum rex scripsit libellum impudicum.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for comments see above
% less freedom in word order - is that assumption right ??????

[finite,predfsuba] --->
[ 
 mapped(pred,FSub),
 mapped(pred,FMain),
 FSub \= FMain,				%  2 clauses

 constraint([class:s,type:finite,punct:P,
             pathlist:PLSub,distance:[Distp1], gap:[],argbound:no,
             mood:MoodSub,
             tense:TenseSub,subordinator:Fsubordinator,
             w:W1,c_str:Head1],FSub),

	

 constraint([type:finite,class:m,
             pathlist:PLMain,distance:[Distp2], gap:[],argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:W2,c_str:Head2],FMain),

 extremity(PLMain,StartSub),
 start(PLSub,StartSub),
							
 append(PLSub, PLMain,PL),                    
 msort(PL,Sorted),
 \+dup(Sorted),
 quasicontiguous(Sorted),                       % still room for one or two words out
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight),

% Clause semantics should be developed further
% we can use the nature of the subordinator as well as both mood and tense in both clauses


 ifthen( (constraint([lex:si],Fsubordinator),			% OPEN condition
          MoodMain=indicative,
          MoodSub=indicative),
          Clausesem=open),

 ifthen( (constraint([lex:si],Fsubordinator),                   % POTENTIALIS
          MoodSub=subjunctive,
          (TenseSub=perfect;TenseSub=present),
          MoodMain=subjunctive,
          TenseMain=present),
          Clausesem=potentialis),

 ifthen( (constraint([lex:si],Fsubordinator),			% IRREALIS (present)
          MoodSub=subjunctive,
          TenseSub=imperfect,
          MoodMain=subjunctive,
          TenseMain=imperfect),
          Clausesem=irrealis_present),

 ifthen( (constraint([lex:si],Fsubordinator),			% IRREALIS (past)
          MoodSub=subjunctive,
          (TenseSub=pluperfect;TenseSub=imperfect),
          MoodMain=subjunctive,
          TenseMain=pluperfect),
          Clausesem=irrealis_past),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),		       % CUM DUM for time full stop
          MoodSub=indicative,
          MoodMain=indicative),
          Clausesem=modality_free_time_clause),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),			% CUM DUM for time+ and so on...
          MoodSub=subjunctive,
          MoodMain=indicative),
          Clausesem=modality_laden_time_clause),

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             gap:[],class:m,
             argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:Weight,
             c_str:[clause_semantics:Clausesem, subordinator:Fsubordinator,subordinate_clause:Head1, main_clause:Head2]])
].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% with inserted, comma-bound subordinate
% Id tu, quoniam Macronem tanti facis, ignoscas mihi uelim (Cic., Ad Att. Liber IV, XII)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% here the comma proves essential to guide the parsing !


[finite,predfsub1] --->

[ 
 mapped(pred,FSub),
 mapped(pred,FMain),
 FSub \= FMain,				%  2 clauses

 constraint([class:s,type:finite,punct:lr,
             pathlist:PLSub,distance:[Distp1], gap:[],argbound:no,
             mood:MoodSub,
             tense:TenseSub,subordinator:Fsubordinator,
             w:W1,c_str:Head1],FSub),

 constraint([type:finite,class:m,
             pathlist:PLMain,distance:[Distp2], gap:[],argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:W2,c_str:Head2],FMain),

 recorded(fin,fin(Fin),_),
 start(PLMain,0),
 extremity(PLMain,Fin),
							
 append(PLSub, PLMain,PL),                    
 msort(PL,Sorted),
 \+dup(Sorted),
 quasicontiguous(Sorted),                      
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight),

% Clause semantics should be developed further
% we can use the nature of the subordinator as well as both mood and tense in both clauses


 ifthen( (constraint([lex:si],Fsubordinator),			% OPEN condition
          MoodMain=indicative,
          MoodSub=indicative),
          Clausesem=open),

 ifthen( (constraint([lex:si],Fsubordinator),                   % POTENTIALIS
          MoodSub=subjunctive,
          (TenseSub=perfect;TenseSub=present),
          MoodMain=subjunctive,
          TenseMain=present),
          Clausesem=potentialis),

 ifthen( (constraint([lex:si],Fsubordinator),			% IRREALIS (present)
          MoodSub=subjunctive,
          TenseSub=imperfect,
          MoodMain=subjunctive,
          TenseMain=imperfect),
          Clausesem=irrealis_present),

 ifthen( (constraint([lex:si],Fsubordinator),			% IRREALIS (past)
          MoodSub=subjunctive,
          (TenseSub=pluperfect;TenseSub=imperfect),
          MoodMain=subjunctive,
          TenseMain=pluperfect),
          Clausesem=irrealis_past),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),		       % CUM DUM for time full stop
          MoodSub=indicative,
          MoodMain=indicative),
          Clausesem=modality_free_time_clause),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),			% CUM DUM for time+ and so on...
          MoodSub=subjunctive,
          MoodMain=indicative),
          Clausesem=modality_laden_time_clause),

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             gap:[],class:m,
             argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:Weight,
             c_str:[clause_semantics:Clausesem, subordinator:Fsubordinator,subordinate_clause:Head1, main_clause:Head2]])
].




% SUBORDINATE WITH LEFT AND RIGHT PUNCT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,predfsubxlr] --->

[mapped(punct,[from:StP1,to:StartSubordinator, lex:comma|_]), % in front of Subordinator
 mapped(punct,[from:EndSub,to:EndP2, lex:comma|_]),  % at the end of SubClause
 StP1  \= EndSub,
 mapped(sub,[from:StartSubordinator,to:StartSubClause|Fsubordinator]),        % subordinatOR
 constraint([argbound:no],Fsubordinator),
 mapped(pred,SubClause),
 constraint([type:finite,class:m,pathlist:PLSub,distance:Dis, gap:[],
             number:Nsubj,person:Psubj,
             mood:MoodSub, tense:TenseSub,w:W1,c_str:HeadSub],SubClause),
 start(PLSub,StartSubClause),
 extremity(PLSub,EndSub),
 append([p(StartSubordinator,StartSubClause)], PLSub,NPLSub),                     % add subordinator to subordinate clause
 append([p(StP1,StartSubordinator)],NPLSub,NPLSub1),                              % add first comma
 append([p(EndSub,EndP2)],NPLSub1,NPLSub2),                                       % add second comma
 msort(NPLSub2,NPL1sorted),
 contiguous(NPL1sorted),
 map(pred,[cat:pred,type:finite,class:s,punct:lr, % lr: left-handside comma and right-handside comma
             pathlist:NPL1sorted,distance:Dis,
             gap:[],
             mood:MoodSub, tense:TenseSub,
             w:W1,subordinator:Fsubordinator,
             argbound:no,
             c_str:HeadSub])].


% SUBORDINATE WITH LEFT PUNCT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,predfsubxl] --->

[mapped(punct,[from:StP1,to:StartSubordinator, lex:comma|_]), % in front of Subordinator
 mapped(sub,[from:StartSubordinator,to:StartSubClause|Fsubordinator]),        % subordinatOR
 constraint([argbound:no],Fsubordinator),
 mapped(pred,SubClause),
 constraint([type:finite,class:m,pathlist:PLSub,distance:Dis, gap:[],
             number:Nsubj,person:Psubj,
             mood:MoodSub, tense:TenseSub,w:W1,c_str:HeadSub],SubClause),
 start(PLSub,StartSubClause),
 extremity(PLSub,EndSub),
 append([p(StartSubordinator,StartSubClause)], PLSub,NPLSub),                     % add subordinator to subordinate clause
 append([p(StP1,StartSubordinator)],NPLSub,NPLSub2),                              % add first comma
 msort(NPLSub2,NPL1sorted),
 contiguous(NPL1sorted),
 map(pred,[cat:pred,type:finite,class:s,punct:l, % left-handside comma
             pathlist:NPL1sorted,distance:Dis,
             gap:[],
             mood:MoodSub, tense:TenseSub,
             w:W1,subordinator:Fsubordinator,
             argbound:no,
             c_str:HeadSub])].


% SUBORDINATE WITH RIGHT PUNCT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,predfsubxr] --->

[ mapped(pred,SubClause), 
  constraint([type:finite,class:m,pathlist:PLSub,distance:Dis, gap:[],
             number:Nsubj,person:Psubj,
             mood:MoodSub, tense:TenseSub,w:W1,c_str:HeadSub],SubClause),
 start(PLSub,StartSubClause),
 extremity(PLSub,EndSub),
 mapped(punct,[from:EndSub,to:EndP2, lex:comma|_]),  % at the end of SubClause
 mapped(sub,[from:StartSubordinator,to:StartSubClause|Fsubordinator]),        % subordinatOR
 constraint([argbound:no],Fsubordinator),
 append([p(StartSubordinator,StartSubClause)], PLSub,NPLSub),                     % add subordinator to subordinate clause
 append([p(EndSub,EndP2)],NPLSub,NPLSub2),                                       % add second comma
 msort(NPLSub2,NPL1sorted),
 contiguous(NPL1sorted),
 map(pred,[cat:pred,type:finite,class:s,punct:r, % right-handside comma 
             pathlist:NPL1sorted,distance:Dis,
             gap:[],
             mood:MoodSub, tense:TenseSub,
             w:W1,subordinator:Fsubordinator,
             argbound:no,
             c_str:HeadSub])].

% SUBORDINATE WITHOUT PUNCT
%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,predfsubxn] --->

[ mapped(sub,[from:StartSubordinator,to:StartSubClause|Fsubordinator]), 
  mapped(pred,SubClause), 
  constraint([type:finite,class:m,pathlist:PLSub,distance:Dis, gap:[],
             number:Nsubj,person:Psubj,
             mood:MoodSub, tense:TenseSub,w:W1,c_str:HeadSub],SubClause),
 start(PLSub,StartSubClause),       
 constraint([argbound:no],Fsubordinator),
 append([p(StartSubordinator,StartSubClause)], PLSub,NPLSub2),                     % add subordinator to subordinate clause
 msort(NPLSub2,NPL1sorted),
 contiguous(NPL1sorted),
 map(pred,[cat:pred,type:finite,class:s,punct:n, % n: naked, no comma on either side
             pathlist:NPL1sorted,distance:Dis,
             gap:[],
             mood:MoodSub, tense:TenseSub,
             w:W1,subordinator:Fsubordinator,
             argbound:no,
             c_str:HeadSub])].





% ELLIPSIS
%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rex patriam amat, regina regem. (Finite + comma + arglist2)
% mater insanit, filius, pater.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,ellipted] --->

[ ( (mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]), CC=comma) ;
   (mapped(coord,[from:EndMain,to:StartEllipted, lex:Coord|_]), CC=Coord)),
 mapped(pred,Main),
 
constraint([type:finite,class:m,pathlist:PLMain,distance:[DistMain], gap:[],
             number:NMain,person:PMain,polarity:Pol,
             mood:MoodMain, tense:TenseMain,w:WMain,c_str:[illocutionary_force:Force,vg:FSverb|ST]],Main),
start(PLMain,0),
extremity(PLMain,EndMain),
append([p(EndMain,StartEllipted)],PLMain,NPLMain),
constraint([number:Nsubj,
              person:Psubj,
              voice:Voice,
              lex:Clex],FSverb),
lexarg(Clex,arglist:ArgList),
pick(ws(Lex,Class,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

   ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist and turn it into a subject
                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:nom],Oconstraints1,NewOconstraints),
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause

                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
               Argstomatch=Args),                                    % active voice : leave arglist as it is

match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:Nsubj,gender:_,person:Psubj]),
              finite,
              gap:[],
              w:WeightArgs,
              Int),
full_expansions(FE),
   expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WeightExpands, Int, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts

% PATH
flatten(PathlistArgs,FlatArgs),
flatten(ExpandPaths,FlatExpands),
msort(FlatArgs,ArgsSorted),
msort(FlatExpands,ExpandsSorted),
adjacent(ArgsSorted,ExpandsSorted),
append(ArgsSorted,ExpandsSorted,EP),
flatten(EP,EPflat),
msort(EPflat,EPsorted),
contiguous(EPsorted),
 \+dup(EPsorted),
start(EPsorted,StartEllipted),
append(NPLMain,EPsorted,PP),
msort(PP,PPsorted),
contiguous(PPsorted),

% DISTANCES
myappend(DistanceArgs,ExpandDistances,Distances),
flatten(Distances, FlatDist),
sum(FlatDist, DistanceSum),
myplus(DistanceSum,DistMain,DistanceAll),

% WEIGHTS
 myplus(WeightArgs,(round(WeightExpands/4)),Wpartial),
 myplus(Wmain,Wpartial,Wtot),

% ARG TREES
append(TreeArgs,ExpandTrees,AllTrees),
insort(AllTrees, NST),


map(pred,[cat:pred,
              type:finite,class:m,
              pathlist:PPsorted,
              distance:[DistanceAll],
              illocutionary_force:Force,
              number:NMain,                      % nber, gender, and person of the CLAUSE, not its subject			
              person:PMain,
              gender:neuter,
              mood:MoodMain,
              tense:TenseMain,
              polarity:Pol,
              argbound:no,
              gap:[],
              w:Wtot,
              add:Add,
              flagint:IntFlag,
              c_str:[illocutionary_force:Force,vg:FSverb,arglist:ST,linker:CC,arglist:NST]]) ].




% regem a regina amari, reginam autem a marco. (NONFinite + comma + arglist2)
% matres insanire, filios, patres.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


[finite,ellipted2] --->

[  mapped(flags,active(nonfinite)),
 ( (mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]), CC=comma) ;
   (mapped(coord,[from:EndMain,to:StartEllipted, lex:Coord|_]), CC=Coord)),
 mapped(pred,Main),
 constraint([type:nonfinite,pathlist:PLMain,distance:[DistMain], gap:[],
             number:NMain,person:PMain,polarity:Pol,
             mood:MoodMain, tense:TenseMain,w:WMain,c_str:[vg:FSverb|ST]],Main),

extremity(PLMain,EndMain),
append([p(EndMain,StartEllipted)],PLMain,NPLMain),

constraint([ type:nonfinite,
                voice:Voice,
                lex:Clex],FSverb),

lexarg(Clex,arglist:ArgList),
pick(ws(Lex,Class,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

   ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist and turn it into a subject
                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:acc],Oconstraints1,NewOconstraints),
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl,sem:[thing]]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause

                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
               Argstomatch=Args),                                    % active voice : leave arglist as it is




 match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:NNN,gender:GGG,person:PPP]),
              nonfinite,
              gap:[],
              w:WeightArgs,
              Int),

 full_expansions(FE),
   expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WeightExpands, Int, partial),  % here too weight is computed
									    % and the interrogative can hide in the adjuncts

% PATH

flatten(PathlistArgs,FlatArgs),
flatten(ExpandPaths,FlatExpands),
msort(FlatArgs,ArgsSorted),
msort(FlatExpands,ExpandsSorted),
adjacent(ArgsSorted,ExpandsSorted),
append(ArgsSorted,ExpandsSorted,EP),
flatten(EP,EPflat),
msort(EPflat,EPsorted),
contiguous(EPsorted),
 \+dup(EPsorted),
start(EPsorted,StartEllipted),
append(NPLMain,EPsorted,PP),
msort(PP,PPsorted),
contiguous(PPsorted),

% DISTANCES
myappend(DistanceArgs,ExpandDistances,Distances),
flatten(Distances, FlatDist),
sum(FlatDist, DistanceSum),
myplus(DistanceSum,DistMain,DistanceAll),



% ARG TREES
append(TreeArgs,ExpandTrees,AllTrees),
insort(AllTrees, NST),


 ifthenelse(constraint([subject:[hp:Pathsubj]],NST),  % we have a non-gapped subject, we record its head path
	     true,				      % and abstain from doing anything else
             Pathsubj=[p(0,0)]),		      % otherwise the subject is higher up and therefore necessarily precedes

  ifthenelse(constraint([object:[hp:Pathobj]],NST),				        % IF-CLAUSE we have an object
             ifthenelse(precedes(Pathsubj,Pathobj), NW is WeightArgs+1, NW=WeightArgs),         % THEN-CLAUSE
             NW=WeightArgs),								% ELSE-CLAUSE

  ifthenelse(constraint([i_object:[hp:Pathiobj1]],NST),					%  we have an indirect object (first type: dat)
             ifthenelse(precedes(Pathsubj,Pathiobj1), NW1 is NW+1, NW1=NW),
             NW1=NW),
  ifthenelse(constraint([object_i:[hp:Pathiobj2]],NST),					%  we have an indirect object (second type: acc)
             ifthenelse(precedes(Pathsubj,Pathiobj2), NW2 is NW1+1, NW2=NW1),           % as with doceo, for instance
             NW2=NW1),

% WEIGHTS
 myplus(NW2,(round(WeightExpands/4)),Wpartial),
 myplus(Wmain,Wpartial,Wtot),
 map(pred,[cat:pred,
              type:nonfinite,class:m,
              mood:MoodMain,
              tense:TenseMain,
              pathlist:PPsorted,
              distance:[DistanceAll],
              number:NMain, 
              gender:neuter, 
              case:or([nom,acc]),                       % nber, gender, and person of the CLAUSE, not its subject
              person:PMain,
              polarity:Pol,
              argbound:no,
              gap:[],
              w:Wtot,
              add:Add,
              flagint:IntFlag,
              c_str:[vg:FSverb,arglist:ST,linker:CC,arglist:NST]]) ].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RELATIVE CLAUSES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recall that the function of the np is independent from its function in the relative clause:
% "liber quem  rex legit ..." : liber is subject in the main clause and quem is object in the relative
% The index is shared; it reports the positions spanned by the np.
% A relative clause is an S displaying a [gap:Gap] feature corresponding to the antecedent:
% same category (np, pp) and shared index
% The gap site can specify any type of constraints on the constituent structure of the antecedent NP;
% this power is necessary to deal with mwus where the deletion site
% can point to an NP that must be lexically described,
% not just in terms of features such as number and broad semantic category


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% with a relative pronoun filling an np slot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% "(vir) qui  epistulas ad Marcum misit" ; "(librum) quem ancilla legit"


% subject, object, i_object, object_i ('doceo' type verbs)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,rel_clause_1] --->
[ (Function=subject; Function=object; Function=i_object;Function=object_i),

 mapped(relative,[from:X,to:Y|FS1]),		% a relative pronoun

 constraint([number:Nb, case:Case,gender:Gender, function:Functions],FS1),

 mapped(pred,FS2),                                % a clause

 constraint([type:finite,class:m,                        % relative clauses are finite
             mood:Mood, tense:Tense,pathlist:Pathlist,distance:Distance,w:Weight],FS2),

 msort(Pathlist,Sorted),
 start(Sorted,Y),
 contiguous(Sorted),		% the relative clause cannot bind structures outside of itself
                                % this restriction is important
				% strict contiguity seems called for      % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 constraint([gap:GAPARG,c_str:C_str],FS2),      % there MUST be a gap in the clause

 nonvar(GAPARG),				% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                % without such a test the danger is that the following test
						% is no test at all, but simply results in unification of the GAPARG variable
						% with whatever comes its way

 GAPARG=[gap:[type:np,                          % GAP specifies type
              index:Index,                      % opens a place for the Index of the antecedent to fill
              function:Function,                 % function must be compatible with relative pronoun
              Function:[e:Index],                % info for the parse tree - e (empty, trace) followed by the Index
              constraints:Constraints           % we put the constraints to be checked on the antecedent in this box
                           ]],


  member(Function,Functions),                   % remember that the acc rel pronouns must bear the 'subject' function as well as the
					       % object one

  append([p(X,Y)],Pathlist,Pnew),		% appending the relative pronoun to the path

  map(relative_clause,[pathlist:Pnew,
                         distance:Distance,
                         gap:GAPARG,              % gap info carried by the clause
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         case:Case,
                         type:finite,
                         mood:Mood,
                         tense:Tense,
                         constraints:Constraints,
                         w:Weight,
                         c_str:C_str]) ].





% prep+rel filling a pp slot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% "(vir) ad quem epistulam misisti" [pp-gap here]

[finite,rel_clause_2] --->
[mapped(prep,[from:A, to:B|FS1]),		% we have a prep
 constraint([lex:Prep, requires:Required],FS1), % and the requirement it sets on the case of the np it governs

 % the case is checked on the relative pronoun:
 mapped(relative,[from:B, to:C|FS2]),
 constraint([number:Nb,gender:Gender],FS2),
 constraint([case:Required,function:Functions],FS2),


 mapped(pred,FS3),
 constraint([type:finite,class:m, mood:Mood, tense:Tense,pathlist:Pathlist,distance:Distance,w:Weight],FS3),
 msort(Pathlist,Sorted),
 start(Sorted,C),
 contiguous(Sorted),                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 append([p(A,C)],Sorted,PL),
 constraint([gap:GAPARG,c_str:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:pp,
               index:Index,
               function:prep_cplt,
               prep_cplt:[e:Index],
               constraints:Constraints
                           ]],

 member(prep_cplt,Functions),  % this is the function that should be associated with prep+np pairs
                               % make sure that this is true or increase the possibilities here !!!!!!!!!!

 select_ppconstraints(Constraints,PPConstraints,OtherConstraints),

 % this predicates divides the constraints into two classes : the ones that need to be satisfied here
 % locally, and the ones that must wait for the antecedent to be available

 % the OtherConstraints concern the NP and are projected to the relative clause constraints

 constraint(PPConstraints,[prep:Prep]), % the prep constraints are satisfied here

 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:GAPARG,
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         case:Required,
                         type:finite,
			 mood:Mood, tense:Tense,
                         constraints:OtherConstraints,
                         w:Weight,
                         c_str:C_str]) ].



%  UBI OR CUM : specific constraints on antecedent, no gap to be filled
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Relative UBI
%%%%%%%%%%%%%%

% simplification, as here the relative cannot fill an arg position within the relative clause

[finite,rel_clause_2a] --->
[ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO
 mapped(adv,[from:A, to:B|FS2]),
 constraint([lex:ubi],FS2),         % UBI only here

 mapped(pred,FS3),
 constraint([type:finite, class:m,mood:Mood, tense:Tense,pathlist:Pathlist,
             distance:Distance,w:Weight,gap:[],c_str:C_str],FS3),

 start(Pathlist,B),
 append(Adtopath,[p(A,B)],HeadPath),
 append(HeadPath,Pathlist,PL),

 contiguous(PL),                                      % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:[],           % such relative clauses exhibit NO gap
                         index:_,
                         number:_,
                         gender:_,
                         case:_,           % and have no restrictions to impose on the antecedent
                         type:finite,
			 mood:Mood,
                         tense:Tense,
                         constraints:[sem:[loc]],  % except a semantic one (must be a place of some sort;
						   % this is checked by imposing the loc value
                         w:Weight,
                         c_str:[rel_type:Rel_type,C_str]]) ].


% Relative CUM
%%%%%%%%%%%%%%

% Memini temporis cum ancilla mea tuos libros in horto legeret.


[finite,rel_clause_2b] --->
[ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(adv,[from:A, to:B|FS2]),
 constraint([lex:cum],FS2),                    % idem for CUM

 mapped(pred,FS3),
 constraint([type:finite, class:m,mood:Mood, tense:Tense,
            pathlist:Pathlist,distance:Distance,w:Weight,gap:[],c_str:C_str],FS3),
 start(Pathlist,B),
 append(Adtopath,[p(A,B)],HeadPath),
 append(HeadPath,Pathlist,PL),

 contiguous(PL),                                 % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:[],
                         index:_,
                         number:_,
                         gender:_,
                         case:_,
                         type:finite,
			 mood:Mood,
                         tense:Tense,
                         constraints:[sem:[time]],    % a time-related antecedent is needed
                         w:Weight,
                         c_str:[rel_type:Rel_type,C_str]]) ].





% noun cplt
%%%%%%%%%%%

% amo reginam [ [cuius librum] legi]

% here we have an np containing the relative pronoun


[finite,rel_clause_2a] --->
[mapped(np,NP),
 constraint([pathlist:PLnoun,distance:[Distancenoun],
             type:core,					% we insist on the np being core
             index:Index,sem:_,
             c_str:Cstrnoun],NP),

 msort(PLnoun,Sortednoun),
 start(Sortednoun,B),                   % the np follows the relative pronoun : cuius  -> librum

 extremity(Sortednoun,Startrelative),  % the pred of the relative clause follows the NP: cuius librum -> legi

 mapped(relative,[from:A, to:B|FSrelative]),     % the relative pronoun must be genitive : cuius quorum quarum

 constraint([number:Nb,gender:Gender,case:gen],FSrelative), % relative shares gender and number with antecedent


 mapped(pred,FS3),			% the pred of the relative clause : legi [GAP]

 constraint([type:finite, class:m,mood:Mood, tense:Tense,
             pathlist:Pathlist,distance:[Distancepred],
             w:Weight],FS3),

 msort(Pathlist,Sorted),
 start(Sorted,Startrelative),            % the pred follows the noun
 contiguous(Sorted),                                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 constraint([gap:GAPARG,c_str:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:np,			% the gap is the whole np
               index:Index,
               function:Function,
               Function:[e:Index],
               constraints:Constraints
                           ]],

 % the Constraints concern the NP above, not the antecedent !!!!

 constraint(Constraints,NP),

 append([p(A,B)],Sortednoun,PLnp),
 append(PLnp,Sorted,PL),
 Distance is Distancenoun+Distancepred,

 map(relative_clause,[pathlist:PL,
                        distance:[Distance],
                        gap:[],				% the gap is already filled
                        index:Index,
                        number:Nb,                     % checks on number and gender are the only checks to be performed
                        gender:Gender,
                        case:_,
                        type:finite,
			mood:Mood, tense:Tense,
                        constraints:[],			% no constraint to be imposed on the antecedent
                        w:Weight,
                        c_str:[head:Cstrnoun, rel_clause:C_str]]) ].



% PP constraints
%%%%%%%%%%%%%%%%

select_ppconstraints([],[],[]) :- !.   % end of recursion

select_ppconstraints(Constraints,[prep:Prep],OtherC) :-        % the pp constraint has one form only
							       % namely restriction on the lex of the prep
							pick(prep:Prep,Constraints,OtherC),!.

select_ppconstraints(Constraints,[],Constraints).




% myappend
%%%%%%%%%%

% useful for gaps, which are most of the time uninstantiated variables or empty lists

 myappend([],[],[]) :- !.
 myappend([],X,X) :- nonvar(X),!.
 myappend(X,[],X) :- nonvar(X),!.
 myappend(X,Y,[]) :- var(X), var(Y), !.
 myappend(X,Y,X) :- var(Y),!.
 myappend(X,Y,Y) :- var(X),!.
 myappend([Head|L1],L2,[Head|L3]) :- myappend(L1,L2,L3).




% myplus
%%%%%%%%

% gets rid of vars on the fly
% recall that 'is' does not like uninstantiated vars !!!

myplus(A,B,0) :- var(A), var(B), !.
myplus(A,B,A) :- var(B), !.
myplus(A,B,B) :- var(A), !.
myplus(A,B,Sum) :- Sum is A+B.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COORDINATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ADJ PHRASES
%%%%%%%%%%%%%

[finite,adjp4] --->
[mapped(flags,active(coord)),  % coordination is computationally VERY expensive,
                              % as a standard coordinator like 'et' or 'comma'
                              % can join any two structures of the same type,
			      % from a single word to a whole clause.
			      % We therefore flag it using the occurrence of an explicit
			      % coordinator in the input string (not the comma but coordinators
			      % such as 'et' 'que' 'an'...)
 mapped(adjp,Fadj1),
 mapped(coord,[from:X,to:Y|Fcoord]),
ifthenelse( (constraint([lex:Second,type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

 mapped(adjp,Fadj2),
 constraint([pathlist:Path1,hp:HL1,distance:[Dis1],w:Weight1,case:Case, number:N, gender:G,type:Type,lex:Lex1],Fadj1),

 ifthenelse(constraint([c_str:C_str1],Fadj1),Headinfo1=C_str1,Headinfo1=Lex1),
	    % we have to take care of the phrases which do not sport a c_str feature

 constraint([pathlist:Path2,hp:HL2,distance:[Dis2],w:Weight2,case:Case, number:N, gender:G,type:_,lex:Lex2],Fadj2),
			% we insist on the usual coherence triple (Case, gender, number)
 ifthenelse(constraint([c_str:C_str2],Fadj2),Headinfo2=C_str2,Headinfo2=Lex2),
 Path1 \= Path2, % they are different !!!
 extremity(Path1,X), % the coordinator sits in between the 2 adj phrases -
                     % this condition amounts to an oversimplification
 start(Path2,Y),
 append(Path1,[p(X,Y)],Path1new),  % add coordinator to the path
 append(Path1new,Path2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Dis1+Dis2,
 myplus(Dis1,Dis2,Distance),
 % Weight is Weight1+Weight2+WeightCoord,
 myplus(Weight1,Weight2,WeightT),
  myplus(WeightT,WeightCoord,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,hp:HL,distance:[Distance],
             case:Case,number:N, gender:G,type:Type,lex:Lex1,w:Weight,
             c_str:[head:Headinfo1,coord:CoordOut,head:Headinfo2]])].


% coordinated gerundives (inadequate)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Urbs est mihi capienda delendaque.

% here is an example of what the lexicon yields for gerundives:

% lex(delenda,
%       gdiv,
%       [pos:gdiv, txt:delenda, case:abl, gender:fem, number:sing, lex:delere,
%        class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).


% we take the required extra info from the first conjunct

[core,gdivcoord] --->
[mapped(flags,active(coord)),

 mapped(gdiv,[from:A,to:B |Fgdiv1]),
 mapped(coord,[from:B,to:C|_]),
 mapped(gdiv,[from:C, to:D|Fgdiv2]),

 constraint([case:Case, number:N, gender:G,lex:Lex1, class:Class, kind:Kind],Fgdiv1),
 constraint([case:Case, number:N, gender:G,lex:Lex2],Fgdiv2),
 atomic_list_concat([Lex1,Lex2],' et ',TwoLex),                 % building the compound lex out of the two verbs

 map(gdiv,[from:A, to:D, case:Case,number:N, gender:G,
             lex:Lex1,compound_lex:TwoLex, class:Class,type:gdiv,kind:Kind,mood:gerund,person:3,
             c_str:[head:Lex1,coord:TwoLex,head:Lex2]]),


 map(gerundive,[cat:gerundive,pathlist:[p(A,D)],distance:[0], hp:[p(A,B)],
                  case:Case,number:N, gender:G,
                  lex:Lex1,compound_lex:TwoLex, class:Class,
                  type:gdiv,kind:Kind,mood:gerund,person:3,w:2,
                  c_str:[head:Lex1,coord:TwoLex,head:Lex2]])].

 % we map two different boxes:
 % 'gdiv' is used by gerundive predications (urbs est mihi capienda et delenda),
 % 'gerundive' is adjective-like and is used in predicative argument construction (karthago capienda delendaque est)


 % problem: the second lex does not appear in the tree
 % even if we put it as value of the compound_lex feature in the info box
 % we need to keep a clean Lex1 to connect with the arglist
 % and it is this lex that appears in the info box associated with the gerund-type predicate
 % if we do not put it as value of the coord feature, the compound_lex is properly built but remains hidden...


%%%%%%%%%%%%%%%%%%%%
% ADVERBIAL PHRASES
%%%%%%%%%%%%%%%%%%%%

[core,adv4] --->
[mapped(flags,active(coord)),  
 mapped(advp,Fadv1),
 mapped(coord,[from:X,to:Y|Fcoord]),
 constraint([lex:Second],Fcoord),             % Second because it MIGHT be the second member of a correlative pair
					     % Remember that variable names do NOT constraint variable values !!!!	

ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

 mapped(advp,Fadv2),
 
 constraint([pathlist:Path1,distance:[Dis1],w:Weight1,sem:Sem,type:Type,lex:Lex1],Fadv1),

 
 constraint([pathlist:Path2,distance:[Dis2],w:Weight2,type:_,lex:Lex2],Fadv2),
		
 Path1 \= Path2, % they are different !!!
 extremity(Path1,X), % the coordinator sits in between the 2 adv phrases -
                     % this condition amounts to an oversimplification
 start(Path2,Y),
 append(Path1,[p(X,Y)],PL1new),  % add coordinator to the path
 start(Path1,PL1Start),

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]), 
            					 % coordination in front of the first conjunct 'et hic et nunc'
            					 % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),
 
 append(PL1new2,Path2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 myplus(Dis1,Dis2,Distance),
 myplus(Weight1,Weight2,WeightT),
 myplus(WeightT,WeightCoord,Weight),
 myplus(1,Weight,WeightTot),        % bonus for cohesion due to coordination
 map(advp,[cat:advp,pathlist:Sorted,distance:[Distance],
             type:Type,lex:Lex1,w:WeightTot,sem:Sem,
             c_str:[head:Lex1,coord:CoordOut,head:Lex2]])].


% ADJUNCTS
%%%%%%%%%%%

[finite,adjunct4] --->
[mapped(flags,active(coord)),  
 mapped(adjunct,Fadjunct1),
 mapped(coord,[from:X,to:Y|Fcoord]),
 constraint([lex:Second],Fcoord),             % Second because it MIGHT be the second member of a correlative pair
					     % Remember that variable names do NOT constraint variable values !!!!	

ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

 mapped(adjunct,Fadjunct2),
 
 constraint([pathlist:Path1,distance:[Dis1],w:Weight1,value:Value,c_str:C_str1],Fadjunct1),

 
 constraint([pathlist:Path2,distance:[Dis2],w:Weight2,value:_,c_str:C_str2],Fadjunct2),
		
 Path1 \= Path2, % they are different !!!
 extremity(Path1,X), % the coordinator sits in between the 2 adjuncts -
                     % this condition amounts to an oversimplification
 start(Path2,Y),
 append(Path1,[p(X,Y)],PL1new),  % add coordinator to the path
 start(Path1,PL1Start),

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]), 
            					 % coordination in front of the first conjunct 'et in Roma et per annos'
            					 % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),
 
 append(PL1new2,Path2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 myplus(Dis1,Dis2,Distance),
 myplus(Weight1,Weight2,WeightT),
 myplus(WeightT,WeightCoord,Weight),
 myplus(6,Weight,WeightTot),        % bonus for cohesion due to coordination
 map(adjunct,[pathlist:Sorted,distance:[Distance],
             class:adjunct,value:Value,w:WeightTot,type:_,
             c_str:[head:C_str1,coord:CoordOut,head:C_str2]])].




% NPS
%%%%%%

% a proper treatment has to wait for the 'finite' run
% on account of rel clauses within one or more of the conjoined nps


[finite,np4] --->
[mapped(flags,active(coord)),     % making sure we have a coordinator

 mapped(np,Fnp1),
 mapped(np,Fnp2),
 mapped(coord,[from:X,to:Y|Fcoord]),
 
constraint([lex:Second],Fcoord),             % Second because it MIGHT be the second member of a correlative pair
					     % Remember that variable names do NOT constraint variable values !!!!	

ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 3, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],sem:Sem,
            number:_,person:3,gender:G1,case:Case,lex:Lexnp1,w:W1], Fnp1),

 ifthenelse(constraint([c_str:C_str1],Fnp1),Headinfo1=C_str1,Headinfo1=Lexnp1),
                                            % we have to take into account nps which do not have a c_str feature

 ifthenelse(constraint([type:int],Fnp1), Type=int,Type=full),  %  interrogative ? (useful for spotting wh-questions)

 constraint([pathlist:PL2, hp:HL2,distance:[Distnp2],case:Case,gender:G2,w:W2,lex:Lexnp2],Fnp2),
								% only CASE needs to be shared

 ifthenelse(constraint([c_str:C_str2],Fnp2),Headinfo2=C_str2,Headinfo2=Lexnp2),

 Lexnp2 \= dummy_np,  % not acceptable as second member of a conjoined np - prioritize the reading
                      % where a full relative clause is coordinated with another, the two
                      % sharing an antecedent

 PL1 \= PL2, % they ARE different

 Xminus1 is X-1,
 
 ifthenelse( (extremity(PL1,Xminus1), recorded(pos,position(Xminus1,X,First),_) ),  % leaving room for the first part of the correlative
                append(PL1,[p(Xminus1,X)],PL1n), % append first part
               (extremity(PL1,X), PL1n=PL1)       % first part elsewhere, or simply not the second member of a correlative pair
             ), 

 start(PL2,Y),
 start(PL1,PL1Start),

 ifthen( G1=G2,Gout=G1),
 ifthen((G1=masc;G2=masc),Gout=masc),


 
 append(PL1n,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]), 
            					 % coordination in front of the first conjunct 'et pater et mater'
            					 % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),
 
 
 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Distnp1+Distnp2,
 myplus(Distnp1,Distnp2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight1),
 myplus(Weight1,WeightCoord,Weight),

map(np,[pathlist:Sorted, hp:HL,index:i(HL),distance:[Distance],
           sem:Sem,class:common,lex:Lexnp1,lextype:full,type:Type,
           number:pl, person:3, gender:Gout,case:Case,w:Weight,coord:yes,
           c_str:[head:Headinfo1,coord:CoordOut, head:Headinfo2]])].


% number should not be registered as plural (domus et hortus...)
% and gender is best left open as well (sometimes neuter
% plural for two non-neuter nps - anyway, the two conjuncts
% do not necessarily share gender...)
% SUCH A POLICY WOULD BE DISASTROUS IN GENERATION !!!
% and even in parsing leads to spurious parses being produced

% perhaps more advisable to relax the requirements with 'esse' only...




% NON x SED Y non miles sed imperator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np4a] --->
[mapped(flags,active(coord)),     % making sure we have a coordinator

 mapped(np,Fnp1),
 mapped(np,Fnp2),
 mapped(neg,[from:A,to:B|Fneg]),
 mapped(coord,[from:X,to:Y|Fcoord]),
 constraint([lex:sed],Fcoord),
 constraint([lex:non],Fneg),

constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],sem:Sem,
            number:_,person:3,gender:_,case:Case,lex:Lexnp1,w:W1], Fnp1),

 ifthenelse(constraint([c_str:C_str1],Fnp1),Headinfo1=C_str1,Headinfo1=Lexnp1),
                                            % we have to take into account nps which do not have a c_str feature

 ifthenelse(constraint([type:int],Fnp1), Type=int,Type=full),  %  interrogative ? (useful for spotting wh-questions)

 constraint([pathlist:PL2, hp:HL2,distance:[Distnp2],case:Case,w:W2,lex:Lexnp2],Fnp2),
								% only CASE needs to be shared
 ifthenelse(constraint([c_str:C_str2],Fnp2),Headinfo2=C_str2,Headinfo2=Lexnp2),
 Lexnp2 \= dummy_np,  % not acceptable as second member of a conjoined np - prioritize the reading
                      % where a full relative clause is coordinated with another, the two
                      % sharing an antecedent
 PL1 \= PL2, % they ARE different

 start(PL1,B),

extremity(PL1,X),               % the coordinator sits in between the 2 NPs - this condition amounts to an oversimplification
 
start(PL2,Y),

 append(PL1,[p(X,Y)],PL1more),  % add coordinator to the path
 append([p(A,B)],PL1more,PL1new),
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Distnp1+Distnp2,
 myplus(Distnp1,Distnp2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight),

map(np,[pathlist:Sorted, hp:HL,index:i(HL),distance:[Distance],
           sem:Sem,class:common,lex:Lexnp1,lextype:full,type:Type,
           number:pl, person:3, gender:_,case:Case,w:Weight,coord:yes,
           c_str:[neg:Fneg,head:Headinfo1,coord:Fcoord, head:Headinfo2]])].






% we still need to account for the adjectives that can be associated with coordinate nps:

% COORD NPS with adj
%%%%%%%%%%%%%%%%%%%%

[finite,np5] --->
[mapped(flags,active(coord)),     % making sure we have a coordinator

 mapped(np,Fnp),
 mapped(adjp,Fadj),

 constraint([pathlist:PLNP,
             hp:HLNP,
             distance:[Distnp],
             sem:Sem,index:Index,
             class:common,
             lextype:full,
             type:Type,
             number:pl,person:3,
             gender:Gender,case:Case,
             lex:LexNoun,w:WNP,
             coord:yes,				% making sure we have a coordinated np
             c_str:HeadNP], Fnp),

  constraint([pathlist:PLADJ, hp:_,distance:[Distadj],
             case:Case,number:pl,gender:Gender,                % gender gets projected from the adj,
                                                               % as it is left open in the coordinated np
             w:WADJ,lex:_],Fadj),
								%  CASE and NUMBER need to be shared
						                % good grounds for insisting on a plural

  adjacent(PLNP, PLADJ),   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

  append(PLNP,PLADJ,PL),
   msort(PL, Sorted),
  \+dup(Sorted),
  % Distance is Distnp+Distadj,
  % Weight is WNP+WADJ,
  myplus(Distnp,Distadj,Distance),
  myplus(WNP,WADJ,Weight),
  map(np,[pathlist:Sorted, hp:HLNP,index:Index,distance:[Distance],
           sem:Sem,class:common,lex:LexNoun,lextype:full,type:Type,
           number:pl, person:3, gender:Gender,case:Case,w:Weight,coord:yes,
           c_str:[np,HeadNP,adjp:Fadj]])].




% PPs
%%%%%%

% very sketchy, as are ALL coordinate structures in this parser !!!!!!!!!!!!!!!!!!!!!!

[finite,pp4] --->
[mapped(flags,active(coord)),  % we have a coordinator somewhere in the string
 mapped(pp,Fpp1),
 mapped(pp,Fpp2),
 Fpp1 \= Fpp2,                % ought to be different
 mapped(coord,[from:X,to:Y|Fcoord]),
 ifthenelse( (constraint([lex:Second,type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),
 constraint([case:Case, prep:Lexprep,pathlist:PL1,hp:HL,distance:[Distpp1],
             sem:Sem,lex:LexNoun,w:W1,c_str:Head1],Fpp1),
 constraint([pathlist:PL2,distance:[Distpp2],w:W2,c_str:Head2],Fpp2),
                                                                     % the pps need not share anything else
								     % than the fact that they are pps

 extremity(PL1,X), % the coordinator sits in between the 2 PPs
 start(PL2,Y),
 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
 % Distance is Distpp1+Distpp2,
 % Weight is W1+W2,
 myplus(Distpp1,Distpp2,Distance),
 myplus(W1,W2,Weight1),
 myplus(Weight1,WeightCoord,Weight),
 map(pp,[pathlist:Sorted, hp:HL, distance:[Distance],index:_,
           cat:pp,case:Case,prep:Lexprep, sem:Sem,lex:LexNoun,      % we carry the info over from the FIRST pp
								    % obviously inadequate
								    % the other 'solution' is to leave the features
								    % with uninstantiated variables as values
							            % trouble is then on the way
           w:Weight,
           c_str:[head:Head1,coord:CoordOut, head:Head2]])].



% VERBS
%%%%%%%


[verb,vg2] --->
[ mapped(flags,active(coord)),
  mapped(v,[from:A, to:B|Fverb1]),
  mapped(coord,[from:B, to:C|Fcoord]),
  ifthenelse( (constraint([lex:Second,type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),
  mapped(v,[from:C, to:D|Fverb2]),
  constraint([class:Class,type:Type, voice:Voice,number:Nb, person:P, lex:Lex1, mood:Mood, tense:Tense],Fverb1),
  constraint([class:Class,type:Type,             number:Nb, person:P, lex:_],Fverb2),

% a certain amount of compatibility is needed :
		% class (transitivity etc.),
                % type (finite , nonfinite),
                % number,
                % person

% we copy voice mood and tense from the first verb

  map(vgpos,[cat:vg,class:Class, voice:Voice,type:Type,pathlist:[p(A,D)],hp:[p(A,B),p(C,D)],
              number:Nb, person:P, mood:Mood, gender:_,tense:Tense,lex:Lex1, w:WeightCoord,
              c_str:[head:Fverb1,coord:CoordOut,head:Fverb2]])].


% PARTICIPLE CLAUSES
%%%%%%%%%%%%%%%%%%%%%

[finite,part_clause_coord] --->
[mapped(flags,active(coord)),     % making sure we have a coordinator

 mapped(participle_clause,Fpc1),
 mapped(participle_clause,Fpc2),

 mapped(coord,[from:X,to:Y|Fcoord]),
 
constraint([lex:Second],Fcoord),             % Second because it MIGHT be the second member of a correlative pair
					     % Remember that variable names do NOT constraint variable values !!!!	

ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 3, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

constraint([pathlist:PL1, hp:HL1,distance:[Dist1],number:N1,gender:G1,case:C1,w:W1, c_str:C_str1, type:Type, lex:Lex], Fpc1),
constraint([pathlist:PL2, hp:HL2,distance:[Dist2],number:N1,gender:G1,case:C1,w:W2, c_str:C_str2], Fpc2),
								
PL1 \= PL2, % they ARE different

 Xminus1 is X-1,
 
 ifthenelse( (extremity(PL1,Xminus1), recorded(pos,position(Xminus1,X,First),_) ),  % leaving room for the first part of the correlative
                append(PL1,[p(Xminus1,X)],PL1n), % append first part
               (extremity(PL1,X), PL1n=PL1)       % first part elsewhere, or simply not the second member of a correlative pair
             ), 

 start(PL2,Y),
 start(PL1,PL1Start),

 append(PL1n,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]), 
            					 % coordination in front of the first conjunct 'et credens et amaturus'
            					 % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),
 
 
 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Dist1+Dist2,
 myplus(Dist1,Dist2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight1),
 myplus(Weight1,WeightCoord,Weight),

map(participle_clause,[pathlist:Sorted, hp:HL,distance:[Distance],
           type:Type,cat:ppclause,lex:Lex,
           number:N1, gender:G1,case:C1,w:Weight,
           c_str:[C_str1,coord:CoordOut, C_str2]])].



% GERUNDS
%%%%%%%%%

[finite,coord_gerund] --->
[mapped(flags,active(coord)),     % making sure we have a coordinator

 mapped(pred,FG1),
 mapped(pred,FG2),

 mapped(coord,[from:X,to:Y|Fcoord]),
 
constraint([lex:Second],Fcoord),             % Second because it MIGHT be the second member of a correlative pair
					     % Remember that variable names do NOT constraint variable values !!!!	

ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 3, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

constraint([pathlist:PL1, distance:[Dist1],local_case:Case,w:W1, c_str:C_str1, type:gerund], FG1),
constraint([pathlist:PL2, distance:[Dist2],local_case:Case,w:W2, c_str:C_str2, type:gerund], FG2),
								
PL1 \= PL2, % they ARE different

 Xminus1 is X-1,
 
 ifthenelse( (extremity(PL1,Xminus1), recorded(pos,position(Xminus1,X,First),_) ),  % leaving room for the first part of the correlative
                append(PL1,[p(Xminus1,X)],PL1n), % append first part
               (extremity(PL1,X), PL1n=PL1)       % first part elsewhere, or simply not the second member of a correlative pair
             ), 

 start(PL2,Y),
 start(PL1,PL1Start),

 append(PL1n,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]), 
            					 % coordination in front of the first conjunct 'et credens et amaturus'
            					 % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),
 
 
 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 % Distance is Dist1+Dist2,
 myplus(Dist1,Dist2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight1),
 myplus(Weight1,WeightCoord,Weight),

map(pred,[cat:pred,type:gerund,	class:m,		% type:gerund is how we specify them in args and adjuncts
               case:_,
               mood:gerund,
               local_case:Case,                         % case is used for subject agreement,
							% local_case to store the case of the gerund
               number:sing,person:3,
               gender:neuter,
               pathlist:Sorted, distance:[Distance],
               gap:[],w:Weight,c_str:[C_str1,coord:CoordOut, C_str2]])].




% ABL ABS
%%%%%%%%%

[finite,coord_aa] --->
[mapped(flags,active(coord)),     % making sure we have a coordinator

 mapped(pred,FAA1),
 mapped(pred,FAA2),

 mapped(coord,[from:X,to:Y|Fcoord]),
 
constraint([lex:Second],Fcoord),             % Second because it MIGHT be the second member of a correlative pair
					     % Remember that variable names do NOT constraint variable values !!!!	

ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 3, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),


constraint([pathlist:PL1, distance:[Dist1],type:aa,w:W1, c_str:C_str1], FAA1),
constraint([pathlist:PL2, distance:[Dist2],type:aa,w:W2, c_str:C_str2], FAA2),
								
PL1 \= PL2, % they ARE different

 Xminus1 is X-1,
 
 ifthenelse( (extremity(PL1,Xminus1), recorded(pos,position(Xminus1,X,First),_) ),  % leaving room for the first part of the correlative
                append(PL1,[p(Xminus1,X)],PL1n), % append first part
               (extremity(PL1,X), PL1n=PL1)       % first part elsewhere, or simply not the second member of a correlative pair
             ), 

 start(PL2,Y),
 start(PL1,PL1Start),

 append(PL1n,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]), 
            					 % coordination in front of the first conjunct 'et rege amato a regina et mutatis mutandis'
            					 % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),
 
 
 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 % Distance is Dist1+Dist2,
 myplus(Dist1,Dist2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight1),
 myplus(Weight1,WeightCoord,Weight),
 map(pred,[cat:pred,type:aa,case:abl,mood:aa,number:sing,person:3,
               class:m,
               pathlist:Sorted, distance:[Distance],
               gap:[],w:Weight,c_str:[C_str1,coord:CoordOut, C_str2]])].

 

%%%%%%%%%%%%%%%%%%%%
% Coordinated preds
%%%%%%%%%%%%%%%%%%%%

%
% FULL PREDICATIONS
%

% NONFINITE
%%%%%%%%%%%

% (puto) regem epistulas scrivere et reginam librum regis legisse

[finite,prednfcc] --->
[mapped(flags,active(nonfinite)),
 mapped(flags,active(coord)),        % two flags to check...

 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
 
 mapped(coord,[from:X,to:Y|Fcoord]),
 constraint([lex:Second],Fcoord),  
 ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

 constraint([type:nonfinite,mood:Mood,tense:Tense,
             pathlist:PL1,distance:[Distp1],gap:Gap1,w:W1,c_str:Head1],Fp1),
 constraint([type:nonfinite,mood:Mood,
             pathlist:PL2,distance:[Distp2],gap:Gap2,w:W2,c_str:Head2],Fp2),
 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),
 myappend(Gap1,Gap2,Gap),
 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 % Distance is Distp1+Distp2,
 % Weight is W1+W2,
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight1),
 myplus(Weight1,WeightCoord,Weight),
 map(pred,[pathlist:Sorted,distance:[Distance],
             type:nonfinite,
             cat:pred,
             mood:Mood,tense:Tense,
             class:m,
             number:sing,gender:neuter,
             case:or([nom,acc]),person:3,
             gap:Gap, w:Weight,
             c_str:[head:Head1,coord:CoordOut, head:Head2]])
].


% FINITE
%%%%%%%%

% rex scribit epistulas et regina legit librum

[finite,predfcc] --->
[mapped(flags,active(coord)),
 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,

 mapped(coord,[from:X,to:Y|Fcoord]),
 constraint([lex:Second],Fcoord),  
ifthenelse( (constraint([type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

 constraint([type:finite,class:m,
             pathlist:PL1,
             distance:[Distp1],
             mood:Mood, tense:Tense,number:Nsubj,person:Psubj,
             gap:Gap1,
             w:W1,
             c_str:Head1],Fp1),
 constraint([type:finite,class:m,
             pathlist:PL2,
             distance:[Distp2],
             gap:Gap2,
             w:W2,
             c_str:Head2],Fp2),
 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),
 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 myappend(Gap1,Gap2,Gap),      % special type of append for gaps - tries to get rid of empty gaps
 msort(PL, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
% Distance is Distp1+Distp2,
% Weight is W1+W2,
   myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight1),
  myplus(Weight1,WeightCoord,Weight),

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             argbound:no,
             mood:Mood, tense:Tense,
             class:m,
             number:Nsubj,person:Psubj,
             gap:Gap,
             w:Weight,
             c_str:[head:Head1,coord:CoordOut, head:Head2]])
].


% SUBORDINATE CLAUSES
%%%%%%%%%%%%%%%%%%%%%

[finite,predfsub] --->
[mapped(flags,active(coord)),
 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
mapped(coord,[from:X,to:Y|Fcoord]),
constraint([lex:Et],Fcoord),
constraint([type:finite,class:s,subordinator:Fs1,
             pathlist:PL1,
             distance:[Distp1],
             mood:Mood, tense:Tense,
             w:W1,
             c_str:Head1],Fp1),
 constraint([type:finite,class:s,subordinator:Fs2,
             pathlist:PL2,
             distance:[Distp2],
             w:W2,
             c_str:Head2],Fp2),
 constraint([lex:S1],Fs1),
  constraint([lex:S2],Fs2),

 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),
 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 flatten(PL, PLflat),
 msort(PLflat, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight),
 myplus(Weight,4,WeightWithBonus),
 map(pred,[cat:pred,type:finite,class:s,punct:Punct,
             pathlist:Sorted,distance:[Distance],
             argbound:no,
             subordinator:Fs1,subordinator:Fs2,
             mood:Mood, tense:Tense,
             gap:[],
             w:WeightWithBonus,
             c_str:[lex:S1,head:Head1, coord:Et, lex:S2,head:Head2]])].



%%%%%%%%%%%%%%%%%%%%%%%%
% PARTIAL PREDICATIONS
%%%%%%%%%%%%%%%%%%%%%%%%

% FINITE
%%%%%%%%

% subject + coordinated verb group
% rex scribit epistulas et legit librum
% -> rex scribit epistulas + legit librum
% find a pred and record person, number of the subject
% insert coord
% find a finite pred with no subject
% check subject features
% register the lot as a single clause

% this looks a bit ad hoc ;-)
% you bet !


[finite,predfnp] --->
[mapped(flags,active(coord)),  % we have a coordinator
 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,

 mapped(coord,[from:X,to:Y|Fcoord]), % here it is
 ifthenelse( (constraint([lex:Second,type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

 constraint([type:finite,class:m,
             mood:Mood,
             tense:Tense,
             pathlist:PL1,distance:[Distp1],
             number:Nsubj,			% subject features have to be shared
             person:Psubj,
             c_str:Head1,gap:Gap1,w:W1],Fp1),

 constraint([type:finite_i,      % subjectless
             mood:Mood,
             pathlist:PL2,distance:[Distp2],
             number:Nsubj,                        % the subject is shared
             person:Psubj,
             c_str:Head2,gap:Gap2,w:W2],Fp2),

 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),

 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
   \+dup(Sorted),
 contiguous(Sorted),                                         % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 % Distance is Distp1+Distp2,
 % Weight is W1+W2,
  myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight1),
  myplus(Weight1,WeightCoord,Weight),
  myplus(Weight,2,WeightWithBonus),

 myappend(Gap1,Gap2,Gaps),		% adding up the gaps, i.e. getting rid of empty gaps

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             number:Nsubj,person:Psubj,
             gender:neuter,
             mood:Mood, tense:Tense,
             class:m,
             argbound:no,
             gap:Gaps,
             w:WeightWithBonus,
             c_str:[head:Head1,coord:CoordOut, head:Head2]])
].




% NONFINITE
%%%%%%%%%%%

% puto regem epistulas scribere et librum legere

% treatment along the same lines : regem epistulas scribere (full nonfinite clause)
% followed by partial, subject-less non finite clause (nonfinite_i):

[finite,prednfp] --->
[mapped(flags,active(coord)),
 mapped(flags,active(nonfinite)),    % both flags need to be on

 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,

 mapped(coord,[from:X,to:Y|Fcoord]),
 ifthenelse( (constraint([lex:Second,type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),

 constraint([type:nonfinite,
             mood:Mood, tense:Tense,gap:Gap1,
             pathlist:PL1,distance:[Distp1],w:W1,
             c_str:Head1],Fp1),

 constraint([type:nonfinite_i,
             mood:Mood,
             pathlist:PL2,distance:[Distp2],
             gap:Gap2,w:W2,
             c_str:Head2],Fp2),

 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),

 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                          % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
% Distance is Distp1+Distp2,
% Weight is W1+W2,
  myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight1),
  myplus(Weight1,WeightCoord,Weight),
  myplus(Weight,2,WeightWithBonus),
 myappend(Gap1,Gap2,Gaps),     % special append for gaps
 map(pred,[cat:pred,
             type:nonfinite,
             mood:Mood, tense:Tense,
             class:m,
             pathlist:Sorted,distance:[Distance],
             number:sing,gender:neuter,case:or([nom,acc]),person:3,
             gap:Gaps,w:WeightWithBonus,
             c_str:[head:Head1,coord:CoordOut, head:Head2]])
].


% obliuisci et tacere
%%%%%%%%%%%%%%%%%%%%%

% coupling of infinitive clauses

[finite,prednfp1] --->
[mapped(flags,active(coord)),
 mapped(flags,active(nonfinite)),  % two flags

 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,

 mapped(coord,[from:X,to:Y|Fcoord]),

 ifthenelse( (constraint([lex:Second,type:correlative],Fcoord), 
              corr(Pair,First,Second), 
              recorded(pos,position(_,_,First),_)
              ), 
          
              (WeightCoord is 10, CoordOut=Pair),
              (WeightCoord is 0, CoordOut=Second)
            ),



constraint([type:nonfinite_i,              % no subject for the infinitives
             mood:infinitive,
             tense:Tense,
             pathlist:PL1,distance:[Distp1],
             gap:[],			  % the two clauses must be gap-free
             w:W1,c_str:Head1],Fp1),

 constraint([type:nonfinite_i,
             mood:infinitive,
             pathlist:PL2,distance:[Distp2],
             gap:[],
             w:W2,c_str:Head2],Fp2),

 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),

 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                              % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 % Distance is Distp1+Distp2,
% Weight is W1+W2,
 myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight1),
  myplus(Weight1,WeightCoord,Weight),
  myplus(Weight,2,WeightWithBonus),
 map(pred,[cat:pred,type:nonfinite_i,
             mood:infinitive,
             tense:Tense,
             class:m, 
             pathlist:Sorted,distance:[Distance],
             number:sing,gender:neuter,case:or([nom,acc]),person:3,
             gap:[],
             w:WeightWithBonus,
             c_str:[head:Head1,coord:CoordOut, head:Head2]])
].



%%%%%%%%%%%%%%%%%%
% RELATIVE CLAUSES
%%%%%%%%%%%%%%%%%%

 % with relative pronouns at the start of each clause

 % vir [qui	[np_e] librum legit] et [qui	[np_e] epistulas ad Marcum mittit]
 % librum [quem		amas [np_e]] et [quem	regina legit [np_e]]
 % vir [qui	[np_e] amat reginam] et [ad quem	rex epistulas longas mittit [pp_e]]

 % the other types of conjuncts concern the clause itself and are dealt with elsewhere,
 % in a **** VERY INADEQUATE FASHION **** !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


[finite,rel_clause_3] --->
[ mapped(flags,active(coord)),
  mapped(coord,[from:X, to:Y|FS2]),


  mapped(relative_clause,FS1),
  mapped(relative_clause,FS3),

  constraint([pathlist:PL1,
              distance:[D1],
              number:Nb,          % number and gender must be shared
              gender:Gender,
              case:Case,          % not case
              gap:GAPARG1,
              mood:Mood,
              tense:Tense,
              index:Index,
              constraints:Co1,
              w:W1,
              c_str:C_str1],FS1),

  constraint([pathlist:PL2,
              distance:[D2],
              number:Nb,
              gender:Gender,
              % gap:GAPARG2,       % simplification
              index:Index,
              % constraints:Co2,   % simplification
              w:W2,
              c_str:C_str2],FS3),

 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),
 append(PL1,[p(X,Y)],PLL),  % add coordinator to the path
 append(PLL,PL2,Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 % Distance is D1+D2,
  myplus(D1,D2,Distance),

 % Weight is W1+W2+3,  % bonus for greater cohesion - 2 relative clauses with the same antecedent
                     % rather than a second clause with antecedent understood

  myplus(W1,W2,T1),
  myplus(T1,3,Weight),
  % funify(Co1,Co2,UnifiedCos),  % the constraints add up - this is why we have to use 'funify' instead of 'constraint'
                                % but we ought to keep only the constraints which concern the antecedent !!!!!!!!!!!!!!!!
			        % this procedure needs to be worked on

  % myappend(GAPARG1,GAPARG2,GAPARG), % such an appending is needed, but likely to lead to trouble
                                      % in this simplified treatment we simply copy the gap value of the first clause

  map(relative_clause,[pathlist:Sorted,
                         distance:[Distance],
                         gap:GAPARG1,		% copied from the first rel clause
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         case:Case,
                         type:finite,
                         mood:Mood,
                         tense:Tense,
                         constraints:Co1,       % ditto
                         w:Weight,
                         c_str:[rel_clause:C_str1, coord:FS2,
                                rel_clause:C_str2]]) ].






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCEDURES RELATING TO THE PATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% these procedures examine which parts of the string are covered by various elements
% they are meant to measure properties like adjacency, contiguousness and distance

% the path is a list of p(X,Y) structures, where X and Y stand for positions in the string,
% as computed when the string is entered in and processed, just after being delivered as a list of words
% by 'getsentence'


% FINDING A PATH THAT COVERS THE WHOLE SENTENCE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% all words must be used up
% no gap and the end of the sentence must be reached
% Begin and End are the extremities of the pathlist (0 and whatever fin(Fin) records)

 path(Begin,End,Pathlist) :-
	pick(p(Begin,Next),Pathlist,RPaths),
	path(Next,End,RPaths).

 path(E,E,[]).


% ADJACENCY
%%%%%%%%%%%

% see nps with genitive np as subconstituent for an example of the relevance of such a procedure

% strict
%

% one pair in the first path has an end which corresponds to the beginning of a pair
% belonging to the second path
% [p(3,5), p(2,3] and [p(5,6), p(6,8), p(8,9)] for instance

 adjacent(PL1,PL2) :- member(p(_,Y),PL1), member(p(Y,_),PL2),!.
 adjacent(PL1,PL2) :- member(p(_,Y),PL2), member(p(Y,_),PL1),!.
 adjacent([],_).
 adjacent(_,[]).



/* the following algorithm is not to be used because of potential holes in path:
   rex recubat patulae sub tegmine fagi
   PL1 p(4,5) = tegmine
   PL2 p(2,3), p(5,6) = patulae fagi

adjacent(PL1,[p(Y,_)|_]) :- last(PL1, p(_,Y)).
adjacent([p(Y,_)|_],PL2) :- last(PL2, p(_,Y)).
relaxadjacent(PL1,[p(Y,_)|_]) :- last(PL1, p(_,X)), succ(X,Y).
relaxadjacent([p(X,_)|_],PL2) :- last(PL2, p(_,Y)), succ(Y,X).
*/


% relaxed adjacency
%%%%%%%%%%%%%%%%%%%

% a distance of 1 or 2 (in the case of relaxadjacent2) is allowed between the two corresponding pairs

 relaxadjacent(PL1,PL2) :- member(p(_,Y),PL1), member(p(X,_),PL2), succ(Y,X),!.
 relaxadjacent(PL1,PL2) :- member(p(_,Y),PL2), member(p(X,_),PL1), succ(Y,X).

 relaxadjacent2(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(C,_),PL2),
                              succ(A,B),
                              succ(B,C),!.

 relaxadjacent2(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(C,_),PL1),
                              succ(A,B),
                              succ(B,C).

 relaxadjacent3(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(D,_),PL1),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D).

relaxadjacent3(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(D,_),PL2),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D).

relaxadjacent4(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(E,_),PL1),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E).


relaxadjacent4(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(E,_),PL2),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E).

relaxadjacent5(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(F,_),PL1),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E),
                              succ(E,F).


relaxadjacent5(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(F,_),PL2),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E),
                              succ(E,F).


% relaxed adjacency with control on intervening elements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% with respect to POS:noun
%

% sometimes we have to check that no noun occurs in an interval
% as when we wish to relate the 2 heads of nps linked by the cplt noun relation
% involving a gentitive phrase
% Marci servas amicos
% Marci preferably linked with servas rather than amicos:
% putabas Marci servas amicos reginae amasse

relaxedadjacent1_n(PL1,PL2,n) :- member(p(_,Y),PL1), member(p(X,_),PL2),
                                                succ(Y,X),
                                                \+ mapped(noun,[from:Y,to:X|_]).

relaxedadjacent2_n(PL1,PL2,n) :- member(p(_,A),PL1), member(p(C,_),PL2),
                                                succ(A,B),succ(B,C),
                                                \+ mapped(noun,[from:A,to:B|_]),
                                                \+ mapped(noun,[from:B,to:C|_]).
% with respect to CASE GENDER and NUMBER
%

% when we try to relate adj and noun
% we are not likely to be allowed to jump a noun with all the right properties in terms of
% case gender and number:
% putabas malas servas amicas reginae fuisse
% malas is not likely to link with amicas by 'jumping' servas

relaxedadjacent1_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,Y),PL1), member(p(X,_),PL2),
                                                succ(Y,X),
                                                mapped(noun,[from:Y,to:X|FSnoun]),
                                                constraint([case:Case,gender:Gender,number:Nb],FSnoun),
                                                !, fail.

relaxedadjacent1_cgn(_,_,_,_,_).

relaxedadjacent2_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(C,_),PL2),
                                                succ(A,B),succ(B,C),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                   (mapped(noun,[from:B,to:C|FSnoun2]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun2))),
                                                !, fail.

relaxedadjacent2_cgn(_,_,_,_,_).

relaxedadjacent3_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(D,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3)))),
                                                !, fail.

relaxedadjacent3_cgn(_,_,_,_,_).


relaxedadjacent4_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(E,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4))


                                                 )),
                                                !, fail.

relaxedadjacent4_cgn(_,_,_,_,_).

relaxedadjacent5_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(F,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),succ(E,F),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4));
								(mapped(noun,[from:E,to:F|FSnoun5]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun5))


                                                 )),
                                                !, fail.

relaxedadjacent5_cgn(_,_,_,_,_).



% PATH CONTIGUITY
%%%%%%%%%%%%%%%%%

% the various elements follow each other without leaving a gap        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

contiguous([]).
contiguous([_]).

contiguous([p(_,Y),p(Y,Z)|Tail]) :- contiguous([p(Y,Z)|Tail]).


% in quasicontiguous we allow one or two or even three or even four elements to be out of place, i.e.
% out of the clause they belong to, provided they themselves are found together

quasicontiguous(L) :- contiguous(L), !. % Qui peut le plus...

quasicontiguous(L) :- pick(_,L,L1), contiguous(L1), !.  % ONE OUT

quasicontiguous(L) :- pick(A,L,L1), pick(B,L1,L2),contiguous([A,B]),contiguous(L2), !. % TWO OUT IF TO BE FOUND TOGETHER

quasicontiguous(L) :- pick(A,L,L1), pick(B,L1,L2),pick(C,L2,L3), contiguous([A,B,C]),contiguous(L3), !.
                      % THREE OUT IF TO BE FOUND TOGETHER

quasicontiguous(L) :- pick(A,L,L1), pick(B,L1,L2),pick(C,L2,L3), pick(D,L3,L4),contiguous([A,B,C,D]),contiguous(L4), !.
                      % FOUR OUT IF TO BE FOUND TOGETHER





% DISTANCE BETWEEN TWO PATHS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% we first determine the end points of the two paths
% we determine the order in which they appear
% and then the distance between extremity of the first one and start of the second


distance(Path1,Path2,Distance) :- extremity(Path1,Ext1), extremity(Path2,Ext2),
                                  start(Path1,St1), start(Path2, St2),
                                  ifthenelse(Ext1 =< St2,
                                             Distance is St2 - Ext1,
                                             Distance is St1 - Ext2).

% extremity : last position in pathlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% we select the very last position registered, i.e. the second element of the p(X,Y) structure that ends the path

extremity(PathList, Ex):- last(PathList,p(_,Ex)).

% start : first position in a pathlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a first element is easy to find by simple unification:
% we select the first element of the relevant p structure

start([p(Start,_)|_],Start).

% precedes(Path1,Path2)
%%%%%%%%%%%%%%%%%%%%%%%

precedes(P1,P2) :- extremity(P1,Extremity), start(P2,Start), Extremity =< Start.






% SE-reference binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% The solution, if there is one, consists in working on the parse when it's ready for output, exploring the tree structure via the C-string structure
% and binding the indices for SE, left uninstantiated up to that point


% TOP LEVEL

bind_se(ParseTot) :-

constraint([c_str:[_|Parse]],ParseTot),


ifthen(
          constraint([subject:[cat:np,sem:[hum],gender:GUP,number:NUP,index:IndexUP]],Parse), true),

ifthen(   constraint([object:OBJ1],Parse), true),
ifthen(   constraint([i_object:I_OBJ1],Parse), true),
ifthen(   constraint([prep_cplt:PREPCPLT1],Parse), true),

ifthen(   (nonvar(I_OBJ1),constraint([lex:pp3refl],I_OBJ1)),
                                                constraint([gender:GUP,number:NUP,index:IndexUP],I_OBJ1)),
ifthen(   (nonvar(OBJ1),constraint([lex:pp3refl],OBJ1)),
                                                constraint([gender:GUP,number:NUP,index:IndexUP],OBJ1)),
ifthen(   nonvar(PREPCPLT1),
                           constraint([index:IndexUP,lex:pp3refl,c_str:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPCPLT1)),

ifthen(    (nonvar(OBJ1),constraint([cat:pred],OBJ1)),
                                                constraint([c_str:CSTROBJ1],OBJ1)),


% the first of the following constraints fails in the case of some nonfinite_i clauses such as "vincere scis" where the subject is missing
% since we use bind_se  for its side effects we can call it within an ifthen clause when we wish to apply it : ifthen(bind_se(X),true) will do the trick

ifthen( nonvar(CSTROBJ1),(constraint([subject:SUBJ2],CSTROBJ1);true)      ),
ifthen( nonvar(CSTROBJ1),(constraint([object:OBJ2],CSTROBJ1);true)        ),
ifthen( nonvar(CSTROBJ1),(constraint([i_object:I_OBJ2],CSTROBJ1);true)    ),
ifthen( nonvar(CSTROBJ1),(constraint([prep_cplt:PREPCPLT2],CSTROBJ1); true) ),


% ONE DOWN

% only the first choice fires since bind_se is called in an ifthen sequence
% unless, of course, the adjective ipse is used with se and provides info on nber and gender
% that allows us to choose: rex credit reginam se amare vs rex credit reginam se ipsam amare

% but otherwise we choose to promote the one-up binding : rex putat reginam se amare, se bound to rex and not to reginam


ifthen(   (nonvar(SUBJ2), constraint([lex:pp3refl],SUBJ2)),
                                                 constraint([index:IndexUP,number:NUP,gender:GUP],SUBJ2)),

ifthen(    (nonvar(SUBJ2), constraint([cat:np,lex:Lex2,sem:[hum]],SUBJ2), Lex2 \= pp3refl),
                                                  constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],SUBJ2)),

ifthen(   (nonvar(OBJ2),constraint([lex:pp3refl],OBJ2)),
                                                ( constraint([gender:GUP,number:NUP,index:IndexUP],OBJ2) ;
                                                  constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],OBJ2)
                                                 )),

ifthen(   (nonvar(I_OBJ2),constraint([lex:pp3refl],I_OBJ2)),
                                                ( constraint([gender:GUP,number:NUP,index:IndexUP],I_OBJ2) ;
                                                  constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],I_OBJ2)
                                                 )),
ifthen(   nonvar(PREPCPLT2),(
                             constraint([index:IndexUP,lex:pp3refl,c_str:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPCPLT2) ;

                               constraint([index:IndexMIDDLE,lex:pp3refl,c_str:[head:[number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE]]],PREPCPLT2)
                              )
      ),

% prep_cplt has a more complex buildup, we have to enter into the Head belonging to the C_str (Constituent Structure)



ifthen(    (nonvar(OBJ2),constraint([cat:pred],OBJ2)),
                                                (constraint([c_str:CSTROBJ2],OBJ2),
                                                 constraint([subject:SUBJ3],CSTROBJ2))),

ifthen( (nonvar(CSTROBJ2),constraint([object:OBJ3],CSTROBJ2)),true),
ifthen( (nonvar(CSTROBJ2),constraint([i_object:I_OBJ3],CSTROBJ2)), true),
ifthen( (nonvar(CSTROBJ2),constraint([prep_cplt:PREPCPLT3],CSTROBJ2)), true),




% TWO DOWN

ifthen(   (nonvar(SUBJ3), constraint([lex:pp3refl],SUBJ3)),
                                                 (constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],SUBJ3);
                                                  constraint([number:NUP,gender:GUP,index:IndexUP],SUBJ3)
                                                  )),


ifthen(    (nonvar(SUBJ3), constraint([cat:np,lex:Lex3,sem:[hum]],SUBJ3),Lex3 \= pp3refl),
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],SUBJ3)),

ifthen(   (nonvar(OBJ3),constraint([lex:pp3refl],OBJ3)),
                                                  (
                                                  constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],OBJ3);
                                                  constraint([number:NUP,gender:GUP,index:IndexUP],OBJ3) ;
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],OBJ3)
                                                 )),
ifthen(   (nonvar(I_OBJ3),constraint([lex:pp3refl],I_OBJ3)),
                                                  (
                                                  constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],I_OBJ3);
                                                  constraint([number:NUP,gender:GUP,index:IndexUP],I_OBJ3) ;
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],I_OBJ3)
                                                 )),
ifthen(   (nonvar(PREPCPLT3),constraint([lex:pp3refl,c_str:[head:CSTRPP3]],PREPCPLT3)),
                                                  (
                                                  constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],CSTRPP3);
                                                 constraint([number:NUP,gender:GUP,index:IndexUP],CSTRPP3) ;
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],CSTRPP3)
                                                 )),

% THREE DOWN


ifthen(    (nonvar(OBJ3),constraint([cat:pred],OBJ3)),
                                                (constraint([c_str:CSTROBJ3],OBJ3),
                                                 constraint([subject:SUBJ4],CSTROBJ3))),

ifthen( (nonvar(CSTROBJ3),constraint([object:OBJ4],CSTROBJ3)),true),
ifthen( (nonvar(CSTROBJ3),constraint([i_object:I_OBJ4],CSTROBJ3)), true),
ifthen( (nonvar(CSTROBJ3),constraint([prep_cplt:PREPCPLT4],CSTROBJ3)), true),

ifthen(   (nonvar(SUBJ4), constraint([lex:pp3refl],SUBJ4)),
                                                 ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],SUBJ4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],SUBJ4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],SUBJ4)
                                                 )),

ifthen(   (nonvar(OBJ4),constraint([lex:pp3refl],OBJ4)),
                                                  ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],OBJ4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],OBJ4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],OBJ4)
                                                 )),
ifthen(   (nonvar(I_OBJ4),constraint([lex:pp3refl],I_OBJ4)),
                                                  ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],I_OBJ4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],I_OBJ4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],I_OBJ4)
                                                 )),
ifthen(   (nonvar(PREPCPLT4),constraint([lex:pp3refl,c_str:[head:CSTRPP4]],PREPCPLT4)),
                                                  ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],CSTRPP4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],CSTRPP4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],CSTRPP4)
                                                 )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% SUM OF THE ELEMENTS IN A LIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% used to sum up the distances between members of the same constituent to work out the Straining factor
% the distances are integers
% myplus is used instead of 'is' to avoid problems with potentially uninstantiated vars

sum([],0).
sum([H|T],Sum):- sum(T,Tsum), myplus(Tsum,H,Sum).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEALING WITH THE PREDICATE'S ARGLIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% INSTANTIATING THE ARGS ON THE ARGLIST


% partial (subjectless) arglists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (scis) uincere
%%%%%%%%%%%%%%%%

% nonfinite_i(ncomplete) is the value used to indicate that a subject should NOT be looked for

match_list(Args,                             % list of args to be satisfied
            ArgTrees,			      % argtrees built by the process
            PathlistArgs,		      % portions of the string covered by the args
            DistanceArgs,		      % straining factor discovered in args
            sujet(SConstraints),              % constraints on the subject carried over for inspection if needed
            nonfinite_i,                      % indication on the nature and completeness
                                              % of the search with respect to the subject
            gap:Gap,                          % Gap remains a variable until an arg returns a gap
                                              % (i.e. is not found in the string)
            w:W,                              % cumulated weights of the args
            Int) :-                           % remains a variable until an interrogative element
                                              % is found in one of the args

  pick(subject:_,Args,RemainingArgs),		  % subject non instantiated - left out

  match_list(RemainingArgs,ArgTrees,PathlistArgs,DistanceArgs,sujet(SConstraints), nonfinite,gap:Gap,w:W, Int).
     %  the remaining args are looked for as in any nonfinite clause







% (rex legit librum et) scribit epistulas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% finite_i parallel to nonfinite_i

match_list(Args,ArgTrees,PathlistArgs,DistanceArgs,
            sujet(SConstraints), finite_i,gap:Gap,w:W, Int) :-
  pick(subject:_,Args,RemainingArgs),		 % subject non instantiated - left out
  match_list(RemainingArgs,ArgTrees,PathlistArgs,DistanceArgs,sujet(SConstraints), finite,gap:Gap,w:W, Int).
			     % like in any finite clause




% ALL ARGS
%%%%%%%%%%

% rex scribit epistulas
%

match_list([Function:Specs|RemainingArgs],
            [ArgTree|ArgTrees],
            [PathlistArg|PathlistArgs],
            [DistanceArg|DistanceArgs],
            sujet(SConstraints),
            ClauseType,
            gap:Gap,
            w:BW,
            Int) :-

  ClauseType \= nonfinite_i,
  ClauseType \= finite_i,

  match(Function:Specs,ArgTree,SConstraints,PathlistArg,DistanceArg,ClauseType,gap:Gap1,w:W1,Int),
                                            % match it

  match_list(RemainingArgs,ArgTrees,PathlistArgs,DistanceArgs,sujet(SConstraints), ClauseType,gap:Gap2,w:W, Int),
                                            % match all the other args on the arglist
  % BW is W+W1,                               % sum the weights
  myplus(W,W1,BW),
  (Gap1=[] ; Gap2=[]),                      % only one gap per clause - either in this arg or in the other args on the arglist
  ifthenelse(Gap1=[], Gap=Gap2, Gap=Gap1).  % we register the gap found -if any-  as gap for the whole clause

  % in other words:
  % we cannot have two gaps; either the first arg has no gap, or the list of the other args displays no gap
  % this does not imply that there must be a gap; both gaps may return the empty list ([]) as value



% all arguments consumed or only optional args left unsatisfied

match_list([Function:Specs|RemainingArgs],
            ArgTrees,
            PathlistArgs,
            DistanceArgs,
            sujet(SConstraints),
            ClauseType,
            gap:Gap,
            w:W,
            Int) :-
		constraint([oblig:no], Specs),
		match_list(RemainingArgs,
				ArgTrees,
				PathlistArgs,
				DistanceArgs,
				sujet(SConstraints),
				ClauseType,
				gap:Gap,
				w:W,
				Int).



match_list([],[],[],[0],_,_,_,w:0,_).






% instantiating an arg of the arglist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SUBJECT
%%%%%%%%%

% finite : rex reginam amat
%

% nominative case required

match(subject:Specs,
        subject:OutSpecs,
        [number:Nsubj,gender:Gendersubj,person:Psubj], % information on the subject
									  % taken over from the verb
        PathlistArg,
        DistanceArg,
        finite,              % therefore subject in the nominative case
        gap:[],
        w:W,
        Int ) :-

constraint([type:T],Specs),   % looking at the nature of the required filler : pred, np, etc.
T \= dummy,                   % excluding spurious subjects,
                              % sometimes needed for coherence of the system (see impersonal verbs)
constraint([constraints:Cs],Specs),  % fishing out the constraints on the arg


mapped(T,FSSpecs),		     % we have the type of phrase needed for the arg

constraint([number:NumberinSpecs,gender:GenderinSpecs,case:nom,person:Psubj],FSSpecs), % retrieving gender and number from the proposed subject
									% and requiring nominative



funify([number:Nsubj,gender:Gendersubj],[number:NumberinSpecs,gender:GenderinSpecs],[number:NNsubj,gender:GGsubj]),
                                     % for cases where gender and number are specified as OR-sets
                                     % we project the needed gender and number before registering in the OutSpecs

pick(number:NumberinSpecs,FSSpecs,FSSpecs1), pick(gender:GenderinSpecs,FSSpecs1,FSSpecs2), append([number:NNsubj,gender:GGsubj],FSSpecs2,OutSpecs),
                                     % substituting gender and number with possibly tighter constraints



ifthen(constraint([type:int],OutSpecs),Int=int),  % we register the fact that it includes an interrogative element
						 % ... if it does !




                                                 % the subject constraints are imposed -
                                                 % after all this is why they were carried over from the verb group
                                                 % down to here !
constraint(Cs,OutSpecs),                          % applying all other relevant constraints
constraint([pathlist:PathlistArg, distance:DistanceArg,w:W],OutSpecs).


% summing up:
% the process consists in projecting the constraints found in the Specs for the arg onto a mapped structure FSSpecs
% to see if it can act as arg-filler





% without explicit subject : amo reginam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the subject is declared context-retrievable - it must be able to meet
% the constraints set by the predicate on its subject, e.g. +HUM and such like
% this is *** NOT *** a gapped subject

match(subject:Specs,
        subject:[source:context_retrievable,   % we map up the info returned as arg filler
                 number:Nsubj,                 % info carried over from the verb group
                 gender:Gendersubj,
                 person:Psubj,
                 cat:np,
               %  sem:[hum],			% not necessarily so !!!!!! more interesting to keep track of the constraints set by the predicate
                 index:i(0,0),                 % conventionally assigned to context-retrievable subjects
                 constraints_to_be_met:Cs,     % carried over from the arg specs
                 distance:[0],
                 pathlist:[],
                 case:nom,			% we are in a finite clause
                 w:W],
        [number:Nsubj,gender:Gendersubj,person:Psubj],
        [],      % no pathlist
        [0],     % no straining factor
        finite,
        gap:[], % no gap
        w:W,
        Int ) :-

% we need a penalty for introducing a contextual subject - as there may be a true one around...

ifthen(Psubj=1,constraint([gender:Gendersubj],[gender:or([masc,fem])])),
ifthen(Psubj=2,constraint([gender:Gendersubj],[gender:or([masc,fem])])),
ifthenelse(Psubj=3,W=(-3),W=0),			% the penalty applies only to third person subjects
constraint([type:np],Specs),                     % the dummy subject must assume np status (not pred)
constraint([constraints:Cs],Specs),
constraint(Cs,[number:Nsubj,gender:Gendersubj,person:Psubj,case:nom,sem:_]),
not(constraint([lex:_],Cs)).	% we cannot supply a dummy subject if a definite lex is required !!!!




% nonfinite : regem reginam amare
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% accusative case required ; subject cannot be left out
% contrast : scis vincere <-> scis te vicisse

match(subject:Specs,
        subject:OutSpecs,
        [number:Nsubj,gender:Gendersubj,person:Psubj],
        PathlistArg,DistanceArg,
        nonfinite,
        gap:[],
        w:W,
        Int) :-
constraint([type:T],Specs),
T \= dummy,
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
constraint([number:NumberinSpecs,gender:GenderinSpecs, case:acc,person:Psubj],FSSpecs), % retrieving gender and number from the proposed subject
											% requiring acc


funify([number:Nsubj,gender:Gendersubj],[number:NumberinSpecs,gender:GenderinSpecs],[number:NNsubj,gender:GGsubj]),
                                     % in case gender and number are specified as OR-sets in the Specs
                                     % we project the needed gender and number before registering in the OutSpecs

pick(number:NumberinSpecs,FSSpecs,FSSpecs1), pick(gender:GenderinSpecs,FSSpecs1,FSSpecs2), append([number:NNsubj,gender:GGsubj],FSSpecs2,OutSpecs),
                                     % substituting gender and number with possibly tighter constraints



ifthen(constraint([type:int],OutSpecs),Int=int),
constraint([person:Psubj,case:acc],OutSpecs),
constraint(Cs,OutSpecs),
constraint([pathlist:PathlistArg, distance:DistanceArg,w:W],OutSpecs).




% dummy subject
%%%%%%%%%%%%%%%%

% see impersonal verbs - it would be too much of a bother to work out a new procedure whereby
% subjectless clauses are OK
% example :   eos peccatorum paenitet, where neither EOS nor PECCATORUM is parsed as a subject

match(subject:Specs,_,
        [number:_,gender:_,person:_],
        [],[0],_,gap:[],w:0, _) :-
constraint([type:dummy],Specs).



% gapped subject
%%%%%%%%%%%%%%%%

% in nonfinite clauses
%%%%%%%%%%%%%%%%%%%%%%

% the subject is the local one bearing the 'down' index :
% important info for interpretation of SE in such clauses

% regem quem // dicis reginam [subject-gap] amasse (in one of the two interpretations)

match(subject:Specs,
        subject:[index:Indexdown], % we register the index in the arg tree for the subject
        [number:Nsubj,gender:Gendersubj,person:Psubj],
			% index sharing
        [],
        [0],
        nonfinite, % important !!!
        gap:[gap:[type:Type,
                  index:Indexdown,  % index sharing via the gap structure
                  function:subject,
                  subject:[e:Indexdown],
                  constraints:GapConstraints]], % the gap constraints result from appending the constraints
                                                % found in the arg specs and the local constraints on the subject
                                                % derived from the verbal group
         w:1,
         Int) :-  % the Int won't be instantiated here

 mapped(flags,active(gap)),   % we only look for gaps if there are structures to receive them (as in relative clauses)
 constraint([type:Type,constraints:Constraints],Specs),
 Type \= dummy,
 append([number:Nsubj,gender:Gendersubj,person:Psubj,case:acc,index:Indexdown],Constraints,GapConstraints).

% in finite clauses
%%%%%%%%%%%%%%%%%%%

% the subject is the one of the potentially main clause, i.e. the 'up' one
% rex qui // [subject-gap] amat reginam


match(subject:Specs,
        subject:[index:Indexup],
        [number:Nsubj,gender:Gendersubj,person:Psubj],
        [],
        [0],
        finite, % important !
        gap:[gap:[type:Type,
                  index:Indexup,
                  function:subject,
                  subject:[e:Indexup],
                  constraints:GapConstraints]],
        w:1,
        Int) :-

 constraint([type:Type,constraints:Constraints],Specs),
 Type \= dummy,
 append([number:Nsubj,gender:Gendersubj,person:Psubj,case:nom,index:Indexup],Constraints,GapConstraints).

% remember that a gapped constituent simply puts its constraints in the Gap feature,
% to be satisfied when the pred is connected to the antecedent



%%%%%%%%%%%%%%%%%%%
% OTHER ARGS
%%%%%%%%%%%%%%%%%%%


% same treatment for a nber of args 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
match(To_be_matched:Specs,To_be_matched:OutSpecs,[number:Nsubj,gender:Gendersubj,person:_],
                          PathlistArg,DistanceArg,ClauseType,gap:[],w:W, Int) :-

      (To_be_matched=object;
      To_be_matched=agent;

      To_be_matched=i_object; % standard indirect objects

      To_be_matched=object_i; % indirect objects in the accusative case 
                              % in double-accusative constructions (doceo-type verbs)

      To_be_matched=cplt; % this arg type is used when we do not really care to further specify the nature of the argument
                          % used with specific predicates such as some of the impersonal
      To_be_matched=adjunct;
      To_be_matched=arg   % a rather indefinite filler, such as cplt above
                          % perhaps we do not need both, but perhaps they can be used to reflect a distinction still to be decided on

      ),

constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthen(constraint([type:int],FSSpecs),Int=int),
constraint(Cs,FSSpecs),
ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), Out=FSSpecs) % ELSE keep the info
          ),
constraint([pathlist:PathlistArg, w:W],Out),

binding_se(ClauseType,Nsubj,Gendersubj,Out,OutSpecs).




% PREPOSITIONAL CPLT
%%%%%%%%%%%%%%%%%%%%

match(prep_cplt:Specs,prep_cplt:OutSpecs,[number:Nsubj,gender:Gendersubj,person:_],
       PathlistArg,DistanceArg,ClauseType,gap:[],w:W, Int) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthen(constraint([type:int],FSSpecs),Int=int),
constraint(Cs,FSSpecs),
ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), Out=FSSpecs) % ELSE keep the info
          ),
constraint([pathlist:PathlistArg, w:W],Out),

% specific treament of se_binding because we have to enter into the c_str to retrieve the head of the pp

ifthenelse(constraint([lex:Lex],Out),   % if the arg has a lexical head
                                ifthenelse( (Lex=pp3refl,ClauseType=finite),                    % and it is SE and the clause is finite
												% we can project subject nber and gender on the SE
				   (
                                  constraint([c_str:[head:[number:NumberinSpecs,gender:GenderinSpecs]]],Out), 
                                                                         % retrieving gender and number from the proposed subject
                                  funify([number:Nsubj,gender:Gendersubj],[number:NumberinSpecs,gender:GenderinSpecs],[number:NNsubj,gender:GGsubj]),
                                  pick(c_str:C_str,Out,NewOut),
                                 pick(head:Head,C_str,NewC_str),
                                 pick(number:NumberinSpecs,Head,NewHead1),
                                 pick(gender:GenderinSpecs,NewHead1,NewHead2),

                                 append([number:NNsubj,gender:GGsubj],NewHead2,NewHead),
                                 append([head:NewHead],NewC_str,C_strOut),
                                 append([c_str:C_strOut],NewOut,OutSpecs)
                                    ),
                                  OutSpecs=Out
                  ),


            OutSpecs=Out
      ).



% INT ADVERB 1
%%%%%%%%%%%%%%

% we regard an interrogative adverb (e.g. unde or quo) as being able to fill in this type of gap
% even without being a prepositional phrase
% but the semantics must fit

match(prep_cplt:Specs,prep_cplt:OutSpecs,[number:_,gender:_,person:_],
       PathlistArg,DistanceArg,_,gap:[],w:W, Int) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),

mapped(advp,FSSpecs),
constraint([type:int],FSSpecs),
Int=int,

ifthen(constraint([case:acc],Cs), constraint([sem:direction_to],FSSpecs)),                     % QUO
ifthen(constraint([case:abl, prep:or([ex,ab])],Cs), constraint([sem:direction_from],FSSpecs)), % UNDE

ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),
constraint([pathlist:PathlistArg, w:W],FSSpecs).


% INT ADVERB 2
%%%%%%%%%%%%%%

match(cplt:Specs,cplt:OutSpecs,[number:_,gender:_,person:_],
       PathlistArg,DistanceArg,_,gap:[],w:W, Int) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),

mapped(advp,FSSpecs),
constraint([type:int],FSSpecs),
Int=int,

ifthen(constraint([case:abl,sem:loc],Cs), constraint([sem:position],FSSpecs)),  % e.g. UBI
ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),
constraint([pathlist:PathlistArg, w:W],FSSpecs).



% OBJECT CPLT ('attribut de l'objet direct')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match(object_cplt:Specs,object_cplt:OutSpecs,[number:_,gender:_,person:_],
        PathlistArg,DistanceArg,_,gap:[],w:W, Int) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthen(constraint([type:int],FSSpecs),Int=int),
constraint(Cs,FSSpecs),
constraint([pathlist:PathlistArg,w:W],FSSpecs),
ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),
ifthen(T=np,constraint([class:common],FSSpecs)).	% e.g. appellare Murenam saltatorem  -
                                                        % Murenam is object, saltatorem (a common noun) object_cplt



% PREDICATIVE (after 'sum')
%%%%%%%%%%%%%%%%%%%%%%%%%%%

match(predicative:Specs,predicative:OutSpecs,[number:Nsubj,gender:Gendersubj,person:_],
       PathlistArg,DistanceArg,_,gap:[],w:Wtot, Int) :-

constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthenelse(constraint([type:int],FSSpecs),(Int=int, Bonus=3),Bonus=0), % needs heavy bonus for interrogatives, as they must compete
                                                                       % with (often spurious) relatives with dummy antecedents !!!!!
constraint(Cs,FSSpecs),
ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),
ifthen((T=np;T=adjp;T=gerundive), constraint([number:Nsubj, case:or([nom,acc])],FSSpecs)),
               % Number constraint on predicatives seems jusfifiable; case is restricted to nominative and accusative
ifthen((T=adjp;T=gerundive), constraint([gender:Gendersubj],FSSpecs)),
               % Gender constraint on predicative adjectives seems jusfifiable

constraint([pathlist:PathlistArg,w:W],FSSpecs),
% Wtot is W+Bonus.
myplus(W,Bonus,Wtot).




% GAPPED non-subject arg
%%%%%%%%%%%%%%%%%%%%%%%%%

% here a variable IS used for the function to be filled (a non-subject one, subjects being dealt with on their own)

match(F:Specs,F:[e:I],[number:_,gender:_,person:_],
           [],[0],
            _,
            gap: [gap:[type:Type,
                       index:I,
                       function:F,
                       F:[e:I],
                       constraints:Constraints]],
            w:1,
            _) :-
mapped(flags,active(gap)),
F \= subject,
constraint([type:Type,constraints:Constraints],Specs).



% Dealing with a member of the SE family - same treatment for each of the arg that can be filled by a member of the SE family
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binding_se(ClauseType,Nsubj,Gendersubj,Out,OutSpecs) :-

ifthenelse(mapped(flags,active(se)),  % IF1

                   ifthenelse( constraint([lex:Lex],Out),   % if the arg has a lexical head IF 2          % THEN 1

                              ifthenelse( (Lex=pp3refl,ClauseType=finite),   % and it is SE and the clause is finite
									     % we can project subject nber and gender on the SE

                                   % careful !!!!! we need a way to undo that we are doing here
                                   % when we are in an ****argbound**** *****object pred******
                                   % orant te patres ut ad se venias, where SE is plural !!!!!!! patres laudant se : OK to project the plural on SE

		                           (constraint([number:NumberinSpecs,gender:GenderinSpecs],Out),
									% retrieving gender and number from the proposed subject
                                            funify([number:Nsubj,gender:Gendersubj],[number:NumberinSpecs,gender:GenderinSpecs],[number:NNsubj,gender:GGsubj]),

                                            pick(number:NumberinSpecs,Out,FSSpecs1),
                                            pick(gender:GenderinSpecs,FSSpecs1,FSSpecs2),
                                            append([number:NNsubj,gender:GGsubj],FSSpecs2,OutSpecs)
                                            ),
                                            OutSpecs=Out % ELSE 3
                                          ),
                                OutSpecs=Out % ELSE 2
                               ),

           OutSpecs=Out). % ELSE  1




% CLAUSAL CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%

clause_constraints([Constraint|OtherC],ClauseF) :-
   constraint(Constraint,ClauseF),
   clause_constraints(OtherC,ClauseF).

clause_constraints([],_).
% out of recursion

% these Constraints are treated like any other
% i.e. by being imposed on the clause features

% they are meant to cover restrictions on the whole clause,
% e.g. concerning polarity (e.g. non pili/haud magni facere)





% EXTRA KERNEL STRUCTURES
%%%%%%%%%%%%%%%%%%%%%%%%%%

% mainly adjuncts and adjunct-like elements, not taking part in arg structure
% note that we assign a lighter weight to extra-kernel structures
% (the weight value (w:Wtot) gets reduced when the results of expandkernel are used)
% precisely on account of the fact that
% they do NOT take part in arg saturation

% the 'Nat' variable is always instantiated to 'full' or 'partial'
% in the present implementation it is immaterial
% all expansions being of the 'full' type


expandkernel([Function:Specs|Others], [Function:FSpecs|OtherTrees],
             [PathFirst|OtherPaths],[DistFirst|OtherDists],
             w:WT, Int, _Nat) :-                                % the Int arg remains an uninstantiated variable
						            % unless an interrogative element is spotted in the
							    % specifications of the filler
constraint([type:Type,constraints:Constraints], Specs),
mapped(Type,FSpecs),
ifthen(constraint([type:int],FSpecs),Int=int),
constraint(Constraints,FSpecs),
constraint([pathlist:PathFirst, distance:DistFirst,w:Weight], FSpecs),
expandkernel(Others,OtherTrees,OtherPaths, OtherDists,w:Ws, Int, _Nat),
myplus(Weight,Ws,WT).

% all are optional
expandkernel([_:_|Others], OtherTrees,OtherPaths,OtherDists,w:W, Int, _Nat) :-
expandkernel(Others,OtherTrees,OtherPaths, OtherDists,w:W, Int, _Nat).

% out of recursion
expandkernel([],[],[],[0],w:0, _, _).






% EXPAND (weights are standard - EXPAND is used with adj and noun arg-bearers)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% we do not expect these elements to instantiate an INT variable
% i.e. they are not supposed to contain interrogative elements
% * ars quae cognoscendi , * cupidus quae legendi / * quarum legendarum

expand([Function:Specs|Others],
       [Function:FSpecs|OtherTrees],
       [PathFirst|OtherPaths],
       [DistFirst|OtherDists],
       w:Wtot) :-
constraint([type:Type,constraints:Constraints], Specs),
mapped(Type,FSpecs),
constraint(Constraints,FSpecs),
constraint([pathlist:PathFirst, distance:DistFirst, w:Weight], FSpecs),
expand(Others,OtherTrees,OtherPaths, OtherDists,w:Ws),
% Wtot is Weight+Ws.
myplus(Weight,Ws,Wtot).

% optional elements
expand([_:Specs|Others], OtherTrees,OtherPaths,OtherDists,w:W) :-
constraint([oblig:no], Specs),
expand(Others,OtherTrees,OtherPaths, OtherDists,w:W).

% out of recursion
expand([],[],[],[0],w:0).





%%%%%%%%
% OUTPUT
%%%%%%%%

% the constraint specifying the absence of duplicates in the path
% makes sure a constituent is not filling in more than a slot
% ************* WITHIN A GIVEN TREE ********************************
% it does **** NOT **** entail the rejection of ambiguity by the parser !



% no duplicates - failure of call to dup predicate:
%

dup(List) :- pick(El, List, Rest), pick(El, Rest, _). % if we can pick an element twice,
                                                      % there is at least one duplicate



% the output procedure involves sorting of the parses according to their ratings
% it should be attempted only once
% and ***not*** restarted through imposed failure (the usual thing for runs to do in this grammar)

[output,results] --->
[recorded(out,Lists,_),
 mapped(fin, fin(Fin)),                 % sentence extremity
 statistics(cputime,TE),
 recorded(time,TB),
 TimeUsed is TE-TB,                    % computing time used by the parsing i.e.
                                       % the difference between time now and time then (when the S was read in)
 write('cputime : '), write(TimeUsed), nl,
 write(Lists,'cputime : '), write(Lists,TimeUsed), nl(Lists),
 recorded(alltime,TotTime,RefTime),
 NTotTime is TotTime+TimeUsed,
 erase(RefTime),     % forget the old total
 recorda(alltime,NTotTime),

 ifthen(recorded(stg,[type_of_structures,1]), % full sentences only
 setof(Rating-Parse,
             Pred^Pathlist^Distance^Weight^Fin^(mapped(pred,Pred),   % existential quantification to avoid multiple results

                                                
						constraint([cat:pred,argbound:no,pathlist:Pathlist,distance:[Distance],gap:[],w:Weight,c_str:Parse],Pred),
                                                                        % the constraints enable us to show only complete parses,
                                                                        % i.e. parses for the sentence submitted
                                                                        % that make of it an independent gapless finite clause

									% The weight is computed on the basis of arg saturation
                                                                        % the Distance holds the value for the Straining factor
						                        % measuring constituent inner non-contiguity

									% the 'type:finite' requirement is to be dropped if we have to deal
									% with ORATIO OBLIQUA : scire se illa esse uera (Caesar)

                                                \+dup(Pathlist),  % no word used twice within a single parse !!,
                                                path(0,Fin,Pathlist), % the path must cover all the words of the sentence !

                                                ifthen( mapped(flags,active(se)), ifthen(bind_se(Pred),true)), % we have a member of the SE family
										 % we apply binding without allowing failure
										% we are only interested in the side-effects of the binding procedure

                                                Rating is -(Weight-Distance) % sign is inversed so that the weightiest appear first
                                                                              % Rating is only meaningful in the case of ambiguity -
                                                                              % it is a preference factor
                                                 ),
              ParseList)
),

 ifthen( recorded(stg,[type_of_structures,2]), % various types of syntagms including preds
          (   (C=relative_clause;C=participle_clause;C=free_sub;C=exclamation;C=adjunct;C=gerundive;c=gdiv;C=pp;C=np;C=advp;C=adjp;C=pred),
              setof(Rating-Parse,
                            Pred^Pathlist^Distance^Weight^Fin^(mapped(C,Pred),   % existential quantification to avoid multiple results


						constraint([pathlist:Pathlist,distance:[Distance],w:Weight,c_str:Parse],Pred),
                                                                        % the constraints enable us to show only complete parses,
                                                                        % i.e. parses for the string submitted


									% The weight is computed on the basis of arg saturation
                                                                        % the Distance holds the value for the Straining factor
						                        % measuring constituent inner non-contiguity



                                                \+dup(Pathlist),  % no word used twice within a single parse !!,
                                                path(0,Fin,Pathlist), % the path must cover all the words of the sentence !

                                            %    ifthen( mapped(flags,active(se)), ifthen(bind_se(Pred),true)), % we have a member of the SE family
										 % we apply binding without allowing failure
										% we are only interested in the side-effects of the binding procedure

                                                Rating is -(Weight-Distance) % sign is inversed so that the weightiest appear first
                                                                              % Rating is only meaningful in the case of ambiguity -
                                                                              % it is a preference factor
                                                                   ),
              ParseList)
              )
          ),

  keysort(ParseList,Sorted),
  printthem(Sorted,user),
  printthem(Sorted,Lists),
  nl,nl(Lists)].




% no parse
printthem([],Stream) :- nl(Stream), !.

% one parse						% one : print it
printthem([H],Stream) :- nl(Stream),
                           prpr2(H,0,Stream),
                           nl(Stream), nl(Stream),
                           !.




% two parses or more

printthem([H1,H2|Tail],Stream) :-    ifthen(recorded(stg,[number_of_parses,1]),

                                  (nl(Stream),		% two or more : print them both if the second is as good as the first
                                  prpr2(H1,0,Stream),
                                  nl(Stream),
                                  ifthen((H2=R-_,H1=R-_),prpr2(H2,0,Stream)),
                                  nl(Stream),
                                  !)),

                                   ifthen(recorded(stg,[number_of_parses,2]),
                                   (nl(Stream),		% two or more : print them both
                                  prpr2(H1,0,Stream),
                                  nl(Stream),
                                  prpr2(H2,0,Stream),
                                  nl(Stream),
                                  !)),
                                    ifthen(recorded(stg,[number_of_parses,3]),
                                   (nl(Stream),		% two or more : print them all
                                  prpr2(H1,0,Stream),
                                  nl(Stream),
                                  prpr2(H2,0,Stream),
                                  nl(Stream),
                                  printthemall(Tail,Stream))).


printthemall([H|T],Stream):-      prpr2(H,0,Stream),
                                  nl(Stream),
                                  printthemall(T,Stream).


printthemall([],_).






%%%%%%%%%%%%%%%%%%%
% DEBUGGING OUTPUTS
%%%%%%%%%%%%%%%%%%%

% The example queries show how we can obtain partial structures
% which would not necessarily make it up to the top level (the whole S)

% !!!! WARNING !!!
% the single pass restriction on the output should be removed - see metainterpreter well above

% example query : getting the nps
/*
 [output,nps] --->
[mapped(np,FS),
 not(recorded(parse,parse(FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),
 prpr(FS,0,Lists),
 nl,nl(Lists),
 recorda(parse,parse(FS),_)].
*/

% getting the clauses
% cplt clauses included even if they do not make it up to the top level of the main clause
% ***************** FOR DEBUGGING PURPOSES  ********************************

/*
 [output,preds] --->
[mapped(pred,FS),
 not(recorded(parse,parse(FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),                  % writing pretty-printed parses onto the screen
 write(Lists,FS),                  % writing raw parses into the file
 nl,nl(Lists),
 recorda(parse,parse(FS),_)].
*/

/*
 [output,relative_clauses] --->
[mapped(relative_clause,FS),
 not(recorded(parse,parse(FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),
 write(Lists,FS),
 nl,nl(Lists),
 recorda(parse,parse(FS),_)].
*/





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSORT : arg insertion sort
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insort([H:V|T],S) :-
                   insort(T,L),
                   insert(H:V,L,S).

insort([],[]) :- !.

% INSERT predicate
%%%%%%%%%%%%%%%%%%

insert(F:V,[F1:V1|T],[F1:V1|L]) :-
                 before(F1,F),
                 !,
                 insert(F:V,T,L).

insert(F:V,L,[F:V|L]).


% BEFORE predicate
%%%%%%%%%%%%%%%%%%

before(F1,F2) :-
                      assoc(F1,Rank1),
                      assoc(F2,Rank2),
                      Rank1=<Rank2.


% ASSOC predicate
%%%%%%%%%%%%%%%%%

% assigns a canonical order in the returned parse

assoc(arglist,0).
assoc(subject,1).
assoc(agent,2).
assoc(predicative,3).
assoc(object,4).
assoc(i_object,5).
assoc(object_i,5).  % acc object playing the semantic role of an indirect object
                    % i_object and object_i are not suppposed to co-occur and can therefore
		    % be assigned the same place in the canonical order
assoc(object_cplt,6).
assoc(prep_cplt,7).
assoc(cplt,8).
assoc(arg,9).
assoc(adjunct,10).
assoc(clause_level_adjunct_1,11).
assoc(clause_level_adjunct_2,12).
assoc(clause_level_adjunct_3,13).
assoc(clause_level_adjunct_4,14).
assoc(clause_level_adjunct_5,15).
assoc(clause_level_adjunct_6,16).
assoc(clause_level_adjunct_7,17).
assoc(clause_level_adjunct_8,18).
assoc(ablative_absolute,19).
assoc(dativus_ethicus,20).
assoc(linker,21).





%%%%%%%%%%%%%%%%%%%%%%%
% Clearing the database
%%%%%%%%%%%%%%%%%%%%%%%

% all these boxes could have been filled by either lexical or grammar clauses
% they must be emptied from one sentence to the next

% any time 'map' is enriched with a new structure as arg,
% that new structure should also be the target of the 'forgetting' process

clear :-
       eraseall(time),
       eraseall(pos),
       eraseall(fin),
       eraseall(unmapped),
       eraseall(flags),
       eraseall(parse),
       eraseall(noun),
       eraseall(prpers),
       eraseall(prindef),
       eraseall(prdem),
       eraseall(print),
       eraseall(relative),
       eraseall(relative_clause),
       eraseall(prep),
       eraseall(coord),
       eraseall(sub),
       eraseall(neg),
       eraseall(part),
       eraseall(punct),
       eraseall(pp),
       eraseall(adj),
       eraseall(adv),
       eraseall(advp),
       eraseall(gdiv),
       eraseall(p_p),
       eraseall(p_pr),
       eraseall(p_f),
       eraseall(adjp),
       eraseall(gerundive),
       eraseall(participle_clause),
       eraseall(v),
       eraseall(vg),
       eraseall(vgpos),
       eraseall(np),
       eraseall(phrase),
       eraseall(adjunct),
       eraseall(free_sub),
       eraseall(apposed_sub),
       eraseall(exclamation),
       eraseall(pred).

 eraseall(X) :-
     recorded(X,_,Ref),
     erase(Ref),
     fail.

 eraseall(_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Detach -QUE/-UE and -CUM where appropriate - + deals with nec (--> et non)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
    % end of recursion clause:

     deque([],[]).

     % NEC
     %

     deque([nec|T],[et,non|T1]) :- !, deque(T,T1).   % toy !!!!!

     % QUE
     %

     % !!!!!!!!!!!!!!!!!!!!!!!!!!!!! TOY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     deque([H|T],[H|T1]) :- member(H,[absque, abusque, aeque, antique, atque,
                                      bellique,  domique,  % included in this parser as parts of mwu string
                                      denique,
                                      extorque,
                                      iamque, inique, itaque,ideoque,
                                      linque,
                                      militiaeque,			% cf bellique
                                      namque, neque,
                                      oblique,
                                      peraeque, plerumque, pleraque, pleraeque, plerique,
                                      quandoque, quidque, quinque, quodque,
                                      quisque, quoque, quousque,
                                      relinque,
                                      simulatque,
                                      torque,
                                      ubique, utrubique, undique,usque,utique,
                                      utrimque]),
                              !,                 % list of words where -que should remain attached
                              deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(_,cumque,H),  % all words ending in -cumque should be left alone
                            !,
                            deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(Rel,que,H),  % ditto for those words made up of a relative followed by -que
                            lex(Rel,relative,_),!,
                            deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(Lex,que,H),  % ditto for uterque, utrumque and friends
                            lex(Lex,adj,FS),
                            constraint([lex:uter],FS),
                            !,
                            deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(Lex,que,H),
					% ditto for a form of UNUS followed by a relative (or quis or quid) and then -que
                            atom_concat(First,Second,Lex),
                                    % atom_concat 'suggests' string divisions until the right one is found
                            lex(First,adj,FS1),
                            constraint([lex:unus],FS1),
                            (lex(Second,relative,_); Second=quis; Second=quid),
                            !,
                            deque(T,T1).

     deque([Word|Tail],[que,Mot|Tail1]) :-  atom_concat(Mot,que,Word), !, deque(Tail, Tail1).
                                           % all other words ending in -que
                                           % note that we put the que in front (normal place for 'et')
					   % e.g. populusque -> que populus





    % UE (-ve as in prouidendumue)

     deque([H|T],[H|T1]) :- member(H,[aue, adsidue, assidue, ambigue, angue, ablue, adnuue, amoue,
                                      breue, boue,
                                      caue, ciue, coargue, constitue, calue, captiue,
                                      diue, decliue, distingue, dilue,
                                      exigue, exsangue, extingue,
                                      faue, furtiue,
                                      graue, praegraue,
                                      ignaue, ingue, ioue, intempestiue, ingenue,
                                      leue, lue,
                                      minue, metue, moue, remoue,
                                      naue, neue, niue,
                                      oue,
                                      praue, promiscue, pingue, procliue, parue, praecipue, 
                                      restitue, respue,
                                      sangue, suaue, saeue, strue, statue,siue, salue, solue, strenue,
                                      tempestiue,  tenue, 
                                      ungue, uiue]),

                              !,                 % list of words where -ue should remain attached
                                                 % PARTIAL (from Lasla's 'Indexe inverse de la langue latine')
                              deque(T,T1).

    
      deque([Word|Tail],[ue,Mot|Tail1]) :-  atom_concat(Mot,ue,Word), !, deque(Tail, Tail1).

                               % all other words ending in -ue
                                           % note that we put the ue in front (normal place for 'uel')
					   % e.g. populusue -> ue populus
  

   % CUM
     %

     deque([mecum|T],[cum,me|T1]):- !, deque(T,T1).
     deque([tecum|T],[cum,te|T1]):- !, deque(T,T1).
     deque([secum|T],[cum,se|T1]):- !, deque(T,T1).
     deque([nobiscum|T],[cum,nobis|T1]):- !, deque(T,T1).
     deque([uobiscum|T],[cum,uobis|T1]):- !, deque(T,T1).
     deque([quocum|T],[cum,quo|T1]):- !, deque(T,T1).
     deque([quacum|T],[cum,qua|T1]):- !, deque(T,T1).
     deque([quibuscum|T],[cum,quibus|T1]):- !, deque(T,T1).

     % DO NOTHING
     %

     deque([H|T],[H|T1]) :- deque(T,T1).
               % the word does not end in -que or -ue, neither is it food for the other processes described here





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  GENERAL PROCEDURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% FEATURE LIST UNIFICATION
%%%%%%%%%%%%%%%%%%%%%%%%%%

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


% pushing an item on a list

push(Item,List,[Item|List]).





% TEST GENERATION
%%%%%%%%%%%%%%%%%

% used only for special purposes

% generating strings to test parser robustness with respect to word order
% e.g. a call such as  genstring([rex, mittit, epistulas, longas, uxori, imperatoris]).
% will generate 6! (720) strings for the parser to try to parse

genstring(List) :-
   protocola(strings),
   setof(Perm,
         permutation(List,Perm),     % permutation belongs to the list library
         Perms),
   writel(Perms),
   write('stop.'),
   nl,
   noprotocol.

writel([]).
writel([A|B]) :- writelist(A), writel(B).
writelist([]) :- write('.'),nl.
writelist([A|B]) :- write(A), tab(1), writelist(B).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRETTY PRINTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DECLARATIONS

% special features are printed as lists, with each subfeature on its own line

  special(c_str).
  special(head).
  special(apposition).
  special(neg).
  special(object).
  special(agent).
  special(object_cplt).
  special(prep_cplt).
  special(cplt).
  special(arg).
  special(i_object).
  special(object_i).
  special(adjunct).
  special(clause_level_adjunct_1).
  special(clause_level_adjunct_2).
  special(clause_level_adjunct_3).
  special(clause_level_adjunct_4).
  special(clause_level_adjunct_5).
  special(clause_level_adjunct_6).
  special(clause_level_adjunct_7).
  special(clause_level_adjunct_8).
  special(linker).
  special(ablative_absolute).
  special(dativus_ethicus).
  special(predicative).
  special(noun_cplt).
  special(comp_cplt).
  special(sup_cplt).
  special(rel_clause).
  special(subject).
  special(pred).
  special(exclamation).
  special(adjp).
  special(participle_clause).
  special(subordinate_clause).
  special(subordinator).
  special(main_clause).
  special(np).
  special(pp).
  special(gdiv).
  special(vg).
  special(string).
  special(free_sub).
  special(apposed_sub).
  special(arglist).
 

% The pretty-printing process begins here:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 % weight printed in front of parse
  prpr2(Weight-X,I,Handle):- NewWeight is -Weight,
					% reverse before printing so that the highest (=least!!!) values appear on top
                            write(Handle,NewWeight),
                            write(Handle,'--->'),
                            nl(Handle),
                            write(Handle,X),
                            nl(Handle), nl(Handle),
                            prpr(X,I,Handle).

  % pretty-printing the parse
  prpr(X,_,_) :- var(X),!.   % we drop (i.e. do not print) uninstantiated variables and empty lists
  prpr([],_,_) :- !.
  prpr([H|T],I,Handle):-
                  !,J is I+4,prpr(H,J,Handle),prprx(T,J,Handle).   % we tab away from the left margin to increase indentation
								   % and thereby reveal structure
                                                                   % we use recursive prprx to print the tail of the list
% NOT PRINTED
%%%%%%%%%%%%%

  prpr(_:Value,_,_) :- var(Value),!.
          % features with uninstantiated values are not printed; neither is a variable index within an index structure : i()

  prpr(index:i(Value),_,_) :- var(Value),!.
  prpr(index:i(Indexup,Indexdown),_,_) :- var(Indexup), var(Indexdown), !.
  prpr(index:i(Indexup,Indexdown),I,Handle) :- var(Indexup),  !, prpr(index:Indexdown,I,Handle).
  prpr(index:i(Indexup,Indexdown),I,Handle) :- var(Indexdown),  !, prpr(index:Indexup, I, Handle).

  % - ditto for empty lists
  prpr(_:[],_,_) :- !.

% We choose not to pretty-print the following features - they can be studied in the raw parse, if necessary
%							mostly for debugging purposes
 
 prpr(class:_,_,_) :- !.
  prpr(constraints:_,_,_) :- !.
  prpr(txt:_,_,_) :- !.
  prpr(type:_,_,_) :- !.
  prpr(function:_,_,_) :- !.
  prpr(hp:_,_,_) :- !.
  prpr(distance:_,_,_) :- !.
  prpr(gap:_,_,_) :- !.
  prpr(w:_,_,_) :- !.
  prpr(lextype:_,_,_) :- !.
  prpr(kind:_,_,_) :- !.


% PRINTED AS LISTS
%%%%%%%%%%%%%%%%%%

 prpr(pathlist:List,I,Handle) :- tab(Handle,I), prprxh(List,I,Handle),!.
                             
 prpr(Special:V,I,Handle) :- special(Special),
                              !,
                              tab(Handle,I),
                              write(Handle,Special),
                              nl(Handle),
                              prpr(V,I,Handle).

   % special cases: printed as lists, i.e. with each element on its own line
   % following a header line made up of the name of the Special element

   % the other features just get printed in the standard way, i.e. written


% STANDARD
%%%%%%%%%%

  prpr(X,I,Handle):- tab(Handle,I),write(Handle,X),nl(Handle).


% we print each feature in turn, using recursive prprx

  prprx([],_,_):- !.
  prprx(X,_,_) :- var(X),!.
  prprx([H|T],I,Handle):- prpr(H,I,Handle),prprx(T,I,Handle).



% PATHLIST  e.g.  pathlist:[p(1,2),p(2,3),p(3,4)]
%%%%%%%%%%

  prprxh([],_,Handle):- nl(Handle),!.

  prprxh([H|T],I,Handle):- prprh(H,I,Handle),prprxh(T,I,Handle).

  prprh(p(A,B),I,Handle) :- recorded(pos,position(A,B,Word)), 
                            write(Handle,'** '),
                            write(Handle,Word), write(Handle,' ** ').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GETS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* String to word list converter */
%

/* first arg of getsentence is the wordlist to be obtained from the string
   second arg is either user (keyboard input) or an input stream identifier computed by the system

   third arg is a list of sentence delimiters
   fourth arg is a list of word delimiters
   fifth arg is a list of punctuation signs to be returned as quoted atoms
   last arg but one is either caps (capitals preserved) or nocaps (capitals turned into small letters)
   last arg is last char read

   a licit call is for example:
   getsentence(Sentence, user, ".!?:;" , "\'" , "" ,nocaps,EOF).

   -----Sentence delimiters----
   1. No sentence delimiter is assumed; only eof forces termination of the procedure.
      This way a whole text can be read in at one go, and there is no need for list flattening

   2. Period (.), exclamation mark (!), question mark (?),
      as well as colon (:) and semi-colon (;),
      are typically declared sentence/chunk delimiters.
   3. Sentence delimiters are all ipso facto also word delimiters


   -----Word delimiters-----
   1. All sentence delimiters are word delimiters.
   2. Space and eol are word delimiters by right and should not be declared as such.
   3. Comma (,), quotes ("), parentheses ('()'), brackets ([]), curly brackets ({}) and slash (/)
      are typically declared as word delimiters


   -----Returned punctuation signs-----
   Only the punctuation signs specified in the fifth arg are returned as quoted atoms.
   They can be any of the following:

   Period						.
   Exclamation mark					!
   Question mark					?
   Colon						:
   Semi-colon						;
   Apostrophe						'
   Hyphen						-
   Comma						,
   Quotes						"
   Parenthesis						()
   Brackets						[]
   Curly brackets					{}
   Slash						/


   Note that all the char lists are presented as double-quoted strings, i.e. Prolog sees them
   as CHARACTER CODES - they need to be converted to real chars before being returned
   this gets done with char_code(Char,Code)

*/


% THE WORDS ARE REPRESENTED AS ATOMS
%
% they should be written with writeq
% when the results are printed to file
%

getsentence(Wordlist,Handle,EOF) :-  % use defaults
                                 getsentence(Wordlist,Handle,
                                             ".!?",
                                             ".!?,;:/[]{}()",
                                             "",
                                             nocaps,EOF).


getsentence(Wordlist,Handle,SD, WD, PS,CapsFlag,EOF) :- get_code(Handle,Char),
                                (Char = -1 -> EOF = end_of_file ; true),
                                getrest(Handle,Char,Wordlist,SD, WD, PS,CapsFlag,EOF).


/* we read a character in from the stream pointed to by Handle (get_code)
   we use this character as look-ahead character
   the end result is the Wordlist corresponding to the input string

   we return the atom end_of_file if we hit the eof character (-1)
   else we leave EOF as a variable (its status should therefore be examined using ==
   (!! not simple = !!)

   Example call:

     getthem(HandleIn,[Sentence|Var]) :-
                                         getsentence(Sentence,
                                                     HandleIn,
                                                     "",
                                                     ".?!\',\"-;:/{}()[]",
                                                     ".?!",
                                                     nocaps,
                                                     EOF),
                                         (EOF == end_of_file -> Var = []; getthem(HandleIn,Var)).



   Alternatively we can also use stop. as last line of a file,
   and interrupt the repeated getsentence process
   as soon as [stop,'.']  or [stop]  is returned, depending on whether
   period (.) is a returned puctuation sign */

%

/* end of sentence markers */
%---------------------------

/* eof 1 */
getrest(_,-1,[],_,_,_,_,end_of_file):- !.


/* ! 2   */
getrest(_,Code,[Char],SD,_,PS,_,_):- memberchk(Code,SD),
                                                    memberchk(Code,PS),
                                                    char_code(Char,Code),!.

% we return the Char only if its code is a member of the PS list,
% the list of punctuation signs to be returned


getrest(_,Code,[],SD,_,_,_,_):-     memberchk(Code,SD),!.

% we have a sentence delimiter, but we do not return it in the word list


/* at the end of the sentence getrest simply succeeds,
   and thereby puts an end to the getsentence process too  */

%

/* end of word markers */
%-------------------------

/* eol       1 */
getrest(Handle,10,Wordlist,SD,WD,PS,CapsFlag,EOF) :-
              !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* space     2 */
getrest(Handle,32,Wordlist,SD,WD,PS,CapsFlag,EOF) :-
             !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* EOL and SPACE are always word end markers, and are never returned */


getrest(Handle,Code,[Char|Wordlist],SD,WD,PS,CapsFlag,EOF) :-   memberchk(Code,WD),
                                                                memberchk(Code,PS),
                                                                char_code(Char,Code),
                                                                !,
                                                                getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* we return the word end marker */


getrest(Handle,Code,Wordlist,SD,WD,PS,CapsFlag,EOF) :-          memberchk(Code,WD),!,
                                                                getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* we do not return the word end marker */


/* in these clauses getrest has come to the end of a word, and relaunches getsentence with the
   current Wordlist, to which new words will be added */

%

getrest(Handle,Letter,[Word|Wordlist],SD,WD,PS,CapsFlag,EOF):-
             getletters(Handle,Letter,Letters,Nextchar,SD,WD,CapsFlag,EOF),
             name(Word,Letters),
             getrest(Handle,Nextchar,Wordlist,SD,WD,PS,CapsFlag,EOF).

/* getletters collects the letters of a word in a list 'Letters'
   then the predicate name turns the list of letters into an atom representing the word read
   the word is put in front of the resulting wordlist
   getrest should get the other words of the string */


%

getletters(_,-1,[],-1,_,_,_,end_of_file):- !.		 /* eof                             1 */
getletters(_,10,[],10,_,_,_,_):-!.	                /* eol                              2 */
getletters(_,32,[],32,_,_,_,_):-!.			/* space                            3 */

getletters(_,Code,[],Code,_,WD,_,_):- memberchk(Code,WD),!.
getletters(_,Code,[],Code,SD,_,_,_):- memberchk(Code,SD),!.


/* when we hit a word delimiter we get out of getletters, adding nothing to the letter list */

%

getletters(Handle,Let,Word,Nextchar,SD,WD,CapsFlag,EOF):-
                 transform(Let,LetList,CapsFlag),!,
                 get_code(Handle,Char),
                 getletters(Handle,Char,Letters,Nextchar,SD,WD,CapsFlag,EOF),
                 append(LetList,Letters,Word).

/* we examine the lookahead letter 'Let'
   we see if we need to keep it
   we keep it only if we can transform it

   transform operates vacuously on most chars
   but if CapsFlag is set to nocaps it turns capital letters into small letters
   transform fails for elements below ascii 32, i.e. non-printing chars

   we then add the resulting letter to the front of the letter list
   we get a fresh char with get_code
   we use this char as look-ahead char for the next getletters goal

   note that transform returns a list of letters, not a single char
   this enables us to transform linked oe into o+e at this stage */

%

getletters(Handle,_,Letters,Nextchar,SD,WD,CapsFlag,EOF):-
                 get_code(Handle,Char),
                 getletters(Handle,Char,Letters,Nextchar,SD,WD,CapsFlag,EOF).

/* here the 'letter' carried in Let is simply dropped */


%

/* checking and transforming the char */
%---------------------------------------

transform(118,[117],nocaps) :- !.  % v->u
transform(86,[85],caps) :- !.  % V->U
transform(86,[117],nocaps) :- !.  % V->u

transform(C,[C1],nocaps):-C>64,C<91,!,C1 is C+32.
transform(C,[C1],nocaps):-C>191,C<222,!,C1 is C+32.

% nocaps : small letters are capital letters+32;
% e.g. A is 65 and a is 97(=65+32)


transform(C,[C],_) :- C>31, C<513.

% chucks non-printing and widely foreign ;-) chars out
% C gets transformed into itself, i.e. kept,
% only if its ascii number is above 31 and below 513

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END OF PROG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

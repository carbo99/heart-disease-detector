
:- ensure_loaded(heart_attributi).
:- ensure_loaded(heart_training_set).
:- ensure_loaded(heart_test_set).

:- op(300,xfx,<==).

% ================================================================================

apprendi(Classe) :-
	findall( e(C,O), e(C,O), Esempi),     
	apprendi(Esempi, Classe, Descrizione), 
	nl,write(Classe),write('<=='),nl,     
	writelist(Descrizione),
	assert( Classe <== Descrizione ).     


apprendi( Esempi, Classe, []) :-               
	\+ member( e(Classe,_), Esempi ).      
apprendi( Esempi, Classe, [Cong|Congi] ) :-
	apprendi_cong( Esempi, Classe, Cong),  
						
	rimuovi( Esempi, Cong, RestoEsempi ),   
	apprendi( RestoEsempi, Classe, Congi ). 


apprendi_cong( Esempi, Classe, []) :-
	\+ (member( e(Cl,_), Esempi), Cl \== Classe),
	!.	                              
apprendi_cong( Esempi, Cl, [Cond|Conds] ) :-
	scegli_cond( Esempi, Cl, Cond ),      
	filtra( Esempi, [Cond], Esempi1 ),     
	apprendi_cong( Esempi1, Cl, Conds ).

scegli_cond( Esempi, Classe, AttVal) :-
	findall( AV/Punti, punteggioAV(Esempi,Classe,AV,Punti), AVs),
	best( AVs, AttVal).

best([AttVal/_],AttVal).
best([AV0/S0,AV1/S1|AVSlist],AttVal) :-
	S1 > S0, !,    % AV1 è meglio di AV0
	best([AV1/S1|AVSlist],AttVal)
	;
	best([AV0/S0|AVSlist],AttVal).


filtra(Esempi,Cond,Esempi1) :-
	findall(e(Classe,Ogg), (member(e(Classe,Ogg),Esempi),soddisfa(Ogg,Cond)), Esempi1).


rimuovi([],_,[]).
rimuovi([e(_,Ogg)|Es],Conge,Es1) :-
	soddisfa(Ogg,Conge), !, 
	rimuovi(Es,Conge,Es1).  

rimuovi([E|Es],Conge,[E|Es1]) :- 
	rimuovi(Es,Conge,Es1).


soddisfa( Oggetto, Congiunzione) :-
%	hanno_attributo_in_comune(Oggetto,Congiunzione),
	\+ (member(Att=Valx,Congiunzione), member(Att=Valy,Oggetto), Valx \== Valy).

hanno_attributo_in_comune(Oggetto,Congiunzione) :-
	member(Att=_,Congiunzione),
	member(Att=_,Oggetto),
	!.

punteggioAV( Esempi, Classe, AttVal, Punti ) :-
	candidato( Esempi, Classe, AttVal),  
	filtra(Esempi,[AttVal],Esempi1), 
	length(Esempi1,N1),
	conta_pos(Esempi1,Classe,Npos1),  
	Npos1 > 0,                       
	Punti is (Npos1 + 1) / (N1 + 2).

candidato(Esempi,Classe,Att=Val) :-
	a(Att, Valori),                 
	member(Val,Valori),             
	adatto(Att=Val,Esempi,Classe).


adatto(AttVal,Esempi,Classe) :-
	member(e(ClasseX,OggX),Esempi),	
	ClasseX \== Classe,		
	\+ soddisfa(OggX,[AttVal]), !.	


conta_pos([],_,0).
conta_pos([e(ClasseX,_)|Esempi],Classe,N) :-
	conta_pos(Esempi,Classe,N1),
	(ClasseX=Classe,!,N is N1+1 ; N=N1).

writelist([]).
writelist([X|L]) :-
	tab(2), writeq(X), nl,
	writelist(L).

% ==================================================================================

classifica(Oggetto,Classe) :- 
	Classe <== Descrizione, 
	member(CongiunzioneAttributi,Descrizione), 
	soddisfa(Oggetto,CongiunzioneAttributi). 

stampa_matrice_di_confusione :-
	findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
	length(TestSet,N),
	valuta(TestSet,VN,0,VP,0,FN,0,FP,0,NC,0),
	A is (VP + VN) / (N - NC), 
	E is 1 - A,		  
	write('Test effettuati :'),  writeln(N),
	write('Test non classificati :'),  writeln(NC),
	write('Veri Negativi  '), write(VN), write('   Falsi Positivi '), writeln(FP),
	write('Falsi Negativi '), write(FN), write('   Veri Positivi  '), writeln(VP),
	write('Accuratezza: '), writeln(A),
	write('Errore: '), writeln(E).

valuta([],VN,VN,VP,VP,FN,FN,FP,FP,NC,NC).
valuta([infarto/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(Oggetto,infarto), !,     
	VNA1 is VNA + 1,
	valuta(Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
valuta([nessun_infarto/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(Oggetto,nessun_infarto), !, 
	VPA1 is VPA + 1,
	valuta(Coda,VN,VNA,VP,VPA1,FN,FNA,FP,FPA,NC,NCA).
valuta([nessun_infarto/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(Oggetto,infarto), !,     
	FNA1 is FNA + 1,
	valuta(Coda,VN,VNA,VP,VPA,FN,FNA1,FP,FPA,NC,NCA).
valuta([deceduto/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(Oggetto,nessun_infarto), !, 
	FPA1 is FPA + 1,
	valuta(Coda,VN,VNA,VP,VPA,FN,FNA,FP,FPA1,NC,NCA).
valuta([_/_|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :- 
	NCA1 is NCA + 1,
	valuta(Coda,VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA1).


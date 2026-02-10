% ================================================================================
% PROGRAMMA PER APPRENDERE ALBERI DI DECISIONE CON PRUNING
% ================================================================================

:- ensure_loaded(heart_attributi).
:- ensure_loaded(heart_training_set).
:- ensure_loaded(heart_test_set).

:- dynamic alb/1.

% ==============================
% PARAMETRI DI PRUNING
% ==============================

min_esempi(5).          % minimo numero di esempi per espandere un nodo
soglia_entropia(0.1).   % soglia di entropia per fermare l'espansione

% ==============================
% INDUZIONE DELL'ALBERO
% ==============================

induce_albero(Albero) :-
	findall(e(Classe,Oggetto), e(Classe,Oggetto), Esempi),
	findall(Att, a(Att,_), Attributi),
	induce_albero(Attributi, Esempi, Albero),
	mostra(Albero),
	assert(alb(Albero)).

% ------------------------------
% CASI BASE
% ------------------------------

induce_albero(_, [], null) :- !.

induce_albero(_, [e(Classe,_)|Esempi], l(Classe)) :-
	\+ (member(e(ClasseX,_),Esempi), ClasseX \== Classe), !.

% ------------------------------
% PRUNING: pochi esempi
% ------------------------------

induce_albero(_, Esempi, l(Classe)) :-
	min_esempi(Min),
	length(Esempi, N),
	N < Min, !,
	classe_maggioritaria(Esempi, Classe).

% ------------------------------
% PRUNING: entropia bassa
% ------------------------------

induce_albero(_, Esempi, l(Classe)) :-
	entropia_set(Esempi, Ent),
	soglia_entropia(S),
	Ent < S, !,
	classe_maggioritaria(Esempi, Classe).

% ------------------------------
% CASO RICORSIVO
% ------------------------------

induce_albero(Attributi, Esempi, t(Attributo, SottoAlberi)) :-
	sceglie_attributo(Attributi, Esempi, Attributo), !,
	del(Attributo, Attributi, Rimanenti),
	a(Attributo, Valori),
	induce_alberi(Attributo, Valori, Rimanenti, Esempi, SottoAlberi).

% ------------------------------
% NESSUN ATTRIBUTO UTILE
% ------------------------------

induce_albero(_, Esempi, l(Classi)) :-
	findall(C, member(e(C,_),Esempi), Classi).

% ================================================================================
% SCELTA ATTRIBUTO (ENTROPIA)
% ================================================================================

sceglie_attributo(Attributi, Esempi, Migliore) :-
	setof(Dis/A,
	      (member(A,Attributi), disuguaglianza(Esempi,A,Dis)),
	      [_/Migliore|_]).

disuguaglianza(Esempi, Att, Dis) :-
	a(Att, Valori),
	somma_pesata(Esempi, Att, Valori, 0, Dis).

somma_pesata(_, _, [], S, S).
somma_pesata(Esempi, Att, [Val|Vs], Acc, S) :-
	length(Esempi, N),
	findall(C,
		(member(e(C,O),Esempi), soddisfa(O,[Att=Val])),
		Classi),
	length(Classi, NV),
	NV > 0, !,
	probabilita(Classi, Prob),
	entropia(Prob, Ent),
	Acc1 is Acc + Ent * NV / N,
	somma_pesata(Esempi, Att, Vs, Acc1, S).
somma_pesata(Esempi, Att, [_|Vs], Acc, S) :-
	somma_pesata(Esempi, Att, Vs, Acc, S).

% ================================================================================
% ENTROPIA
% ================================================================================

entropia(ListaProb, Ent) :-
	entropia(ListaProb, 0, Ent).

entropia([], Acc, Acc).
entropia([P|Ps], Acc, Ent) :-
	P > 0, !,
	Acc1 is Acc - P * log(P) / log(2),
	entropia(Ps, Acc1, Ent).
entropia([_|Ps], Acc, Ent) :-
	entropia(Ps, Acc, Ent).

entropia_set(Esempi, Ent) :-
	findall(C, member(e(C,_),Esempi), Classi),
	probabilita(Classi, Prob),
	entropia(Prob, Ent).

probabilita(Classi, Prob) :-
	length(Classi, N),
	sort(Classi, Dist),
	findall(P,
		(member(C,Dist),
		 include(=(C),Classi,L),
		 length(L,NC),
		 P is NC/N),
		Prob).

% ================================================================================
% CLASSE MAGGIORITARIA
% ================================================================================

classe_maggioritaria(Esempi, Classe) :-
	findall(C, member(e(C,_),Esempi), Classi),
	sort(Classi, Dist),
	findall(N-C,
		(member(C,Dist),
		 include(=(C),Classi,L),
		 length(L,N)),
		Conta),
	keysort(Conta, Ord),
	last(Ord, _-Classe).

% ================================================================================
% COSTRUZIONE SOTTOALBERI
% ================================================================================

induce_alberi(_, [], _, _, []).
induce_alberi(Att, [Val|Vs], AttRim, Esempi, [Val:Alb|Altri]) :-
	attval_subset(Att=Val, Esempi, Sub),
	induce_albero(AttRim, Sub, Alb),
	induce_alberi(Att, Vs, AttRim, Esempi, Altri).

attval_subset(Cond, Esempi, Sub) :-
	findall(e(C,O),
		(member(e(C,O),Esempi), soddisfa(O,[Cond])),
		Sub).

% ================================================================================
% SODDISFACIBILITÀ
% ================================================================================

soddisfa(Oggetto, Condizioni) :-
	\+ (member(A=V,Condizioni),
	    member(A=VX,Oggetto),
	    VX \== V).

del(X,[X|C],C) :- !.
del(X,[T|C],[T|C1]) :- del(X,C,C1).

% ================================================================================
% STAMPA ALBERO
% ================================================================================

mostra(T) :- mostra(T,0).
mostra(null,_) :- writeln(' ==> ?').
mostra(l(X),_) :- write(' ==> '), writeln(X).
mostra(t(A,L),I) :-
	nl, tab(I), write(A), nl,
	I1 is I+2,
	mostratutto(L,I1).

mostratutto([], _).
mostratutto([V:T|C], I) :-
	tab(I), write(V),
	I1 is I+2,
	mostra(T,I1),
	mostratutto(C,I).

% ================================================================================
% CLASSIFICAZIONE
% ================================================================================

classifica(Oggetto, nc, t(Att,Valori)) :-
	member(Att=Val,Oggetto),
	member(Val:null,Valori).

classifica(Oggetto, Classe, t(Att,Valori)) :-
	member(Att=Val,Oggetto),
	member(Val:l(Classe),Valori).

classifica(Oggetto, Classe, t(Att,Valori)) :-
	member(Att=Val,Oggetto),
	delete(Oggetto,Att=Val,Resto),
	member(Val:t(AttF,ValF),Valori),
	classifica(Resto,Classe,t(AttF,ValF)).

% ================================================================================
% MATRICE DI CONFUSIONE
% ================================================================================

stampa_matrice_di_confusione :-
	alb(Albero),
	findall(Classe/Oggetto, s(Classe,Oggetto), TestSet),
	length(TestSet,N),
	valuta(Albero,TestSet,VN,0,VP,0,FN,0,FP,0,NC,0),
	A is (VP + VN) / (N - NC),
	E is 1 - A,
	writeln('==========================='),
	write('Test effettuati: '), writeln(N),
	write('Non classificati: '), writeln(NC),
	write('VN: '), write(VN), write('  FP: '), writeln(FP),
	write('FN: '), write(FN), write('  VP: '), writeln(VP),
	write('Accuratezza: '), writeln(A),
	write('Errore: '), writeln(E).

valuta(_,[],VN,VN,VP,VP,FN,FN,FP,FP,NC,NC).
valuta(A,[infarto/O|C],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(O,infarto,A), !,
	VNA1 is VNA+1,
	valuta(A,C,VN,VNA1,VP,VPA,FN,FNA,FP,FPA,NC,NCA).
valuta(A,[nessun_infarto/O|C],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(O,nessun_infarto,A), !,
	VPA1 is VPA+1,
	valuta(A,C,VN,VNA,VP,VPA1,FN,FNA,FP,FPA,NC,NCA).
valuta(A,[nessun_infarto/O|C],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(O,infarto,A), !,
	FNA1 is FNA+1,
	valuta(A,C,VN,VNA,VP,VPA,FN,FNA1,FP,FPA,NC,NCA).
valuta(A,[infarto/O|C],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(O,nessun_infarto,A), !,
	FPA1 is FPA+1,
	valuta(A,C,VN,VNA,VP,VPA,FN,FNA,FP,FPA1,NC,NCA).
valuta(A,[_/O|C],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	classifica(O,nc,A), !,
	NCA1 is NCA+1,
	valuta(A,C,VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA1).

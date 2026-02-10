
:- ensure_loaded(heart_database).

% ------------------------------------------------------------------------------
% 1. CONTEGGI GENERALI
% ------------------------------------------------------------------------------

% numero_pazienti(-N)
numero_pazienti(N) :-
    findall(_, heart(_,_,_,_,_,_,_,_,_,_,_,_,_,_), L),
    length(L, N).

% numero_malati(-N)
% Conta quanti hanno 'presence' nell'ultimo campo
numero_malati(N) :-
    findall(_, heart(_,_,_,_,_,_,_,_,_,_,_,_,_,presence), L),
    length(L, N).

% numero_sani(-N)
% Conta quanti hanno 'absence' nell'ultimo campo
numero_sani(N) :-
    findall(_, heart(_,_,_,_,_,_,_,_,_,_,_,_,_,absence), L),
    length(L, N).

% ------------------------------------------------------------------------------
% 2. PROBABILITÀ SEMPLICI
% ------------------------------------------------------------------------------

% prob_infarto_globale(-Prob)
% P(Infarto) su tutta la popolazione
prob_infarto_globale(Prob) :-
    numero_pazienti(Tot),
    numero_malati(Malati),
    Tot > 0,
    Prob is Malati / Tot.

% prob_sesso(+Sesso, -Prob)
% Probabilità di essere di un certo sesso (0 o 1) nel dataset
prob_sesso(Sesso, Prob) :-
    numero_pazienti(Tot),
    findall(_, heart(_,Sesso,_,_,_,_,_,_,_,_,_,_,_,_), L),
    length(L, N),
    Prob is N / Tot.

% ------------------------------------------------------------------------------
% 3. ANALISI DEL RISCHIO (Probabilità Condizionata)
% P(Infarto | Condizione) = N(Infarto e Condizione) / N(Condizione)
% ------------------------------------------------------------------------------

% --- PER VARIABILI CATEGORICHE (Sesso, Tipo Dolore, Angina) ---

% prob_infarto_dato_sesso(+Sesso, -Prob)
% Sesso: 0 o 1
prob_infarto_dato_sesso(Sesso, Prob) :-
    % 1. Contiamo tutti quelli di quel sesso
    findall(_, heart(_,Sesso,_,_,_,_,_,_,_,_,_,_,_,_), Gruppo),
    length(Gruppo, N_Gruppo),
    N_Gruppo > 0, % Evita divisione per zero
    % 2. Contiamo quanti di quel gruppo hanno l'infarto
    findall(_, heart(_,Sesso,_,_,_,_,_,_,_,_,_,_,_,presence), Malati),
    length(Malati, N_Malati),
    Prob is N_Malati / N_Gruppo.

% prob_infarto_dato_dolore(+TipoDolore, -Prob)
% TipoDolore: 1, 2, 3, 4
prob_infarto_dato_dolore(Tipo, Prob) :-
    findall(_, heart(_,_,Tipo,_,_,_,_,_,_,_,_,_,_,_), Gruppo),
    length(Gruppo, N_Gruppo),
    N_Gruppo > 0,
    findall(_, heart(_,_,Tipo,_,_,_,_,_,_,_,_,_,_,presence), Malati),
    length(Malati, N_Malati),
    Prob is N_Malati / N_Gruppo.

% prob_infarto_data_angina(+Angina, -Prob)
% Angina da sforzo: 0 o 1
prob_infarto_data_angina(Angina, Prob) :-
    findall(_, heart(_,_,_,_,_,_,_,_,Angina,_,_,_,_,_), Gruppo),
    length(Gruppo, N_Gruppo),
    N_Gruppo > 0,
    findall(_, heart(_,_,_,_,_,_,_,_,Angina,_,_,_,_,presence), Malati),
    length(Malati, N_Malati),
    Prob is N_Malati / N_Gruppo.

% --- PER VARIABILI CONTINUE (Età, Colesterolo, Pressione) ---
% Qui usiamo range (Maggiore di, Min-Max) perché i valori esatti sono troppi.

% prob_infarto_dato_eta_range(+Min, +Max, -Prob)
% Probabilità di infarto se l'età è compresa tra Min (incluso) e Max (escluso)
prob_infarto_dato_eta_range(Min, Max, Prob) :-
    % Denominatore: Persone nel range di età
    findall(E, (heart(E,_,_,_,_,_,_,_,_,_,_,_,_,_), E >= Min, E < Max), Gruppo),
    length(Gruppo, N_Gruppo),
    N_Gruppo > 0,
    % Numeratore: Persone nel range CON infarto
    findall(E, (heart(E,_,_,_,_,_,_,_,_,_,_,_,_,presence), E >= Min, E < Max), Malati),
    length(Malati, N_Malati),
    Prob is N_Malati / N_Gruppo.

% prob_infarto_dato_colesterolo_sopra(+Soglia, -Prob)
% Probabilità di infarto se il colesterolo è superiore a una Soglia
prob_infarto_dato_colesterolo_sopra(Soglia, Prob) :-
    findall(C, (heart(_,_,_,_,C,_,_,_,_,_,_,_,_,_), C > Soglia), Gruppo),
    length(Gruppo, N_Gruppo),
    N_Gruppo > 0,
    findall(C, (heart(_,_,_,_,C,_,_,_,_,_,_,_,_,presence), C > Soglia), Malati),
    length(Malati, N_Malati),
    Prob is N_Malati / N_Gruppo.

% prob_infarto_dato_pressione_sopra(+Soglia, -Prob)
prob_infarto_dato_pressione_sopra(Soglia, Prob) :-
    findall(P, (heart(_,_,_,P,_,_,_,_,_,_,_,_,_,_), P > Soglia), Gruppo),
    length(Gruppo, N_Gruppo),
    N_Gruppo > 0,
    findall(P, (heart(_,_,_,P,_,_,_,_,_,_,_,_,_,presence), P > Soglia), Malati),
    length(Malati, N_Malati),
    Prob is N_Malati / N_Gruppo.

% ------------------------------------------------------------------------------
% 4. ANALISI CONGIUNTA (Due fattori insieme)
% ------------------------------------------------------------------------------

% prob_infarto_dato_sesso_e_dolore(+Sesso, +TipoDolore, -Prob)
prob_infarto_dato_sesso_e_dolore(Sesso, Tipo, Prob) :-
    findall(_, heart(_,Sesso,Tipo,_,_,_,_,_,_,_,_,_,_,_), Gruppo),
    length(Gruppo, N_Gruppo),
    N_Gruppo > 0,
    findall(_, heart(_,Sesso,Tipo,_,_,_,_,_,_,_,_,_,_,presence), Malati),
    length(Malati, N_Malati),
    Prob is N_Malati / N_Gruppo.
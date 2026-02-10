/* =========================================
   CONFUSION MATRIX – DATASET CARDIOLOGICO
   ========================================= */

:- consult('heart_test_set.pl').

/* =========================================
   CLASSI
   ========================================= */

classe_positiva(infarto).
classe_negativa(nessun_infarto).

/* =========================================
   MODELLO (ALBERO DECISIONALE)
   ========================================= */

predict(Attributi, Classe) :-
    alb(Albero),
    classifica(Attributi, Classe, Albero).

/* =========================================
   CONFUSION MATRIX
   ========================================= */

confusion_matrix(TP, FP, FN, TN) :-
    findall((Reale, Predetta),
        ( s(Reale, Attributi),
          predict(Attributi, Predetta)
        ),
        Risultati),
    conta_risultati(Risultati, TP, FP, FN, TN).

/* =========================================
   CONTEGGIO CASI
   ========================================= */

conta_risultati([], 0, 0, 0, 0).

conta_risultati([(infarto, infarto) | T], TP, FP, FN, TN) :-
    conta_risultati(T, TP1, FP, FN, TN),
    TP is TP1 + 1.

conta_risultati([(nessun_infarto, infarto) | T], TP, FP, FN, TN) :-
    conta_risultati(T, TP, FP1, FN, TN),
    FP is FP1 + 1.

conta_risultati([(infarto, nessun_infarto) | T], TP, FP, FN, TN) :-
    conta_risultati(T, TP, FP, FN1, TN),
    FN is FN1 + 1.

conta_risultati([(nessun_infarto, nessun_infarto) | T], TP, FP, FN, TN) :-
    conta_risultati(T, TP, FP, FN, TN1),
    TN is TN1 + 1.

conta_risultati([(_, nc) | T], TP, FP, FN, TN) :-
    conta_risultati(T, TP, FP, FN, TN).

/* =========================================
   METRICHE DI VALUTAZIONE
   ========================================= */

accuracy(TP, FP, FN, TN, Acc) :-
    Tot is TP + FP + FN + TN,
    Tot > 0,
    Acc is (TP + TN) / Tot.

precision(TP, FP, Prec) :-
    Den is TP + FP,
    Den > 0,
    Prec is TP / Den.

recall(TP, FN, Rec) :-
    Den is TP + FN,
    Den > 0,
    Rec is TP / Den.

specificity(TN, FP, Spec) :-
    Den is TN + FP,
    Den > 0,
    Spec is TN / Den.

/* =========================================
   REPORT COMPLETO
   ========================================= */

report :-
    confusion_matrix(TP, FP, FN, TN),
    accuracy(TP, FP, FN, TN, Acc),
    precision(TP, FP, Prec),
    recall(TP, FN, Rec),
    specificity(TN, FP, Spec),

    nl,
    write('==== CONFUSION MATRIX ===='), nl,
    write('TP (Infarto correttamente rilevati): '), writeln(TP),
    write('FP (Falsi positivi): '), writeln(FP),
    write('FN (Infarti non rilevati): '), writeln(FN),
    write('TN (Sani correttamente classificati): '), writeln(TN),

    nl,
    write('==== METRICHE ===='), nl,
    write('Accuracy: '), writeln(Acc),
    write('Precision: '), writeln(Prec),
    write('Recall (Sensibilità): '), writeln(Rec),
    write('Specificità: '), writeln(Spec),
    nl.

% Дозволяє динамічно змінювати базу знань
:- dynamic transition/3.
:- dynamic accepting/1.
:- dynamic initial_state/1.

% Встановлення початкового стану
set_initial_state(State) :- assertz(initial_state(State)).

% Додає факти про автомат
add_transition(From, Symbol, To) :- assertz(transition(From, Symbol, To)).
add_accepting(State) :- assertz(accepting(State)).

% Видаляє всі факти про автомат (для нового вводу)
clear_automaton :- retractall(transition(_, _, _)), retractall(accepting(_)).

% Модифікуємо правило move та accept, щоб використовувати заданий початковий стан
move(State, [Char|Tail], NextState) :-
    transition(State, Char, State2),
    move(State2, Tail, NextState).
move(State, [], State).

accept(Word, State) :-
    initial_state(InitialState),
    move(InitialState, Word, State),
    accepting(State).

% Генерує префікси, щоб слово було прийнято
generate_prefix(Word, Prefix) :-
    findall(X, transition(_, X, _), Symbols), % Зібрати всі можливі символи
    generate_prefix_iter(Word, Symbols, [], 10, Prefix).

generate_prefix_iter([], _, Prefix, _, Prefix). % Базовий випадок: не залишилося символів для додавання

generate_prefix_iter([Char|Tail], Symbols, Accumulated, Depth, Prefix) :-
    Depth > 0,
    (   accept([Char|Tail], _) ->
        reverse(Accumulated, Prefix) % Якщо слово приймається, повернути накопичений префікс
    ;   member(Symbol, Symbols),
        append(Accumulated, [Symbol], NewAccumulated),
        append([Symbol], [Char|Tail], NewWord), % Спробувати додати кожен символ до слова
        NewDepth is Depth - 1,
        generate_prefix_iter(NewWord, Symbols, NewAccumulated, NewDepth, Prefix)
    ).

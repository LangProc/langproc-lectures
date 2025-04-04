% A type-inference engine for a simple programming language
% John Wickerson, January 2020
% brew install swi-prolog
% swipl typechecker.pl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    1. The language we want to type    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Types, T ::= int | bool | T → T
% Variables, X
% Integer literals, N
% Expressions, E ::= num(N) |  add(E, E)
%                  | true | false | equal(E, E) | ifthenelse(E, E, E)
%                  | fun(X, E) | apply(E, E) | var(X) | let(X, E, E)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    2. Preliminaries    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Declare "→" as a right-associative infix operator. This means
% that "T → U → V" will be interpreted as "T → (U → V)".
:- op(700, xfy, →).

% Declare ":" and "⊢" as non-associative infix operators, with
% precedences set so that "Γ ⊢ E : T" is interpreted as "Γ ⊢ (E : T)"
:- op(900, xfx, :).
:- op(910, xfx, ⊢).

% Declare "∈" as an infix operator, and define it so that "X ∈ L"
% holds when X is one of the elements of the list L.
:- op(920, xfx, ∈).
X ∈ [X | _] :- !.
X ∈ [_ | L] :- X ∈ L.   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   3. The actual typing rules   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

_ ⊢ num(_) : int.

Γ ⊢ add(E1, E2) : int :-
  Γ ⊢ E1 : int,
  Γ ⊢ E2 : int.

Γ ⊢ equal(E1, E2) : bool :-
  Γ ⊢ E1 : int,
  Γ ⊢ E2 : int.

Γ ⊢ if(E1, E2, E3) : T :-
  Γ ⊢ E1 : bool,
  Γ ⊢ E2 : T,
  Γ ⊢ E3 : T.

_ ⊢ true : bool.

_ ⊢ false : bool.

Γ ⊢ fun(X, E) : T1 → T2 :-
  [X:T1 | Γ] ⊢ E : T2.

Γ ⊢ apply(E1, E2) : T :-
  Γ ⊢ E1 : T2 → T,
  Γ ⊢ E2 : T2.

Γ ⊢ var(X) : T :-
  (X:T) ∈ Γ.

Γ ⊢ let(X, E1, E2) : T :-
  Γ ⊢ E1 : T1,
  [X:T1 | Γ] ⊢ E2 : T.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   4. Some sample queries to try   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Arithmetic expressions ("type checking")
% [] ⊢ add(num(1), add(num(2), num(3))) : int. % (should succeed)
% [] ⊢ add(num(1), add(num(2), num(3))) : bool. % (should fail)

% Arithmetic expressions ("type inference")
% [] ⊢ add(num(1), add(num(2), num(3))) : T.

% Boolean expressions
% [] ⊢ if(true, add(num(1), num(2)), num(3)) : T.
% [] ⊢ if(equal(add(num(1), num(2)), num(3)), false, true) : T.

% Functions
% [] ⊢ fun(x, var(x)) : T.
% [] ⊢ fun(x, add(var(x), num(1))) : T.
% [] ⊢ fun(x, fun(y, equal(var(x), var(y)))) : T.
% [] ⊢ fun(x, add(var(x), var(y))) : T.
% Γ ⊢ fun(x, add(var(x), var(y))) : T.
% [f:int→bool] ⊢ apply(var(f), num(1)) : T.
% [f:int→bool, x:int] ⊢ apply(var(f), var(x)) : T.
% [] ⊢ fun(x, apply(var(x), var(x))) : T.

% Let-expressions
% [] ⊢ let(d, num(5), add(var(d), num(2))) : T.
% [] ⊢ let(i, fun(a, var(a)), let(d, fun(b, add(var(b), var(b))), apply(var(d), apply(var(i), num(2))))) : T.
% [] ⊢ let(i, fun(a, var(a)), let(d, fun(b, add(var(b), var(b))), apply(apply(var(i), var(d)), apply(var(i), num(2))))) : T.

% Asking for program that has a given type ("program synthesis").
% [] ⊢ E : int.
% [] ⊢ E : bool.
% [] ⊢ E : int → int. % (beware: gets stuck)
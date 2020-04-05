:- use_module(library(lists)).

% Predicados auxiliares
%
% Extensão do predicado ultimo_digito_valido: Nif ->{V,F}
% Validação do último bit através da técnica módulo 11
ultimo_digito_valido(Nif):-
    Primeiros_oito_digitos is div(Nif,10),
    Ultimo_digito is mod(Nif,10),
    digito_de_controlo(Primeiros_oito_digitos,Digito),
    Ultimo_digito =:= Digito.

% Extensão do predicadoque obtém o digito de controlo a partir dos primeiros 8 dígitos através da técnica módulo 11
digito_de_controlo(Primeiros_oito_digitos,Digito_de_controlo):-
    multiplica_lista(Primeiros_oito_digitos,Ultimo_digito),
    (0 is mod(Ultimo_digito,11); 1 is mod(Ultimo_digito,11)),
    Digito_de_controlo is 0.
digito_de_controlo(Nif,Digito_de_controlo):-
    multiplica_lista(Nif,Ultimo_digito),
    Mod_Result is mod(Ultimo_digito,11),
    Digito_de_controlo is 11-Mod_Result.


% Extensão do predicado que obtém o número a ser operado com
multiplica_lista(Nif,Ultimo_digito):-
    nif_para_lista(Nif,Lista),
    multiplicacao_decrescente(Lista,Ultimo_digito,9).


% Extensão do predicado que converte uma lista de códigos ascii na lista de dígitos que representam.
nif_para_lista(Nif,L):-
    number_codes(Nif,X),
    maplist(ascii_to_digit,X, L).

%Extensao do predicado que aplica uma função a todos os elementos de uma lista
/*mapear(Pred, Xs, Ys) :-
	(foreach(X,Xs),
	 foreach(Y,Ys),
   param(Pred)
   do  call(Pred, X, Y)
  ).*/

% Extensão do predicado converte um código ascii para o número correspondente ascii_to_digit
ascii_to_digit(Ascii_number,R) :- R is Ascii_number-48.


% Extensão do predicado que multiplica cada elemento da lista por um número que vai decrescendo e soma todos os valores resultantes
multiplicacao_decrescente([X],R,Num):- R is X*Num.
multiplicacao_decrescente([H|T],R,Num):-
    multiplicacao_decrescente(T,Res,Num-1),
    R is Res + (H*Num).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado data: Dia,Mes,Ano -> {V,F}
% Caso especial de Fevereiro em ano não bissexto
data(Dia,2,Ano):-
    Dia >= 1,
    Dia =<29,
    0 =:= mod(Ano,4). % ano bissexto

data(Dia,2,_):-
    Dia >= 1,
    Dia =< 28. % ano não bissexto

data(Dia,Mes,_):-
    member(Mes,[4,6,9,11]), % meses de 30 dias
    Dia >=1,
    Dia =<30.


data(Dia,Mes,_):-
    member(Mes,[1,3,5,7,8,10,12]), %meses de 31 dias
    Dia >=1,
    Dia =<31.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado data_valida: Dia,Mes,Ano -> {V,F}
data_valida(data(Dia,Mes,Ano)):- data(Dia,Mes,Ano).

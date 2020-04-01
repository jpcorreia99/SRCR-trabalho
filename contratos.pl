
:- consult('aux.pl').
% Definições iniciais

:- op(900, xfy, '::').
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.


% Factos
%
% Extensão do predicado adjudicante #IdAd, Nome, NIF, Morada ->{V,F,D}
adjudicante(1,'Camara de Braga',705330336,'Praça do Município').

% Extensão do predicado adjudicataria #IdAda, Nome, Nif, Morada ->{V,F,D}
adjudicataria(1,'Universidade do minho',502011378, 'Largo do Paço'). 
/*
nifs:
123456789
111111110
200000004
*/

% Invariantes
%
% Invariante estrutural: não permitir a entrada repetida de conhecimento, no campo do Id, nome e Nif

+adjudicante(Id,Nome,Nif,_) :: (
    solucoes( (Id),(adjudicante( Id,_,_,_)),S1 ),
    solucoes( (Nome),(adjudicante( _,Nome,_,_)),S2 ),
    solucoes( (Nif),(adjudicante( _,_,Nif,_)),S3 ),
    comprimento( S1,N1 ), 
    comprimento( S2,N2 ),
    comprimento( S3,N3 ),
    Total is N1 + N2 + N3,
    Total == 3
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: o dígito de controlo do id deve seguir a convenção de validação 'módulo 11'
+adjudicante(_,_,Nif,_) :: (
    integer(Nif), 
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: não permitir a entrada repetida de conhecimento, no campo do Id, nome e Nif

+adjudicataria(Id,Nome,Nif,_) :: (
    solucoes( (Id),(adjudicataria( Id,_,_,_)),S1 ),
    solucoes( (Nome),(adjudicataria( _,Nome,_,_)),S2 ),
    solucoes( (Nif),(adjudicataria( _,_,Nif,_)),S3 ),
    comprimento( S1,N1 ), 
    comprimento( S2,N2 ),
    comprimento( S3,N3 ),
    Total is N1 + N2 + N3,
    Total == 3
).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: o dígito de controlo do id deve seguir a convenção de validação 'módulo 11'
+adjudicataria(_,_,Nif,_) :: (
    integer(Nif), 
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).


% Predicados
%
% Extensao do predicado add_adjudicante: IdAd, Nome, NIF, Morada -> {V,F}
add_adjudicante(Id,Nome,Nif,Morada) :- 
    evolucao(adjudicante(Id, Nome, Nif, Morada)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado add_adjudicataria: IdAd, Nome, NIF, Morada -> {V,F}
add_adjudicataria(Id,Nome,Nif,Morada) :- 
    evolucao(adjudicataria(Id, Nome, Nif, Morada)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
    solucoes( Invariante, +Termo::Invariante,Lista ),  %coloca numa lista todos os invariantes
    insercao( Termo ), %insere o termo na base de informação para ser testado a seguir
    teste( Lista ).  % testa o termo contra os invariantes, se passar fica

insercao( Termo ) :-
    assert( Termo ). % o assert insere o termo na base de conhecimento
insercao( Termo ) :-
    retract( Termo ), !,fail. % se não o consegui inserir direito, tem de o remover
	
teste( [] ).
teste( [R|LR] ) :-  % verifica se todos os testes ao invariante são positivos
    R,   
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extens�o do predicado que permite a involucao do conhecimento

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :- 
    assert( Termo ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( _ ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

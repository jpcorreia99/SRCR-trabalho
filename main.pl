/*A fazer

- adicionar invariantes para os -termos
- adicionar predicados de remoção
- trocar os predicados de inserção por predicados de inserção de informação completa e imcompleta
*/

% Consulta de predicados auxiliares
:- consult('predicados_auxiliares.pl').
:- consult('base_de_informacao.pl').
:- consult('invariantes.pl').
:- consult('evolucao_involucao.pl').

% Definições iniciais
:- multifile (-)/1. % para aceitar as definições de (-) tanto neste dicheiro como na base de informação


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pressuposto do Mundo Fechado

-adjudicante(IdAd,Nome,NIF,TipoEntidade,Morada):-
    nao(adjudicante(IdAd,Nome,NIF,TipoEntidade,Morada)),
    nao(excecao(adjudicante(IdAd,Nome,NIF,TipoEntidade,Morada))).

-adjudicataria(IdAda,Nome,NIF,TipoEntidade,Morada):-
    nao(adjudicataria(IdAda,Nome,NIF,TipoEntidade,Morada)) ,
    nao(excecao(adjudicataria(IdAda,Nome,NIF,TipoEntidade,Morada))).

-contrato(IdCont,IdAd,IdAda,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado):-
    nao(contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data, Subsidiado)),
    nao(excecao(contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data, Subsidiado))).



% Predicados
% Extensao do predicado numeroContratosAdjudicante: IdAdjudicante,R -> {V,F}
% Devolve em quantos uma entidade adjudicante está envolvida
numeroContratosAdjudicante(IdAd,R) :-
    solucoes((IdAd),contrato(_,IdAd,_,_,_,_,_,_,_,_,_),Lista),
    comprimento(Lista,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado numeroContratosAdjudicante: IdAdjudicante,R -> {V,F}
% Devolve em quantos uma entidade adjudicatária está envolvida
numeroContratosAdjudicataria(IdAda,R) :-
    solucoes((IdAda),contrato(_,_,IdAda,_,_,_,_,_,_,_,_),Lista),
    comprimento(Lista,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado valorContratosAdjudicante: IdAdjudicante,R -> {V,F}
% Devolve a soma do valor dos contratos em que uma entidade adjudicante está envolvida
valorContratosAdjudicante(IdAd,R) :-
    solucoes((Valores),contrato(_,IdAd,_,_,_,_,Valores,_,_,_,_),Lista),
    sum_list(Lista,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado valorContratosAdjudicante: IdAdjudicante,R -> {V,F}
% Devolve a soma do valor dos contratos em que uma entidade adjudicante está envolvida
valorContratosAdjudidicataria(IdAda,R) :-
    solucoes((Valores),contrato(_,_,IdAda,_,_,_,Valores,_,_,_,_),Lista),
    sum_list(Lista,R).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaAdjudicatariasAdjudicante: IdAdjudicante,R -> {V,F}
% Devolve a lista de ids de entidades adjudicataria com que a dada entidade adjudicante esteve envolvida em contratos
listaAdjudicatariasAdjudicante(IdAd,R) :-
    solucoes((IdsAdjudicatarias),contrato(_,IdAd,IdsAdjudicatarias,_,_,_,_,_,_,_,_),R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaAdjudicantesAdjudicataria: IdAdjudicataria,R -> {V,F}
% Devolve a lista de nomes de entidades adjudicantes com que a dada entidade adjudicataria esteve envolvida em contratos
listaAdjudicantesAdjudicataria(IdAda,R) :-
    solucoes((IdsAdjudicantes),contrato(_,IdsAdjudicantes,IdAda,_,_,_,_,_,_,_,_),R).





%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( _ ).

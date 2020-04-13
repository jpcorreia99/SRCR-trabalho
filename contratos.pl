/*A fazer

- adicionar invariantes para os -termos
- adicionar predicados de remoção
- trocar os predicados de inserção por predicados de inserção de informação completa e imcompleta
*/

% Consulta de predicados auxiliares
:- consult('predicados_auxiliares.pl').
:- consult('base_de_informacao.pl').
:- consult('invariantes.pl').

% Definições iniciais
:- multifile (-)/1. % para aceitar as definições de (-) tanto neste dicheiro como na base de informação


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pressuposto do Mundo Fechado

-adjudicante(IdAd,Nome,NIF,Morada):-
    nao(adjudicante(IdAd,Nome,NIF,Morada)) ,
    nao(excecao(adjudicante(IdAd,Nome,NIF,Morada))).

-adjudicataria(IdAda,Nome,NIF,Morada):-
    nao(adjudicataria(IdAda,Nome,NIF,Morada)) ,
    nao(excecao(adjudicataria(IdAda,Nome,NIF,Morada))).

-contrato(IdCont,IdAd,IdAda,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data):-
    nao(contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data)),
    nao(excecao(contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data))).






% Predicados
%
% Extensao do predicado add_adjudicante: IdAd, Nome, NIF, Morada -> {V,F}
add_adjudicante(Id,Nome,Nif,Morada) :-
    evolucao(adjudicante(Id, Nome, Nif, Morada)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado add_contrato: #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Valor, Prazo, Local, Data -> {V,F}
add_contrato(IdCont, IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data ):-
    evolucao(contrato(IdCont, IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado add_adjudicataria: IdAd, Nome, NIF, Morada -> {V,F}
add_adjudicataria(Id,Nome,Nif,Morada) :-
    evolucao(adjudicataria(Id, Nome, Nif, Morada)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado numeroContratosAdjudicante: IdAdjudicante,R -> {V,F}
% Devolve em quantos uma entidade adjudicante está envolvida
numeroContratosAdjudicante(IdAd,R) :-
    solucoes((IdAd),contrato(_,IdAd,_,_,_,_,_,_,_,_),Lista),
    comprimento(Lista,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado numeroContratosAdjudicante: IdAdjudicante,R -> {V,F}
% Devolve em quantos uma entidade adjudicatária está envolvida
numeroContratosAdjudicataria(IdAda,R) :-
    solucoes((IdAda),contrato(_,_,IdAda,_,_,_,_,_,_,_),Lista),
    comprimento(Lista,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
    solucoes( Invariante, +Termo::Invariante,Lista ),  %coloca numa lista todos os invariantes,
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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

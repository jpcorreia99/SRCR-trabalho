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
% Devolve a soma do valor dos contratos em que uma entidade adjudicatária está envolvida
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
% Devolve a lista de nomes de entidades adjudicantes com que a dada entidade adjudicatária esteve envolvida em contratos
listaAdjudicantesAdjudicataria(IdAda,R) :-
    solucoes((IdsAdjudicantes),contrato(_,IdsAdjudicantes,IdAda,_,_,_,_,_,_,_,_),R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaContratosAtivosAdjudicante: IdAdjudicante,data(Dia,Mes,Ano), R -> {V,F}
% Devolve a lista de ids dos contratos em que um adjudicante está envolvido e que estão ativos numa certa data
% O facto de um dos parametros ser uma data de comparação perimite flexibilidade
% sendo possível saber a lista dos contratos ativos no momento da pesquisa ou num momento no passado
listaContratosAtivosAdjudicante(IdAd,DataReferencia,R):-
    solucoes((IdsContratos,PrazosContratos,DatasContratos),
        contrato(IdsContratos,IdAd,_,_,_,_,_,PrazosContratos,_,DatasContratos,_),
        ListaTriplosIdPrazoData),
    filtraListaTriplosIdPrazoDataAtivo(ListaTriplosIdPrazoData,DataReferencia,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaContratosAtivosAdjudicataria IdAdjudicante,data(Dia,Mes,Ano), R -> {V,F}
% Devolve a lista de ids dos contratos em que uma entidade adjudicatária está envolvida e que estão ativos numa certa data
listaContratosAtivosAdjudicataria(IdAda,DataReferencia,R):-
    solucoes((IdsContratos,PrazosContratos,DatasContratos),
        contrato(IdsContratos,_,IdAda,_,_,_,_,PrazosContratos,_,DatasContratos,_),
        ListaTriplosIdPrazoData),
    filtraListaTriplosIdPrazoDataAtivo(ListaTriplosIdPrazoData,DataReferencia,R).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaContratosAcabadosAdjudicante: IdAdjudicante,data(Dia,Mes,Ano), R -> {V,F}
% Devolve a lista de ids dos contratos que já terminaram a uma dada data relativos a uma entidade adjudicante
% O predicado é cauteloso em não escolher tabmbém contratos que ainda não começaram
listaContratosAcabadosAdjudicante(IdAd,DataReferencia,R):-
    solucoes((IdsContratos,PrazosContratos,DatasContratos),
        contrato(IdsContratos,IdAd,_,_,_,_,_,PrazosContratos,_,DatasContratos,_),
        ListaTriplosIdPrazoData),
    filtraListaTriplosIdPrazoDataAcabado(ListaTriplosIdPrazoData,DataReferencia,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado listaContratosAcabadosAdjudicataria IdAdjudicante,data(Dia,Mes,Ano), R -> {V,F}
% Devolve a lista de ids dos contratos que já terminaram a uma dada data relativos a uma entidade adjudicatária
listaContratosAcabadosAdjudicataria(IdAda,DataReferencia,R):-
    solucoes((IdsContratos,PrazosContratos,DatasContratos),
        contrato(IdsContratos,_,IdAda,_,_,_,_,PrazosContratos,_,DatasContratos,_),
        ListaTriplosIdPrazoData),
    filtraListaTriplosIdPrazoDataAcabado(ListaTriplosIdPrazoData,DataReferencia,R).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( _ ).

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

:- discontiguous remove_incerto/1.
:- discontiguous remove_imperfeito/1.
:- discontiguous evolucao_perfeito/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao(Termo) :-
    solucoes(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    teste(Lista).

evolucao(-Termo) :-
    insercao(-Termo),
    solucoes(Invariante, +(-Termo)::Invariante, Lista),
    teste(Lista).

evolucao_incerto(Termo) :-
    solucoes(Invariante, +Termo::Invariante, Lista),
    solucoes(Invariante, +Termo:-:Invariante, ListaImperfeito),
    insercao(Termo),
    teste(Lista),
    teste(ListaImperfeito).

evolucao_impreciso(Termo) :-
  solucoes(Invariante, +Termo::Invariante, Lista),
  solucoes(Invariante, +Termo:~:Invariante, ListaImpreciso),
  insercao(excecao(Termo)),
  teste(Lista),
  teste(ListaImpreciso).

insercao( Termo ) :-
    assert( Termo ). % o assert insere o termo na base de conhecimento
insercao( Termo ) :-
    retract( Termo ), !,fail. % se não o consegui inserir direito, tem de o remover

teste( [] ).
teste( [R|LR] ) :- % verifica se todos os testes ao invariante são positivos
    R,
    teste( LR ).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento perfeito positivo/negativo

%Inserir conhecimento perfeito: adjudicante
evolucao_perfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    remove_imperfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)),
    evolucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicante(Id))).

%Inserir conhecimento perfeito negativo: adjudicante
evolucao_perfeito(-adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    evolucao(-adjudicante(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicante(Id))).

%Remoção de conhecimento relativo a adjudicante imperfeito
remove_imperfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    retract(excecao(adjudicante(Id,_,_,_,_))),
    remove_imperfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)).

remove_imperfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    retract(impreciso(adjudicante(Id))),
    remove_imperfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)).

remove_imperfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    remove_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)).

remove_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    incertoMorada(adjudicante(Id), Morada_desconhecido),
    remocao((excecao(adjudicante(IdE, NomeE,NifE,TipoE,MoradaE)) :-
      adjudicante(IdE, NomeE, NifE, TipoE, Morada_desconhecido))),
    remocao(adjudicante(Id,_,_,_,_)),
    retract(incertoMorada(adjudicante(Id), _)).


remove_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)).


%Inserir conhecimento perfeito: adjudicataria
evolucao_perfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    remove_imperfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)),
    evolucao(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicataria(Id))).

%Inserir conhecimento perfeito negativo: adjudicataria
evolucao_perfeito(-adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    evolucao(-adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicataria(Id))).

remove_imperfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    retract(excecao(adjudicataria(Id,_,_,_,_))),
    remove_imperfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)).

remove_imperfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    retract(impreciso(adjudicataria(Id))),
    remove_imperfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)).

remove_imperfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    remove_incerto(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)).

remove_incerto(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    incertoMorada(adjudicataria(Id), Morada_desconhecido),
    remocao((excecao(adjudicataria(IdE, NomeE,NifE,TipoE,MoradaE)) :-
      adjudicataria(IdE, NomeE, NifE, TipoE, Morada_desconhecido))),
    remocao(adjudicataria(Id,_,_,_,_)),
    retract(incertoMorada(adjudicataria(Id), _)).

remove_incerto(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)).

%Inserir conhecimento perfeito: contrato
evolucao_perfeito(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)) :-
    remove_imperfeito(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)),
    evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)),
    insercao(perfeito(contrato(IdContrato))).

%Inserir conhecimento perfeito negativo: contrato
evolucao_perfeito(-contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)) :-
    evolucao(-contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)),
    insercao(perfeito(contrato(IdContrato))).

%Remoção de conhecimento relativo a contrato imperfeito
remove_imperfeito(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)) :-
    retract(excecao(contrato(IdContrato,_,_,_,_,_,_,_,_,_,_))),
    remove_imperfeito(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)).

remove_imperfeito(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)) :-
    remove_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)).

remove_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)) :-
    incertoDescricao(contrato(IdContrato), Descricao_desconhecida),
    remocao((excecao(contrato(IdC,Id_Adj,IdAd,TPC,TDP,Desc,V,P,L,D,S)) :-
      adjudicante(IdC, Id_Adj, IdAd, TPC, TDP, Descricao_desconhecida, V, P, L, D, S))),
    remocao(contrato(IdContrato,_,_,_,_,_,_,_,_,_,_)),
    retract(incertoDescricao(contrato(IdContrato), _)).

remove_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local,Data,Subsidiado)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito incerto

%Adjudicante

%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    evolucao_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    insercao(incertoMorada(adjudicante(Id), Morada_desconhecido)),
    insercao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, N, Ni, T, Morada_desconhecido))).


%adjudicataria

%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    evolucao_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    insercao(incertoMorada(adjudicataria(IdAda), Morada_desconhecido)),
    insercao((excecao(adjudicataria(IdAdj, N, Ni, T, M)) :-
            adjudicataria(IdAdj, N, Ni, T, Morada_desconhecido))).


%Contrato


%Inserir conhecimento imperfeito na base de conhecimento: descricao desconhecido
evolucao_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local,Data,Subsidiado), descricao) :-
    evolucao_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local,Data,Subsidiado)),
    insercao(incertoDescricao(contrato(IdContrato), Descricao_desconhecida)),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc,D, Sub)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc,D,Sub))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito impreciso

% Inserir conhecimento imperfeito na base de conhecimento: morada imprecisa
evolucao_impreciso_morada(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
  remove_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)),
  insercao(impreciso(adjudicante(Id))),
  evolucao_impreciso(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)).

% Inserir conhecimento imperfeito na base de conhecimento: morada imprecisa
evolucao_impreciso_morada(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
  remove_incerto(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)),
  insercao(impreciso(adjudicataria(Id))),
  evolucao_impreciso(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)).

%/////////////////////////// ADICIONAR /////////////////////////////////////////////

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito interdito

%Adjudicante

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao_interdito(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    evolucao_incerto(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada)),
    insercao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))),
    insercao(interdito(adjudicante(Id))).

%Adjudicataria

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao_interdito(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    evolucao_incerto(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada)),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))),
    insercao(interdito(adjudicataria(Id))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento perfeito

%Adjudicante
%Remover conhecimento perfeito: adjudicante
involucao_perfeito(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    involucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)),
    remocao(perfeito(adjudicante(Id))).

%Adjudicataria
%Remover conhecimento perfeito: adjudicataria
involucao_perfeito(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    involucao(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)),
    remocao(perfeito(adjudicataria(Id))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito incerto

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    involucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    remocao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, T, Morada_desconhecido))),
    remocao(incerto(adjudicante(Id))).


%adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, T, Morada_desconhecido))),
    remocao(incerto(adjudicataria(IdAda))).


%Contrato

%Remover conhecimento imperfeito na base de conhecimento: descricao desconhecido
involucao_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local,Data,Subsidiado), descricao) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local,Data,Subsidiado)),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc,D,Sub)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc,D,Sub))),
    remocao(incerto(contrato(IdContrato))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito impreciso

%///////////////////////////////  ADICIONAR  ////////////////////////////////////

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito interdito

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao_interdito(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada)),
    remocao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    remocao((nulo(Nome_interdito))),
    remocao(interdito(adjudicante(Id))).

%Adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao_interdito(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada)),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    remocao((nulo(Nome_interdito))),
    remocao(interdito(adjudicataria(Id))).




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

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

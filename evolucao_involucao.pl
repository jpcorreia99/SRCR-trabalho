:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
/*:- set_prolog_flag( unknown,fail ).*/

:- style_check(-singleton).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao_original( Termo ) :-
    solucoes( Invariante, +Termo::Invariante,Lista ),  %coloca numa lista todos os invariantes,
    write(Lista),
    comprimento(Lista,R),
    write(R),
    insercao( Termo ), %insere o termo na base de informação para ser testado a seguir
    teste( Lista ).  % testa o termo contra os invariantes, se passar fica

evolucao(Termo) :-
    solucoes(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    teste(Lista).

evolucao(-Termo) :-
    insercao(-Termo),
    solucoes(Invariante, +(-Termo)::Invariante, Lista),
    teste(Lista).

evolucaoIncerto(Termo) :-
    solucoes(Invariante, +Termo::Invariante, Lista),
    solucoes(Invariante, +Termo:-:Invariante, ListaIncerto),
    insercao(Termo),
    teste(Lista),
    teste(ListaIncerto).

insercao( Termo ) :-
    assert( Termo ). % o assert insere o termo na base de conhecimento
insercao( Termo ) :-
    retract( Termo ), !,fail. % se não o consegui inserir direito, tem de o remover

incerto(adjudicante(Id)) :- incertoMorada(adjudicante(Id, _)).

incerto(contrato(Id)) :- incertoDescricao(contrato(Id, _)).

teste( [] ).
teste( [R|LR] ) :-  % verifica se todos os testes ao invariante são positivos
    R,
    teste( LR ).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento perfeito positivo/negativo

%Inserir conhecimento perfeito: adjudicante
evolucao_ad(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    evolucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicante(Id))).

%Inserir conhecimento perfeito negativo: adjudicante
evolucao_ad(-adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    evolucao(-adjudicante(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicante(Id))).

%Remoção de conhecimento relativo a adjudicante imperfeito
remove_imperfeito_ad(adjudicante(Id, Nome, NIF, TipoEntidade, Morada)) :-
    retract(excecao(adjudicante(Id,_,_,_,_))),
    retract(impreciso(adjudicante(Id))),
    incertoMorada(Id, Morada_desconhecido),
    retract(excecao(adjudicante(IdE, NomeE,NifE,TipoE,MoradaE))) :-
      adjudicante(IdE, NomeE, NifE, TipoE, Morada_desconhecido),
    retract(adjudicante(Id,_,_,_,_)),
    retract(incertoMorada(adjudicante(Id, _))).


%Inserir conhecimento perfeito: adjudicataria
evolucao_ada(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    evolucao(adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicataria(Id))).

%Inserir conhecimento perfeito negativo: adjudicataria
evolucao_ada(-adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)) :-
    evolucao(-adjudicataria(Id, Nome, NIF, TipoEntidade, Morada)),
    insercao(perfeito(adjudicataria(Id))).

%Inserir conhecimento perfeito: contrato
evolucao_cont(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local)) :-
    evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local)),
    insercao(perfeito(contrato(IdContrato))).

%Inserir conhecimento perfeito: contrato
evolucao_cont(-contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local)) :-
    evolucao(-contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local)),
    insercao(perfeito(contrato(IdContrato))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito incerto

%Adjudicante

%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao_ad_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    evolucaoIncerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    insercao(incertoMorada(adjudicante(Id)), Morada_desconhecido),
    insercao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%adjudicataria


%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao_ada_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    evolucaoIncerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    insercao(incertoMorada(adjudicataria(IdAda), Morada_desconhecido)),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%Contrato


%Inserir conhecimento imperfeito na base de conhecimento: descricao desconhecido
evolucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), descricao) :-
    evolucaoIncerto(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local)),
    insercao(incertoDescricao(contrato(IdContrato), Descricao_desconhecida)),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito inpreciso

%Contrato

%Inserir conhecimento imperfeito na base de conhecimento: valor impreciso
evolucao_cont_impreciso(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local), valor, LimInf, LimSup) :-
    solucoes(Invariante, +(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local))::Invariante, Lista),
    solucoes(Invariante, +(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local)):~:Invariante, ListaImpreciso),
    insercao((excecao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local)) :-
              Valor_impreciso >= LimInf, Valor_impreciso =< LimSup)),
    insercao(impreciso(contrato(IdContrato))),
    teste(Lista),
    teste(ListaImpreciso).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito interdito

%Adjudicante

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao_ad_interdito(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    evolucaoIncerto(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada)),
    insercao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))),
    insercao(interdito(adjudicante(Id))).

%Adjudicataria

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao_ada_interdito(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    evolucaoIncerto(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada)),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))),
    insercao(interdito(adjudicataria(Id))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito incerto

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: Nome desconhecido
involucao_ad_incerto(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada)),
    remocao((excecao(adjudicante(IdAdj, Nome, N, T, M)) :-
            adjudicante(IdAdj, Nome_desconhecido, N, T, M))),
    remocao(incerto(adjudicante(Id))).

%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao_ad_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    involucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    remocao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, T, Morada_desconhecido))),
    remocao(incerto(adjudicante(Id))).


%adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: Nome desconhecido
involucao_ada_incerto(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada)),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome_desconhecido, Ni, T, M))),
    remocao(incerto(adjudicataria(IdAda))).


%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao_ada_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido)),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, T, Morada_desconhecido))),
    remocao(incerto(adjudicataria(IdAda))).


%Contrato

%Remover conhecimento imperfeito na base de conhecimento: descricao desconhecido
involucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), descricao) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local)),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc))),
    remocao(incerto(contrato(IdContrato))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito impreciso

%///////////////////////////////  ADICIONAR  ////////////////////////////////////

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito interdito

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao_ad_interdito(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada)),
    remocao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    remocao((nulo(Nome_interdito))),
    remocao(interdito(adjudicante(Id))).

%Adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao_ada_interdito(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
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

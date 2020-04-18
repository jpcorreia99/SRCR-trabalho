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

evolucao(Termo , positivo) :-
    solucoes(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    teste(Lista).

evolucao(Termo , negativo) :-
    insercao(-Termo),
    solucoes(Invariante, +(-Termo)::Invariante, Lista),
    teste(Lista).

evolucao(Termo, impreciso) :-
    solucoes(Invariante, +(excecao(Termo))::Invariante, Lista),
    insercao(excecao(Termo)),
    teste(Lista).

insercao( Termo ) :-
    assert( Termo ). % o assert insere o termo na base de conhecimento
insercao( Termo ) :-
    retract( Termo ), !,fail. % se não o consegui inserir direito, tem de o remover

teste( [] ).
teste( [R|LR] ) :-  % verifica se todos os testes ao invariante são positivos
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito incerto

%Adjudicante

%Inserir conhecimento imperfeito na base de conhecimento: Nome desconhecido
evolucao_ad_incerto(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), nome) :-
    evolucao(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicante(IdAdj, Nome, N, T, M)) :-
            adjudicante(IdAdj, Nome_desconhecido, N, T, M))).


%Inserir conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecido
evolucao_ad_incerto(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), tipo_entidade) :-
    evolucao(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    insercao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao_ad_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    evolucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    insercao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%adjudicataria

%Inserir conhecimento imperfeito na base de conhecimento: Nome desconhecido
evolucao_ada_incerto(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), nome) :-
    evolucao(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome_desconhecido, Ni, T, M))).


%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao_ada_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    evolucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%Inserir conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecida
evolucao_ada_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), tipo_entidade) :-
    evolucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Contrato

%Inserir conhecimento imperfeito na base de conhecimento: id_adjudicante desconhecido
evolucao_cont_incerto(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), id_adjudicante) :-
    evolucao(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,Id_Adjudicante_desconhecido,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: id_adjudicataria desconhecido
evolucao_cont_incerto(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), id_adjudicataria) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdj,IdAdjudicataria_desconhecido,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: tipo_procedimento desconhecido
evolucao_cont_incerto(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), tipo_procedimento) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoDeProcedimento_desconhecido,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: tipo_de_contrato desconhecido
evolucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), tipo_de_contrato) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdjudicante,IdAda,TipoDeContrato_desconhecido,TipoP,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: descricao desconhecido
evolucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), descricao) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: local desconhecido
evolucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), local) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Local_desconhecido))).

%Inserir conhecimento imperfeito na base de conhecimento: valor desconhecido
evolucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), valor) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Valor_desconhecido,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: prazo desconhecido
evolucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), prazo) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Prazo_desconhecido,Loc))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito inpreciso

%Contrato

%Inserir conhecimento imperfeito na base de conhecimento: valor impreciso
evolucao_cont_impreciso(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local), valor, LimInf, LimSup) :-
    insercao((excecao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local)) :-
              Valor_impreciso >= LimInf, Valor_impreciso =< LimSup)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito interdito

%Adjudicante

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao_ad_interdito(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    evolucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))).

%Adjudicataria

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao_ada_interdito(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    evolucao(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito incerto

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: Nome desconhecido
involucao_ad_incerto(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    remocao((excecao(adjudicante(IdAdj, Nome, N, T, M)) :-
            adjudicante(IdAdj, Nome_desconhecido, N, T, M))).


%Remover conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecido
involucao_ad_incerto(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), tipo_entidade) :-
    involucao(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    remocao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao_ad_incerto(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    involucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    remocao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: Nome desconhecido
involucao_ada_incerto(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome_desconhecido, Ni, T, M))).


%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao_ada_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), morada) :-
    involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%Remover conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecida
involucao_ada_incerto(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), tipo_entidade) :-
    involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Contrato

%Remover conhecimento imperfeito na base de conhecimento: id_adjudicante desconhecido
involucao_cont_incerto(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), id_adjudicante) :-
    involucao(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,Id_Adjudicante_desconhecido,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: id_adjudicataria desconhecido
involucao_cont_incerto(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), id_adjudicataria) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdj,IdAdjudicataria_desconhecido,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: tipo_procedimento desconhecido
involucao_cont_incerto(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), tipo_procedimento) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoDeProcedimento_desconhecido,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: tipo_de_contrato desconhecido
involucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), tipo_de_contrato) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdjudicante,IdAda,TipoDeContrato_desconhecido,TipoP,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: descricao desconhecido
involucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), descricao) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: local desconhecido
involucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), local) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Local_desconhecido))).

%Remover conhecimento imperfeito na base de conhecimento: valor desconhecido
involucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), valor) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Valor_desconhecido,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: prazo desconhecido
involucao_cont_incerto(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), prazo) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Prazo_desconhecido,Loc))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito inpreciso

%Contrato

%Remover conhecimento imperfeito na base de conhecimento: valor impreciso
involucao_cont_impreciso(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local), valor, LimInf, LimSup) :-
    remocao((excecao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local)) :-
              Valor_impreciso >= LimInf, Valor_impreciso =< LimSup)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito interdito

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao_ad_interdito(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), positivo),
    remocao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    remocao((nulo(Nome_interdito))).

%Adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao_ada_interdito(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), nome) :-
    involucao(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), positivo),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    remocao((nulo(Nome_interdito))).




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

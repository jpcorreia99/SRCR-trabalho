:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
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
evolucao(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), adjudicante, incerto, nome) :-
    evolucao(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicante(IdAdj, Nome, N, T, M)) :-
            adjudicante(IdAdj, Nome_desconhecido, N, T, M))).


%Inserir conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecido
evolucao(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), adjudicante, incerto, tipo_entidade) :-
    evolucao(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    insercao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), adjudicante, incerto, morada) :-
    evolucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    insercao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%adjudicataria

%Inserir conhecimento imperfeito na base de conhecimento: Nome desconhecido
evolucao(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), adjudicataria, incerto, nome) :-
    evolucao(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome_desconhecido, Ni, T, M))).


%Inserir conhecimento imperfeito na base de conhecimento: Morada desconhecida
evolucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), adjudicataria, incerto, morada) :-
    evolucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%Inserir conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecida
evolucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), adjudicataria, incerto, tipo_entidade) :-
    evolucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Contrato

%Inserir conhecimento imperfeito na base de conhecimento: id_adjudicante desconhecido
evolucao(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), contrato, incerto, id_adjudicante) :-
    evolucao(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,Id_Adjudicante_desconhecido,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: id_adjudicataria desconhecido
evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), contrato, incerto, id_adjudicataria) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdj,IdAdjudicataria_desconhecido,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: tipo_procedimento desconhecido
evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), contrato, incerto, tipo_procedimento) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoDeProcedimento_desconhecido,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: tipo_de_contrato desconhecido
evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), contrato, incerto, tipo_de_contrato) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdjudicante,IdAda,TipoDeContrato_desconhecido,TipoP,Desc,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: descricao desconhecido
evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), contrato, incerto, descricao) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: local desconhecido
evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), contrato, incerto, local) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Local_desconhecido))).

%Inserir conhecimento imperfeito na base de conhecimento: valor desconhecido
evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), contrato, incerto, valor) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Valor_desconhecido,Praz,Loc))).

%Inserir conhecimento imperfeito na base de conhecimento: prazo desconhecido
evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), contrato, incerto, prazo) :-
    evolucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), positivo),
    insercao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Prazo_desconhecido,Loc))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito inpreciso

%Contrato

%Inserir conhecimento imperfeito na base de conhecimento: valor impreciso
evolucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local), contrato, impreciso, valor, LimInf, LimSup) :-
    insercao((excecao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local)) :-
              Valor_impreciso >= LimInf, Valor_impreciso =< LimSup)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolução de conhecimento imperfeito interdito

%Adjudicante

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), adjudicante, interdito, nome) :-
    evolucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))).

%Adjudicataria

%Inserir conhecimento imperfeito na base de conhecimento: nome interdito
evolucao(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), adjudicataria, interdito, nome) :-
    evolucao(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), positivo),
    insercao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    insercao((nulo(Nome_interdito))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito inpreciso

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: Nome desconhecido
involucao(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), adjudicante, incerto, nome) :-
    involucao(adjudicante(Id, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    remocao((excecao(adjudicante(IdAdj, Nome, N, T, M)) :-
            adjudicante(IdAdj, Nome_desconhecido, N, T, M))).


%Remover conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecido
involucao(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), adjudicante, incerto, tipo_entidade) :-
    involucao(adjudicante(Id, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    remocao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), adjudicante, incerto, morada) :-
    involucao(adjudicante(Id, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    remocao((excecao(adjudicante(IdAdj, N, Ni, T, M)) :-
            adjudicante(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: Nome desconhecido
involucao(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), adjudicataria, incerto, nome) :-
    involucao(adjudicataria(IdAda, Nome_desconhecido, NIF, TipoEntidade, Morada), positivo),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome_desconhecido, Ni, T, M))).


%Remover conhecimento imperfeito na base de conhecimento: Morada desconhecida
involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), adjudicataria, incerto, morada) :-
    involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada_desconhecido), positivo),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, T, Morada_desconhecido))).


%Remover conhecimento imperfeito na base de conhecimento: TipoEntidade desconhecida
involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), adjudicataria, incerto, tipo_entidade) :-
    involucao(adjudicataria(IdAda, Nome, NIF, TipoEntidade_desconhecido, Morada), positivo),
    remocao((excecao(adjudicataria(IdAd, N, Ni, T, M)) :-
            adjudicataria(IdAdj, Nome, Ni, TipoEntidade_desconhecido, M))).


%Contrato

%Remover conhecimento imperfeito na base de conhecimento: id_adjudicante desconhecido
involucao(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), contrato, incerto, id_adjudicante) :-
    involucao(contrato(IdContrato,Id_Adjudicante_desconhecido,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,Id_Adjudicante_desconhecido,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: id_adjudicataria desconhecido
involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), contrato, incerto, id_adjudicataria) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria_desconhecido,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdj,IdAdjudicataria_desconhecido,TipoC,TipoP,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: tipo_procedimento desconhecido
involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), contrato, incerto, tipo_procedimento) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento_desconhecido,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoDeProcedimento_desconhecido,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: tipo_de_contrato desconhecido
involucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), contrato, incerto, tipo_de_contrato) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato_desconhecido,TipoDeProcedimento,Descricao,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAdjudicante,IdAda,TipoDeContrato_desconhecido,TipoP,Desc,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: descricao desconhecido
involucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), contrato, incerto, descricao) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao_desconhecida,Valor,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Descricao_desconhecida,Val,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: local desconhecido
involucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), contrato, incerto, local) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local_desconhecido), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Local_desconhecido))).

%Remover conhecimento imperfeito na base de conhecimento: valor desconhecido
involucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), contrato, incerto, valor) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_desconhecido,Prazo,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Valor_desconhecido,Praz,Loc))).

%Remover conhecimento imperfeito na base de conhecimento: prazo desconhecido
involucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), contrato, incerto, prazo) :-
    involucao(contrato(IdContrato,IdAdjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo_desconhecido,Local), positivo),
    remocao((excecao(contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Praz,Loc)) :-
                    contrato(IdC,IdAd,IdAda,TipoC,TipoP,Desc,Val,Prazo_desconhecido,Loc))).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito inpreciso

%Contrato

%Remover conhecimento imperfeito na base de conhecimento: valor impreciso
involucao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local), contrato, impreciso, valor, LimInf, LimSup) :-
    remocao((excecao(contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor_impreciso,Prazo,Local)) :-
              Valor_impreciso >= LimInf, Valor_impreciso =< LimSup)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Involução de conhecimento imperfeito interdito

%Adjudicante

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), adjudicante, interdito, nome) :-
    involucao(adjudicante(Id, Nome_interdito, NIF, TipoEntidade, Morada), positivo),
    remocao((excecao(adjudicante(IdAd, N, Ni, T, M)) :-
        adjudicante(IdAd, Nome_interdito, Ni, T, M))),
    remocao((nulo(Nome_interdito))).

%Adjudicataria

%Remover conhecimento imperfeito na base de conhecimento: nome interdito
involucao(adjudicataria(Id, Nome_interdito, NIF, TipoEntidade, Morada), adjudicataria, interdito, nome) :-
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

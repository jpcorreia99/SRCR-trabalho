/*A fazer

- adicionar invariantes para os -termos
- adicionar predicados de remoção
- trocar os predicados de inserção por predicados de inserção de informação completa e imcompleta
*/

% Consulta de predicados auxiliares
:- consult('predicados_auxiliares.pl').
:- consult('base_de_informacao.pl').

% Definições iniciais
:- multifile (-)/1. % para aceitar as definições de (-) tanto neste dicheiro como na base de informação
:- op(900, xfy, '::').


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



% Invariantes sobre qualquer termo
%
%Invariante estrutural: Não permitir  a inserção de conhecimento contraditório
+Termo :: (
    write(Termo),
    nao(-Termo)
).

%Invariante estrutural: Não permitir  a inserção de conhecimento contraditório
+(-Termo) :: (
    write(Termo),
    nao(Termo)
).

% Invariante estrutural: Não permitir excecoes repetidas
+(excecao(Termo)) :: (
    solucoes(Termo, excecao(Termo), R),
    comprimento(R, 1)
).

% Invariantes sobre entidades adjudicantes
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
% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
+adjudicante(Id,Nome,Nif,Morada) :: (
    integer(Id),
    atom(Nome),
    integer(Nif),
    atom(Morada)
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o dígito de controlo do id deve seguir a convenção de validação 'módulo 11'
+adjudicante(_,_,Nif,_) :: (
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Não deve ser possivel remover uma entidade adjudicante se esta estiver presente num contrato
-adjudicante(Id,_,_,_) :: (
    solucoes(Id, contrato(_,Id,_,_,_,_,_,_,_,_), S),
    comprimento( S,N ),
    N == 0
).




%Invariantes sobre entidades adjudicatárias
%
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
% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
+adjudicataria(Id,Nome,Nif,Morada) :: (
    integer(Id),
    atom(Nome),
    integer(Nif),
    atom(Morada)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
+adjudicataria(Id,Nome,Nif,Morada) :: (
    integer(Id),
    string(Nome),
    invteger(Nif),
    string(Morada)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o dígito de controlo do id deve seguir a convenção de validação 'módulo 11'
+adjudicataria(_,_,Nif,_) :: (
    integer(Nif),
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Não deve ser possivel remover uma entidade adjudicataria se esta estiver presente num contrato
-adjudicataria(Id,_,_,_) :: (
    solucoes(Id, contrato(_,_,Id,_,_,_,_,_,_,_), S),
    comprimento( S,N ),
    N == 0
).

% Invariantes sobre contratos
%
% Invariante estrutural: verificar que os ids das entidades contratuais existem
% condição de N1 + N2 ao ser 2 pode-se inserir é assegurada pelos invariantes de adjudicante e adjudicatária
+contrato(_,IdAd,IdAda,_,_,_,_,_,_,_) :: (
    solucoes( (IdAd),(adjudicante( IdAd,_,_,_)),S1 ),
    solucoes( (IdAda),(adjudicataria( IdAda,_,_,_)),S2 ),
    comprimento( S1,N1 ),
    comprimento( S2,N2 ),
    Total is N1 + N2,
    Total == 2
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
+contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data) :: (
    integer(IdCont),
    integer(IdAd),
    integer(IdAda),
    atom(TipoDeContrato),
    atom(TipoDeProcedimento),
    atom(Descricao),
    integer(Valor),
    integer(Prazo),
    atom(Local),
    data_valida(Data)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o tipo de procedimento deve pertencer a um certo conjunto
+contrato(_,_,_,_,Procedimento,_,_,_,_,_) :: (
    member(Procedimento,['Ajuste direto','Consulta prévia','Concurso público'])
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o custo e duração devem ser superiores a 0
+contrato(_,_,_,_,_,_,Custo,Prazo,_,_) :: (
    Custo >= 0,
    Prazo > 0
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: O valor de um contrato de ajuste direto deve ser menor ou igual a 5000
% Deve ser de um dos seguintes tipos: Contrato de aquisição ou locação de bens móveis ou aquisição de serviços
% Prazo de vigência até 1 ano, inclusive, a contar da decisão de adjudicação.
+contrato(_,_,_,TipoDeContrato,'Ajuste direto',_,Custo,Prazo,_,_) :: (
    Custo =< 5000,
    Prazo =< 365,
    member(TipoDeContrato,['Aquisição de bens móveis','Locação de bens móveis','Aquisição de serviços'])
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: Uma entidade adjudicante não pode convidar a mesma empresa para celebrar um contrato com prestações de serviço
% do mesmo tipo ou idênticas às de contratos que já lhe foram atribuídos, no ano económico em curso e nos dois anos económicos anteriores,
% sempre que O preço contratual acumulado dos contratos já celebrados (não incluindo o contrato que se pretende celebrar) seja igual ou superior a 75.000 euros.
+contrato(_,IdAd,IdAda,'Aquisição de serviços',_,_,Valor,_,_,data(_,_,Ano)) :: (
    solucoes((Data,Custo),contrato(_,IdAd,IdAda,'Aquisição de serviços',_,_,Custo,_,_,Data),ParesDataCusto), %lista de pares
    somaCustosContratos(ParesDataCusto,Ano,SomaCustos),
    (SomaCustos-Valor) < 75000
).


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

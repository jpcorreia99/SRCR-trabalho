% Consulta de predicados auxiliares
:- consult('predicados_auxiliares.pl').

% Definições iniciais
:- op(900, xfy, '::').
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.


% Factos
%
% Extensão do predicado adjudicante #IdAd, Nome, NIF, Morada ->{V,F,D}
adjudicante(1,'Camara de Braga',705330336,'Praça do Município').
adjudicante(2,'Município de Altode Basto', 705330336, 'Portugal, Braga,Altode Basto').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicataria #IdAda, Nome, Nif, Morada -> {V,F,D}
adjudicataria(1,'Universidade do minho',502011378, 'Largo do Paço'). 
adjudicataria(2,'XXX -Associados -Sociedade de Advogados, SP, RL.',702675112,'Portugal').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado contrato #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Valor, Prazo, Local, Data -> {V,F,D}
contrato(2,2,'Aquisição de serviços', 'Consulta Prévia','Assessoria jurídica', 13599, 547,'Alto de Basto',data(11,02,2020)).
%contrato(1,1,'Aquisição de serviços', 'Consulta Prévia','Assessoria jurídica', 13599, 547,'Alto de Basto',data(11,02,2020)).

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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: verificar que os ids das entidades contratuais existem
% condição de N1 + N2 ao ser 2 pode-se inserir é assegurada pelos invariantes de adjudicante e adjudicatária
+contrato(IdAd,IdAda,_,_,_,_,_,_,_) :: (
    solucoes( (IdAd),(adjudicante( IdAd,_,_,_)),S1 ),
    solucoes( (IdAda),(adjudicataria( IdAda,_,_,_)),S2 ),
    comprimento( S1,N1 ), 
    comprimento( S2,N2 ),
    Total is N1 + N2,
    Total == 2
).



% Predicados
%
% Extensao do predicado add_adjudicante: IdAd, Nome, NIF, Morada -> {V,F}
add_adjudicante(Id,Nome,Nif,Morada) :- 
    evolucao(adjudicante(Id, Nome, Nif, Morada)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado add_contrato: #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Valor, Prazo, Local, Data -> {V,F}
add_contrato(IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data ):-
    evolucao(contrato(IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado add_adjudicataria: IdAd, Nome, NIF, Morada -> {V,F}
add_adjudicataria(Id,Nome,Nif,Morada) :- 
    evolucao(adjudicataria(Id, Nome, Nif, Morada)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
    solucoes( Invariante, +Termo::Invariante,Lista ),  %coloca numa lista todos os invariantes,
    comprimento( Lista,N ),
    write(N),
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

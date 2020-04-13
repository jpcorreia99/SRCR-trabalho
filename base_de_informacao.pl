
%Nifs usáveis
%876543212

:- multifile (-)/1.
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.
:- dynamic excecao/1.
:- dynamic (-)/1.

:- discontiguous adjudicante/4.
:- discontiguous adjudicataria/4.
:- discontiguous contrato/10.
:- discontiguous (-)/1.
:- discontiguous excecao/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicante #IdAd, Nome, NIF, Morada ->{V,F,D}

% Conhecimento Perfeito Positivo
adjudicante(1,'Camara de Braga',123456789,'Praça do Município').
adjudicante(2,'Município de Alto de Basto',705330336, 'Portugal, Braga,Alto de Basto').


% Conhecimento Perfeito Negativo
-adjudicante(1000,'André Alves',111111110,'Rua dos Barros Nº45').

% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicante(100,'Tasquinha Bracarense',nif_desconhecido,'Rua Nova de Santa Cruz').
excecao(adjudicante(IdAd,Nome,_,Morada)):-
    adjudicante(IdAd,Nome,nif_desconhecido,Morada).


% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicante(200,'António Reparações LDA',222222220,'Rua da Nascente')).
excecao(adjudicante(200,'António Reparações LDA',333333330,'Rua da Nascente')).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicataria #IdAda, Nome, Nif, Morada -> {V,F,D}

% Conhecimento Perfeito Positivo
adjudicataria(1,'Universidade do minho',502011378, 'Largo do Paço').
adjudicataria(2,'XXX -Associados -Sociedade de Advogados, SP, RL.',702675112,'Portugal').

% Conhecimento Perfeito Negativo
-adjudicante(1000,'Rodrigo Guedes LDA',876543212,'Rua 25 de Abril').

% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicataria(100,'Bruno Batista',nif_desconhecido,'Rua dos Barros Nº46').
excecao(adjudicataria(IdAd,Nome,_,Morada)):-
    adjudicataria(IdAd,Nome,nif_desconhecido,Morada).

% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicante(200,'Salvador Eletrecista LDA',222222220,'Rua José Alves')).
excecao(adjudicante(200,'Salvador Eletrecista LDA',333333330,'Rua André Gomes')).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado contrato #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Valor, Prazo, Local, Data -> {V,F,D}

% Conhecimento Perfeito Positivo
contrato(0,1,2,'Aquisição de serviços','Consulta prévia','Assessoria jurídica', 400, 50,'Alto de Basto',data(14,02,2020)).
contrato(1,2,1,'Aquisição de serviços','Consulta prévia','Assessoria jurídica', 500, 60,'Alto de Basto',data(15,02,2020)).
contrato(2,2,2,'Aquisição de serviços','Consulta prévia','Assessoria jurídica', 600, 70,'Alto de Basto',data(16,02,2020)).

% Conhecimento Perfeito Negativo
-contrato(3,1,2,'Locação de bens móveis', 'Ajuste direto','Arrendamento de espaço', 100, 30,'Cruzeiro de Felgueiras',data(2,02,2020)).

% Conhecimento Imperfeito Incerto
% Não se sabe o Adjudicante
contrato(4,adjudicante_desconhecido,1,'Locação de bens móveis', 'Ajuste direto','Arrendamento de espaço', 100, 30,'Cruzeiro de Felgueiras',data(2,02,2020)).
excecao(contrato(IdCont,_, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data)):-
    contrato(IdCont,adjudicante_desconhecido, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data).

% Conhecimento Imperfeito Incerto
% Não se sabe em que data se realizou
excecao(contrato(5,1,1,'Aquisição de serviços','Consulta prévia','Obras num pavilhão', 100, 100,'Alto de Basto',data(1,03,2020))).
excecao(contrato(5,1,1,'Aquisição de serviços','Consulta prévia','Obras num pavilhão', 100, 100,'Alto de Basto',data(2,03,2020))).

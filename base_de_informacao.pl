
%Nifs usáveis
%876543212
%211111112
%222222220
%233333339

:- multifile (-)/1.
:- dynamic adjudicante/5.
:- dynamic adjudicataria/5.
:- dynamic contrato/9.
:- dynamic excecao/1.
:- dynamic (-)/1.

:- discontiguous adjudicante/5.
:- discontiguous adjudicataria/5.
:- discontiguous contrato/10.
:- discontiguous (-)/1.
:- discontiguous excecao/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicante #IdAd, Nome, NIF,TipoEntidade, Morada ->{V,F,D}
% Conhecimento Perfeito Positivo
adjudicante(1,'Camara de Braga',123456789,'Organismo de Administração Pública','Praça do Município').
adjudicante(2,'Município de Alto de Basto','Organismo de Administração Pública',705330336, 'Portugal, Braga,Alto de Basto').


% Conhecimento Perfeito Negativo
-adjudicante(1000,'André Alves',111111110,'Pessoa Singular','Rua dos Barros Nº45').

% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicante(100,'Tasquinha Bracarense',nif_desconhecido,'Pessoa Coletiva','Rua Nova de Santa Cruz').
excecao(adjudicante(IdAd,Nome,_,TipoEntidade,Morada)):-
    adjudicante(IdAd,Nome,nif_desconhecido,TipoEntidade,Morada).


% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicante(200,'António Reparações LDA','Pessoa Coletiva',222222220,'Rua da Nascente')).
excecao(adjudicante(200,'António Reparações LDA','Pessoa Coletiva',333333330,'Rua da Nascente')).


% Conhecimento Imperfeito Interdito
% Interdito saber o nome do adjudicante
adjudicante(300,nome_interdito,876543212,'Pessoa Coletiva','Rua Nova').
excecao(adjudicante(IdAd,_,NIF,TipoEntidade,Morada)):-
    adjudicante(IdAd,nome_interdito,NIF,TipoEntidade,Morada).
nuloInterdito(nome_interdito).

/*+utente(Id,N,I,G,M) ::
    (solucoes((Id,N,I,G,M), (utente(15,'Tobias',50,'M',morada_impossivel), nao(nulointerdito(morada_impossivel))), R),
     comprimento(R,0)).*/

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicataria #IdAda, Nome, Nif, Morada -> {V,F,D}

% Conhecimento Perfeito Positivo
adjudicataria(1,'Universidade do minho',502011378, 'Pessoa Coletiva', 'Largo do Paço').
adjudicataria(2,'XXX -Associados -Sociedade de Advogados, SP, RL.',702675112, 'Pessoa Coletiva', 'Portugal').

% Conhecimento Perfeito Negativo
-adjudicataria(1000,'Rodrigo Guedes LDA',876543212, 'Pessoa Coletiva','Rua 25 de Abril').

% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicataria(100,'Bruno Batista',nif_desconhecido,'Pessoa Coletiva','Rua dos Barros Nº46').
excecao(adjudicataria(IdAd,Nome,_,TipoEntidade,Morada)):-
    adjudicataria(IdAd,Nome,nif_desconhecido,TipoEntidade,Morada).

% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicataria(200,'Salvador Eletrecista LDA',222222220, 'Pessoa Coletiva','Rua José Alves')).
excecao(adjudicataria(200,'Salvador Eletrecista LDA',333333330,'Pessoa Coletiva','Rua André Gomes')).






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
excecao(contrato(_,adjudicante_desconhecido, _, _, _, _, _, _, _, _)).

% Conhecimento Imperfeito Impreciso
% Não se sabe em que data se realizou
excecao(contrato(5,1,1,'Aquisição de serviços','Consulta prévia','Obras num pavilhão', 100, 100,'Alto de Basto',data(1,03,2020))).
excecao(contrato(5,1,1,'Aquisição de serviços','Consulta prévia','Obras num pavilhão', 100, 100,'Alto de Basto',data(2,03,2020))).

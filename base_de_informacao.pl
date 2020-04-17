
%Nifs usáveis
%876543212


%pessoa singulas
%211111112
%222222220
%233333339

% Pessoa coletiva
%512345678
%543212343
%598765433


%
%organiusmo de administração publica
%644444444
%655555552

:- multifile (-)/1.
:- dynamic adjudicante/5.
:- dynamic adjudicataria/5.
:- dynamic contrato/10.
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
adjudicante(1,'Camara de Braga',622222228,'Organismo de administração pública','Praça do Município').
adjudicante(2,'Município de Alto de Basto',600000001,'Organismo de administração pública', 'Portugal, Braga,Alto de Basto').


% Conhecimento Perfeito Negativo
-adjudicante(1000,'André Alves',111111110,'Pessoa singular','Rua dos Barros Nº45').

% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicante(100,'Tasquinha Bracarense',nif_desconhecido,'Pessoa coletiva','Rua Nova de Santa Cruz').
excecao(adjudicante(IdAd,Nome,_,TipoEntidade,Morada)):-
    adjudicante(IdAd,Nome,nif_desconhecido,TipoEntidade,Morada).


% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicante(200,'António Reparações LDA',500000000,'Pessoa coletiva','Rua da Nascente')).
excecao(adjudicante(200,'António Reparações LDA',500000000,'Pessoa coletiva',,'Rua da Nascente')).


% Conhecimento Imperfeito Interdito
% Interdito saber o nome do adjudicante
adjudicante(300,nome_interdito,511111118,'Pessoa coletiva','Rua Nova').
excecao(adjudicante(IdAd,_,NIF,TipoEntidade,Morada)):-
    adjudicante(IdAd,nome_interdito,NIF,TipoEntidade,Morada).
nulo(nome_interdito).

/*+utente(Id,N,I,G,M) ::
    (solucoes((Id,N,I,G,M), (utente(15,'Tobias',50,'M',morada_impossivel), nao(nulointerdito(morada_impossivel))), R),
     comprimento(R,0)).*/

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicataria #IdAda, Nome, Nif, Morada -> {V,F,D}

% Conhecimento Perfeito Positivo
adjudicataria(1,'Universidade do minho',633333336, 'Pessoa coletiva', 'Largo do Paço').
adjudicataria(2,'XXX -Associados -Sociedade de Advogados, SPa, RL.',512345678, 'Pessoa coletiva', 'Portugal').
adjudicataria(3,'Camara de Braga',622222228,'Organismo de administração pública','Praça do Município').

% Conhecimento Perfeito Negativo
-adjudicataria(1000,'Rodrigo Guedes LDA',876543212, 'Pessoa coletiva','Rua 25 de Abril').

% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicataria(100,'Bruno Batista',nif_desconhecido,'Pessoa coletiva','Rua dos Barros Nº46').
excecao(adjudicataria(IdAd,Nome,_,TipoEntidade,Morada)):-
    adjudicataria(IdAd,Nome,nif_desconhecido,TipoEntidade,Morada).

% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicataria(200,'Salvador Eletrecista LDA',222222220, 'Pessoa coletiva','Rua José Alves')).
excecao(adjudicataria(200,'Salvador Eletrecista LDA',333333330,'Pessoa coletiva','Rua André Gomes')).






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

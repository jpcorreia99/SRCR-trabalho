%Nifs usáveis
%876543212


%pessoa singular
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

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- multifile (-)/1.
:- dynamic adjudicante/5.
:- dynamic adjudicataria/5.
:- dynamic contrato/11.
:- dynamic excecao/1.
:- dynamic (-)/1.
:- dynamic excecao/1.
:- dynamic perfeito/1.
:- dynamic incerto/1.
:- dynamic impreciso/1.
:- dynamic interdito/1.
:- dynamic incertoMorada/2.

:- discontiguous adjudicante/5.
:- discontiguous adjudicataria/5.
:- discontiguous contrato/11.
:- discontiguous (-)/1.

:- discontiguous excecao/1.
:- discontiguous perfeito/1.
:- discontiguous incerto/1.
:- discontiguous impreciso/1.
:- discontiguous interdito/1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicante #IdAd, Nome, NIF,TipoEntidade, Morada ->{V,F,D}
% Conhecimento Perfeito Positivo
adjudicante(1,'Município do Porto',501306099 ,'Pessoa coletiva','R. Clube dos Fenianos 5, 4000-407 Porto').
perfeito(adjudicante(1)).
adjudicante(2,'SERVICOS DE ACAO SOCIAL DA UNIVERSIDADE DO MINHO',680047360,'Pessoa coletiva','Largo do Paco, 4704-553 Braga').
perfeito(adjudicante(2)).
adjudicante(3,'Município de Braga',506901173,'Organismo de administração pública','Praça do Município  4700-435 Braga').
perfeito(adjudicante(3)).
adjudicante(4,'Ola John',211111112,'Pessoa singular','Av. São Gonçalo 1028, Guimarães').
perfeito(adjudicante(4)).
% adjudicante(5,'XXX -Associados -Sociedade de Advogados, SPa, RL.',512345678, 'Pessoa coletiva', 'Portugal').

% Conhecimento Perfeito Negativo
-adjudicante(1000,'André Alves',111111110,'Pessoa singular','Rua dos Barros Nº45').
perfeito(adjudicante(1000)).
% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicante(100,'Tasquinha Bracarense',nif_desconhecido,'Pessoa coletiva','Rua Nova de Santa Cruz').
excecao(adjudicante(IdAd,Nome,_,TipoEntidade,Morada)):-
    adjudicante(IdAd,Nome,nif_desconhecido,TipoEntidade,Morada).
incerto(adjudicante(100)).


% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicante(200,'António Reparações LDA',500000000,'Pessoa coletiva','Rua do Crescente')).
excecao(adjudicante(200,'António Reparações LDA',500000000,'Pessoa coletiva','Rua da Nascente')).
impreciso(adjudicante(200)).
% Conhecimento Imperfeito Interdito
% Interdito saber o nome do adjudicante
adjudicante(300,nome_interdito,511111118,'Pessoa coletiva','Rua Nova').
excecao(adjudicante(IdAd,_,NIF,TipoEntidade,Morada)):-
    adjudicante(IdAd,nome_interdito,NIF,TipoEntidade,Morada).
nulo(nome_interdito).
interdito(adjudicante(300)).
/*+utente(Id,N,I,G,M) ::
    (solucoes((Id,N,I,G,M), (utente(15,'Tobias',50,'M',morada_impossivel), nao(nulointerdito(morada_impossivel))), R),
     comprimento(R,0)).*/

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicataria #IdAda, Nome, Nif, Morada -> {V,F,D}

% Conhecimento Perfeito Positivo
adjudicataria(1,'Município do Porto',501306099 ,'Pessoa coletiva', 'R. Clube dos Fenianos 5, 4000-407 Porto').
perfeito(adjudicataria(1)).
adjudicataria(2,'SERVICOS DE ACAO SOCIAL DA UNIVERSIDADE DO MINHO',680047360 , 'Pessoa coletiva', 'Largo do Paco, 4704-553 Braga').
perfeito(adjudicataria(2)).
adjudicataria(3,'Município de Braga',506901173,'Organismo de administração pública','Praça do Município,  4700-435 Braga').
perfeito(adjudicataria(3)).
%adjudicataria(4,'Município de Lisboa',500051070,'Pessoa coletiva','Paços do Concelho 1100-038 Lisboa').

% Conhecimento Perfeito Negativo
-adjudicataria(1000,'Rodrigo Guedes LDA',876543212, 'Pessoa coletiva','Rua 25 de Abril').
perfeito(adjudicataria(1000)).

% Conhecimento Imperfeito Incerto
% Não se sabe o NIF
adjudicataria(100,'Bruno Batista',nif_desconhecido,'Pessoa coletiva','Rua dos Barros Nº46').
excecao(adjudicataria(IdAd,Nome,_,TipoEntidade,Morada)):-
    adjudicataria(IdAd,Nome,nif_desconhecido,TipoEntidade,Morada).
incerto(adjudicataria(100)).

% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicataria(200,'Salvador Eletrecista LDA',222222220, 'Pessoa coletiva','Rua José Alves')).
excecao(adjudicataria(200,'Salvador Eletrecista LDA',333333330,'Pessoa coletiva','Rua André Gomes')).
impreciso(adjudicataria(200)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado contrato #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Valor, Prazo, Local, Data -> {V,F,D}

% Conhecimento Perfeito Positivo
contrato(0,1,2,'Aquisição de serviços','Consulta prévia','Assessoria jurídica', 400, 50,'Alto de Basto',data(14,02,2020),false).
perfeito(contrato(0)).
contrato(1,2,1,'Aquisição de serviços','Consulta prévia','Assessoria jurídica', 500, 60,'Alto de Basto',data(15,02,2020),false).
perfeito(contrato(1)).
contrato(2,2,2,'Aquisição de serviços','Consulta prévia','Assessoria jurídica', 600, 70,'Alto de Basto',data(16,02,2020),false).
perfeito(contrato(2)).
contrato(3,1,3,'Aquisição de serviços','Consulta prévia','Assessoria jurídica', 700, 80,'Alto de Basto',data(17,02,2020),false).
perfeito(contrato(3)).

% Conhecimento Perfeito Negativo
-contrato(4,1,2,'Locação de bens móveis', 'Ajuste direto','Arrendamento de espaço', 100, 30,'Cruzeiro de Felgueiras',data(2,02,2020),false).
perfeito(contrato(4)).

% Conhecimento Imperfeito Incerto
% Não se sabe o Adjudicante
contrato(5,adjudicante_desconhecido,1,'Locação de bens móveis', 'Ajuste direto','Arrendamento de espaço', 100, 30,'Cruzeiro de Felgueiras',data(2,02,2020),false).
excecao(contrato(_,adjudicante_desconhecido, _, _, _, _, _, _, _, _, _)).
incerto(contrato(5)).

% Conhecimento Imperfeito Impreciso
% Não se sabe em que data se realizou
excecao(contrato(6,1,1,'Aquisição de serviços','Consulta prévia','Obras num pavilhão', 100, 100,'Alto de Basto',data(1,03,2020),false)).
excecao(contrato(6,1,1,'Aquisição de serviços','Consulta prévia','Obras num pavilhão', 100, 100,'Alto de Basto',data(2,03,2020),false)).
impreciso(contrato(6)).



% ||||||||||||||||||||||||||||||||| TO DO |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
% ADICIONAR MORADA IMPRECISA
% TRATAR INVOLUÇÃO
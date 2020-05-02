%Nifs usáveis
%876543212


%pessoa singular
%211111112
%222222220
%233333339

% Pessoa coletiva
%512345678
%543212343


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
:- dynamic impreciso/1.
:- dynamic interdito/1.
:- dynamic incertoMorada/2.
:- dynamic incertoDescricao/2.
:- dynamic nulo/1.

:- discontiguous adjudicante/5.
:- discontiguous adjudicataria/5.
:- discontiguous contrato/11.
:- discontiguous (-)/1.

:- discontiguous excecao/1.
:- discontiguous perfeito/1.
:- discontiguous incertoMorada/2.
:- discontiguous impreciso/1.
:- discontiguous interdito/1.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicante #IdAda, Nome, Nif, Morada -> {V,F,D}

% Conhecimento Perfeito Positivo
adjudicante(1,'Municipio do Porto',501306099 ,'Pessoa coletiva', 'R. Clube dos Fenianos 5, 4000-407 Porto').
perfeito(adjudicante(1)).
adjudicante(2,'SERVICOS DE ACAO SOCIAL DA UNIVERSIDADE DO MINHO',680047360 , 'Organismo de administração pública', 'Largo do Paco, 4704-553 Braga').
perfeito(adjudicante(2)).
adjudicante(3,'Municipio de Braga',506901173,'Pessoa coletiva','Praça do Município,  4700-435 Braga').
perfeito(adjudicante(3)).
adjudicante(4,'Municipio de Lisboa',500051070,'Pessoa coletiva','Paços do Concelho 1100-038 Lisboa').
perfeito(adjudicante(4)).
adjudicante(5,'Municipio de Viana do Castelo',612332330, 'Organismo de administracao publica', 'Travessa das lavradeiras').
perfeito(adjudicante(5)).
adjudicante(6, 'Construcoes Cunha',563646322 , 'Pessoa coletiva', 'Avenida do Castelo, Viana do Castelo').
perfeito(adjudicante(6)).
adjudicante(7, 'Adegas Trincao',563646322 , 'Pessoa coletiva', 'Avenida 25 de Abril, Encosta do Elevador 12, Viana do Castelo').
perfeito(adjudicante(7)).
adjudicante(8, 'MELIT servicos de informatica', 567454320, 'Pessoa coletiva', 'Encosta de Neiva, Guimaraes').
perfeito(adjudicante(8)).
adjudicante(9, 'Pneus Neiva', 522989934, 'Pessoa coletiva', 'Avenida do Mar, Braga').
perfeito(adjudicante(9)).
adjudicante(10, 'Metamorfosis', 992334330, 'Sociedade civil', 'Rua das Amoreiras, Porto').
perfeito(adjudicante(10)).

% Conhecimento Perfeito Negativo
-adjudicante(1000,'Rodrigo Guedes LDA',876543212, 'Pessoa coletiva','Rua 25 de Abril').
perfeito(adjudicante(1000)).
-adjudicante(1001, 'Alvaro Almeida e irmaos LDA', 838223232, 'Pessoa coletiva', 'Travessa das Neves').
perfeito(adjudicante(1000)).
-adjudicante(1002, 'Municipio de Guimaraes', 855454345, 'Organismo de administracao publica', 'Paços do concelho, Guimaraes').
perfeito(adjudicante(1002)).

% Conhecimento Imperfeito Incerto
% Não se sabe a morada
adjudicante(100,'Bruno Batista',511233450,'Pessoa coletiva',morada_desconhecida).
excecao(adjudicante(IdAd,Nome,NIF,TipoEntidade,_)):-
    adjudicante(IdAd,Nome,NIF,TipoEntidade,morada_desconhecida).
incertoMorada(adjudicante(100),_).
adjudicante(101,'Bruno Carvalho',511222343,'Pessoa coletiva',morada_desconhecida).
excecao(adjudicante(IdAd,Nome,NIF,TipoEntidade,_)):-
    adjudicante(IdAd,Nome,NIF,TipoEntidade,morada_desconhecida).
incertoMorada(adjudicante(101),_).

% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
%excecao(adjudicante(200,'Salvador Eletrecista LDA',598765433, 'Pessoa coletiva','Rua Jose Alves')).
%excecao(adjudicante(200,'Salvador Eletrecista LDA',598765433,'Pessoa coletiva','Rua Andre Gomes')).
%impreciso(adjudicante(200)).
excecao(adjudicante(201,'Cadeiras Guerra',593245679, 'Pessoa coletiva','Rua Alvaro de Melo')).
excecao(adjudicante(201,'Cadeiras Guerra',593245679,'Pessoa coletiva','Rua do Sol')).
impreciso(adjudicante(201)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado adjudicante #IdAd, Nome, NIF,TipoEntidade, Morada ->{V,F,D}
% Conhecimento Perfeito Positivo
adjudicataria(1,'Municipio do Porto',501306099 ,'Pessoa coletiva','R. Clube dos Fenianos 5, 4000-407 Porto').
perfeito(adjudicataria(1)).
adjudicataria(2,'SERVICOS DE ACAO SOCIAL DA UNIVERSIDADE DO MINHO',580047360,'Pessoa coletiva','Largo do Paco, 4704-553 Braga').
perfeito(adjudicataria(2)).
adjudicataria(3,'Municipio de Braga',606901175,'Organismo de administracao publica','Praça do Municipio  4700-435 Braga').
perfeito(adjudicataria(3)).
adjudicataria(4,'Ola John',211111112,'Pessoa singular','Av. Sao Goncalo 1028, Guimaraes').
perfeito(adjudicataria(4)).
adjudicataria(5, 'Pedro Parente', 266334598, 'Pessoa singular', 'Avenida 25 de Abril, Encosta do Elevador 6, Viana do Castelo').
perfeito(adjudicataria(5)).
adjudicataria(6, 'Condominios Vida Nova', 912355476, 'Condominio', 'Avenida Primeiro de Maio, Lisboa').
perfeito(adjudicataria(6)).
adjudicataria(7, 'Municipio de Faro', 622753436, 'Organismo de administracao publica', 'Avenida das Alamedas, Faro').
perfeito(adjudicataria(7)).
adjudicataria(8, 'Vitor Alves', 233456880, 'Pessoa singular', 'Rua de Barros, Braga').
perfeito(adjudicataria(8)).
adjudicataria(9, 'Joaquim Silva', 221334297, 'Pessoa singular', 'Rua de Magos, Tondela').
perfeito(adjudicataria(9)).
adjudicataria(10, 'Artesiz', 723334560, 'Fundo de investimento', 'Travessa dos Reis, Paços de Ferreira').
perfeito(adjudicataria(10)).

% Conhecimento Perfeito Negativo
-adjudicataria(1000,'Andre Alves',111111110,'Pessoa singular','Rua dos Barros Nº45').
perfeito(adjudicante(1000)).
-adjudicataria(1001,'Pedro Almeida',11311110,'Pessoa singular','Alameda do Norte').
perfeito(adjudicante(1001)).
-adjudicataria(1002,'Andre Pereira',322112233,'Pessoa singular','Via dos trabalhadores nº 6').
perfeito(adjudicante(1002)).

% Conhecimento Imperfeito Incerto
% Não se sabe a morada
adjudicataria(100,'Tasquinha Bracarense',568744322,'Pessoa coletiva',morada_desconhecida).
excecao(adjudicataria(IdAda,Nome,NIF,TipoEntidade,_)):-
    adjudicataria(IdAda,Nome,NIF,TipoEntidade,morada_desconhecida).
incertoMorada(adjudicataria(100),_).
adjudicataria(101,'Alberto Sá',568744365,'Pessoa singular',morada_desconhecida).
excecao(adjudicataria(IdAda,Nome,NIF,TipoEntidade,_)):-
    adjudicataria(IdAda,Nome,NIF,TipoEntidade,morada_desconhecida).
incertoMorada(adjudicataria(101),_).

% Conhecimento Imperfeito Impreciso
% Não se sabe qual das moradas é
excecao(adjudicataria(200,'Antonio Reparações LDA',500000000,'Pessoa coletiva','Rua do Crescente')).
excecao(adjudicataria(200,'Antonio Reparações LDA',500000000,'Pessoa coletiva','Rua da Nascente')).
impreciso(adjudicataria(200)).
excecao(adjudicataria(201,'Padaria Armando',500123454,'Pessoa coletiva','Rua de Barros')).
excecao(adjudicataria(201,'Padaria Armando',500123454,'Pessoa coletiva','Rua do Fontelo')).
impreciso(adjudicataria(201)).

% Conhecimento Imperfeito Interdito
% Interdito saber o nome do adjudicante
adjudicataria(300,nome_interdito,511111118,'Pessoa coletiva','Rua Nova').
excecao(adjudicataria(IdAd,_,NIF,TipoEntidade,Morada)):-
    adjudicataria(IdAd,nome_interdito,NIF,TipoEntidade,Morada).
nulo(nome_interdito).
interdito(adjudicataria(300)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado contrato #IdContrato, #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Valor, Prazo, Local, Data -> {V,F,D}

% Conhecimento Perfeito Positivo
contrato(0,1,2,'Aquisicão de serviços','Consulta previa','Concerto', 400, 50,'Alto de Basto',data(14,02,2020),false).
perfeito(contrato(0)).
contrato(1,2,1,'Aquisicão de serviços','Dialogo concorrencial','Pintura de espaço' ,500, 60,'Alto de Basto',data(15,02,2020),false).
perfeito(contrato(1)).
contrato(2,2,2,'Aquisição de serviços','Nani','Assessoria juridica',600, 70,'Alto de Basto',data(16,02,2020),false).
perfeito(contrato(2)).
contrato(3,1,3,'Aquisição de serviços','Consulta previa','Assessoria juridica', 'Concerto',700, 80,'Alto de Basto',data(17,02,2020),false).
perfeito(contrato(3)).
contrato(4,1,3,'Aquisicao de serviços','Procedimento de negociacao','Concerto', 800, 90,'Alto de Basto',data(18,02,2020),false).
perfeito(contrato(4)).
contrato(5,7,5,'Aquisicao de servicos', 'Consulta previa', 'Acabamentos', 1000, 100, 'Adega Trincao', data(12,05,2020),false).
perfeito(contrato(5)).
contrato(6,6,1,'Aquisicao de bens moveis', 'Parceria para a inovacao', 'Concerto',500, 90, 'Porto', data(11,04,2020),false).
perfeito(contrato(6)).
contrato(7,9,9,'Aquisicao de servicos', 'Consulta previa', 'Concerto',600, 100, 'Braga', data(12,12,2019),false).
perfeito(contrato(7)).
contrato(8,9,4,'Aquisicao de servicos', 'Consulta previa', 'Acabamentos', 600, 100, 'Braga', data(12,12,2019),false).
perfeito(contrato(8)).
contrato(9,10,8,'Aquisicao de servicos', 'Dialogo concorrencial', 'Conversacao', 800, 100, 'Braga', data(02,11,2019),false).
perfeito(contrato(9)).
contrato(10,9,4,'Aquisicao de servicos', 'Consulta previa', 'Acabamentos', 600, 100, 'Braga', data(12,01,2019),false).
perfeito(contrato(10)).

% Conhecimento Perfeito Negativo
-contrato(1000,1,2,'Locacao de bens moveis', 'Ajuste direto','Arrendamento de espaço', 100, 30,'Cruzeiro de Felgueiras',data(2,02,2020),false).
perfeito(contrato(1000)).
-contrato(1001,1,3,'Locacao de bens moveis', 'Ajuste direto','Arrendamento de espaço', 200, 40,'Avenida das flores',data(4,05,2020),false).
perfeito(contrato(1001)).

% Conhecimento Imperfeito Incerto
% Não se sabe a Descrição
contrato(100,1,1,'Locacao de bens moveis', 'Ajuste direto',desc, 100, 30,'Cruzeiro de Felgueiras',data(2,02,2020),false).
excecao(contrato(_, _, _, _, _, desc, _, _, _, _, _)).
incertoDescricao(contrato(100),desc).

% Conhecimento Imperfeito Impreciso
% Não se sabe em que data se realizou
excecao(contrato(101,1,1,'Aquisicao de servicos','Consulta previa','Obras num pavilhao', 100, 100,'Alto de Basto',data(1,03,2020),false)).
excecao(contrato(101,1,1,'Aquisicao de servicos','Consulta previa','Obras num pavilhao', 100, 100,'Alto de Basto',data(2,03,2020),false)).
impreciso(contrato(101)).

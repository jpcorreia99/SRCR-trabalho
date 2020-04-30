:- use_module(library(lists)).

% Predicados auxiliares
%
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado ultimo_digito_valido: Nif ->{V,F}
% Validação do último dígito através da técnica módulo 11
ultimo_digito_valido(Nif):-
    Primeiros_oito_digitos is div(Nif,10),
    Ultimo_digito is mod(Nif,10),
    digito_de_controlo(Primeiros_oito_digitos,Digito),
    Ultimo_digito =:= Digito.

% Extensão do predicado que obtém o digito de controlo a partir dos primeiros 8 dígitos através da técnica módulo 11
digito_de_controlo(Primeiros_oito_digitos,Digito_de_controlo):-
    multiplica_lista(Primeiros_oito_digitos,Ultimo_digito),
    (0 is mod(Ultimo_digito,11); 1 is mod(Ultimo_digito,11)),
    Digito_de_controlo is 0.

digito_de_controlo(Primeiros_oito_digitos,Digito_de_controlo):-
    multiplica_lista(Primeiros_oito_digitos,Ultimo_digito),
    Mod_Result is mod(Ultimo_digito,11),
    Digito_de_controlo is 11-Mod_Result.


% Extensão do predicado que, dado um nif, o transforma numa lista de dígitos e obtém o dígito de validação
multiplica_lista(Nif,Ultimo_digito):-
    nif_para_lista(Nif,Lista),
    multiplicacao_decrescente(Lista,Ultimo_digito,9).


% Extensão do predicado que converte uma lista de códigos ascii na lista de dígitos que representam.
nif_para_lista(Nif,L):-
    number_codes(Nif,X),
    maplist(ascii_to_digit,X, L).

% Extensão do predicado que converte um dígito código ascii para o número correspondente ascii_to_digit
ascii_to_digit(Ascii_number,R) :- R is Ascii_number-48.


% Extensão do predicado que multiplica cada elemento da lista por um número que vai decrescendo e soma todos os valores resultantes
multiplicacao_decrescente([X],R,Num):- R is X*Num.
multiplicacao_decrescente([H|T],R,Num):-
    multiplicacao_decrescente(T,Res,Num-1),
    R is Res + (H*Num).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado somaCustosContratos: Lista, DataDeReferencia,R -> {V,F}
%Devolve o somatório dos valores dos contratos nos últimos dois anos económicos mais o atual
%somaCustosContratos([(data(1,1,2010),500),(data(2,2,2020),300),(data(3,3,2011),600)],2012,R).
somaCustosContratos([],_,0).

somaCustosContratos([(data(_,_,Ano),Custo)|T],AnoDeReferencia,R):-
    X = AnoDeReferencia-2,
    Ano >= (X), Ano =< AnoDeReferencia,
    somaCustosContratos(T,AnoDeReferencia,R1),
    R is (Custo + R1).

somaCustosContratos([(data(_,_,Ano),_)|T],AnoDeReferencia,R):-
    X = AnoDeReferencia-2,
    Ano < X, Ano > AnoDeReferencia,
    somaCustosContratos(T,AnoDeReferencia,R1),
    R is R1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado NifCorrespondeTipoEntidadeAdjudicante: NifLista, TipoEntidade -> {V,F}
% Apenas permite que certos tipos de entidades sejam entidadade adjudicantes 
% artigo 2.º n.º 2, alíneas a), b) e d).
% e artigo 7.º n.º 1.º

nifCorrespondeTipoEntidadeAdjudicante([5|_],'Pessoa coletiva').

nifCorrespondeTipoEntidadeAdjudicante([6|_],'Organismo de administração pública').

nifCorrespondeTipoEntidadeAdjudicante([9,9|_],'Sociedade civil').


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado NifCorrespondeTipoEntidadeAdjudicatária: NifLista, TipoEntidade -> {V,F}
% Um conjunto maior de entidades poderão ser entidades adjudicatária

nifCorrespondeTipoEntidadeAdjudicataria([Primeiro_digito|_],'Pessoa singular'):- 
    Primeiro_digito>= 1, 
    Primeiro_digito =< 3.

nifCorrespondeTipoEntidadeAdjudicataria([5|_],'Pessoa coletiva').

nifCorrespondeTipoEntidadeAdjudicataria([6|_],'Organismo de administração pública').

nifCorrespondeTipoEntidadeAdjudicataria([7,2|_],'Fundo de investimento').

nifCorrespondeTipoEntidadeAdjudicataria([7,7|_],'Sujeito passivo').

nifCorrespondeTipoEntidadeAdjudicataria([Primeiro_digito,Segundo_digito|_],'Condomínio'):-
    Primeiro_digito == 9 , 
    (Segundo_digito ==0 ; Segundo_digito == 1).

nifCorrespondeTipoEntidadeAdjudicataria([9,9|_],'Sociedade civil').



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado data: Dia,Mes,Ano -> {V,F}
% Caso especial de Fevereiro em ano não bissexto
data(Dia,2,Ano):-
    Dia >= 1,
    Dia =<29,
    0 =:= mod(Ano,4). % ano bissexto

data(Dia,2,_):-
    Dia >= 1,
    Dia =< 28. % ano não bissexto

data(Dia,Mes,_):-
    member(Mes,[4,6,9,11]), % meses de 30 dias
    Dia >=1,
    Dia =<30.


data(Dia,Mes,_):-
    member(Mes,[1,3,5,7,8,10,12]), %meses de 31 dias
    Dia >=1,
    Dia =<31.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado data_valida: Dia,Mes,Ano -> {V,F}
data_valida(data(Dia,Mes,Ano)):- data(Dia,Mes,Ano).


%% Predicados acerca de procesamento de diferença de datas

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado diferenca_entre_datas: Data1,Data2,R -> {V,F}
% Devolve a diferença enre duas datas em dias
diferenca_entre_datas(Data1,Data2,R):-
    numero_dias(Data1,NumeroDiasData1),
    numero_dias(Data2,NumeroDiasData2),
    R is NumeroDiasData2-NumeroDiasData1.


% Coloca em R o número de dias Desde o Ano 0 até à data data
numero_dias(data(Dia,Mes,Ano),R):-
    AnoEDia is (Ano*365 + Dia),
    MesAnterior is (Mes-1), % pois os dias deste mês já estão contados
    soma_meses(MesAnterior,SomaMeses),
    contaAnosBissextos(data(Dia,Mes,Ano),NumeroDiasBissextos),
    R is (AnoEDia+SomaMeses+NumeroDiasBissextos).


%Devolve o nº de dias que já passaram no dado ano, a partir do mês atual
soma_meses(0,0).
soma_meses(Mes,R):-
    Meses = [31,28,31,30,31,30,31,31,30,31,30,31],
    Indice is (Mes - 1), % os meses e o indice do array estão desfasados de 1 devido à natureza de indexação dos arrays
    (nth0(Indice,Meses,NumeroDeDias)),
    soma_meses(Indice,SomaMesesAnteriores), % aproveita-se que Indice já é MEs -1
    R is (SomaMesesAnteriores + NumeroDeDias).


% Predicado que conta o número de anos bissextos desde o ano 0 até ao ano dado
contaAnosBissextos(data(_,Mes,Ano),R):-
    Mes =< 2,
    NovoAno is Ano-1,
    R is (NovoAno//4 - NovoAno//100 + NovoAno//400).

contaAnosBissextos(data(_,Mes,Ano),R):-
    Mes > 2,
    R is (Ano//4 - Ano//100 + Ano//400).




% Predicado auxiliar do predicado listaContratosAtivosAdjudicante no ficheiro amin.pl
% Dada uma lista de triplos, filtra os triplos em que o contrato ainda está ativo na data de referência 
%e cria uma lista de ids de contratos que satisfazem essa condição

filtraListaTriplosIdPrazoDataAtivo([],_,[]).

filtraListaTriplosIdPrazoDataAtivo([(IdContrato,PrazoContrato,DataContrato)|T] ,DataReferencia,R):-
    diferenca_entre_datas(DataContrato,DataReferencia,DiferencaEmDias),
    DiferencaEmDias >= 0,
    DiferencaEmDias =< PrazoContrato,
    filtraListaTriplosIdPrazoDataAtivo(T,DataReferencia,R1),
    prepend(IdContrato,R1,R).

filtraListaTriplosIdPrazoDataAtivo([_|T],DataReferencia,R):-
    filtraListaTriplosIdPrazoDataAtivo(T,DataReferencia,R).



% Predicado auxiliar do predicado listaContratosAcabadosAdjudicante no ficheiro amin.pl
% Dada uma lista de triplos, filtra os triplos em que o contrato ainda está ativo na data de referência 
%e cria uma lista de ids de contratos que satisfazem essa condição

filtraListaTriplosIdPrazoDataAcabado([],_,[]).

filtraListaTriplosIdPrazoDataAcabado([(IdContrato,PrazoContrato,DataContrato)|T] ,DataReferencia,R):-
    diferenca_entre_datas(DataContrato,DataReferencia,DiferencaEmDias),
    DiferencaEmDias >= 0,
    DiferencaEmDias >= PrazoContrato,
    filtraListaTriplosIdPrazoDataAcabado(T,DataReferencia,R1),
    prepend(IdContrato,R1,R).

filtraListaTriplosIdPrazoDataAcabado([_|T],DataReferencia,R):-
    filtraListaTriplosIdPrazoDataAcabado(T,DataReferencia,R).





%predicado que coloca um elemento na cabeça da lista
prepend(Head, Tail, [Head| Tail]).





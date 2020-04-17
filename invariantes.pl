:- op(900, xfy, '::').
:- style_check(-singleton).
% Invariantes sobre qualquer termo
%
%Invariante estrutural: Não permitir  a inserção de conhecimento contraditório
+Termo :: (
    nao(-Termo)
).

%Invariante estrutural: Não permitir  a inserção de conhecimento contraditório
+(-Termo) :: (
    nao(Termo)
).

% Invariante estrutural: Não permitir excecoes repetidas
+(excecao(Termo)) :: (
    solucoes(Termo, excecao(Termo), R),
    comprimento(R, 1)
).


% Invariantes sobre entidades adjudicantes
%
% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
% Aplicado a conhecimento perfeito positivo
+adjudicante(IdAd,Nome,Nif,TipoEntidado,Morada) :: (
    integer(IdAd),
    atom(Nome),
    integer(Nif),
    atom(TipoEntidado),
    atom(Morada)
).

% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
% Aplicado a conhecimento perfeito negativo
+(-adjudicante(IdAd,Nome,Nif,TipoEntidado,Morada)) :: (
    integer(IdAd),
    atom(Nome),
    integer(Nif),
    atom(TipoEntidado),
    atom(Morada)
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: não permitir a entrada repetida de conhecimento, no campo do Id, nome e Nif
% Aplicado a conhecimento perfeito positivo
+adjudicante(IdAd,Nome,Nif,_,_) :: (
    solucoes( (IdAd),(adjudicante( IdAd,_,_,_,_)),S1 ),
    solucoes( (Nome),(adjudicante( _,Nome,_,_,_)),S2 ),
    solucoes( (Nif),(adjudicante( _,_,Nif,_,_)),S3 ),
    comprimento( S1,N1 ),
    comprimento( S2,N2 ),
    comprimento( S3,N3 ),
    Total is N1 + N2 + N3,
    Total == 3
).

% Invariante estrutural: não permitir a entrada repetida de conhecimento, no campo do Id
%  Aplicado a conhecimento perfeito negativo
+(-adjudicante(IdAd,_,_,_)) :: (
    solucoes( (IdAd),(-adjudicante( IdAd,_,_,_)),S1 ),
    comprimento(S1,R1),
    R1==1
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o dígito de controlo do id deve seguir a convenção de validação 'módulo 11'
% Aplicado a conhecimento perfeito positivo
+adjudicante(_,_,Nif,_,_) :: (
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).

% Aplicado a conhecimento perfeito negativo
+(-adjudicante(_,_,Nif,_,_)) :: (
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: Os digitos iniciais do nif devem corresponder ao tipo de entidade correta
% Aplicado a conhecimento perfeito positivo
+adjudicante(_,_,Nif,TipoEntidade,_) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidade(NifLista,TipoEntidade)
).

% Aplicado a conhecimento perfeito positivo
+(-adjudicante(_,_,Nif,TipoEntidade,_)) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidade(NifLista,TipoEntidade)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Impede a inserção de um nif associado a um nome se esse nif estiver associado a outro nome no predicado adjudicataria
+adjudicante(_,Nome,Nif,_,_) :: (
    solucoes((Nomes),adjudicataria(_,Nomes,Nif,_,_),S1),
    comprimento(S1,N1),
    write(S1),
    (N1 == 0; % Se o comprimento for 0 indica que o nif não está registado para nenhuma entidade adjudicataria
    (nth0(0, S1,NomeAdjudicataria),  % Se o comprimento não for 0, sua-se o predicado nth para aceder ao elemento da lista pois o predicado soluções só devolve listas
    write(NomeAdjudicataria),
    write(Nome),
    NomeAdjudicataria == Nome   % e verifica-se que se existir um nome de adjudicante associado ao nif tem de ser o mesmo nome do nif que estamos a inserir
    )
    )
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante que impede a inserção de conhecimento perfeito positivo relativo
% a um adjudicante com nome interdito
+adjudicante(IdAd,_,Ni,T,M) :: (
    solucoes((IdAd,Nome_interdito,Ni,T,M), 
    (adjudicante(IdAd,Nome_interdito,Ni,T,M), nulo(Nome_interdito)), R),
    comprimento(R,0)
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Não deve ser possivel remover uma entidade adjudicante se esta estiver presente num contrato
-adjudicante(IdAd,_,_,_,_) :: (
    solucoes(IdAd, contrato(_,IdAd,_,_,_,_,_,_,_,_), S),
    comprimento( S,N ),
    N == 0
).





%Invariantes sobre entidades adjudicatárias
%

% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
% Aplicado a conhecimento perfeito positivo
+adjudicataria(IdAda,Nome,Nif,TipoEntidade,Morada) :: (
    integer(IdAda),
    atom(Nome),
    integer(Nif),
    atom(TipoEntidade),
    atom(Morada)
).

% Aplicado a conhecimento perfeito negativo
+(-adjudicataria(IdAda,Nome,Nif,TipoEntidade,Morada)) :: (
    integer(IdAda),
    atom(Nome),
    integer(Nif),
    atom(TipoEntidade),
    atom(Morada)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: não permitir a entrada repetida de conhecimento, no campo do Id, nome e Nif
% Aplicado a conhecimento perfeito positivo
+adjudicataria(IdAda,Nome,Nif,_,_) :: (
    solucoes( (IdAda),(adjudicataria(IdAda,_,_,_,_)),S1 ),
    solucoes( (Nome),(adjudicataria( _,Nome,_,_,_)),S2 ),
    solucoes( (Nif),(adjudicataria( _,_,Nif,_,_)),S3 ),
    comprimento( S1,N1 ),
    comprimento( S2,N2 ),
    comprimento( S3,N3 ),
    Total is N1 + N2 + N3,
    Total == 3
).

% Invariante estrutural: não permitir a entrada repetida de conhecimento, no campo do Id
%  Aplicado a conhecimento perfeito negativo
+(-adjudicataria(IdAda,_,_,_)) :: (
    solucoes( (IdAda),(-adjudicante( IdAda,_,_,_)),S1 ),
    comprimento(S1,R1),
    R1==1
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o dígito de controlo do id deve seguir a convenção de validação 'módulo 11'
% Aplicado a conhecimento perfeito positivo
+adjudicataria(_,_,Nif,_,_) :: (
    integer(Nif),
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).

% Aplicado a conhecimento perfeito negativo
+(-adjudicataria(_,_,Nif,_,_)) :: (
    integer(Nif),
    Nif>=100000000, Nif=<999999999,
    ultimo_digito_valido(Nif)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: Os digitos iniciais do nif devem corresponder ao tipo de entidade correta
% Aplicado a conhecimento perfeito positivo
+adjudicataria(_,_,Nif,TipoEntidade,_) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidade(NifLista,TipoEntidade)
).

% Aplicado a conhecimento perfeito positivo
+(-adjudicataria(_,_,Nif,TipoEntidade,_)) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidade(NifLista,TipoEntidade)
).

% Invariante referencial: Impede a inserção de um nif associado a um nome se esse nif estiver associado a outro nome no predicado adjudicante
+adjudicataria(_,Nome,Nif,_,_) :: (
    solucoes((Nomes),adjudicante(_,Nomes,Nif,_,_),S1),
    comprimento(S1,N1),
    write(S1),
    (N1 == 0; % Se o comprimento for 0 indica que o nif não está registado para nenhuma entidade adjudicataria
    (nth0(0, S1,NomeAdjudicataria),  % Se o comprimento não for 0, sua-se o predicado nth para aceder ao elemento da lista pois o predicado soluções só devolve listas
    write(NomeAdjudicataria),
    write(Nome),
    NomeAdjudicataria == Nome   % e verifica-se que se existir um nome de adjudicante associado ao nif tem de ser o mesmo nome do nif que estamos a inserir
    )
    )
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Não deve ser possivel remover uma entidade adjudicataria se esta estiver presente num contrato
-adjudicataria(IdAda,_,_,_,_) :: (
    solucoes(IdAda, contrato(_,_,IdAda,_,_,_,_,_,_,_), S),
    comprimento( S,N ),
    N == 0
).

% Invariante que impede a inserção de conhecimento perfeito positivo relativo
% a um adjudicante com nome interdito
+adjudicataria(IdAd,N,Ni,T,M) :: (
    solucoes((IdAd,Nome_interdito,Ni,T,M), (adjudicataria(Id,Nome_interdito,Ni,T,M), nulo(Nome_interdito)), R),
    comprimento(R,0)
).

% Invariantes sobre contratos
%

% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
% Aplicado a conhecimento perfeito positivo
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

% Aplicado a conhecimento perfeito negativo
+(-contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data)) :: (
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
% Invariante estrutural: o custo e duração devem ser superi ores a 0
% Aplicado a conhecimento perfeito positivo
+contrato(_,_,_,_,_,_,Custo,Prazo,_,_) :: (
    Custo >= 0,
    Prazo > 0
).

% Aplicado a conhecimento perfeito negativo
+(-contrato(_,_,_,_,_,_,Custo,Prazo,_,_)) :: (
    Custo >= 0,
    Prazo > 0
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: verificar que os ids das entidades contratuais existem
% Aplicado a conhecimento perfeito positivo
+contrato(_,IdAd,IdAda,_,_,_,_,_,_,_) :: (
    solucoes( (IdAd),(adjudicante( IdAd,_,_,_,_)),S1 ),
    solucoes( (IdAda),(adjudicataria( IdAda,_,_,_,_)),S2 ),
    comprimento( S1,N1 ),
    comprimento( S2,N2 ),
    N1 == 1,
    N2 ==1
).

% Aplicado a conhecimento perfeito negativo
% Não faria sentido aplicar o predicado -contrato a entidades que não existem no sistema
+(-contrato(_,IdAd,IdAda,_,_,_,_,_,_,_)) :: (
    solucoes( (IdAd),(adjudicante( IdAd,_,_,_,_)),S1 ),
    solucoes( (IdAda),(adjudicataria( IdAda,_,_,_,_)),S2 ),
    comprimento( S1,N1 ),
    comprimento( S2,N2 ),
    N1 == 1,
    N2 ==1
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Um contrato não pode ter o mesmo adjudicante e adjudicatario
% isto será verificado pelos nifs associados aos ids
+contrato(_,IdAd, IdAda,_,_,_,_,_,_,_) :: (
    solucoes((NifAd), (adjudicante( IdAd,NifAd,_,_,_)),S1 ),
    solucoes((NifAda), (adjudicataria( IdAda,NifAda,_,_,_)),S2),
    S1 \= S2,
).





%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o tipo de procedimento deve pertencer a um certo conjunto
% Aplicado a conhecimento perfeito positivo
+contrato(_,_,_,_,Procedimento,_,_,_,_,_) :: (
    member(Procedimento,['Ajuste direto','Consulta prévia','Concurso público'])
).

% Aplicado a conhecimento perfeito negativo

+(-contrato(_,_,_,_,Procedimento,_,_,_,_,_)) :: (
    member(Procedimento,['Ajuste direto','Consulta prévia','Concurso público'])
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: O valor de um contrato de ajuste direto deve ser menor ou igual a 5000
% Deve ser de um dos seguintes tipos: Contrato de aquisição ou locação de bens móveis ou aquisição de serviços
% Prazo de vigência até 1 ano, inclusive, a contar da decisão de adjudicação.
% Aplicado a conhecimento perfeito positivo
+contrato(_,_,_,TipoDeContrato,'Ajuste direto',_,Custo,Prazo,_,_) :: (
    Custo =< 5000,
    Prazo =< 365,
    member(TipoDeContrato,['Aquisição de bens móveis','Locação de bens móveis','Aquisição de serviços'])
).

% Aplicado a conhecimento perfeito negativo
+(-contrato(_,_,_,TipoDeContrato,'Ajuste direto',_,Custo,Prazo,_,_)) :: (
    Custo =< 5000,
    Prazo =< 365,
    member(TipoDeContrato,['Aquisição de bens móveis','Locação de bens móveis','Aquisição de serviços'])
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Uma entidade adjudicante não pode convidar a mesma empresa para celebrar um contrato com prestações de serviço
% do mesmo tipo ou idênticas às de contratos que já lhe foram atribuídos, no ano económico em curso e nos dois anos económicos anteriores,
% sempre que O preço contratual acumulado dos contratos já celebrados (não incluindo o contrato que se pretende celebrar) seja igual ou superior a 75.000 euros.
+contrato(_,IdAd,IdAda,'Aquisição de serviços',_,_,Valor,_,_,data(_,_,Ano)) :: (
    solucoes((Data,Custo),contrato(_,IdAd,IdAda,'Aquisição de serviços',_,_,Custo,_,_,Data),ParesDataCusto), %lista de pares
    somaCustosContratos(ParesDataCusto,Ano,SomaCustos),
    (SomaCustos-Valor) < 75000
).


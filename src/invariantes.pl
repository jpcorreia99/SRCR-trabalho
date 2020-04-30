% Invariantes regular
:- op(900, xfy, '::').
% Invariantes impreciso
:- op(900,xfy,:~:).
% Invariantes incerto/interdito
:- op(900,xfy,:-:).
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
    N1 = 1,
    N2 = 1,
    N3 = 1
).

% Invariante estrutural: não permitir a entrada repetida de conhecimento, no campo do Id
%  Aplicado a conhecimento perfeito negativo
+(-adjudicante(IdAd,_,_,_,_)) :: (
    solucoes( (IdAd),(-adjudicante( IdAd,_,_,_)),S1 ),
    comprimento(S1,R1),
    R1==2 % pois há sempre uma por negação por falha
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
% Invariante estrutural: Os digitos iniciais do nif devem corresponder a uma categoria de entidade que seja permitida ser entidade adjudicatária
% Estas entidades estão explicitadas no artigo 2.º n.º 2, alíneas a), b) e d) e no artigo 7.º n.º 1.º do CCP
% Aplicado a conhecimento perfeito positivo
% Aplicado a conhecimento perfeito positivo
+adjudicante(_,_,Nif,TipoEntidade,_) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidadeAdjudicante(NifLista,TipoEntidade)
).

% Aplicado a conhecimento perfeito positivo
+(-adjudicante(_,_,Nif,TipoEntidade,_)) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidadeAdjudicante(NifLista,TipoEntidade)
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Impede a inserção de um nif associado a um nome se esse nif estiver associado a um nome diferente no predicado adjudicataria
+adjudicante(_,Nome,Nif,_,_) :: (
    solucoes((Nomes),adjudicataria(_,Nomes,Nif,_,_),S1),
    comprimento(S1,N1),
    (N1 == 0; % Se o comprimento for 0 indica que o nif não está registado para nenhuma entidade adjudicataria
     (nth0(0, S1,NomeAdjudicataria),  % %Caso contrário, acede-se ao elemento na lista
     NomeAdjudicataria == Nome)   % %verifica-se que o nome é o mesmo
    )
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante que impede a inserção de conhecimento perfeito positivo relativo
% a um adjudicante com nome interdito
+adjudicante(IdAd,_,Nif,TipoEntidade,Morada) :: (
    solucoes((IdAd,Nome_interdito,Nif,TipoEntidade,Morada),
    (adjudicante(IdAd,Nome_interdito,Nif,TipoEntidade,Morada), nulo(Nome_interdito)), R),
    comprimento(R,0)
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Não deve ser possivel remover uma entidade adjudicante se esta estiver presente num contrato
-adjudicante(IdAd,_,_,_,_) :: (
    solucoes(IdAd, contrato(_,IdAd,_,_,_,_,_,_,_,_,_), S),
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
    R1==2
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
% Invariante estrutural: Os digitos iniciais do nif devem corresponder a uma categoria de entidade que seja permitida ser entidade adjudicatária
% Apenas entidades de administração pública podem ser entidades adjudicatárias
% Aplicado a conhecimento perfeito positivo
+adjudicataria(_,_,Nif,TipoEntidade,_) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidadeAdjudicataria(NifLista,TipoEntidade)
).

% Aplicado a conhecimento perfeito positivo
+(-adjudicataria(_,_,Nif,TipoEntidade,_)) :: (
    nif_para_lista(Nif,NifLista),
    nifCorrespondeTipoEntidadeAdjudicataria(NifLista,TipoEntidade)
).

% Invariante referencial: Impede a inserção de um nif associado a um nome se esse nif estiver associado a outro nome no predicado adjudicante
+adjudicataria(_,Nome,Nif,_,_) :: (
    solucoes((Nomes),adjudicante(_,Nomes,Nif,_,_),S1),
    comprimento(S1,N1),
    (N1 == 0;% Se o comprimento for 0 indica que o nif não está registado para nenhuma entidade adjudicante
    (nth0(0, S1,NomeAdjudicante),%Caso contrário, acede-se ao elemento na lista
    NomeAdjudicante == Nome %verifica-se que o nome é o mesmo
    )
    )
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Não deve ser possivel remover uma entidade adjudicataria se esta estiver presente num contrato
-adjudicataria(IdAda,_,_,_,_) :: (
    solucoes(IdAda, contrato(_,_,IdAda,_,_,_,_,_,_,_,_), S),
    comprimento( S,N ),
    N == 0
).

% Invariante que impede a inserção de conhecimento perfeito positivo relativo
% a um adjudicante com nome interdito
+adjudicataria(IdAd,_,Nif,TipoEntidade,Morada) :: (
    solucoes((IdAd,Nome_interdito,Nif,TipoEntidade,Morada), (adjudicataria(IdAd,Nome_interdito,Nif,TipoEntidade,Morada), nulo(Nome_interdito)), R),
    comprimento(R,0)
).

% Invariantes sobre contratos
%

% Invariante estrutural: verifica que os campos respeitam o tipo de dados correto
% Aplicado a conhecimento perfeito positivo
+contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data, Subsidiado) :: (
    integer(IdCont),
    integer(IdAd),
    integer(IdAda),
    atom(TipoDeContrato),
    atom(TipoDeProcedimento),
    atom(Descricao),
    integer(Valor),
    integer(Prazo),
    atom(Local),
    data_valida(Data),
    member(Subsidiado,[true,false])
).

% Aplicado a conhecimento perfeito negativo
+(-contrato(IdCont,IdAd, IdAda, TipoDeContrato, TipoDeProcedimento, Descricao, Valor, Prazo, Local, Data, Subsidiado)) :: (
    integer(IdCont),
    integer(IdAd),
    integer(IdAda),
    atom(TipoDeContrato),
    atom(TipoDeProcedimento),
    atom(Descricao),
    integer(Valor),
    integer(Prazo),
    atom(Local),
    data_valida(Data),
    member(Subsidiado,[true,false])
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o custo e duração devem ser superi ores a 0
% Aplicado a conhecimento perfeito positivo
+contrato(_,_,_,_,_,_,Custo,Prazo,_,_,_) :: (
    Custo >= 0,
    Prazo > 0
).

% Aplicado a conhecimento perfeito negativo
+(-contrato(_,_,_,_,_,_,Custo,Prazo,_,_,_)) :: (
    Custo >= 0,
    Prazo > 0
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: verificar que os ids das entidades contratuais existem
% Aplicado a conhecimento perfeito positivo
+contrato(_,IdAd,IdAda,_,_,_,_,_,_,_,_) :: (
    solucoes( (IdAd),(adjudicante( IdAd,_,_,_,_)),S1 ),
    solucoes( (IdAda),(adjudicataria( IdAda,_,_,_,_)),S2 ),
    comprimento( S1,N1 ),
    comprimento( S2,N2 ),
    N1 == 1,
    N2 == 1
).

% Aplicado a conhecimento perfeito negativo
% Não faria sentido aplicar o predicado -contrato a entidades que não existem no sistema
+(-contrato(_,IdAd,IdAda,_,_,_,_,_,_,_,_)) :: (
    solucoes( (IdAd),(adjudicante( IdAd,_,_,_,_)),S1 ),
    solucoes( (IdAda),(adjudicataria( IdAda,_,_,_,_)),S2 ),
    comprimento( S1,N1 ),
    comprimento( S2,N2 ),
    N1 == 1,
    N2 == 1
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Um contrato não pode ter o mesmo adjudicante e adjudicatario
% isto será verificado pelos nifs associados aos ids
+contrato(_,IdAd, IdAda,_,_,_,_,_,_,_,_) :: (
    solucoes((NifAd), (adjudicante( IdAd,NifAd,_,_,_)),S1 ),
    solucoes((NifAda), (adjudicataria( IdAda,NifAda,_,_,_)),S2),
    S1 \= S2
).





%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante estrutural: o tipo de procedimento deve pertencer a um certo conjunto
% Aplicado a conhecimento perfeito positivo
+contrato(_,_,_,_,Procedimento,_,_,_,_,_,_) :: (
    member(Procedimento,['Ajuste direto','Consulta prévia','Concurso público',
        'Concurso limitado por prévia qualificação','Procedimento de negociação',
        'Diálogo concorrencial','Parceria para a inovação'])
).

% Aplicado a conhecimento perfeito negativo

+(-contrato(_,_,_,_,Procedimento,_,_,_,_,_,_)) :: (
    member(Procedimento,['Ajuste direto','Consulta prévia','Concurso público',
        'Concurso limitado por prévia qualificação','Procedimento de negociação',
        'Diálogo concorrencial','Parceria para a inovação'])
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: se o contrato não for subsidiado, apenas permitir um conjunto de tipos de entidade adjudicante
% Aplicado a conhecimento perfeito positivo
+contrato(_,IdAd,_,_,_,_,_,_,_,_,false) :: (
    solucoes((Tipo),adjudicante(IdAd,_,_,Tipo,_),ListaTipo), % devido aos outros invariantes sabemos que esta lista terá comprimento 1
    nth0(0, ListaTipo, TipoAdjudicante), %acedemos então ao tipo do adjudicante que é o único habitante da lista
    member(TipoAdjudicante,['Pessoa coletiva','Organismo de administração pública']) %CCP artigo 2.º n.º 2, alíneas a), b) e d), %  artigo 7.º n.º 1.º
).


%--------------------------------- - - - - - - - - - -  -  -  -  -
% Invariante estrutural: O valor de um contrato de ajuste direto deve ser menor ou igual a 5000
% Deve ser de um dos seguintes tipos: Contrato de aquisição ou locação de bens móveis ou aquisição de serviços
% Prazo de vigência até 1 ano, inclusive, a contar da decisão de adjudicação.
% Aplicado a conhecimento perfeito positivo
+contrato(_,_,_,TipoDeContrato,'Ajuste direto',_,Custo,Prazo,_,_,_) :: (
    Custo =< 5000,
    Prazo =< 365,
    member(TipoDeContrato,['Aquisição de bens móveis','Locação de bens móveis','Aquisição de serviços'])
).

% Aplicado a conhecimento perfeito negativo
+(-contrato(_,_,_,TipoDeContrato,'Ajuste direto',_,Custo,Prazo,_,_,_)) :: (
    Custo =< 5000,
    Prazo =< 365,
    member(TipoDeContrato,['Aquisição de bens móveis','Locação de bens móveis','Aquisição de serviços'])
).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante referencial: Uma entidade adjudicante não pode convidar a mesma empresa para celebrar um contrato com prestações de serviço
% do mesmo tipo ou idênticas às de contratos que já lhe foram atribuídos, no ano económico em curso e nos dois anos económicos anteriores,
% sempre que O preço contratual acumulado dos contratos já celebrados (não incluindo o contrato que se pretende celebrar) seja igual ou superior a 75.000 euros.
+contrato(_,IdAd,IdAda,'Aquisição de serviços',_,_,Valor,_,_,data(_,_,Ano),_) :: (
    solucoes((Data,Custo),contrato(_,IdAd,IdAda,'Aquisição de serviços',_,_,Custo,_,_,Data,_),ParesDataCusto), %lista de pares
    somaCustosContratos(ParesDataCusto,Ano,SomaCustos), % somará os valores dos contratos no ano de assinatura do contrato assim como nos dois anos anteriores
    (SomaCustos-Valor) < 75000
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes referentes a conhecimento imperfeito

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante que nao permite a insercao de conhecimento
% incerto/interdito se ja exisitir conhecimento.

% Adjudicante incerto/interdito
+adjudicante(IdAd, Nome, NIF, TipoEntidade, Morada) :-: (
	nao(perfeito(adjudicante(IdAd))),
	nao(impreciso(adjudicante(IdAd))),
	nao(incerto(adjudicante(IdAd)))
).

% Adjudicataria incerto/interdito
+adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada) :-: (
	nao(perfeito(adjudicataria(IdAda))),
	nao(impreciso(adjudicataria(IdAda))),
	nao(incerto(adjudicataria(IdAda)))
).

% Contrato incerto/interdito
+contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local) :-: (
	nao(perfeito(contrato(IdContrato))),
	nao(impreciso(contrato(IdContrato))),
	nao(incerto(contrato(IdContrato)))
).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante que nao permite a insercao de conhecimento
% impreciso se ja exisitir conhecimento.

% Adjudicante impreciso
+adjudicante(IdAd, Nome, NIF, TipoEntidade, Morada) :~: (
	nao(perfeito(adjudicante(IdAd))),
	nao(impreciso(adjudicante(IdAd)))
).

% Adjudicataria impreciso
+adjudicataria(IdAda, Nome, NIF, TipoEntidade, Morada) :~: (
	nao(perfeito(adjudicataria(IdAda))),
	nao(impreciso(adjudicataria(IdAda)))
).

% Contrato impreciso
+contrato(IdContrato,Id_Adjudicante,IdAdjudicataria,TipoDeContrato,TipoDeProcedimento,Descricao,Valor,Prazo,Local) :~: (
	nao(perfeito(contrato(IdContrato))),
	nao(impreciso(contrato(IdContrato)))
).

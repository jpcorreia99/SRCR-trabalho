% Definições iniciais

:- op(900, xfy, '::').
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.


% Factos
%
% Extensao do predicado adjudicante #IdAd, Nome, NIF, Morada ->{V,F,D}
adjudicante(1,'Camara_de_barcelos',705330336,'Rua_dos_barros').


% Invariantes
%
% Invariante estrutural: não permitir a entrada repetida de conhecimento (mudar para verificar cada entrada)
+adjudicante(Id,Nome,Nif,Morada) :: (solucoes( (Id,Nome,Nif,Morada),(adjudicante( Id,Nome,Nif,Morada )),S ),
                  comprimento( S,N ), 
				  N == 1  ).

% Invariante referencial: o nif deve conter 9 números (Compltar)
+adjudicante(_,_,Nif,_) :: integer(Nif).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extens�o do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
    solucoes( Invariante, +Termo::Invariante,Lista ),  %coloca numa lista todos os invariantes
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

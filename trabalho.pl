:- dynamic robo/2.
:- dynamic pessoa/3.
:- dynamic maca/1.
:- dynamic log/1.
:- dynamic vertice/2.
:- dynamic pessoas/1.
:- dynamic lugares/1.

%inicializando o robo

robo(calcada, 100).
maca([]).

% Definindo os locais do ambiente
vertice(hub, semIncendio).
vertice(cozinha, incendio).
vertice(quarto1, semIncendio).
vertice(quarto2, semIncendio).
vertice(banheiro, semIncendio).
vertice(calcada).

% Definindo as arestas

aresta(calcada, hub).
aresta(hub, calcada).

aresta(hub, cozinha).
aresta(cozinha, hub).

aresta(hub, quarto1).
aresta(quarto1, hub).

aresta(hub, quarto2).
aresta(quarto2, hub).

aresta(banheiro, quarto1).
aresta(quarto1, banheiro).


% Definindo pessoas
pessoa(p1,hub,100).

pessoa(p2,hub,100).

pessoa(p3,cozinha,100).

pessoa(p4,quarto1,100).

pessoa(p5,quarto2,100).

pessoa(p6,banheiro,100).

pessoas([p1,p2,p3,p4,p5,p6]).

lugares([cozinha, hub, quarto1, quarto2, banheiro]).

%inspecionar
inspecionar:- robo(Local,_),
              findall(Pessoa-Oxigenio, pessoa(Pessoa, Local,Oxigenio), Pessoas),
              length(Pessoas, NumPessoas),
              NumPessoas >= 1,
              format('Voce esta em ~w e este local possui ~w pessoa(s).~n',[Local, NumPessoas]),
              exibir_oxigenacao(Pessoas),
              registrar_acoes(inspecionar),!.
              
exibir_oxigenacao([]).

exibir_oxigenacao([Pessoa-Oxigenio| Resto]):- format('Pessoa: ~w - Oxigênio: ~w.~n',[Pessoa, Oxigenio]),
                                              exibir_oxigenacao(Resto),!.
                                              
%status robo

status:- robo(Local,Oxigenio),
         maca([P]),
         format('Local do robo: ~w.~n',[Local]),
         format('Oxigenio do robo: ~w.~n',[Oxigenio]),
         format('A pessoa ~w esta na maca.~n',[P]).

status:- robo(Local,Oxigenio),
         maca([]),
         format('Local do robo: ~w.~n',[Local]),
         format('Oxigenio do robo: ~w.~n',[Oxigenio]),
         format('Nao ha pessoas na maca.~n').
              
%pegar pessoa
pegar(P):- robo(Local,_),
           pessoa(P, Local, O),
           maca([]),
           retract(pessoa(P, Local, O)),
           assert(pessoa(P, maca, O)),
           retract(maca([])),
           assert(maca([P])),
           retract(pessoas(ListaAtual)), % Remove a lista atual de pessoas
           select(P, ListaAtual, NovaLista), % Remove a pessoa da lista
           assert(pessoas(NovaLista)),
           format('Pegou a pessoa ~w do local ~w.~n', [P,Local]),
           registrar_acoes(pegar),!.
           
pegar(P):- maca(Pessoas),
           length(Pessoas, N),
           N == 1,
           format('A maca ja possui uma pessoa'),!.

pegar(P): format('A pessoa ~w nao esta no local',[P]),!.

%Soltar
           
soltar(P):- robo(Local,_),
            Local \= calcada,
            maca([P]),
            pessoa(P, maca, O),
            retract(maca([P])),
            assert(maca([])),
            retract(pessoa(P, maca,O)),
            assert(pessoa(P, Local, O)),
            retract(pessoas(ListaPessoas)),
            format('~w.~n',[ListaPessoas]),
            NovaLista = [P| ListaPessoas],
            assert(pessoas(NovaLista)),
            format('~w.~n',[NovaLista]).
            format('voce soltou a pessao ~w no local ~w.~n', [P, Local]),
            registrar_acoes(soltar),!.
            
soltar(P):- robo(Local,_),
            Local == calcada,
            maca([P]),
            pessoa(P, maca, O),
            retract(maca([P])),
            assert(maca([])),
            retract(pessoa(P, maca,O)),
            assert(pessoa(P, Local, O)),
            format('voce soltou a pessao ~w no local ~w.~n', [P, Local]),
            registrar_acoes(soltar),!.

soltar(P):- format('A pessoa ~w nao esta na maca',[P]),!.

%Caminhar

caminhar(Destino):- robo(Local, O),
                    aresta(Local, Destino),
                    aresta(Destino, Local),
                    retract(robo(Local, O)),
                    assert(robo(Destino, O)),
                    format('Voce caminhou de ~w para ~w.~n',[Local, Destino]),
                    registrar_acoes(caminhar),
                    diminuir_oxigenio,
                    espalharFogo,!.

caminhar(Destino):-robo(Local, _);
                   \+ vertice(Destino,_);
                    \+ aresta(Local, Destino);
                    \+ aresta(Destino, Local),
                   format('Não é possível caminhar para ~w.~n', Destino).
                    
%Oxigenacao
oxigenar(P):- maca([P]),
              pessoa(P, maca, O),
              O > 0,
              O =< 95,
              O1 is O + 10,
              robo(Local,Or),
              Or > 0,
              Or1 is Or - 10,
              retract(robo(Local,Or)),
              assert(robo(Local,Or1)),
              retract(pessoa(P, maca, O)),
              assert(pessoa(P, maca, O1)),
              format('Pessoa ~w teve sua oxigenacao aumentada de ~w para ~w.~n', [P, O, O1]),
              registrar_acoes(oxigenar).
              
oxigenar(P):-pessoa(P, Local, O),
             O =< 0,
             format('Nao foi possivel oxigenar ~w, pois seu oxigenio e menor ou igual a 0.~n', [P]),!.
             
oxigenar(P):-pessoa(P, Local, O),
             Local \= maca,
             format('Nao foi possivel oxigenar ~w, pois a pessoa nao esta na maca.~n', [P]),!.
             
oxigenar(P):-format('Nao foi possivel oxigenar ~w.~n', [P]),!.
              
diminuir_oxigenio :-
    pessoas(ListaPessoas),  % Obtém a lista de pessoas
    diminuir_oxigenio(ListaPessoas).  % Chama o predicado recursivo com a lista

% Caso base: lista vazia
diminuir_oxigenio([]).

% Caso 1: Pessoa em local com incêndio
diminuir_oxigenio([P | Cauda]) :-
    pessoa(P, Lugar, O),
    Lugar \= maca,
    Lugar \= calcada,
    vertice(Lugar, incendio),
    O > 0,
    O1 is O - 15,
    retract(pessoa(P, Lugar, O)),
    assert(pessoa(P, Lugar, O1)),
    format('Pessoa ~w teve sua oxigenacao diminuida de ~w para ~w.~n', [P, O, O1]),
    diminuir_oxigenio(Cauda).

% Caso 2: Pessoa em local sem incêndio
diminuir_oxigenio([P | Cauda]) :-
    pessoa(P, Lugar, O),
    Lugar \= maca,
    Lugar \= calcada,
    vertice(Lugar, semIncendio),
    O > 0,
    O1 is O - 10,
    retract(pessoa(P, Lugar, O)),
    assert(pessoa(P, Lugar, O1)),
    format('Pessoa ~w teve sua oxigenacao diminuida de ~w para ~w.~n', [P, O, O1]),
    diminuir_oxigenio(Cauda).

% Caso 3: Pessoa com oxigênio <= 0 (morta)
diminuir_oxigenio([P | Cauda]) :-
    pessoa(P, Lugar, O),
    O =< 0,
    format('Pessoa ~w esta morta, mas mesmo assim voce deve recuperar o seu corpo.~n', [P]),
    diminuir_oxigenio(Cauda).
                             
% espalhar fogo
espalharFogo :- lugares(Lugares),
                espalharFogo(Lugares).

espalharFogo([]).
espalharFogo([Cabeca|Cauda]) :-
    aresta(Cabeca, Comodo),
    aresta(Comodo, Cabeca),
    vertice(Cabeca, incendio),
    vertice(Comodo, semIncendio),
    Comodo \= calcada,
    retract(vertice(Comodo, semIncendio)),
    assert(vertice(Comodo, incendio)),
    retract(lugares([Cabeca|Cauda])),
    assert(lugares(Cauda)),
    format('O comodo ~w esta agora pegando fogo!.~n', [Comodo]).
                        
%logs
registrar_acoes(Acao):- assert(log(Acao)),
                        format('Acao registrada ~w.~n',[Acao]).
                          
log:- findall(Acao, log(Acao), Acoes),
      length(Acoes, N),
      N == 0,
      format('Nao ha nenhuma acao para mostrar').
      
log:- findall(Acao, log(Acao), Acoes),
      length(Acoes, N),
      N > 0,
      listar_acoes(Acoes).

listar_acoes([]).
listar_acoes([Acao| Restante]):- format(' - ~w~n',[Acao]),
                                 listar_acoes(Restante).


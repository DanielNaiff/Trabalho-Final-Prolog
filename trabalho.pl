:- dynamic robo/2.
:- dynamic pessoa/3.
:- dynamic maca/1.
:- dynamic log/1.
:- dynamic vertice/2.
:- dynamic pessoas/1.
:- dynamic lugares/1.
:- dynamic cozinha_robo/1.
:- dynamic hub_robo/1.
:- dynamic quarto2_robo/1.
:- dynamic quarto1_robo/1.
:- dynamic banheiro_robo/1.
:- dynamic calcada_robo/1.

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

exibir_oxigenacao([Pessoa-Oxigenio| Resto]):- format('Pessoa: ~w - Oxig�nio: ~w.~n',[Pessoa, Oxigenio]),
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
           
pegar(_):- maca(Pessoas),
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
            format('voce soltou a pessoa ~w no local ~w.~n', [P, Local]),
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
                    move_mapa(Local,Destino),
                    espalharFogo,
                    mapa(),
                    diminuir_oxigenio,
                    !.

caminhar(Destino):-robo(Local, _);
                   \+ vertice(Destino,_);
                    \+ aresta(Local, Destino);
                    \+ aresta(Destino, Local),
                   format('N�o � poss�vel caminhar para ~w.~n', Destino).
                    
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
              
oxigenar(P):-pessoa(P, _, O),
             O =< 0,
             format('Nao foi possivel oxigenar ~w, pois seu oxigenio e menor ou igual a 0.~n', [P]),!.
             
oxigenar(P):-pessoa(P, Local, _),
             Local \= maca,
             format('Nao foi possivel oxigenar ~w, pois a pessoa nao esta na maca.~n', [P]),!.
             
oxigenar(P):-format('Nao foi possivel oxigenar ~w.~n', [P]),!.
              
diminuir_oxigenio :-
    pessoas(ListaPessoas),  % Obt�m a lista de pessoas
    diminuir_oxigenio(ListaPessoas).  % Chama o predicado recursivo com a lista

% Caso base: lista vazia
diminuir_oxigenio([]).

% Caso 1: Pessoa em local com inc�ndio
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

% Caso 2: Pessoa em local sem inc�ndio
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

% Caso 3: Pessoa com oxig�nio <= 0 (morta)
diminuir_oxigenio([P | Cauda]) :-
    pessoa(P, _, O),
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





mapa() :-
    % Parte de cima - Cozinha 
    write('                -------------    '), nl,
    write('                |  Cozinha  |    '), nl,
    write('                |     P3    |    '), nl,
    mostrar_cozinha_robo(), nl,
    write('                -------------    '), nl,
    write('                      ||         '), nl,

    % Meio - Salaconectada com Quarto 2 
    write('       |     -------------------   '),write('   ------------- '), nl,
    write('       |    |        Hub        |  '),write('  |   Quarto 2  |'), nl,
    write('Calcada|====|      P1   P2      |=='),write('==|      P5     |'), nl,
    mostrar_hub_robo(),mostrar_quarto2_robo(), nl,
    mostrar_calcada_robo(),write('  |             |'), nl,
    write('       |     -------------------   '),write('   ------------- '), nl,
    write('       |              ||           '),write('                 '), nl,

    % Parte de baixo - Quarto 1 
    write('                -------------       '), nl,
    write('                |  Quarto 1   |     '), nl,
    write('                |     P4      |     '), nl,
    mostrar_quarto1_robo(), nl,
    write('                -------------       '), nl,
    write('                      ||            '), nl,

    % Parte de baixo final - Banheiro 
    write('                --------------      '), nl,
    write('                |  Banheiro   |     '), nl,
    write('                |     P6      |     '), nl,
    mostrar_banheiro_robo(), nl,
    write('                --------------      '), nl.



banheiro_robo('                |             |     ').
mostrar_banheiro_robo() :- banheiro_robo(X), write(X).
quarto1_robo('                |             |     ').
mostrar_quarto1_robo() :- quarto1_robo(X), write(X).
hub_robo('       |    |                   |  ').
mostrar_hub_robo() :- hub_robo(X), write(X).
quarto2_robo('  |             |').
mostrar_quarto2_robo() :- quarto2_robo(X), write(X).
cozinha_robo('                |           |    ').
mostrar_cozinha_robo() :- cozinha_robo(X), write(X).
calcada_robo(' robo  |    |                   |  ').
mostrar_calcada_robo() :- calcada_robo(X),write(X).





move_mapa(banheiro, quarto1) :-  retract(banheiro_robo('                |    robo     |     ')), assertz(banheiro_robo('                |             |     ')),
                                           retract(quarto1_robo('                |             |     ')), assertz(quarto1_robo('                |    robo     |     ')),!.

move_mapa(quarto1, banheiro) :-  retract(quarto1_robo('                |    robo     |     ')), assertz(quarto1_robo('                |             |     ')),
                                           retract(banheiro_robo('                |             |     ')), assertz(banheiro_robo('                |    robo     |     ')),!.

move_mapa(hub, quarto1) :-  retract(hub_robo('       |    |       robo        |  ')), assertz(hub_robo('       |    |                   |  ')),
                                           retract(quarto1_robo('                |             |     ')), assertz(quarto1_robo('                |    robo     |     ')),!.

move_mapa(quarto1, hub) :-  assertz(hub_robo('       |    |       robo        |  ')), retract(hub_robo('       |    |                   |  ')),
                                           assertz(quarto1_robo('                |             |     ')), retract(quarto1_robo('                |    robo     |     ')),!.

move_mapa(hub, quarto2) :-  retract(hub_robo('       |    |       robo        |  ')), assertz(hub_robo('       |    |                   |  ')),
                                           retract(quarto2_robo('  |             |')), assertz(quarto2_robo('  |     robo    |')),!.

move_mapa(quarto2, hub) :-  assertz(hub_robo('       |    |       robo        |  ')), retract(hub_robo('       |    |                   |  ')),
                                           assertz(quarto2_robo('  |             |')), retract(quarto2_robo('  |     robo    |')),!.

move_mapa(calcada, hub) :-  retract(calcada_robo(' robo  |    |                   |  ')), assertz(calcada_robo('       |    |                   |  ')),
                                           assertz(hub_robo('       |    |       robo        |  ')), retract(hub_robo('       |    |                   |  ')),!.

move_mapa(hub, calcada) :-  assertz(calcada_robo(' robo  |    |                   |  ')), retract(calcada_robo('       |    |                   |  ')),
                                           retract(hub_robo('       |    |       robo        |  ')), assertz(hub_robo('       |    |                   |  ')),!.

move_mapa(cozinha, hub) :-  assertz(cozinha_robo('                |           |    ')), retract(cozinha_robo('                |    robo   |    ')),
                                           assertz(hub_robo('       |    |       robo        |  ')), retract(hub_robo('       |    |                   |  ')),!.

move_mapa(hub, cozinha) :-  retract(cozinha_robo('                |           |    ')), assertz(cozinha_robo('                |    robo   |    ')),
                                           retract(hub_robo('       |    |       robo        |  ')), assertz(hub_robo('       |    |                   |  ')),!.






                                           

    

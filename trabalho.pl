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
:- dynamic calcada_P2/1.
:- dynamic calcada_P1/1.
:- dynamic calcada_P3/1.
:- dynamic calcada_P4/1.
:- dynamic calcada_P5/1.
:- dynamic calcada_P6/1.
:- dynamic hub_P1/1.
:- dynamic hub_P2/1.
:- dynamic hub_P3/1.
:- dynamic hub_P4/1.
:- dynamic hub_P5/1.
:- dynamic hub_P6/1.
:- dynamic quarto2_P1/1.
:- dynamic quarto2_P2/1.
:- dynamic quarto2_P3/1.
:- dynamic quarto2_P4/1.
:- dynamic quarto2_P5/1.
:- dynamic quarto2_P6/1.
:- dynamic cozinha_P1/1.
:- dynamic cozinha_P2/1.
:- dynamic cozinha_P3/1.
:- dynamic cozinha_P4/1.
:- dynamic cozinha_P5/1.
:- dynamic cozinha_P6/1.
:- dynamic quarto1_P1/1.
:- dynamic quarto1_P2/1.
:- dynamic quarto1_P3/1.
:- dynamic quarto1_P4/1.
:- dynamic quarto1_P5/1.
:- dynamic quarto1_P6/1.
:- dynamic banheiro_P1/1.
:- dynamic banheiro_P2/1.
:- dynamic banheiro_P3/1.
:- dynamic banheiro_P4/1.
:- dynamic banheiro_P5/1.
:- dynamic banheiro_P6/1.

:- dynamic espalharFogo/1.

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
espalharFogo([cozinha, hub, quarto1, quarto2, banheiro]).

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
           pega_P(P,Local),
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
            solta_P(P,Local),
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
            solta_P(P,Local),
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
                    diminuir_oxigenio, !.

caminhar(Destino):-robo(Local, _),
                   (\+ vertice(Destino,_);
                    \+ aresta(Local, Destino);
                    \+ aresta(Destino, Local)),
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
espalharFogo :- 
    espalharFogo(L),
    espalharFogoLoop(L).

espalharFogoLoop([]) :- vertice(quarto2, semIncendio),
                        retract(vertice(quarto2, semIncendio)),
                        assertz(vertice(quarto2, incendio)),
                        format('O comodo quarto2 agora esta pegando fogo!~n').
espalharFogoLoop([]) :- vertice(quarto2, incendio),!.


espalharFogoLoop([Cabeca|Resto]) :-
    findall(Novo, (
        aresta(Cabeca, Novo),
        aresta(Novo, Cabeca),
        vertice(Cabeca, incendio),
        vertice(Novo, semIncendio),
        Novo \= calcada
    ), NovosComodos),

    retract(espalharFogo(_)),
    assertz(espalharFogo(Resto)),
    % Incendiar os novos cômodos e imprimir
    incendiar_todos(NovosComodos), !.

incendiar_todos([]).
incendiar_todos([Comodo|_]) :-
    retract(vertice(Comodo, semIncendio)),
    assert(vertice(Comodo, incendio)),
    format('O comodo ~w esta agora pegando fogo!.~n', [Comodo]), !.
    

                        
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
    write('                 -----------------    '), nl,
    write('                |    Cozinha      |    '), nl,
    mostrar_cozinha_P1,mostrar_cozinha_P2,mostrar_cozinha_P3,mostrar_cozinha_P4,mostrar_cozinha_P5,mostrar_cozinha_P6, nl,
    mostrar_cozinha_robo(), nl,
    write('                ------------------    '), nl,
    write('                        ||         '), nl,

    % Meio - Salaconectada com Quarto 2 
    mostrar_calcada_P1,write('     |                ||           '),write('                       '), nl,
    mostrar_calcada_P2,write('     |     ----------------------   '),write('   ----------------- '), nl,
    mostrar_calcada_P3,write('     |    |          Hub         |  '),write('  |    Quarto 2     |'), nl,
    mostrar_hub_P1,mostrar_hub_P2,mostrar_hub_P3,mostrar_hub_P4,mostrar_hub_P5,mostrar_hub_P6,mostrar_quarto2_P1,mostrar_quarto2_P2,mostrar_quarto2_P3,mostrar_quarto2_P4,mostrar_quarto2_P5,mostrar_quarto2_P6, nl,
    mostrar_hub_robo(),mostrar_quarto2_robo(), nl,
    mostrar_calcada_robo(),write('  |                 |'), nl,
    mostrar_calcada_P4,write('     |     ----------------------   '),write('   ------------------ '), nl,
    mostrar_calcada_P5,write('     |                ||           '),write('                       '), nl,
    mostrar_calcada_P6,write('     |                ||           '),write('                       '), nl,

    % Parte de baixo - Quarto 1 
    write('                ------------------       '), nl,
    write('                |    Quarto 1     |     '), nl,
    mostrar_quarto1_P1,mostrar_quarto1_P2,mostrar_quarto1_P3,mostrar_quarto1_P4,mostrar_quarto1_P5,mostrar_quarto1_P6, nl,
    mostrar_quarto1_robo(), nl,
    write('                ------------------       '), nl,
    write('                        ||            '), nl,

    % Parte de baixo final - Banheiro 
    write('                ------------------      '), nl,
    write('                |    Banheiro     |     '), nl,
    mostrar_banheiro_P1,mostrar_banheiro_P2,mostrar_banheiro_P3,mostrar_banheiro_P4,mostrar_banheiro_P5,mostrar_banheiro_P6, nl,
    mostrar_banheiro_robo(), nl,
    write('                ------------------      '), nl.



banheiro_robo('                |                 |     ').
quarto1_robo('                |                 |     ').
hub_robo('       |    |                      |  ').
quarto2_robo('  |                 |').
cozinha_robo('                |                 |    ').
calcada_robo(' robo  |    |                      |  ').
mostrar_quarto1_robo() :- quarto1_robo(X), write(X).
mostrar_banheiro_robo() :- banheiro_robo(X), write(X).
mostrar_hub_robo() :- hub_robo(X), write(X).
mostrar_quarto2_robo() :- quarto2_robo(X), write(X).
mostrar_cozinha_robo() :- cozinha_robo(X), write(X).
mostrar_calcada_robo() :- calcada_robo(X),write(X).

calcada_P1('  ').
calcada_P2('  ').
calcada_P3('  ').
calcada_P4('  ').
calcada_P5('  ').
calcada_P6('  ').
hub_P1('Calcada|====|P1  ').
hub_P2('P2  ').
hub_P3('P3  ').
hub_P4('P4  ').
hub_P5('P5  ').
hub_P6('P6|==').
quarto2_P1('==|P1 ').
quarto2_P2('P2 ').
quarto2_P3('P3 ').
quarto2_P4('P4 ').
quarto2_P5('P5 ').
quarto2_P6('P6|').
cozinha_P1('                |P1 ').
cozinha_P2('P2 ').
cozinha_P3('P3 ').
cozinha_P4('P4 ').
cozinha_P5('P5 ').
cozinha_P6('P6|    ').
quarto1_P1('                |P1 ').
quarto1_P2('P2 ').
quarto1_P3('P3 ').
quarto1_P4('P4 ').
quarto1_P5('P5 ').
quarto1_P6('P6|     ').
banheiro_P1('                |P1 ').
banheiro_P2('P2 ').
banheiro_P3('P3 ').
banheiro_P4('P4 ').
banheiro_P5('P5 ').
banheiro_P6('P6|     ').
mostrar_calcada_P1() :- calcada_P1(X),write(X).
mostrar_calcada_P2() :- calcada_P1(X),write(X).
mostrar_calcada_P3() :- calcada_P1(X),write(X).
mostrar_calcada_P4() :- calcada_P1(X),write(X).
mostrar_calcada_P5() :- calcada_P1(X),write(X).
mostrar_calcada_P6() :- calcada_P1(X),write(X).
mostrar_cozinha_P1() :- cozinha_P1(X), write(X).
mostrar_cozinha_P2() :- cozinha_P1(X), write(X).
mostrar_cozinha_P3() :- cozinha_P1(X), write(X).
mostrar_cozinha_P4() :- cozinha_P1(X), write(X).
mostrar_cozinha_P5() :- cozinha_P1(X), write(X).
mostrar_cozinha_P6() :- cozinha_P1(X), write(X).
mostrar_quarto2_P1() :- quarto2_P1(X), write(X).
mostrar_quarto2_P2() :- quarto2_P1(X), write(X).
mostrar_quarto2_P3() :- quarto2_P1(X), write(X).
mostrar_quarto2_P4() :- quarto2_P1(X), write(X).
mostrar_quarto2_P5() :- quarto2_P1(X), write(X).
mostrar_quarto2_P6() :- quarto2_P1(X), write(X).
mostrar_hub_P1() :- hub_P1(X), write(X).
mostrar_hub_P2() :- hub_P1(X), write(X).
mostrar_hub_P3() :- hub_P1(X), write(X).
mostrar_hub_P4() :- hub_P1(X), write(X).
mostrar_hub_P5() :- hub_P1(X), write(X).
mostrar_hub_P6() :- hub_P1(X), write(X).
mostrar_banheiro_P1() :- banheiro_P1(X), write(X).
mostrar_banheiro_P2() :- banheiro_P1(X), write(X).
mostrar_banheiro_P3() :- banheiro_P1(X), write(X).
mostrar_banheiro_P4() :- banheiro_P1(X), write(X).
mostrar_banheiro_P5() :- banheiro_P1(X), write(X).
mostrar_banheiro_P6() :- banheiro_P1(X), write(X).
mostrar_quarto1_P1() :- quarto1_P1(X), write(X).
mostrar_quarto1_P2() :- quarto1_P1(X), write(X).
mostrar_quarto1_P3() :- quarto1_P1(X), write(X).
mostrar_quarto1_P4() :- quarto1_P1(X), write(X).
mostrar_quarto1_P5() :- quarto1_P1(X), write(X).
mostrar_quarto1_P6() :- quarto1_P1(X), write(X).



pega_P(p1,calcada) :- retract(calcada_P1('P1')) , assertz(calcada_P1('  ')).
pega_P(p1,hub) :- retract(hub_P1('Calcada|====|P1  ')), assertz(hub_P1('Calcada|====|    ')).
pega_P(p1,quarto1) :- retract(quarto1_P1('                |P1 ')), assertz(quarto1_P1('                |   ')).
pega_P(p1,quarto2) :- 
pega_P(p1,banheiro) :- 
pega_P(p1,cozinha) :- 

pega_P(p2,calcada) :- retract(calcada_P2('P2')), assertz(calcada_P2('  ')).
pega_P(p2,hub) :- retract(hub_P2('P2  ')), assertz(hub_P2('    ')).
pega_P(p2,quarto1) :- 
pega_P(p2,quarto2) :- 
pega_P(p2,banheiro) :- 
pega_P(p2,cozinha) :- 

pega_P(p3,calcada) :- retract(calcada_P3('P3')), assertz(calcada_P3('  ')).
pega_P(p3,hub) :- retract(hub_P3('P3  ')), assertz(hub_P3('    ')).
pega_P(p3,quarto1) :- 
pega_P(p3,quarto2) :- 
pega_P(p3,banheiro) :- 
pega_P(p3,cozinha) :- 

pega_P(p4,calcada) :- retract(calcada_P4('P4')), assertz(calcada_P4('  ')).
pega_P(p4,hub) :- retract(hub_P4('P4  ')), assertz(hub_P4('    ')).
pega_P(p4,quarto1) :- 
pega_P(p4,quarto2) :- 
pega_P(p4,banheiro) :- 
pega_P(p4,cozinha) :- 

pega_P(p5,calcada) :- retract(calcada_P5('P5')), assertz(calcada_P5('  ')).
pega_P(p5,hub) :- retract(hub_P5('P5  ')), assertz(hub_P5('    ')).
pega_P(p5,quarto1) :- 
pega_P(p5,quarto2) :- 
pega_P(p5,banheiro) :- 
pega_P(p5,cozinha) :- 

pega_P(p6,calcada) :- retract(calcada_P6('P6')), assertz(calcada_P6('  ')).
pega_P(p6,hub) :- retract(hub_P6('P6|==')) , assertz(hub_P6('  |==')).
pega_P(p6,quarto1) :- 
pega_P(p6,quarto2) :- 
pega_P(p6,banheiro) :- 
pega_P(p6,cozinha) :- 


solta_P(p1,calcada) :- assertz(calcada_P1('P1')) , retract(calcada_P1('  ')).
solta_P(p1,hub) :- assertz(hub_P1('Calcada|====|P1  ')), retract(hub_P1('Calcada|====|    ')).
solta_P(p1,quarto1) :- 
solta_P(p1,quarto2) :- 
solta_P(p1,banheiro) :- 
solta_P(p1,cozinha) :- 

solta_P(p2,calcada) :- assertz(calcada_P2('P2')), retract(calcada_P2('  ')).
solta_P(p2,hub) :- assertz(hub_P2('P2  ')), retract(hub_P2('    ')).
solta_P(p2,quarto1) :- 
solta_P(p2,quarto2) :- 
solta_P(p2,banheiro) :- 
solta_P(p2,cozinha) :- 

solta_P(p3,calcada) :- assertz(calcada_P3('P3')), retract(calcada_P3('  ')).
solta_P(p3,hub) :- assertz(hub_P3('P3  ')), retract(hub_P3('    ')).
solta_P(p3,quarto1) :- 
solta_P(p3,quarto2) :- 
solta_P(p3,banheiro) :- 
solta_P(p3,cozinha) :- 

solta_P(p4,calcada) :- assertz(calcada_P4('P4')), retract(calcada_P4('  ')).
solta_P(p4,hub) :- assertz(hub_P4('P4  ')), retract(hub_P4('    ')).
solta_P(p4,quarto1) :- 
solta_P(p4,quarto2) :- 
solta_P(p4,banheiro) :- 
solta_P(p4,cozinha) :- 

solta_P(p5,calcada) :- assertz(calcada_P5('P5')), retract(calcada_P5('  ')).
solta_P(p5,hub) :- assertz(hub_P5('P5  ')), retract(hub_P5('    ')).
solta_P(p5,quarto1) :- 
solta_P(p5,quarto2) :- 
solta_P(p5,banheiro) :- 
solta_P(p5,cozinha) :- 

solta_P(p6,calcada) :- assertz(calcada_P6('P6')), retract(calcada_P6('  ')).
solta_P(p6,hub) :- assertz(hub_P6('P6|==')) , retract(hub_P6('  |==')).
solta_P(p6,quarto1) :- 
solta_P(p6,quarto2) :- 
solta_P(p6,banheiro) :- 
solta_P(p6,cozinha) :- 






move_mapa(banheiro, quarto1) :-  retract(banheiro_robo('                |      robo       |     ')), assertz(banheiro_robo('                |                 |     ')),
                                           retract(quarto1_robo('                |                 |     ')), assertz(quarto1_robo('                |      robo       |     ')),!.

move_mapa(quarto1, banheiro) :-  retract(quarto1_robo('                |      robo       |     ')), assertz(quarto1_robo('                |                 |     ')),
                                           retract(banheiro_robo('                |                 |     ')), assertz(banheiro_robo('                |      robo       |     ')),!.

move_mapa(hub, quarto1) :-  retract(hub_robo('       |    |          robo        |  ')), assertz(hub_robo('       |    |                      |  ')),
                                           retract(quarto1_robo('                |                 |     ')), assertz(quarto1_robo('                |      robo       |     ')),!.

move_mapa(quarto1, hub) :-  assertz(hub_robo('       |    |          robo        |  ')), retract(hub_robo('       |    |                      |  ')),
                                           assertz(quarto1_robo('                |                 |     ')), retract(quarto1_robo('                |      robo       |     ')),!.

move_mapa(hub, quarto2) :-  retract(hub_robo('       |    |          robo        |  ')), assertz(hub_robo('       |    |                      |  ')),
                                           retract(quarto2_robo('  |                 |')), assertz(quarto2_robo('  |      robo       |')),!.

move_mapa(quarto2, hub) :-  assertz(hub_robo('       |    |          robo        |  ')), retract(hub_robo('       |    |                      |  ')),
                                           assertz(quarto2_robo('  |                 |')), retract(quarto2_robo('  |      robo       |')),!.

move_mapa(calcada, hub) :-  retract(calcada_robo(' robo  |    |                      |  ')), assertz(calcada_robo('       |    |                      |  ')),
                                           assertz(hub_robo('       |    |          robo        |  ')), retract(hub_robo('       |    |                      |  ')),!.

move_mapa(hub, calcada) :-  assertz(calcada_robo(' robo  |    |                      |  ')), retract(calcada_robo('       |    |                      |  ')),
                                           retract(hub_robo('       |    |          robo        |  ')), assertz(hub_robo('       |    |                      |  ')),!.

move_mapa(cozinha, hub) :-  assertz(cozinha_robo('                |                 |    ')), retract(cozinha_robo('                |      robo       |    ')),
                                           assertz(hub_robo('       |    |          robo        |  ')), retract(hub_robo('       |    |                      |  ')),!.

move_mapa(hub, cozinha) :-  retract(cozinha_robo('                |                 |    ')), assertz(cozinha_robo('                |      robo       |    ')),
                                           retract(hub_robo('       |    |          robo        |  ')), assertz(hub_robo('       |    |                      |  ')),!.






                                           

    

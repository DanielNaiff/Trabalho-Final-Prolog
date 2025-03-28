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
:- dynamic calcada_p2/1.
:- dynamic calcada_p1/1.
:- dynamic calcada_p3/1.
:- dynamic calcada_p4/1.
:- dynamic calcada_p5/1.
:- dynamic calcada_p6/1.
:- dynamic hub_p1/1.
:- dynamic hub_p2/1.
:- dynamic hub_p3/1.
:- dynamic hub_p4/1.
:- dynamic hub_p5/1.
:- dynamic hub_p6/1.
:- dynamic quarto2_p1/1.
:- dynamic quarto2_p2/1.
:- dynamic quarto2_p3/1.
:- dynamic quarto2_p4/1.
:- dynamic quarto2_p5/1.
:- dynamic quarto2_p6/1.
:- dynamic cozinha_p1/1.
:- dynamic cozinha_p2/1.
:- dynamic cozinha_p3/1.
:- dynamic cozinha_p4/1.
:- dynamic cozinha_p5/1.
:- dynamic cozinha_p6/1.
:- dynamic quarto1_p1/1.
:- dynamic quarto1_p2/1.
:- dynamic quarto1_p3/1.
:- dynamic quarto1_p4/1.
:- dynamic quarto1_p5/1.
:- dynamic quarto1_p6/1.
:- dynamic banheiro_p1/1.
:- dynamic banheiro_p2/1.
:- dynamic banheiro_p3/1.
:- dynamic banheiro_p4/1.
:- dynamic banheiro_p5/1.
:- dynamic banheiro_p6/1.

:- dynamic espalharFogo/1.

%inicializando o robo

robo(calcada, 350).
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
pessoa(p1,hub,120).

pessoa(p2,hub,120).

pessoa(p3,cozinha,120).

pessoa(p4,quarto1,120).

pessoa(p5,quarto2,120).

pessoa(p6,banheiro,120).

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
         format('A pessoa ~w esta na maca.~n',[P]),!.

status:- robo(Local,Oxigenio),
         maca([]),
         format('Local do robo: ~w.~n',[Local]),
         format('Oxigenio do robo: ~w.~n',[Oxigenio]),
         format('Nao ha pessoas na maca.~n'),!.
              
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
           pega_p(P,Local),
           mapa(),
           registrar_acoes(pegar),!.
           
pegar(_):- maca(Pessoas),
           length(Pessoas, N),
           N == 1,
           format('A maca ja possui uma pessoa'),!.

pegar(P):- format('A pessoa ~w nao esta no local',[P]),!.

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
            NovaLista = [P| ListaPessoas],
            assert(pessoas(NovaLista)),
            format('voce soltou a pessoa ~w no local ~w.~n', [P, Local]),
            solta_p(P,Local),
            mapa(),
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
            solta_p(P,Local),
            mapa(),
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
              registrar_acoes(oxigenar), !.
              
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
    O1 is O - 10,
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
    O1 is O - 5,
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
    
missao :- pessoa(p1, calcada, X),
          pessoa(p2, calcada, Y),
            pessoa(p3, calcada, Z),
            pessoa(p4, calcada, W),
            pessoa(p5, calcada, V),
            pessoa(p6, calcada, U),
            X > 0, Y > 0, Z > 0, W > 0, V > 0, U > 0,
            format('Missao cumprida! o robo salvou todas as pessoa com vida!~n'), !.

missao :- findall(Calcada, (pessoa(_, Calcada, _), Calcada == calcada), Lista),
          length(Lista, N),
          N is 6,
          findall(Vida, (pessoa(_, _, Vida), Vida > 0), Vidas),
          length(Vidas, M),
          M >= 1,
          findall(Vivo, (pessoa(Vivo, _, X), X > 0), Vivos),
          findall(Mortos, (pessoa(Mortos, _, Y), Y =< 0), Mortos),
          format('A missao nao foi cumprida, porem foi possivel salvar vidas.~n'),
          format('Pessoas vivas: ~w~n', [Vivos]),
          format('Pessoas mortas: ~w~n', [Mortos]).

missao :- findall(Morto, (pessoa(Morto,Local,X), Local == calcada, X =< 0), Mortes),
            length(Mortes, N), N is 6,
            format('Missao falhou! Todas as pessoas perderam suas vidas!~n').

missao :- format('A missao nao foi cumprida, ainda ha pessoas para serem salvas.~n').
                        
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
    mostrar_cozinha_p1,mostrar_cozinha_p2,mostrar_cozinha_p3,mostrar_cozinha_p4,mostrar_cozinha_p5,mostrar_cozinha_p6, nl,
    mostrar_cozinha_robo(), nl,
    write('                ------------------    '), nl,
    write('                        ||         '), nl,

    % Meio - Salaconectada com Quarto 2 
    mostrar_calcada_p1,write('     |                ||           '),write('                       '), nl,
    mostrar_calcada_p2,write('     |     ----------------------   '),write('   ----------------- '), nl,
    mostrar_calcada_p3,write('     |    |          Hub         |  '),write('  |    Quarto 2     |'), nl,
    mostrar_hub_p1,mostrar_hub_p2,mostrar_hub_p3,mostrar_hub_p4,mostrar_hub_p5,mostrar_hub_p6,mostrar_quarto2_p1,mostrar_quarto2_p2,mostrar_quarto2_p3,mostrar_quarto2_p4,mostrar_quarto2_p5,mostrar_quarto2_p6, nl,
    mostrar_hub_robo(),mostrar_quarto2_robo(), nl,
    mostrar_calcada_robo(),write('  |                 |'), nl,
    mostrar_calcada_p4,write('     |     ----------------------   '),write('   ------------------ '), nl,
    mostrar_calcada_p5,write('     |                ||           '),write('                       '), nl,
    mostrar_calcada_p6,write('     |                ||           '),write('                       '), nl,

    % parte de baixo - Quarto 1 
    write('                ------------------       '), nl,
    write('                |    Quarto 1     |     '), nl,
    mostrar_quarto1_p1,mostrar_quarto1_p2,mostrar_quarto1_p3,mostrar_quarto1_p4,mostrar_quarto1_p5,mostrar_quarto1_p6, nl,
    mostrar_quarto1_robo(), nl,
    write('                ------------------       '), nl,
    write('                        ||            '), nl,

    % parte de baixo final - Banheiro 
    write('                ------------------      '), nl,
    write('                |    Banheiro     |     '), nl,
    mostrar_banheiro_p1,mostrar_banheiro_p2,mostrar_banheiro_p3,mostrar_banheiro_p4,mostrar_banheiro_p5,mostrar_banheiro_p6, nl,
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

calcada_p1('  ').
calcada_p2('  ').
calcada_p3('  ').
calcada_p4('  ').
calcada_p5('  ').
calcada_p6('  ').
hub_p1('Calcada|====|P1  ').
hub_p2('P2  ').
hub_p3('    ').
hub_p4('    ').
hub_p5('    ').
hub_p6('  |==').
quarto2_p1('==|   ').
quarto2_p2('   ').
quarto2_p3('   ').
quarto2_p4('   ').
quarto2_p5('P5 ').
quarto2_p6('  |').
cozinha_p1('                |   ').
cozinha_p2('   ').
cozinha_p3('P3 ').
cozinha_p4('   ').
cozinha_p5('   ').
cozinha_p6('  |    ').
quarto1_p1('                |   ').
quarto1_p2('   ').
quarto1_p3('   ').
quarto1_p4('P4 ').
quarto1_p5('   ').
quarto1_p6('  |     ').
banheiro_p1('                |   ').
banheiro_p2('   ').
banheiro_p3('   ').
banheiro_p4('   ').
banheiro_p5('   ').
banheiro_p6('P6|     ').
mostrar_calcada_p1() :- calcada_p1(X),write(X).
mostrar_calcada_p2() :- calcada_p2(X),write(X).
mostrar_calcada_p3() :- calcada_p3(X),write(X).
mostrar_calcada_p4() :- calcada_p4(X),write(X).
mostrar_calcada_p5() :- calcada_p5(X),write(X).
mostrar_calcada_p6() :- calcada_p6(X),write(X).
mostrar_cozinha_p1() :- cozinha_p1(X), write(X).
mostrar_cozinha_p2() :- cozinha_p2(X), write(X).
mostrar_cozinha_p3() :- cozinha_p3(X), write(X).
mostrar_cozinha_p4() :- cozinha_p4(X), write(X).
mostrar_cozinha_p5() :- cozinha_p5(X), write(X).
mostrar_cozinha_p6() :- cozinha_p6(X), write(X).
mostrar_quarto2_p1() :- quarto2_p1(X), write(X).
mostrar_quarto2_p2() :- quarto2_p2(X), write(X).
mostrar_quarto2_p3() :- quarto2_p3(X), write(X).
mostrar_quarto2_p4() :- quarto2_p4(X), write(X).
mostrar_quarto2_p5() :- quarto2_p5(X), write(X).
mostrar_quarto2_p6() :- quarto2_p6(X), write(X).
mostrar_hub_p1() :- hub_p1(X), write(X).
mostrar_hub_p2() :- hub_p2(X), write(X).
mostrar_hub_p3() :- hub_p3(X), write(X).
mostrar_hub_p4() :- hub_p4(X), write(X).
mostrar_hub_p5() :- hub_p5(X), write(X).
mostrar_hub_p6() :- hub_p6(X), write(X).
mostrar_banheiro_p1() :- banheiro_p1(X), write(X).
mostrar_banheiro_p2() :- banheiro_p2(X), write(X).
mostrar_banheiro_p3() :- banheiro_p3(X), write(X).
mostrar_banheiro_p4() :- banheiro_p4(X), write(X).
mostrar_banheiro_p5() :- banheiro_p5(X), write(X).
mostrar_banheiro_p6() :- banheiro_p6(X), write(X).
mostrar_quarto1_p1() :- quarto1_p1(X), write(X).
mostrar_quarto1_p2() :- quarto1_p2(X), write(X).
mostrar_quarto1_p3() :- quarto1_p3(X), write(X).
mostrar_quarto1_p4() :- quarto1_p4(X), write(X).
mostrar_quarto1_p5() :- quarto1_p5(X), write(X).
mostrar_quarto1_p6() :- quarto1_p6(X), write(X).



pega_p(p1,calcada) :- retract(calcada_p1('P1')) , assertz(calcada_p1('  ')).
pega_p(p1,hub) :- retract(hub_p1('Calcada|====|P1  ')), assertz(hub_p1('Calcada|====|    ')).
pega_p(p1,quarto1) :- retract(quarto1_p1('                |P1 ')), assertz(quarto1_p1('                |   ')).
pega_p(p1,quarto2) :- retract(quarto2_p1('==|P1 ')), assertz(quarto2_p1('==|   ')).
pega_p(p1,banheiro) :- retract(banheiro_p1('                |P1 ')), assertz(banheiro_p1('                |   ')).
pega_p(p1,cozinha) :- retract(cozinha_p1('                |P1 ')), assertz(cozinha_p1('                |   ')).

pega_p(p2,calcada) :- retract(calcada_p2('P2')), assertz(calcada_p2('  ')).
pega_p(p2,hub) :- retract(hub_p2('P2  ')), assertz(hub_p2('    ')).
pega_p(p2,quarto1) :- retract(quarto1_p2('P2 ')) , assertz(quarto1_p2('   ')).
pega_p(p2,quarto2) :- retract(quarto2_p2('P2 ')), assertz(quarto2_p2('   ')).
pega_p(p2,banheiro) :- retract(banheiro_p2('P2 ')), assertz(banheiro_p2('   ')). 
pega_p(p2,cozinha) :- retract(cozinha_p2('P2 ')), assertz(cozinha_p2('   ')).

pega_p(p3,calcada) :- retract(calcada_p3('P3')), assertz(calcada_p3('  ')).
pega_p(p3,hub) :- retract(hub_p3('P3  ')), assertz(hub_p3('    ')).
pega_p(p3,quarto1) :- retract(quarto1_p3('P3 ')), assertz(quarto1_p3('   ')).
pega_p(p3,quarto2) :- retract(quarto2_p3('P3 ')), assertz(quarto2_p3('   ')).
pega_p(p3,banheiro) :- retract(banheiro_p3('P3 ')), assert(banheiro_p3('   ')).
pega_p(p3,cozinha) :- retract(cozinha_p3('P3 ')), assertz(cozinha_p3('   ')).

pega_p(p4,calcada) :- retract(calcada_p4('P4')), assertz(calcada_p4('  ')).
pega_p(p4,hub) :- retract(hub_p4('P4  ')), assertz(hub_p4('    ')).
pega_p(p4,quarto1) :- retract(quarto1_p4('P4 ')), assertz(quarto1_p4('   ')).
pega_p(p4,quarto2) :- retract(quarto2_p4('P4 ')), assertz(quarto2_p4('   ')).
pega_p(p4,banheiro) :- retract(banheiro_p4('P4 ')), assertz(banheiro_p4('   ')).
pega_p(p4,cozinha) :- retract(cozinha_p4('P4 ')), assertz(cozinha_p4('   ')).

pega_p(p5,calcada) :- retract(calcada_p5('P5')), assertz(calcada_p5('  ')).
pega_p(p5,hub) :- retract(hub_p5('P5  ')), assertz(hub_p5('    ')).
pega_p(p5,quarto1) :- retract(quarto1_p5('P5 ')), assertz(quarto1_p5('   ')).
pega_p(p5,quarto2) :- retract(quarto2_p5('P5 ')), assertz(quarto2_p5('   ')).
pega_p(p5,banheiro) :- retract(banheiro_p5('P5 ')), assertz(banheiro_p5('   ')).
pega_p(p5,cozinha) :- retract(cozinha_p5('P5 ')), assertz(cozinha_p5('   ')).

pega_p(p6,calcada) :- retract(calcada_p6('P6')), assertz(calcada_p6('  ')).
pega_p(p6,hub) :- retract(hub_p6('P6|==')) , assertz(hub_p6('  |==')).
pega_p(p6,quarto1) :- retract(quarto1_p6('P6|     ')), assertz(quarto1_p6('  |     ')).
pega_p(p6,quarto2) :- retract(quarto2_p6('P6|')), assertz(quarto2_p6('  |')).
pega_p(p6,banheiro) :- retract(banheiro_p6('P6|     ')), assertz(banheiro_p6('  |     ')).
pega_p(p6,cozinha) :- retract(cozinha_p6('P6|    ')), assertz(cozinha_p6('  |    ')).


solta_p(p1,calcada) :- assertz(calcada_p1('P1')) , retract(calcada_p1('  ')).
solta_p(p1,hub) :- assertz(hub_p1('Calcada|====|P1  ')), retract(hub_p1('Calcada|====|    ')).
solta_p(p1,quarto1) :- assertz(quarto1_p1('                |P1 ')), retract(quarto1_p1('                |   ')).
solta_p(p1,quarto2) :- assertz(quarto2_p1('==|P1 ')), retract(quarto2_p1('==|   ')).
solta_p(p1,banheiro) :- assertz(banheiro_p1('                |P1 ')), retract(banheiro_p1('                |   ')).
solta_p(p1,cozinha) :- assertz(cozinha_p1('                |P1 ')), retract(cozinha_p1('                |   ')).


solta_p(p2,calcada) :- assertz(calcada_p2('P2')), retract(calcada_p2('  ')).
solta_p(p2,hub) :- assertz(hub_p2('P2  ')), retract(hub_p2('    ')).
solta_p(p2,quarto1) :- assertz(quarto1_p2('P2 ')) , retract(quarto1_p2('   ')).
solta_p(p2,quarto2) :- assertz(quarto2_p2('P2 ')), retract(quarto2_p2('   ')).
solta_p(p2,banheiro) :- assertz(banheiro_p2('P2 ')), retract(banheiro_p2('   ')).
solta_p(p2,cozinha) :- assertz(cozinha_p2('P2 ')), retract(cozinha_p2('   ')).

solta_p(p3,calcada) :- assertz(calcada_p3('P3')), retract(calcada_p3('  ')).
solta_p(p3,hub) :- assertz(hub_p3('P3  ')), retract(hub_p3('    ')).
solta_p(p3,quarto1) :- assertz(quarto1_p3('P3 ')), retract(quarto1_p3('   ')).
solta_p(p3,quarto2) :- assertz(quarto2_p3('P3 ')), retract(quarto2_p3('   ')).
solta_p(p3,banheiro) :- assertz(banheiro_p3('P3 ')), retract(banheiro_p3('   ')).
solta_p(p3,cozinha) :- assertz(cozinha_p3('P3 ')), retract(cozinha_p3('   ')).

solta_p(p4,calcada) :- assertz(calcada_p4('P4')), retract(calcada_p4('  ')).
solta_p(p4,hub) :- assertz(hub_p4('P4  ')), retract(hub_p4('    ')).
solta_p(p4,quarto1) :- assertz(quarto1_p4('P4 ')), retract(quarto1_p4('   ')).
solta_p(p4,quarto2) :- assertz(quarto2_p4('P4 ')), retract(quarto2_p4('   ')).
solta_p(p4,banheiro) :- assertz(banheiro_p4('P4 ')), retract(banheiro_p4('   ')).
solta_p(p4,cozinha) :- assertz(cozinha_p4('P4 ')), retract(cozinha_p4('   ')).

solta_p(p5,calcada) :- assertz(calcada_p5('P5')), retract(calcada_p5('  ')).
solta_p(p5,hub) :- assertz(hub_p5('P5  ')), retract(hub_p5('    ')).
solta_p(p5,quarto1) :- assertz(quarto1_p5('P5 ')), retract(quarto1_p5('   ')).
solta_p(p5,quarto2) :- assertz(quarto2_p5('P5 ')), retract(quarto2_p5('   ')).
solta_p(p5,banheiro) :- assertz(banheiro_p5('P5 ')), retract(banheiro_p5('   ')).
solta_p(p5,cozinha) :- assertz(cozinha_p5('P5 ')), retract(cozinha_p5('   ')).

solta_p(p6,calcada) :- assertz(calcada_p6('P6')), retract(calcada_p6('  ')).
solta_p(p6,hub) :- assertz(hub_p6('P6|==')) , retract(hub_p6('  |==')).
solta_p(p6,quarto1) :- assertz(quarto1_p6('P6|     ')), retract(quarto1_p6('  |     ')).
solta_p(p6,quarto2) :- assertz(quarto2_p6('P6|')), retract(quarto2_p6('  |')).
solta_p(p6,banheiro) :- assertz(banheiro_p6('P6|     ')), retract(banheiro_p6('  |     ')).
solta_p(p6,cozinha) :- assertz(cozinha_p6('P6|    ')), retract(cozinha_p6('  |    ')).






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
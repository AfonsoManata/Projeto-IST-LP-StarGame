% lp24 - ist1113406 - projecto 
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Atenção: nao deves copiar nunca os puzzles para o teu ficheiro de código
% Nao remover nem modificar as linhas anteriores. Obrigado.
% Segue-se o código
%%%%%%%%%%%%
% Afonso Manata 113406



%                            #####         #
%                           #            # # 
%                           #####       #  #    
%                               #          #
%                           #####   .      #



/*
    isList(Lista). - Auxiliar
    
    Devolve Verdadeiro se Lista for uma lista válida e False caso contrário. 

    param Lista - A lista de elementos a que vai ser verificada.
    
*/

is_list([]).

is_list([_|T]):-
    is_list(T).


/*
    visualiza(Lista)
    
    Verdadeiro se Lista for uma lista válida. Este predicado exibe cada elemento 
    da lista em uma nova linha.

    param Lista - A lista de elementos a que vai ser verificada e exibida.
    
*/

visualiza([]).

visualiza([H|T]):-
    
    is_list([H|T]),
    writeln(H),
    visualiza(T).


/*
    visualizaLinha(Lista)
    
    Verdadeiro se Lista for uma lista válida. Este predicado exibe cada elemento 
    da lista em uma nova linha, aparecendo antes o número da linha em causa, um 
    ":" e um espaço.

    param Lista - A lista de elementos a que vai ser verificada e exibida.
    
*/

visualizaLinha(Lista):-
    
    is_list(Lista),
    visualizaLinha_iter(Lista,1).

visualizaLinha_iter([], _).

visualizaLinha_iter([H|T],Count):- % Dar print a cada elemento da lista
    
    write(Count), 
    write(': '),
    write(H), 
    
    nl,
    New_Count is Count + 1,
    visualizaLinha_iter(T,New_Count).



%                            #####         ###
%                           #            #    # 
%                           #####           ##    
%                               #          #
%                           #####   .    #####



/*
    in_tabuleiro((L, C), Tabuleiro) - Auxiliar
    
    Devolve Verdadeiro se as coordenadas pertencem ao tabuleiro e False caso 
    contrário.
    
    param (L,C) - Coordenadas que vão ser verificadas
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    
*/

in_tabuleiro((L,C), [H|T]):- % Verificar se o ponto está dentro do tabuleiro
    
    length([H|T], Linhas),
    L > 0,
    L =< Linhas,
    
    length(H, Colunas),
    C > 0,
    C =< Colunas.


/*
    insereObjecto((L, C), Tabuleiro, Obj)
    
    Devolve Verdadeiro se Tabuleiro é um tabuleiro que após a aplicação deste 
    predicado passa a ter o Obj nas coordenadas (L,C), caso nestas
    se encontre uma variável.

    param (L,C) - Coordenadas onde se pretende meter o objeto
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param Obj - Objeto que se pretende meter nas coordenadas do tabuleiro 
    
*/

insereObjecto((L, C), Tabuleiro, Obj) :-
    
    in_tabuleiro((L, C), Tabuleiro),            
    
    nth1(L, Tabuleiro, Linha), % O nth1/3 permite nos ir aos elementos necessários
    nth1(C, Linha, Elemento),        
    
    var(Elemento),
    Elemento = Obj,!.                      

% Caso nenhuma das situações aconteça ele não faz nada
insereObjecto(_, _, _).


/*
    insereVariosObjectos(ListaCoords, Tabuleiro, ListaObjs)
    
    Devolve Verdadeiro se Tabuleiro é um tabuleiro que após a aplicação deste 
    predicado passa a ter os elementos da ListaObjs nas posiões com as coordenadas
    da ListaCoords, caso nestas se encontre uma variável.

    param ListaCoords - Lista com as coordenadas onde se pretende meter o objeto
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaObjs - Lista com os objetos que se pretende meter nas 
    coordenadas do tabuleiro 
    
*/

insereVariosObjectos(ListaCoords, Tabuleiro, ListaObjs):-
    
    length(ListaCoords, Size1),
    length(ListaObjs, Size2),
    
    Size1 =:= Size2,
    insereObjecto_iter(ListaCoords, Tabuleiro,ListaObjs),!.
    

insereObjecto_iter([],_,[]).
    
insereObjecto_iter([H|T], Tabuleiro, [H2|T2]):-
    
    insereObjecto(H, Tabuleiro, H2),
    insereObjecto_iter(T, Tabuleiro, T2).
    
    
/*
    somaCoor((L, C), (L1, C1), Res)
    
    Devolve Verdadeiro se Res é um tuplo que consiste do resultado entre a soma
    das coordenadas.

    param (L,C) - Coordenadas que vão ser somadas
    param (L1,C1) - Coordenadas que vão ser somadas


*/

somaCoor((L, C), (L1, C1), (L2, C2)):-
    
    L2 is L + L1,
    C2 is C + C1.


/*
    inserePontosVolta(Tabuleiro, (L, C))
    
    Devolve Verdadeiro se Tabuleiro é um tabuleiro que após a aplicação deste 
    predicado passa a ter pontos(p) à volta das coordenadas (L,C).

    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param (L,C) - Coordenadas onde se pretende meter à volta pontos

*/

inserePontosVolta(Tabuleiro, (L, C)):-
    
    % Vetores que somados aos pontos permitem obter os pontos a volta
    ListaVetores = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0, 1), (1, 1)],
    Lista_Pontos = [p, p, p, p, p, p, p, p],
    
    maplist(somaCoor((L,C)), ListaVetores, CoorAVolta),
    insereVariosObjectos(CoorAVolta, Tabuleiro, Lista_Pontos),!.


/*
    inserePontos(Tabuleiro, ListaCoords)
    
    Devolve Verdadeiro se Tabuleiro é um tabuleiro que após a aplicação deste 
    predicado passa a ter pontos nas posiões com as coordenadas da ListaCoords,
    caso nestas se encontre uma variável.

    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaCoords - Lista com as coordenadas onde se pretende meter o objeto
    
*/

meter_pontos(_, p).

inserePontos(Tabuleiro, ListaCoords):-
    
    length(ListaCoords, Size),
    length(Lista_Pontos, Size),
    
    maplist(meter_pontos, Lista_Pontos, Lista_Pontos_Completa),
    insereVariosObjectos(ListaCoords, Tabuleiro, Lista_Pontos_Completa).



%                            #####        ####
%                           #                # 
%                           #####         ####    
%                               #            #
%                           #####   .     ####



/*
    objectosEmCoordenadas(ListaCoords, Tabuleiro, ListaObjs)
    
    Devolve Verdadeiro se ListaObjs for a lista de objetos das coordenadas da
    ListaCoords no Tabuleiro, na mesma ordem das coordenadas.

    param ListaCoords - Lista com as coordenadas com os objetos
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaObjs - Lista onde se vai meter os objetos
    
*/

objectosEmCoordenadas([], _, []).

objectosEmCoordenadas([(L,C)|T], Tabuleiro, [Elemento|T1]):-
    
    nth1(L, Tabuleiro, Linha), 
    nth1(C, Linha, Elemento), 
    objectosEmCoordenadas(T, Tabuleiro, T1).


/*
    coordObjectos(Objecto, Tabuleiro, ListaCoords, ListaCoordObjs, NumObjectos)
    
    Devolve Verdadeiro se Tabuleiro é um tabuleiro, ListaCoords uma lista de 
    coordenadas dos objectos do tipo Objecto, tal como ocorrem no tabuleiro 
    (o comportamento deverá ser o mesmo se o objecto pesquisado for uma variável).
    NumObjectos é o número de objectos Objecto encontrados.
    ListaCoordObjs deve estar ordenada por linha e coluna 

    param Objecto - Objecto que queremos procurar
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaCoords - Coordenadas onde vamos procurar o objeto
    param ListaCoordObjs - Lista com as coordenadas que têm o objeto 
    param NumObjectos - Numero de Objetos encontrados

*/

coordObjectos(_,_,[],[],0).

% Caso em que procuramos as variaveis
coordObjectos(Objecto, Tabuleiro, [(L,C)|T], [(L,C)|T1], NumObjectos):-
    
    nth1(L, Tabuleiro, Linha), 
    nth1(C, Linha, Elemento),
    
    var(Objecto),
    var(Elemento), % Verifica se o Elemento é uma variável tal como o Objeto  
    
    coordObjectos(Objecto, Tabuleiro, T, T1, NumObjectos_aux),
    NumObjectos is NumObjectos_aux + 1, !.

% Caso em que nao sao variaveis o que procuramos
coordObjectos(Objecto, Tabuleiro, [(L,C)|T], [(L,C)|T1], NumObjectos):-
    
    nth1(L, Tabuleiro, Linha), 
    nth1(C, Linha, Elemento),    
    
    Elemento == Objecto,
    coordObjectos(Objecto, Tabuleiro, T, T1, NumObjectos_aux),
    NumObjectos is NumObjectos_aux + 1, !.


% Caso em que nao e igual ao que procuramos
coordObjectos(Objecto, Tabuleiro, [_|T], T1, NumObjectos):-
    
    coordObjectos(Objecto, Tabuleiro, T, T1, NumObjectos), !.


/*
    coordenadasVars(Tabuleiro, ListaVars)
    
    Devolve Verdadeiro se ListaVars forem as coordenadas das variáveis do Tabuleiro.
    
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaVars - Lista com as coordenadas das variáveis do tabuleiro
    
*/

% Cria todas as coordenadas do tab

todasCoordenadas(Tabuleiro, Coordenadas_Tab) :-     
    findall((L, C),(nth1(L, Tabuleiro, Linha), nth1(C, Linha, _)),Coordenadas_Tab).

% Predicado principal

coordenadasVars(Tabuleiro, Coordenadas):-  
    todasCoordenadas(Tabuleiro, Coordenadas_Tab),
    coordObjectos(_, Tabuleiro, Coordenadas_Tab, Coordenadas, _).



%                            #####         #
%                           #             ##
%                           #####        # #
%                               #       #####
%                           #####   .      #



/*
    fechaListaCoordenadas(Tabuleiro, ListaCoord)
    
    Devolve Verdadeiro se Tabuleiro for um Tabuleiro e ListaCoord for uma lista 
    de coordenadas que após a aplicação deste predicado as coordenas dessa lista
    vão ser apenas estrelas e pontos. Considerando as hipoteses dos enunciados.
    
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaCoord - Lista com as coordenadas que vão ser ou não fechadas

*/

fechaListaCoordenadas(_, []). 

fechaListaCoordenadas(Tabuleiro, ListaCoord):-
    
    coordObjectos(e, Tabuleiro, ListaCoord, ListaCoorEstrela, _),
    coordObjectos(_, Tabuleiro, ListaCoord, ListaCoorVar, _),
    
    length(ListaCoorEstrela, Size1),     
    
    Size1 =:= 2,  % h1 condição do enunciado
    inserePontos(Tabuleiro, ListaCoorVar), !.


fechaListaCoordenadas(Tabuleiro, ListaCoord):-
    coordObjectos(e, Tabuleiro, ListaCoord, ListaCoorEstrela, _),
    coordObjectos(_, Tabuleiro, ListaCoord, ListaCoorVar, _),
    
    length(ListaCoorEstrela, Size1), 
    length(ListaCoorVar, Size2),

    Size1 =:= 1,  % h2 condição do enunciado
    Size2 =:= 1,

    nth1(1, ListaCoorVar, Coord), % Ir buscar a coordenada do ponto vazio
    insereObjecto(Coord, Tabuleiro, e),
    inserePontosVolta(Tabuleiro, Coord), !.


fechaListaCoordenadas(Tabuleiro, ListaCoord):-
    coordObjectos(e, Tabuleiro, ListaCoord, ListaCoorEstrela, _),
    coordObjectos(_, Tabuleiro, ListaCoord, ListaCoorVar, _),
    
    length(ListaCoorEstrela, Size1), 
    length(ListaCoorVar, Size2),

    Size1 =:= 0,  % h3 condição do enunciado
    Size2 =:= 2,

    insereVariosObjectos(ListaCoorVar, Tabuleiro, [e, e]),

    nth1(1, ListaCoorVar, Coord),
    nth1(2, ListaCoorVar, Coord2),
    inserePontosVolta(Tabuleiro, Coord),
    inserePontosVolta(Tabuleiro, Coord2), !.

fechaListaCoordenadas(_, _).


/*
    fecha(Tabuleiro, ListaListasCoord)
    
    Devolve Verdadeiro se Tabuleiro for um Tabuleiro e ListaListasCoord for uma
    lista de listas de coordenadas que após a aplicação deste predicado 
    as coordenas dessa lista vão ser apenas estrelas e pontos.
    Considerando as hipoteses dos enunciados.
    
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaCoord - Lista de Listas de coordenadas que pretendemos fechar

*/

fecha(_, []).

fecha(Tabuleiro, [H|T]):-
    
    fechaListaCoordenadas(Tabuleiro, H),
    fecha(Tabuleiro, T), !.


/*
    encontraSequencia(Tabuleiro, N, ListaCoord, Seq)
    
    Devolve Verdade se Tabuleiro for um tabuleiro, ListaCoords
    for uma lista de coordenadas e N o tamanho de Seq, que e uma sublista de 
    ListaCoords (portanto, com as coordenadas na mesma ordem) que verifica o 
    seguinte: As coordenadas representam posicoes com variaveis, as suas 
    coordenadas aparecem seguidas numa linha coluna ou regiao, Seq pode ser 
    concatenada com duas listas para chegar a ListaCoord.

    
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param N - Numero de elementos da Seq
    param ListaCoord - Lista inicial das coordenadas
    param Seq - Sublista de ListaCoord

*/

encontraSequencia(Tabuleiro, N, ListaCoord, Seq):-
    
    coordObjectos(_, Tabuleiro, ListaCoord, Seq, N1),
    N =:= N1,
    !.


/*
    aplicaPadraoI(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3)])
    
    Devolve Verdade se Tabuleiro for um tabuleiro for um tabuleiro e 
    [(L1, C1), (L2, C2), (L3, C3)] for uma lista de coordenadas. Apos a aplicacao
    deste prediicado , tabuleiro sera o resultado de colocar uma estrela em 
    (L1, C1) e (L3, C3) e os pontos a volta de cada estrela.
    
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param [(L1, C1), (L2, C2), (L3, C3)] - Lista de Coordenadas

*/

aplicaPadraoI(Tabuleiro, [(L1, C1), _, (L3, C3)]):-
    
    insereVariosObjectos([(L1, C1), (L3, C3)], Tabuleiro, [e, e]),
    inserePontosVolta(Tabuleiro, (L1, C1)),
    inserePontosVolta(Tabuleiro, (L3, C3)). 


/*
    aplicaPadroes(Tabuleiro, ListaListaCoords)
    
    Devolve Verdade se Tabuleiro for um tabuleiro for um tabuleiro e 
    ListaListaCoords for uma lista de listas de coordenadas. Apos a aplicacao
    deste prediicado , tabuleiro sera o resultado de aplicar o aplicapadrao a 
    cada uma das listas de coordenadas.
    
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo
    param ListaListaCoords - Lista de Listas de Coordenadas

*/

aplicaPadroes(_,[]):- !.

aplicaPadroes(Tabuleiro,[H|T]):- % Caso em que e uma sequencia de 3
  
  length(H,3),
  coordObjectos(e,Tabuleiro,H,_,0),
  
  aplicaPadraoI(Tabuleiro,H), !,
  aplicaPadroes(Tabuleiro,T).

aplicaPadroes(Tabuleiro,[H|T]):- % Caso em que e uma sequencia de 4
  
  length(H,4),
  coordObjectos(e,Tabuleiro,H,_,0),
  
  aplicaPadraoT(Tabuleiro,H), !,
  aplicaPadroes(Tabuleiro,T).

aplicaPadroes(Tabuleiro, [_|T]):-
  
  aplicaPadroes(Tabuleiro,T).



%                            #####        #####
%                           #            #
%                           #####        #####
%                               #            #
%                           #####   .    #####



/*
    resolve(Estruturas, Tabuleiro)
    
    Devolve Verdadeiro se Estrutura for uma estrutura e Tabuleiro for um tabuleiro
    que resulta de aplicar os predicados aplicaPadroes/2 e fecha/2 ate ja nao
    haver mais alteracao nas variaveis do tabuleiro.
    
    param Estruturas - Estruturas do tabuleiro
    param Tabuleiro - Tabuleiro que representa o estado atual do jogo

*/

% Caso em que ja chegamos ao limite de alteracoes
resolve(Estruturas, Tabuleiro):-
    
    coordTodas(Estruturas, Coor),
    coordenadasVars(Tabuleiro, ListaVars),
    
    aplicaPadroes(Tabuleiro, Coor),
    fecha(Tabuleiro, Coor),
    
    coordenadasVars(Tabuleiro, ListaVars2),
    ListaVars = ListaVars2,!.

% Caso em que ainda ha coisas para alterar
resolve(Estruturas, Tabuleiro):-
    
    coordTodas(Estruturas, Coor),
    aplicaPadroes(Tabuleiro, Coor),
    fecha(Tabuleiro, Coor),

    resolve(Estruturas, Tabuleiro).




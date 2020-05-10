:- module(proylcc,
	[
		emptyBoard/1,
		goMove/4
	]).
emptyBoard([
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
       ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"]
       ]).

opuesto("b","w").
opuesto("w","b").
adyacente([X,Y],[X1,Y]):-X1 is X-1,X1>=0.
adyacente([X,Y],[X1,Y]):-X1 is X+1,X1<19.
adyacente([X,Y],[X,Y1]):-Y1 is Y-1,Y1>=0.
adyacente([X,Y],[X,Y1]):-Y1 is Y+1,Y1<19.


adyacentes(Pos,Ads):-
    findall(Ad,adyacente(Pos,Ad),Ads).


recuperar(Tablero,[X,Y],Color):-
    iesimo(Tablero,X,Fila),
    iesimo(Fila,Y,Color).


iesimo([X|_Xs],0,X).

iesimo([_X|Xs],I,E):-
    J is I-1,iesimo(Xs,J,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% goMove(+Board, +Player, +Pos, -RBoard)
%
% RBoard es la configuración resultante de reflejar la movida del jugador Player
% en la posición Pos a partir de la configuración Board.

goMove(Board, Player, [R,C], NewBoard):-
    adyacentes([R,C],Ads),
    opuesto(Player,ColorOp),
    replace(Row, R, NRow, Board, RBoard),
    replace("-", C, Player, Row, NRow),
    eliminarAds(RBoard,ColorOp,Player,Ads,NewBoard),
    not(encerrada(NewBoard,[R,C],Player,ColorOp,[],_NoImporta)).



eliminarAds(Tab,_Op,_Player,[],Tab).

eliminarAds(Tab,Op,Player,[Ad|Ads],NewTab):-
    encerrada(Tab,Ad,Op,Player,[],AEliminar),
    sort(AEliminar,SinDup),
    eliminarAds(Tab,Op,Player,Ads,AuxTab),
    eliminarVarias(AuxTab,Op,SinDup,NewTab).

eliminarAds(Tab,Op,Player,[_Ad|Ads],NewTab):-
    eliminarAds(Tab,Op,Player,Ads,NewTab).



eliminarVarias(Tab,_Color,[],Tab).
eliminarVarias(Tab,Color,[Pos|AEliminar],NewTab):-
    eliminarVarias(Tab,Color,AEliminar,AuxTab),
    eliminar(AuxTab,Color,Pos,NewTab).


eliminar(Board,Player,[R,C],RBoard):-
    replace(Row, R, NRow, Board, RBoard),
    replace(Player, C, "-", Row, NRow).



encerrada(Tab,Pos,_ColorAct,ColorEncierra,_Vis,[Pos]):-
    adyacentes(Pos,Ads),
    forall(member(Ad,Ads),(recuperar(Tab,Ad,Color),Color=ColorEncierra)).

encerrada(Tab,Pos,ColorAct,ColorEncierra,Visitados,Encerrados):-
    adyacentes(Pos,Ads),
    forall(member(Ad,Ads),(recuperar(Tab,Ad,Color),(Color=ColorEncierra;Color=ColorAct))),
    findall(Ficha,(member(Ficha,Ads),recuperar(Tab,Ficha,ColorAct),not(member(Ficha,Visitados))),Eliminables),
    checkEncerradas(Tab,Eliminables,ColorAct,ColorEncierra,[Pos|Visitados],Encerrados).

checkEncerradas(_Tab,[],_ColorAct,_ColorEncierra,Vis,Vis).
checkEncerradas(Tab,[Ad|Ads],ColorAct,ColorEncierra,Vis,Encer):-
    encerrada(Tab,Ad,ColorAct,ColorEncierra,Vis,VisAux),
    checkEncerradas(Tab,Ads,ColorAct,ColorEncierra,VisAux,Encer).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).



finDelJuego(Tablero,0,0):-emptyBoard(Tablero).

finDelJuego(Tablero,PuntajeB,PuntajeW):-
    contar(Tablero,18,"b",[],TerritorioNegro,PuntajeB),
    contar(Tablero,18,"w",TerritorioNegro,_TerritorioBlanco,PuntajeW).



contar(Tablero,0,Color,Visitados,AVisitar,Cant):-
    contarEnFila(Tablero,0,18,Color,Visitados,AVisitar,Cant).

contar(Tablero,Fila,Color,Visitados,AVisitar,Cant):-
    FilaAnterior is Fila-1,
    contarEnFila(Tablero,Fila,18,Color,Visitados,VisitFila,Cant1),
    contar(Tablero,FilaAnterior,Color,VisitFila,AVisitar,Cant2),
    Cant is (Cant1+Cant2).



contarEnFila(_Tablero,Fila,0,_Color,Visitados,Visitados,0):-
     member([Fila,0],Visitados).

contarEnFila(Tablero,Fila,0,Color,Visitados,[[Fila,0]|Visitados],1):-
    recuperar(Tablero,[Fila,0],ColorPos),
    ColorPos=Color.


contarEnFila(Tablero,Fila,0,Color,Visitados,AVisitar,Cant):-
    encerrada(Tablero,[Fila,0],"-",Color,[],Encerradas),
    sort(Encerradas,EncerradasSinDuplicado),
    append(Visitados,EncerradasSinDuplicado,AVisitar),
    length(EncerradasSinDuplicado,Cant).


contarEnFila(_Tablero,_Fila,0,_Color,Visitados,Visitados,0).

contarEnFila(Tablero,Fila,Columna,Color,Visitados,AVisitar,Cant):-
    ColumnaAnterior is Columna-1,member([Fila,Columna],Visitados),
    contarEnFila(Tablero,Fila,ColumnaAnterior,Color,Visitados,AVisitar,Cant).

contarEnFila(Tablero,Fila,Columna,Color,Visitados,AVisitar,Cant):-
    ColumnaAnterior is Columna-1,
    (recuperar(Tablero,[Fila,Columna],ColorPos),ColorPos=Color),
    contarEnFila(Tablero,Fila,ColumnaAnterior,Color,[[Fila,Columna]|Visitados],AVisitar,CantAux),
    Cant is CantAux+1.

contarEnFila(Tablero,Fila,Columna,Color,Visitados,AVisitar,Cant):-
    ColumnaAnterior is Columna-1,
    encerrada(Tablero,[Fila,Columna],"-",Color,[],Encerradas),
    sort(Encerradas,EncerradasSinDup),
    append(Visitados,EncerradasSinDup,VisYEncerr),
    contarEnFila(Tablero,Fila,ColumnaAnterior,Color,VisYEncerr,AVisitar,CantAux),
    length(EncerradasSinDup,Size),
    Cant is CantAux+Size.

contarEnFila(Tablero,Fila,Columna,Color,Visitados,AVisitar,Cant):-
    ColumnaAnterior is Columna-1,
    contarEnFila(Tablero,Fila,ColumnaAnterior,Color,Visitados,AVisitar,Cant).



















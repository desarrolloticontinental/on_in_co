/* Movimiento de apertura */
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.

DEF VAR x-Fecha-Corte AS DATE NO-UNDO.

x-Fecha-Corte = DATE(01,01,2010).

DEF VAR x-CtoUni AS DEC NO-UNDO.

FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia:
    /* Buscamos el costo unitario */
    x-CtoUni = 0.
    FIND LAST AlmStkge WHERE AlmStkge.CodCia = Almmmatg.CodCia AND
        AlmStkge.codmat = Almmmatg.CodMat AND
        AlmStkge.Fecha < x-Fecha-Corte
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE AlmStkGe THEN NEXT.    /* NO tiene movimientos antigüos */
    ASSIGN
        x-CtoUni = AlmStkge.CtoUni.
    /* Barremos cada almacén */
    FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-CodCia:
        FIND FIRST AlmStkal WHERE AlmStkal.CodCia = Almmmatg.CodCia AND
            AlmStkal.CodAlm = Almacen.CodAlm AND
            AlmStkal.codmat = Almmmatg.CodMat AND
            AlmStkal.Fecha < x-Fecha-Corte 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE AlmStkAl THEN NEXT. /* NO hay movimientos */
            AlmStkal.StkAct
    END.
END.


/* APERTURA DE MOVIMIENTOS DE ALMACEN */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codmov AS INT INIT 00 NO-UNDO.
DEF VAR x-corte AS DATE NO-UNDO.

DEF TEMP-TABLE t-dmov LIKE almdmov
    INDEX Llave01 AS PRIMARY codcia codalm codmat.

ASSIGN
    x-Corte = DATE(12,31,2008).

/* Movimiento por Almacen */
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia:
    DISPLAY Almmmatg.codmat.
    PAUSE 0.
    FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia:
        FIND LAST Almstkal WHERE Almstkal.codcia = s-codcia
            AND Almstkal.codalm = Almacen.codalm
            AND Almstkal.codmat = Almmmatg.codmat
            AND AlmStkal.Fecha <= x-Corte
            NO-LOCK NO-ERROR.
        IF AVAILABLE AlmStkAl THEN DO:   /* Registramos movimiento */
            CREATE t-dmov.
            ASSIGN
                t-dmov.codcia = s-codcia
                t-dmov.codalm = Almacen.codalm
                t-dmov.codmat = Almmmatg.codmat
                t-dmov.preuni = AlmStkal.CtoUni 
                t-dmov.candes = AlmStkal.StkAct
                t-dmov.factor = 1.
        END.
    END.
END.

OUTPUT TO c:\tmp\corte.txt.
FOR EACH t-dmov WHERE t-dmov.candes <> 0:
    DISPLAY
        t-dmov.codalm
        t-dmov.codmat
        t-dmov.preuni
        t-dmov.candes
        WITH STREAM-IO NO-BOX WIDTH 100.
END.



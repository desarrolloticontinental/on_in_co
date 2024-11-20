/* Migrar productos a tiendas UTILEX */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-fching AS DATE NO-UNDO.
x-fching = DATE(01,11,2018).
FOR EACH almmmatg NO-LOCK WHERE codcia = s-codcia AND
    fching >= x-fching:
    DISPLAY almmmatg.codmat. PAUSE 0.
    FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
        FIRST gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND
        gn-divi.coddiv = almacen.coddiv AND
        GN-DIVI.CanalVenta = "MIN",
        EACH almmmate NO-LOCK WHERE almmmate.codcia = s-codcia AND
        almmmate.codalm = almacen.codalm AND
        almmmate.codmat = almmmatg.codmat:
        RUN replica.
    END.
END.


PROCEDURE replica:

    {rpl/reptrig.i
        &Table  = almmmate
        &Key    =  "string(almmmate.codcia,'999') + string(almmmate.codalm,'x(5)') + string(almmmate.codmat,'x(6)')"
        &Prg    = r-almmmate
        &Event  = WRITE
        &FlgDB0 = TRUE
        &FlgDB1 = TRUE
        &FlgDB2 = TRUE
        &FlgDB3 = TRUE
        &FlgDB4 = TRUE
        &FlgDB5 = TRUE
        &FlgDB6 = TRUE
        &FlgDB7 = TRUE
        &FlgDB8 = TRUE
        &FlgDB9 = TRUE
        &FlgDB10 = TRUE
        &FlgDB11 = TRUE
        &FlgDB12 = TRUE
        &FlgDB13 = TRUE
        &FlgDB14 = TRUE
        &FlgDB15 = TRUE
        &FlgDB16 = TRUE
        &FlgDB17 = TRUE
        &FlgDB18 = TRUE
        &FlgDB19 = TRUE
        &FlgDB20 = NO
        &FlgDB30 = TRUE     /* NO por ahora */
        }


END PROCEDURE.

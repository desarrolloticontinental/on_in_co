DEF VAR s-codcia AS INTE INIT 001.
DEF VAR x-Fecha-1 AS DATE NO-UNDO.
DEF VAR x-Fecha-2 AS DATE NO-UNDO.
DEF VAR x-Cantidad AS DECI NO-UNDO.

x-Fecha-1 = DATE(01,01,2023).
x-Fecha-2 = DATE(06,30,2023).

DEF TEMP-TABLE Detalle NO-UNDO
    FIELD coddoc AS CHAR FORMAT 'x(3)' LABEL 'Doc'
    FIELD nroped AS CHAR FORMAT 'x(15)' LABEL 'Pedido'
    FIELD nrodoc AS CHAR FORMAT 'x(15)' LABEL 'Nro Doc'
    FIELD fchdoc AS DATE FORMAT '99/99/9999' LABEL 'Fecha'
    FIELD codcli AS CHAR FORMAT 'x(15)' LABEL 'Cliente'
    FIELD sede   AS CHAR FORMAT 'x(15)' LABEL 'Cod Local'
    FIELD nomcli AS CHAR FORMAT 'x(80)' LABEL 'Descripcion cliente'
    FIELD codmat AS CHAR FORMAT 'x(10)' LABEL 'Cod articulo'
    FIELD desmat AS CHAR FORMAT 'x(80)' LABEL 'Descripcion articulo'
    FIELD candes AS DECI FORMAT '>>>,>>9.99' LABEL 'Cantidad'
    FIELD master AS DECI FORMAT '>>>,>>9.99' LABEL 'Cantidad master'
    FIELD codalm AS CHAR FORMAT 'x(5)' LABEL 'Almacen origen'
    .

/* OTR */
FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
    Faccpedi.coddoc = "OTR" AND
    Faccpedi.flgest <> "A" AND
    Faccpedi.fchped >= x-Fecha-1 AND
    Faccpedi.fchped <= x-Fecha-2,
    EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
    x-Cantidad = (Facdpedi.canped * Facdpedi.factor).
    CREATE detalle.
    ASSIGN
        detalle.coddoc = faccpedi.coddoc
        detalle.nroped = faccpedi.nroped
        detalle.codcli = faccpedi.codcli
        detalle.sede   = faccpedi.codcli
        detalle.nomcli = faccpedi.nomcli
        detalle.codmat = facdpedi.codmat
        detalle.desmat = almmmatg.desmat
        detalle.candes = x-Cantidad
        detalle.codalm = faccpedi.codalm
        .
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia AND
        almcmov.codref = faccpedi.coddoc AND
        almcmov.nroref = faccpedi.nroped AND
        almcmov.flgest <> 'A' AND
        almcmov.tipmov = 's':
        detalle.nrodoc = STRING(almcmov.nroser, '999') + STRING(almcmov.nrodoc,' 999999999').
        detalle.fchdoc = almcmov.fchdoc.
        LEAVE.
    END.
    ASSIGN
        detalle.master = TRUNCATE(x-Cantidad / almmmatg.canemp, 0).
END.

/* OD */
DEF BUFFER PEDIDO FOR Faccpedi.
FOR EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
    Faccpedi.coddoc = "O/D" AND
    Faccpedi.flgest <> "A" AND
    Faccpedi.fchped >= x-Fecha-1 AND
    Faccpedi.fchped <= x-Fecha-2,
    FIRST PEDIDO NO-LOCK WHERE PEDIDO.codcia = s-codcia AND
    PEDIDO.coddoc = Faccpedi.codref AND     /* PED */
    PEDIDO.nroped = Faccpedi.nroref,
    EACH Facdpedi OF Faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
    x-Cantidad = (Facdpedi.canped * Facdpedi.factor).
    CREATE detalle.
    ASSIGN
        detalle.coddoc = faccpedi.coddoc
        detalle.nroped = faccpedi.nroped
        detalle.codcli = faccpedi.codcli
        detalle.sede   = faccpedi.sede
        detalle.nomcli = faccpedi.nomcli
        detalle.codmat = facdpedi.codmat
        detalle.desmat = almmmatg.desmat
        detalle.candes = x-Cantidad
        detalle.codalm = faccpedi.codalm
        .
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
        ccbcdocu.codped = PEDIDO.coddoc AND
        ccbcdocu.nroped = PEDIDO.nroped AND
        ccbcdocu.flgest <> 'A' AND
        LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0:
        detalle.nrodoc = ccbcdocu.nrodoc.
        detalle.fchdoc = ccbcdocu.fchdoc.
        LEAVE.
    END.
    ASSIGN
        detalle.master = TRUNCATE(x-Cantidad / almmmatg.canemp, 0).
END.

OUTPUT TO d:\INFO.txt.
FOR EACH detalle NO-LOCK:
    DISPLAY detalle WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.
    PAUSE 0.
END.
OUTPUT CLOSE.


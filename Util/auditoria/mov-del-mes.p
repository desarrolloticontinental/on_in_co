DEF VAR s-codcia AS INT INIT 01 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00514' NO-UNDO.
DEF VAR x-fecha-1 AS DATE NO-UNDO.
DEF VAR x-fecha-2 AS DATE NO-UNDO.

ASSIGN
    x-fecha-1 = DATE(01,01,2017)
    x-fecha-2 = DATE(01,31,2017).

RUN comprobantes.
RUN stock-saldo-inicial.
RUN stock-saldo-final.
RUN mov-de-almacen.
RUN transferencias.
RUN cierre-de-caja.

PROCEDURE comprobantes:
/* ******************* */    
OUTPUT TO d:\tmp\comprobantes.txt.
PUT UNFORMATTED
    'CODIGO|NUMERO|FECHA|CLIENTE|NOMBRE|ESTADO|ARTICULO|CANTIDAD|UNIDAD'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddiv = s-coddiv
    AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,N/C') > 0
    AND ccbcdocu.fchdoc >= x-fecha-1
    AND ccbcdocu.fchdoc <= x-fecha-2,
    EACH ccbddocu OF ccbcdocu NO-LOCK:
    PUT UNFORMATTED
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.codcli '|'
        ccbcdocu.nomcli '|'
        ccbcdocu.flgest '|'
        ccbddocu.codmat '|'
        ccbddocu.candes '|'
        ccbddocu.undvta '|'
        SKIP.
END.
OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE stock-saldo-inicial:
/* ************************** */    
DEF VAR x-stkact LIKE almstkal.stkact NO-UNDO.

OUTPUT TO d:\tmp\saldo-inicial.txt.
PUT UNFORMATTED
    'ALMACEN|ARTICULO|UNIDAD|STOCK'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia
    AND almacen.coddiv = s-coddiv,
    EACH almmmate OF almacen NO-LOCK,
    FIRST almmmatg OF almmmate NO-LOCK:
    x-stkact = 0.
    FIND LAST almstkal OF almmmate WHERE almstkal.fecha < x-fecha-1 NO-LOCK NO-ERROR.
    IF AVAILABLE almstkal THEN x-stkact = almstkal.stkact.
    PUT UNFORMATTED
        almmmate.codalm '|'
        almmmate.codmat '|'
        almmmatg.undstk '|'
        x-stkact
        SKIP.
END.
OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE stock-saldo-final:
/* ************************ */    
DEF VAR x-stkact LIKE almstkal.stkact NO-UNDO.

OUTPUT TO d:\tmp\saldo-final.txt.
PUT UNFORMATTED
    'ALMACEN|ARTICULO|UNIDAD|STOCK'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia
    AND almacen.coddiv = s-coddiv,
    EACH almmmate OF almacen NO-LOCK,
    FIRST almmmatg OF almmmate NO-LOCK:
    x-stkact = 0.
    FIND LAST almstkal OF almmmate WHERE almstkal.fecha <= x-fecha-2 NO-LOCK NO-ERROR.
    IF AVAILABLE almstkal THEN x-stkact = almstkal.stkact.
    PUT UNFORMATTED
        almmmate.codalm '|'
        almmmate.codmat '|'
        almmmatg.undstk '|'
        x-stkact
        SKIP.
END.
OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE mov-de-almacen:
/* ********************** */    
OUTPUT TO d:\tmp\mov-de-almacen.txt.
PUT UNFORMATTED
    'ALMACEN|FECHA|TIP.MOV.|COD.MOV.|NRO.SER.|NRO.DOC.|CLIENTE|PROVEEDOR|COD.REF.|NRO.REF.|ESTADO|ARTICULO|CANTIDAD|UNIDAD'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia 
    AND almacen.coddiv = s-coddiv,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.fchdoc >= x-fecha-1
    AND almcmov.fchdoc <= x-fecha-2:
    IF almcmov.flgest = 'A' THEN DO:
        PUT UNFORMATTED
            almcmov.codalm '|'
            almcmov.fchdoc '|'
            almcmov.tipmov '|'
            almcmov.codmov '|'
            almcmov.nroser '|'
            almcmov.nrodoc '|'
            almcmov.codcli '|'
            almcmov.codpro '|'
            almcmov.codref '|'
            almcmov.nroref '|'
            almcmov.flgest '|'
             '|'
             '|'
            SKIP.
        NEXT.
    END.
    FOR EACH almdmov OF almcmov NO-LOCK:
        PUT UNFORMATTED
            almcmov.codalm '|'
            almcmov.fchdoc '|'
            almcmov.tipmov '|'
            almcmov.codmov '|'
            almcmov.nroser '|'
            almcmov.nrodoc '|'
            almcmov.codcli '|'
            almcmov.codpro '|'
            almcmov.codref '|'
            almcmov.nroref '|'
            almcmov.flgest '|'
            almdmov.codmat '|'
            almdmov.candes '|'
            almdmov.codund
            SKIP.
    END.
END.
OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE transferencias:
/* ********************* */    
OUTPUT TO d:\tmp\transferencias.txt.
PUT UNFORMATTED
    'ALMACEN|FECHA|TIP.MOV.|COD.MOV.|NRO.SER.|NRO.DOC.|ALM.ORIGEN/DESTINO|ESTADO|TRANSF./RECEP.|ARTICULO|CANTIDAD|UNIDAD'
    SKIP.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia 
    AND almacen.coddiv = s-coddiv,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 'I'
    AND almcmov.codmov = 03
    AND almcmov.fchdoc >= x-fecha-1
    AND almcmov.fchdoc <= x-fecha-2:
    IF almcmov.flgest = 'A' THEN DO:
        PUT UNFORMATTED
            almcmov.codalm '|'
            almcmov.fchdoc '|'
            almcmov.tipmov '|'
            almcmov.codmov '|'
            almcmov.nroser '|'
            almcmov.nrodoc '|'
            Almcmov.AlmDes '|'
            almcmov.flgest '|'
            '|'
            '|'
            '|'
            SKIP.
        NEXT.
    END.
    FOR EACH almdmov OF almcmov NO-LOCK:
        PUT UNFORMATTED
            almcmov.codalm '|'
            almcmov.fchdoc '|'
            almcmov.tipmov '|'
            almcmov.codmov '|'
            almcmov.nroser '|'
            almcmov.nrodoc '|'
            almcmov.almdes '|'
            almcmov.flgest '|'
            '|'
            almdmov.codmat '|'
            almdmov.candes '|'
            almdmov.codund
            SKIP.
    END.
END.
DEF BUFFER b-almacen FOR almacen.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = almacen.codalm
    AND almcmov.tipmov = 'S'
    AND almcmov.codmov = 03
    AND almcmov.fchdoc >= x-fecha-1
    AND almcmov.fchdoc <= x-fecha-2,
    FIRST b-almacen NO-LOCK WHERE b-almacen.codcia = s-codcia
    AND b-almacen.codalm = almcmov.almdes
    AND b-almacen.coddiv = s-coddiv:
    IF almcmov.flgest = 'A' THEN DO:
        PUT UNFORMATTED
            almcmov.codalm '|'
            almcmov.fchdoc '|'
            almcmov.tipmov '|'
            almcmov.codmov '|'
            almcmov.nroser '|'
            almcmov.nrodoc '|'
            Almcmov.AlmDes '|'
            almcmov.flgest '|'
            '|'
            '|'
            '|'
            SKIP.
        NEXT.
    END.
    FOR EACH almdmov OF almcmov NO-LOCK:
        PUT UNFORMATTED
            almcmov.codalm '|'
            almcmov.fchdoc '|'
            almcmov.tipmov '|'
            almcmov.codmov '|'
            almcmov.nroser '|'
            almcmov.nrodoc '|'
            almcmov.almdes '|'
            almcmov.flgest '|'
            almcmov.flgsit '|'
            almdmov.codmat '|'
            almdmov.candes '|'
            almdmov.codund
            SKIP.
    END.
END.
OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE cierre-de-caja:
/* ********************* */    
OUTPUT TO d:\tmp\cierre-de-caja.txt.
PUT UNFORMATTED 
    'USUARIO|FECHA|HORA|'
    'EFECTIVO S/|CHEQ DEL DIA S/|CHEQ DIF S/|TARJ CRED S/|DEPOSITO S/|N/C S/|ANTICIPOS S/|VALES S/|'
    'EFECTIVO US$|CHEQ DEL DIA US$|CHEQ DIF US$|TARJ CRED US$|DEPOSITO US$|N/C US$|ANTICIPOS US$|VALES US$'
    SKIP.
FOR EACH ccbcierr NO-LOCK WHERE ccbcierr.codcia = s-codcia
    AND INDEX(ccbcierr.usuario, '514') > 0:
    PUT UNFORMATTED
        ccbcierr.usuario '|'
        ccbcierr.fchcie '|'
        ccbcierr.horcie '|'
        ccbcierr.impnac[1] '|'
        ccbcierr.impnac[2] '|'
        ccbcierr.impnac[3] '|'
        ccbcierr.impnac[4] '|'
        ccbcierr.impnac[5] '|'
        ccbcierr.impnac[6] '|'
        ccbcierr.impnac[7] '|'
        ccbcierr.impnac[10] '|'
        ccbcierr.impusa[1] '|'
        ccbcierr.impusa[2] '|'
        ccbcierr.impusa[3] '|'
        ccbcierr.impusa[4] '|'
        ccbcierr.impusa[5] '|'
        ccbcierr.impusa[6] '|'
        ccbcierr.impusa[7] '|'
        ccbcierr.impusa[10] '|'
        SKIP.
END.
OUTPUT CLOSE.


END PROCEDURE.

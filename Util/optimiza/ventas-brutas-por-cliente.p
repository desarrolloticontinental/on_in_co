DEF VAR X-codcli AS CHAR FORMAT 'x(11)'.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00000'.

DEF TEMP-TABLE clientes
    FIELD codcli AS CHAR FORMAT 'x(11)'.
DEF TEMP-TABLE detalle
    FIELD codcia AS INT INIT 0
    FIELD codcli AS CHAR FORMAT 'x(11)'
    FIELD codmat AS CHAR FORMAT 'x(6)'
    FIELD candes AS DEC EXTENT 12
    FIELD ventas AS DEC EXTENT 12.

INPUT FROM c:\tmp\clientes.prn.
REPEAT :
    IMPORT UNFORMATTED x-codcli.
    CREATE clientes.
    ASSIGN clientes.codcli = SUBSTRING (x-codcli,1,11).
END.
INPUT CLOSE.


FOR EACH clientes WHERE codcli <> '':
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddiv = s-coddiv
        AND ccbcdocu.fchdoc >= 01/01/2009
        AND ccbcdocu.fchdoc <= 07/05/2009
        AND ccbcdocu.codcli = clientes.codcli
        AND LOOKUP(ccbcdocu.coddoc, 'fac,bol') > 0
        AND ccbcdocu.flgest <> 'a',
        EACH ccbddocu OF ccbcdocu NO-LOCK WHERE implin > 0:
        FIND detalle WHERE detalle.codcli = clientes.codcli
            AND detalle.codmat = ccbddocu.codmat
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.codcia = s-codcia
                detalle.codcli = clientes.codcli
                detalle.codmat = ccbddocu.codmat.
        END.
        ASSIGN
            detalle.candes[MONTH(ccbcdocu.fchdoc)] = detalle.candes[MONTH(ccbcdocu.fchdoc)] + ccbddocu.candes * ccbddocu.factor.
        IF ccbcdocu.codmon = 1 THEN DO:
            ASSIGN
                detalle.ventas[MONTH(ccbcdocu.fchdoc)] = detalle.ventas[MONTH(ccbcdocu.fchdoc)] + ccbddocu.implin.
        END.
        ELSE DO:
            ASSIGN
                detalle.ventas[MONTH(ccbcdocu.fchdoc)] = detalle.ventas[MONTH(ccbcdocu.fchdoc)] + ccbddocu.implin * ccbcdocu.tpocmb.
        END.
    END.
END.

OUTPUT TO c:\tmp\ventas-brutas.txt.
FOR EACH detalle, FIRST gn-clie WHERE gn-clie.codcia = 000 AND gn-clie.codcli = detalle.codcli NO-LOCK, 
    FIRST almmmatg OF detalle NO-LOCK:
    DISPLAY
        detalle.codcli
        gn-clie.nomcli
        detalle.codmat
        almmmatg.desmat
        almmmatg.desmar FORMAT 'x(20)'
        almmmatg.undbas
        detalle.candes[1]
        detalle.candes[2]
        detalle.candes[3]
        detalle.candes[4]
        detalle.candes[5]
        detalle.candes[6]
        detalle.candes[7]
        detalle.ventas[1]
        detalle.ventas[2]
        detalle.ventas[3]
        detalle.ventas[4]
        detalle.ventas[5]
        detalle.ventas[6]
        detalle.ventas[7]
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

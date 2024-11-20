DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR x-ctouni AS DEC FORMAT '->>>,>>>,>>9.99'.   /* Reposicion  con IGV */
DEF VAR x-prevta AS DEC FORMAT '->>>,>>>,>>9.99'.   /* con IGV */
DEF VAR x-ventas AS DEC COLUMN-LABEL 'S/. con IGV' EXTENT 12 FORMAT '->>>,>>>,>>9.99'.    /* Soles */
DEF VAR x-cantidad AS DEC  EXTENT 12 FORMAT '->>>,>>>,>>9.99'.

INPUT FROM m:\tmp\productos.txt.
OUTPUT TO m:\tmp\jeremy.txt.
REPEAT :
    IMPORT UNFORMATTED x-codmat.
    x-codmat = SUBSTRING(x-codmat,1,6).
    IF x-codmat = '' THEN NEXT.
    FIND almmmatg WHERE codcia = 1 AND codmat = x-codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almmmatg THEN NEXT.
    x-ctouni = (IF monvta = 1 THEN ctotot ELSE ctotot * almmmatg.tpocmb).
    x-prevta = (IF monvta = 1 THEN preofi ELSE preofi * almmmatg.tpocmb).
    x-ventas = 0.
    x-cantidad = 0.
    FOR EACH evtarti NO-LOCK WHERE codcia = 1
        AND coddiv = '00000'
        AND codmat = x-codmat
        AND nrofch >= 200901 AND nrofch <= 200905:
        x-ventas[codmes] = x-ventas[codmes] + EvtArti.VtaxMesMn.
        x-cantidad[codmes] = x-cantidad[codmes] + EvtArti.CanxMes.
    END.
    DISPLAY
        almmmatg.codmat
        almmmatg.desmat
        almmmatg.CHR__01    COLUMN-LABEL 'Und'
        x-prevta     COLUMN-LABEL 'Precio Venta S/. (con IGV)'
        x-ctouni    COLUMN-LABEL 'Costo Reposición S/. (con IGV)'
        x-ventas[01]    
        x-ventas[02]    
        x-ventas[03]    
        x-ventas[04]    
        x-ventas[05]    
        x-cantidad[01]
        x-cantidad[02]
        x-cantidad[03]
        x-cantidad[04]
        x-cantidad[05]
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
INPUT CLOSE.
OUTPUT CLOSE.


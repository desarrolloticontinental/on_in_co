DEF TEMP-TABLE detalle
    FIELD codmat AS CHAR FORMAT 'x(6)'
    FIELD moncto AS CHAR FORMAT 'x(2)'
    FIELD ctotot AS DECI
    FIELD markdown  AS DECI
    FIELD calcular AS CHAR INIT 'SI'
    FIELD monvta AS CHAR FORMAT 'x(3)'
    FIELD preofi AS DECI DECIMALS 4
    .


DEF VAR x-codmat AS CHAR NO-UNDO.

INPUT FROM d:\top100.prn.
REPEAT:
    IMPORT UNFORMATTED x-codmat.
    IF TRUE <> (x-codmat > '') THEN LEAVE.
    CREATE detalle.
    detalle.codmat = x-codmat.
    FIND almmmatg WHERE almmmatg.codcia = 1
        AND almmmatg.codmat = x-codmat NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    detalle.moncto = (IF almmmatg.DsctoProm[1] = 1 THEN 'S/' ELSE '$').
    detalle.ctotot = almmmatg.ctotot.
    detalle.monvta = (IF almmmatg.monvta = 1 THEN 'S/' ELSE '$').
    FIND vtalistamingn WHERE vtalistamingn.codcia = 1
        AND vtalistamingn.codmat = x-codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE vtalistamingn THEN DO:
        detalle.preofi = vtalistamingn.preofi.
    END.
    ELSE DO:
        detalle.preofi = almmmatg.preofi.
    END.
    IF detalle.preofi > 0 THEN DO:
        IF detalle.moncto = detalle.monvta THEN DO:
            detalle.markdown = (detalle.preofi - detalle.ctotot) / detalle.preofi * 100.
        END.
        ELSE DO:
            IF almmmatg.DsctoProm[1] = 1 THEN DO:
                detalle.markdown = (detalle.preofi - (detalle.ctotot / almmmatg.tpocmb)) / detalle.preofi * 100.
            END.
            ELSE DO:
                detalle.markdown = (detalle.preofi - (detalle.ctotot * almmmatg.tpocmb)) / detalle.preofi * 100.
            END.

        END.
    END.
END.
INPUT CLOSE.

OUTPUT TO d:\resultado.txt.
FOR EACH detalle NO-LOCK:
    EXPORT  DELIMITER "|" detalle.
END.
OUTPUT CLOSE.


TRIGGER PROCEDURE FOR DELETE OF gn-ven.

DEFINE SHARED VAR s-codcia AS INT.

IF CAN-FIND(FIRST faccpedi WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddoc = 'COT'
            AND faccpedi.codven = gn-ven.codven
            NO-LOCK)
    THEN DO:
    MESSAGE 'Este vendedor tiene historial de ventas, no se puede anular'
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
IF CAN-FIND(FIRST faccpedi WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddoc = 'P/M'
            AND faccpedi.codven = gn-ven.codven
            NO-LOCK)
    THEN DO:
    MESSAGE 'Este vendedor tiene historial de ventas, no se puede anular'
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
/* FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:                     */
/*     FOR FIRST Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia               */
/*         AND Ccbcdocu.coddiv = gn-divi.coddiv                                  */
/*         AND Ccbcdocu.codven = gn-ven.codven:                                  */
/*         MESSAGE 'Este vendedor tiene historial de ventas, no se puede anular' */
/*             VIEW-AS ALERT-BOX ERROR.                                          */
/*         RETURN ERROR.                                                         */
/*     END.                                                                      */
/* END.                                                                          */


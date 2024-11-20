/* 
    Carga la ventas al WMIGRV, dado un rango de Fechas (MM/DD/AAAA)
*/

DEFINE VAR lConti AS LOGICAL.
DEFINE VAR lItem AS INT.

lConti = NO.
lItem = 0.

FOR EACH ccbcdocu WHERE codcia =  1 AND FchDoc >= 08/01/2014 AND FchDoc <= 08/31/2014 NO-LOCK :
    IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,TCK,N/C,N/D') > 0 THEN DO:   
    /*IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,TCK') > 0 THEN DO:   */
        lItem = lItem + 1.
        DISPLAY lItem coddoc nrodoc imptot codcli ruccli codant WITH STREAM-IO.
        PAUSE 0.

        /* Ventas al Credito */
        RUN aplic/sypsa/registroventas (ROWID(ccbcdocu), "I", lConti).   
        /* Ventas al Contado */
        RUN aplic/sypsa/registroventascontado (ROWID(ccbcdocu), "I", lConti).
    END.
END.



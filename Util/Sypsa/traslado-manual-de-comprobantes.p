/* 
    Carga la ventas al WMIGRV, dado un archivo de texto
    TipoDcto    1 - 5
    NroDcto     6 - 15
*/

DEF VAR x-linea AS CHAR. 
DEF VAR X-coddoc AS CHAR.
DEF VAR X-nrodoc AS CHAR.

/*INPUT FROM z:\dctosfaltante.prn. */
/*INPUT FROM c:\ciman\ciman.prn.*/
INPUT FROM c:\ciman\Dicxx.txt.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    X-coddoc = SUBSTRING(x-linea,1,3).
    X-nrodoc = SUBSTRING(x-linea,6).
    X-nrodoc = TRIM(X-nrodoc).
    FIND ccbcdocu WHERE codcia = 001
        AND coddoc = x-coddoc /*SUBSTRING(x-linea,1,3)*/
        AND nrodoc = x-nrodoc /*SUBSTRING(x-linea,6)*/
        NO-LOCK.
    IF AVAILABLE ccbcdocu THEN DO:    
        DISPLAY coddoc nrodoc imptot codcli ruccli codant WITH STREAM-IO.
        PAUSE 0.
        RUN aplic/sypsa/registroventas (ROWID(ccbcdocu), "I").
        RUN aplic/sypsa/registroventascontado (ROWID(ccbcdocu), "I").
END.
END.
INPUT CLOSE.

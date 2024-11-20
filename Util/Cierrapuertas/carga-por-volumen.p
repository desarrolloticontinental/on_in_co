DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEF VAR i AS INT NO-UNDO.


/* BORRAMOS LAS PROMOCIONES PARA LA DIVISION */
FOR EACH Almmmatg WHERE codcia = s-codcia:
    DO i = 1 TO 10:
        IF PromDivi[i] = s-coddiv THEN DO:
            ASSIGN
                PromDivi[i] = ''
                PromFchD[i] = ?
                PromFchH[i] = ?
                PromDto[i] = 0.
        END.
    END.
END.

/* IMPORTAMOS INFORMACION PARA PROMOCIONES */
DEF VAR x-linea AS CHAR FORMAT 'x(100)' NO-UNDO.
DEF VAR x-codmat LIKE almmmatg.codmat.
DEF VAR x-preuni AS DEC DECIMALS 4 NO-UNDO.

INPUT FROM c:\tmp\principales.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-preuni = DECIMAL(SUBSTRING(x-linea,7,10)).
    IF x-codmat <> '' THEN DO:
        DISPLAY x-codmat.
        PAUSE 0.
        FIND ALmmmatg WHERE codcia = s-codcia
            AND codmat = x-codmat
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Almmmatg THEN DO:
            CICLO:
            DO i = 1 TO 10:
                IF PromDivi[i] = '' THEN DO:
                    ASSIGN
                        PromDivi[i] = s-coddiv
                        PromFchD[i] = 01/05/2010
                        PromFchH[i] = 01/31/2010
                        PromDto[i] = ( 1 - x-PreUni / PreVta[1] ) * 100.
                    LEAVE CICLO.
                END.
            END.
        END.
    END.
END.
INPUT CLOSE.





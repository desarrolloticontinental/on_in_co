DEFINE TEMP-TABLE PEDI-2 NO-UNDO LIKE FacDPedi.
DEF VAR pmensaje AS CHAR.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '14y'.
DEF NEW SHARED VAR s-fmapgo AS CHAR INIT '000'.
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-codmon AS INT INIT 1.
DEF NEW SHARED VAR s-coddoc AS CHAR INIT 'cot'.
DEF NEW SHARED VAR s-tpoped AS CHAR INIT 'E'.
DEF NEW SHARED VAR s-codcli AS CHAR.
DEF NEW SHARED VAR s-nrodec AS INT INIT 4.
DEF NEW SHARED VAR s-porigv AS DEC.

DEFINE BUFFER PEDIDO FOR faccpedi.
DEF TEMP-TABLE resumen
    FIELD codcot AS CHAR
    FIELD nrocot AS CHAR
    FIELD fchcot AS DATE
    FIELD codped AS CHAR
    FIELD nroped AS CHAR
    FIELD fchped AS DATE
    FIELD codmat AS CHAR
    FIELD canped AS DEC
    FIELD canpro AS DEC
    INDEX Idx00 AS PRIMARY codcot nrocot codmat.

FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = '00015'
    AND faccpedi.coddoc = 'cot'
    AND faccpedi.libre_c01 = '10015'
    AND faccpedi.fchped >= 01/01/2019
    AND faccpedi.flgest <> 'A':
    DISPLAY faccpedi.coddoc faccpedi.nroped. PAUSE 0.
    s-codcli = faccpedi.codcli.
    s-porigv = faccpedi.porigv.
    EMPTY TEMP-TABLE PEDI-2.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE PEDI-2.
        BUFFER-COPY facdpedi TO PEDI-2.
    END.
    RUN vta2/promocion-generalv2 (Faccpedi.Libre_c01,   /* División Lista Precio */
                                  Faccpedi.CodCli,
                                  INPUT-OUTPUT TABLE PEDI-2,    /* OJO */
                                  OUTPUT pMensaje).

    FOR EACH pedido NO-LOCK WHERE pedido.codcia = s-codcia
        AND pedido.coddoc = 'PED'
        AND pedido.codref = faccpedi.coddoc
        AND pedido.nroref = faccpedi.nroped
        AND pedido.flgest <> 'A',
        EACH facdpedi OF pedido NO-LOCK WHERE facdpedi.libre_c05 = 'OF':
        CREATE resumen.
        ASSIGN
            resumen.codcot = faccpedi.coddoc
            resumen.nrocot = faccpedi.nroped
            resumen.fchcot = faccpedi.fchped
            resumen.codped = pedido.coddoc
            resumen.nroped = pedido.nroped
            resumen.fchped = pedido.fchped
            resumen.codmat = facdpedi.codmat
            resumen.canped = facdpedi.canped.
    END.
    FOR EACH pedi-2 WHERE pedi-2.libre_c05 = 'OF':
        FIND resumen WHERE resumen.codcot = faccpedi.coddoc
            AND resumen.nrocot = faccpedi.nroped
            AND resumen.codmat = pedi-2.codmat
            NO-ERROR.
        IF NOT AVAILABLE resumen THEN DO:
            CREATE resumen.
            ASSIGN
                resumen.codcot = faccpedi.coddoc
                resumen.nrocot = faccpedi.nroped
                resumen.fchcot = faccpedi.fchped
                resumen.codmat = pedi-2.codmat.
        END.
        ASSIGN
            resumen.canpro = resumen.canpro + pedi-2.canped.
    END.
END.

OUTPUT TO d:\tmp\promociones.txt.
FOR EACH resumen NO-LOCK:
    PUT UNFORMATTED
        resumen.codcot '|'
        resumen.nrocot '|'
        resumen.fchcot '|'
        resumen.codped '|'
        resumen.nroped '|'
        resumen.fchped '|'
        resumen.codmat '|'
        resumen.canped '|'
        resumen.canpro 
        SKIP.
END.
OUTPUT CLOSE.



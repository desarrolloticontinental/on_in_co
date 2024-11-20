DEF VAR FILL-IN-tpocmb LIKE gn-tcmb.venta.
DEF VAR FILL-IN-TcCompra LIKE gn-tcmb.compra.
DEF VAR x-codcta AS CHAR.
DEF VAR cb-codcia AS INT INIT 000.
DEF VAR s-codcia AS INT INIT 001.

DEF VAR x-ctaigv AS CHAR.
DEF VAR x-ctaisc AS CHAR.
DEF VAR x-ctadto AS CHAR.

DEF BUFFER B-CDocu FOR Ccbcdocu.

FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
    AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.
IF AVAILABLE cb-Cfgg THEN DO:
    ASSIGN
        x-ctaisc = cb-cfgg.codcta[2] 
        x-ctaigv = cb-cfgg.codcta[3]
        x-ctadto = cb-cfgg.codcta[10].
END.

OUTPUT TO c:\tmp\sypsa\registrodeventas.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia
    AND LOOKUP(coddoc, 'fac,bol,n/c,n/d') > 0
    AND flgest = 'P':
    FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= ccbcdocu.fchdoc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Gn-tcmb THEN DO:
        MESSAGE 'Tipo de cambio del dia' ccbcdocu.fchdoc 'NO está configurado'
            VIEW-AS ALERT-BOX WARNING.
        NEXT.
    END.
    ASSIGN
        FILL-IN-tpocmb = gn-tcmb.venta
        FILL-IN-TcCompra = gn-tcmb.compra.
    CASE Ccbcdocu.coddoc:
        WHEN 'FAC' OR WHEN 'BOL' THEN RUN Carga-FAC-BOL.
        WHEN 'N/C' THEN RUN Carga-NC.
        WHEN 'N/D' THEN RUN Carga-ND.
    END CASE.
END.


PROCEDURE Carga-FAC-BOL:

    FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
        AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.

    FIND Cb-cfgrv WHERE Cb-cfgrv.Codcia = ccbcdocu.Codcia 
        AND Cb-cfgrv.CodDiv = ccbcdocu.Coddiv 
        AND Cb-cfgrv.Coddoc = ccbcdocu.Coddoc 
        AND Cb-cfgrv.Fmapgo = ccbcdocu.Fmapgo 
        AND Cb-cfgrv.Codmon = ccbcdocu.Codmon NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" 
    THEN x-codcta = ''.
    ELSE x-codcta  = Cb-cfgrv.Codcta.

    PUT UNFORMATTED
        ccbcdocu.codcli '|'
        ccbcdocu.fmapgo '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.codmon '|'
        '|'     /* Centro de Costos */
        '2' '|'.
    /* Importe Bruto */
    IF Ccbcdocu.ImpBrt > 0 THEN DO:
        CASE ccbcdocu.TpoFac:
            WHEN 'S' THEN DO:
                x-codcta = cb-cfgg.codcta[9].
                FIND FIRST CcbDDocu WHERE CcbDDocu.CodCia = Ccbcdocu.CodCia 
                    AND CcbDDocu.CodDiv = Ccbcdocu.CodDiv 
                    AND CcbDDocu.CodDoc = Ccbcdocu.CodDoc 
                    AND CcbDDocu.NroDoc = Ccbcdocu.NroDoc 
                    AND CcbDDocu.CodMat >= "" NO-LOCK NO-ERROR.
                 IF AVAILABLE CcbDDocu THEN DO:
                     FIND FIRST almmserv WHERE almmserv.Codcia = CcbDDocu.CodCia 
                         AND almmserv.CodMat = CcbDDocu.CodMat NO-LOCK NO-ERROR.
                     IF AVAILABLE almmserv THEN x-codcta = almmserv.catconta.
                 END.
             END.
             WHEN 'A' THEN DO:    /* Adelanto Campaña */
                 x-codcta = cb-cfgg.codaux[10].
             END.
             OTHERWISE x-codcta = cb-cfgg.codcta[5].
           END.
           PUT
               Ccbcdocu.impbrt '|'
               x-codcta '|'
               'C' '|'.
    END.
    ELSE PUT
        '|'
        '|'
        '|'.
    IF Ccbcdocu.impexo > 0 THEN 
        PUT 
        Ccbcdocu.impexo '|'
        cb-cfgg.codcta[5] '|'
        'C' '|'.
    ELSE PUT 
        '|'
        '|'
        '|'.
    IF Ccbcdocu.impdto > 0 THEN 
        PUT
        Ccbcdocu.impdto '|'
        x-ctadto '|'
        'C' '|'.
    ELSE PUT
        '|'
        '|'
        '|'.
    IF Ccbcdocu.impigv > 0 THEN
        PUT
        Ccbcdocu.impigv '|'
        x-ctaigv '|'
        'A' '|'.
    ELSE 
        PUT 
            '|'
            '|'
            '|'.
    IF Ccbcdocu.coddoc = "FAC"
        THEN PUT
        ccbcdocu.imptot '|'
        '12122100' '|'
        'A' '|'.
    ELSE PUT 
        ccbcdocu.imptot '|'
        '12121140' '|'
        'A' '|'.

    PUT
        ccbcdocu.fchvto '|'
        ccbcdocu.tpocmb 
        SKIP.

END PROCEDURE.


PROCEDURE Carga-NC:
        
    FIND B-CDocu WHERE B-CDocu.Codcia = S-CODCIA 
        AND B-CDocu.Coddoc = Ccbcdocu.Codref 
        AND B-CDocu.Nrodoc = Ccbcdocu.Nroref NO-LOCK NO-ERROR.

    IF NOT AVAILABLE B-CDocu THEN RETURN.

    FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
        AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.

    PUT UNFORMATTED
        ccbcdocu.codcli '|'
        ccbcdocu.fmapgo '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.codmon '|'
        '|'     /* Centro de Costos */
        '2' '|'.

    IF Ccbcdocu.Cndcre = 'D' THEN DO:
        FIND CcbTabla WHERE CcbTabla.CodCia = s-codcia 
            AND CcbTabla.Tabla = 'N/C' 
            AND CcbTabla.Codigo = "00004" 
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbTabla THEN x-codcta = CcbTabla.Codcta. 
        ELSE x-codcta = ''.
    END.
    ELSE DO:
        x-codcta = ''.
        FIND FIRST Ccbddocu OF Ccbcdocu NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbddocu THEN DO:
            FIND CcbTabla WHERE CcbTabla.CodCia = s-codcia 
                AND CcbTabla.Tabla = 'N/C' AND CcbTabla.Codigo = CcbDDocu.codmat 
                NO-LOCK NO-ERROR.
            IF AVAILABLE CcbTabla THEN x-codcta = CcbTabla.Codcta. 
        END.
    END.
    PUT 
        ccbcdocu.impvta '|'
        x-codcta '|'
        'A' '|'.
    PUT 
        '|'
        '|'
        '|'.
    PUT
        '|'
        '|'
        '|'.
    IF Ccbcdocu.impigv > 0 THEN
        PUT
        Ccbcdocu.impigv '|'
        x-ctaigv '|'
        'A' '|'.
    ELSE 
        PUT 
            '|'
            '|'
            '|'.
    FIND Cb-cfgrv WHERE Cb-cfgrv.Codcia = S-CODCIA 
        AND Cb-cfgrv.CodDiv = Ccbcdocu.coddiv
        AND Cb-cfgrv.Coddoc = Ccbcdocu.coddoc 
        AND Cb-cfgrv.CodRef = Ccbcdocu.Codref 
        AND Cb-cfgrv.Fmapgo = B-CDocu.Fmapgo 
        AND Cb-cfgrv.Codmon = Ccbcdocu.Codmon NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" 
        THEN x-codcta = ''.
    ELSE x-codcta  = Cb-cfgrv.Codcta.
    PUT 
        ccbcdocu.imptot '|'
        x-codcta '|'
        'A' '|'.

    PUT
        ccbcdocu.fchvto '|'
        ccbcdocu.tpocmb 
        SKIP.


END PROCEDURE.


PROCEDURE Carga-ND:

    FIND B-CDocu WHERE B-CDocu.Codcia = S-CODCIA 
        AND B-CDocu.Coddoc = Ccbcdocu.Codref 
        AND B-CDocu.Nrodoc = Ccbcdocu.Nroref NO-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CDocu THEN RETURN.

    FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
        AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.

    PUT UNFORMATTED
        ccbcdocu.codcli '|'
        ccbcdocu.fmapgo '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.codmon '|'
        '|'     /* Centro de Costos */
        '2' '|'.

    x-codcta = ''.
    FIND FIRST Ccbddocu OF Ccbcdocu NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbddocu THEN DO:
        FIND CcbTabla WHERE CcbTabla.CodCia = s-codcia 
            AND CcbTabla.Tabla = 'N/D' AND CcbTabla.Codigo = CcbDDocu.codmat 
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbTabla THEN x-codcta = CcbTabla.Codcta. 
    END.
    PUT 
    ccbcdocu.impvta '|'
    x-codcta '|'
    'A' '|'.

    PUT 
        '|'
        '|'
        '|'.
    PUT
        '|'
        '|'
        '|'.
    IF Ccbcdocu.impigv > 0 THEN
        PUT
        Ccbcdocu.impigv '|'
        x-ctaigv '|'
        'A' '|'.
    ELSE 
        PUT 
            '|'
            '|'
            '|'.
    FIND Cb-cfgrv WHERE Cb-cfgrv.Codcia = S-CODCIA 
        AND Cb-cfgrv.CodDiv = Ccbcdocu.coddiv
        AND Cb-cfgrv.Coddoc = Ccbcdocu.coddoc 
        AND Cb-cfgrv.CodRef = Ccbcdocu.Codref 
        AND Cb-cfgrv.Fmapgo = B-CDocu.Fmapgo 
        AND Cb-cfgrv.Codmon = Ccbcdocu.Codmon NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" 
        THEN x-codcta = ''.
    ELSE x-codcta  = Cb-cfgrv.Codcta.
    PUT 
        ccbcdocu.imptot '|'
        x-codcta '|'
        'A' '|'.
    PUT
        ccbcdocu.fchvto '|'
        ccbcdocu.tpocmb 
        SKIP.

END PROCEDURE.


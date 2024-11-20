DEFINE VARIABLE x-texto AS LONGCHAR NO-UNDO.
DEFINE VARIABLE x-url AS CHAR NO-UNDO.
DEFINE VARIABLE x-url-stock-cissac AS CHAR NO-UNDO.
DEFINE VARIABLE x-xml AS LONGCHAR NO-UNDO.
DEFINE VARIABLE x-pos1 AS INT NO-UNDO.
DEFINE VARIABLE x-pos2 AS INT NO-UNDO.

DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-codcli AS CHAR NO-UNDO.
DEF VAR x-nomcli AS CHAR NO-UNDO.
DEF VAR x-dircli AS CHAR NO-UNDO.
DEF VAR x-ubigeo AS CHAR NO-UNDO.
DEF VAR x-status AS CHAR NO-UNDO.

DEF VAR k AS INT NO-UNDO.

DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
CREATE X-DOCUMENT hDoc.
x-url-stock-cissac = "http://192.168.100.230:7000/AddressConsult".

INPUT FROM d:\tmp\clientes.prn.
/* FOR EACH gn-clie EXCLUSIVE-LOCK WHERE gn-clie.codcia = cl-codcia AND */
/*     gn-clie.flgsit = "A" AND                                         */
/*     LOOKUP(SUBSTRING(gn-clie.ruc,1,1), '1,2') > 0:                   */
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    k = k + 1.
    x-codcli = SUBSTRING(x-linea,1,11).
    FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia AND
        gn-clie.codcli = x-codcli EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR = YES AND NOT LOCKED(gn-clie) THEN DO:
        /*MESSAGE 'Error en el código' x-codcli VIEW-AS ALERT-BOX ERROR.*/
        NEXT.
    END.
    IF k > 1000 AND (k MODULO 1000 = 0) THEN DO:
        DISPLAY gn-clie.codcli gn-clie.ruc WITH STREAM-IO NO-BOX.
        PAUSE 0.
    END.
    x-linea = gn-clie.ruc.
    /* La URL del Webservice */
    x-url = TRIM(x-url-stock-cissac).
    x-url = x-url + "/" + TRIM(x-linea) + "/".
    x-nomcli = ''.
    x-dircli = ''.
    x-ubigeo = ''.
    hDoc:LOAD("FILE", x-url, FALSE) /*NO-ERROR*/.
    hDoc:SAVE("LONGCHAR",x-xml) /*NO-ERROR*/.
    /*x-texto = CAPS(STRING(x-xml)).*/
    x-Texto = CAPS(x-xml).
    IF INDEX(x-texto, 'NOT FOUND') > 0 THEN NEXT.
    /* Status */
    x-pos1 = INDEX(x-texto,"<status>").
    IF x-pos1 > 0 THEN DO:
        x-pos1 = x-pos1 + LENGTH("<status>").
        x-pos2 = INDEX(x-texto,"</status>").
        x-status = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
    END.
    /* Nombre */
    x-pos1 = INDEX(x-texto,"<name>").
    IF x-pos1 > 0 THEN DO:
        x-pos1 = x-pos1 + LENGTH("<name>").
        x-pos2 = INDEX(x-texto,"</name>").
        x-nomcli = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
        x-nomcli = REPLACE(x-nomcli, '&AMP;','&').
    END.
    /* Direccion */
    x-pos1 = INDEX(x-texto,"<address>").
    IF x-pos1 > 0 THEN DO:
        x-pos1 = x-pos1 + LENGTH("<address>").
        x-pos2 = INDEX(x-texto,"</address>").
        x-dircli = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
    END.
    /* Ubigeo */
    x-pos1 = INDEX(x-texto,"<ubigeo>").
    IF x-pos1 > 0 THEN DO:
        x-pos1 = x-pos1 + LENGTH("<ubigeo>").
        x-pos2 = INDEX(x-texto,"</ubigeo>").
        x-ubigeo = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
    END.
    /* Armado de la dirección */
    FIND TabDepto WHERE TabDepto.CodDepto = SUBSTRING(x-Ubigeo,1,2) NO-LOCK NO-ERROR.
    FIND TabProvi WHERE TabProvi.CodDepto = SUBSTRING(x-Ubigeo,1,2) AND
        TabProvi.CodProvi = SUBSTRING(x-Ubigeo,3,2) NO-LOCK NO-ERROR.
    FIND TabDistr WHERE TabDistr.CodDepto = SUBSTRING(x-Ubigeo,1,2) AND
        TabDistr.CodProvi = SUBSTRING(x-Ubigeo,3,2) AND
        TabDistr.CodDistr = SUBSTRING(x-Ubigeo,5,2) NO-LOCK NO-ERROR.
    IF AVAILABLE TabDepto AND AVAILABLE TabProvi AND AVAILABLE TabDistr THEN DO:
        x-DirCli = TRIM(x-DirCli) + ' ' + 
                    CAPS(TRIM(TabDepto.NomDepto)) + ' - ' +
                    CAPS(TRIM(TabProvi.NomProvi)) + ' - ' +
                    CAPS(TabDistr.NomDistr).
    END.
    /* ********************** */
    ASSIGN
        gn-clie.nomcli = x-nomcli
        gn-clie.dircli = x-dircli
        gn-clie.coddept = SUBSTRING(x-ubigeo,1,2)
        gn-clie.codprov = SUBSTRING(x-ubigeo,3,2)
        gn-clie.coddist = SUBSTRING(x-ubigeo,5,2)
        gn-clie.SwCargaSunat = "S"      /* Sunat */
        gn-clie.SwBajaSunat = (IF x-status = "ACTIVO" THEN NO ELSE YES).
    /* Activo o Cesado */
    IF gn-clie.SwBajaSunat = YES THEN gn-clie.FlgSit = "C".
    ELSE gn-clie.FlgSit = "A".
    /* SEDE PRINCIPAL */
    FIND FIRST gn-clied OF gn-clie WHERE gn-clied.sede = "@@@" EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clied THEN DO:
        CREATE gn-clied.
        BUFFER-COPY gn-clie TO gn-clied
            ASSIGN 
            Gn-ClieD.SwSedeSunat = "S"
            Gn-ClieD.DomFiscal = YES
            Gn-ClieD.Sede = "@@@".   /* Valor que no se puede anular */
    END.
    ELSE ASSIGN
            Gn-ClieD.CodDept = Gn-Clie.CodDept
            Gn-ClieD.CodProv = Gn-Clie.CodProv 
            Gn-ClieD.CodDist = Gn-Clie.CodDist
            Gn-ClieD.DirCli  = Gn-Clie.DirCli.
    FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
        TabDistr.CodProvi = Gn-ClieD.CodProv AND
        TabDistr.CodDistr = Gn-ClieD.CodDist 
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN Gn-ClieD.Codpos = TabDistr.CodPos.
END.
QUIT.

/*
INPUT FROM d:\tmp\rucs.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND gn-clie WHERE codcia = 000 AND codcli = x-linea
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    /* La URL del Webservice */
    x-url = TRIM(x-url-stock-cissac).
    x-url = x-url + "/" + TRIM(gn-clie.codcli) + "/".
    x-nomcli = ''.
    x-dircli = ''.
    x-ubigeo = ''.
    hDoc:LOAD("FILE", x-url, FALSE) /*NO-ERROR*/.
    IF ERROR-STATUS:ERROR = NO THEN DO:
        hDoc:SAVE("LONGCHAR",x-xml) /*NO-ERROR*/.
        x-texto = CAPS(STRING(x-xml)).
        IF INDEX(x-texto, 'NOT FOUND') > 0 THEN NEXT.
        /* Nombre */
        x-pos1 = INDEX(x-texto,"<name>").
        IF x-pos1 > 0 THEN DO:
            x-pos1 = x-pos1 + LENGTH("<name>").
            x-pos2 = INDEX(x-texto,"</name>").
            x-nomcli = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
            x-nomcli = REPLACE(x-nomcli, '&AMP;','&').
        END.
        /* Direccion */
        x-pos1 = INDEX(x-texto,"<address>").
        IF x-pos1 > 0 THEN DO:
            x-pos1 = x-pos1 + LENGTH("<address>").
            x-pos2 = INDEX(x-texto,"</address>").
            x-dircli = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
        END.
        /* Ubigeo */
        x-pos1 = INDEX(x-texto,"<ubigeo>").
        IF x-pos1 > 0 THEN DO:
            x-pos1 = x-pos1 + LENGTH("<ubigeo>").
            x-pos2 = INDEX(x-texto,"</ubigeo>").
            x-ubigeo = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
        END.
        /* Armado de la dirección */
        FIND TabDepto WHERE TabDepto.CodDepto = SUBSTRING(x-Ubigeo,1,2) NO-LOCK NO-ERROR.
        FIND TabProvi WHERE TabProvi.CodDepto = SUBSTRING(x-Ubigeo,1,2) AND
            TabProvi.CodProvi = SUBSTRING(x-Ubigeo,3,2) NO-LOCK NO-ERROR.
        FIND TabDistr WHERE TabDistr.CodDepto = SUBSTRING(x-Ubigeo,1,2) AND
            TabDistr.CodProvi = SUBSTRING(x-Ubigeo,3,2) AND
            TabDistr.CodDistr = SUBSTRING(x-Ubigeo,5,2) NO-LOCK NO-ERROR.
        IF AVAILABLE TabDepto AND AVAILABLE TabProvi AND AVAILABLE TabDistr THEN DO:
            x-DirCli = TRIM(x-DirCli) + ' ' + 
                        CAPS(TRIM(TabDepto.NomDepto)) + ' - ' +
                        CAPS(TRIM(TabProvi.NomProvi)) + ' - ' +
                        CAPS(TabDistr.NomDistr).
        END.
        /* ********************** */
        DISPLAY codcli WITH STREAM-IO NO-BOX. PAUSE 0.
        ASSIGN
            gn-clie.nomcli = x-nomcli
            gn-clie.dircli = x-dircli
            gn-clie.coddept = SUBSTRING(x-ubigeo,1,2)
            gn-clie.codprov = SUBSTRING(x-ubigeo,3,2)
            gn-clie.coddist = SUBSTRING(x-ubigeo,5,2).
        FIND FIRST gn-clied OF gn-clie WHERE gn-clied.sede = "@@@" EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE gn-clied THEN DO:
            CREATE gn-clied.
            BUFFER-COPY gn-clie TO gn-clied
                ASSIGN 
                Gn-ClieD.DomFiscal = YES
                Gn-ClieD.Sede = "@@@".   /* Valor que no se puede anular */
        END.
        ELSE ASSIGN
                Gn-ClieD.CodDept = Gn-Clie.CodDept
                Gn-ClieD.CodProv = Gn-Clie.CodProv 
                Gn-ClieD.CodDist = Gn-Clie.CodDist
                Gn-ClieD.DirCli  = Gn-Clie.DirCli.
        FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
            TabDistr.CodProvi = Gn-ClieD.CodProv AND
            TabDistr.CodDistr = Gn-ClieD.CodDist 
            NO-LOCK NO-ERROR.
        IF AVAILABLE TabDistr THEN Gn-ClieD.Codpos = TabDistr.CodPos.
    END.
END.
INPUT CLOSE.
*/

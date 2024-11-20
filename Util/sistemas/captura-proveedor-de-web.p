DEFINE VARIABLE x-texto AS LONGCHAR NO-UNDO.
DEFINE VARIABLE x-url AS CHAR NO-UNDO.
DEFINE VARIABLE x-url-stock-cissac AS CHAR NO-UNDO.
DEFINE VARIABLE x-xml AS LONGCHAR NO-UNDO.
DEFINE VARIABLE x-pos1 AS INT NO-UNDO.
DEFINE VARIABLE x-pos2 AS INT NO-UNDO.

DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-rucpro AS CHAR NO-UNDO.
DEF VAR x-nompro AS CHAR NO-UNDO.
DEF VAR x-dirpro AS CHAR NO-UNDO.
DEF VAR x-ubigeo AS CHAR NO-UNDO.
DEF VAR x-status AS CHAR NO-UNDO.

DEF VAR k AS INT NO-UNDO.

DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
CREATE X-DOCUMENT hDoc.
x-url-stock-cissac = "http://192.168.100.230:7000/AddressConsult".

/*INPUT FROM d:\tmp\transporte.prn.*/
FOR EACH gn-prov EXCLUSIVE-LOCK WHERE gn-prov.codcia = pv-codcia AND
    gn-prov.flgsit = "A":
/* REPEAT:                                 */
/*     IMPORT UNFORMATTED x-linea.         */
/*     IF x-linea = '' THEN LEAVE.         */
    k = k + 1.
/*     x-rucpro = SUBSTRING(x-linea,1,11). */
/*     FIND FIRST gn-prov WHERE gn-prov.codcia = cl-codcia AND                */
/*         gn-prov.Ruc = x-rucpro EXCLUSIVE-LOCK NO-ERROR.                    */
/*     IF ERROR-STATUS:ERROR = YES AND NOT LOCKED(gn-prov) THEN DO:           */
/*         /*MESSAGE 'Error en el código' x-rucpro VIEW-AS ALERT-BOX ERROR.*/ */
/*         NEXT.                                                              */
/*     END.                                                                   */
    IF k > 1000 AND (k MODULO 1000 = 0) THEN DO:
        DISPLAY gn-prov.codpro gn-prov.ruc WITH STREAM-IO NO-BOX.
        PAUSE 0.
    END.
    x-linea = gn-prov.ruc.
    /* La URL del Webservice */
    x-url = TRIM(x-url-stock-cissac).
    x-url = x-url + "/" + TRIM(x-linea) + "/".
    x-nompro = ''.
    x-dirpro = ''.
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
        x-nompro = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
        x-nompro = REPLACE(x-nompro, '&AMP;','&').
    END.
    /* Direccion */
    x-pos1 = INDEX(x-texto,"<address>").
    IF x-pos1 > 0 THEN DO:
        x-pos1 = x-pos1 + LENGTH("<address>").
        x-pos2 = INDEX(x-texto,"</address>").
        x-dirpro = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
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
        x-dirpro = TRIM(x-dirpro) + ' ' + 
                    CAPS(TRIM(TabDepto.NomDepto)) + ' - ' +
                    CAPS(TRIM(TabProvi.NomProvi)) + ' - ' +
                    CAPS(TabDistr.NomDistr).
    END.
    /* ********************** */
    ASSIGN
        gn-prov.nompro = x-nompro
        gn-prov.dirpro = x-dirpro
        gn-prov.coddept = SUBSTRING(x-ubigeo,1,2)
        gn-prov.codprov = SUBSTRING(x-ubigeo,3,2)
        gn-prov.coddist = SUBSTRING(x-ubigeo,5,2)
        /*gn-prov.SwCargaSunat = "S"      /* Sunat */*/
        /*gn-prov.SwBajaSunat = (IF x-status = "ACTIVO" THEN NO ELSE YES)*/.
    /* Activo o Cesado */
/*     IF gn-prov.SwBajaSunat = YES THEN gn-prov.FlgSit = "C". */
/*     ELSE gn-prov.FlgSit = "A".                              */
    /* SEDE PRINCIPAL */
    FIND FIRST gn-provd OF gn-prov WHERE gn-provd.sede = "@@@" EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-provd THEN DO:
        CREATE gn-provd.
        BUFFER-COPY gn-prov TO gn-provd
            ASSIGN 
            /*gn-provD.SwSedeSunat = "S"*/
            gn-provD.DomFiscal = YES
            gn-provD.Sede = "@@@".   /* Valor que no se puede anular */
    END.
    ELSE ASSIGN
            gn-provD.CodDept = gn-prov.CodDept
            gn-provD.CodProv = gn-prov.CodProv 
            gn-provD.CodDist = gn-prov.CodDist
            gn-provD.DirPro  = gn-prov.DirPro.
    FIND TabDistr WHERE TabDistr.CodDepto = gn-provD.CodDept AND
        TabDistr.CodProvi = gn-provD.CodProv AND
        TabDistr.CodDistr = gn-provD.CodDist 
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN gn-provD.Codpos = TabDistr.CodPos.
END.
QUIT.

/*
INPUT FROM d:\tmp\rucs.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND gn-prov WHERE codcia = 000 AND codcli = x-linea
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-prov THEN NEXT.
    /* La URL del Webservice */
    x-url = TRIM(x-url-stock-cissac).
    x-url = x-url + "/" + TRIM(gn-prov.codcli) + "/".
    x-nompro = ''.
    x-dirpro = ''.
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
            x-nompro = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
            x-nompro = REPLACE(x-nompro, '&AMP;','&').
        END.
        /* Direccion */
        x-pos1 = INDEX(x-texto,"<address>").
        IF x-pos1 > 0 THEN DO:
            x-pos1 = x-pos1 + LENGTH("<address>").
            x-pos2 = INDEX(x-texto,"</address>").
            x-dirpro = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
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
            x-dirpro = TRIM(x-dirpro) + ' ' + 
                        CAPS(TRIM(TabDepto.NomDepto)) + ' - ' +
                        CAPS(TRIM(TabProvi.NomProvi)) + ' - ' +
                        CAPS(TabDistr.NomDistr).
        END.
        /* ********************** */
        DISPLAY codcli WITH STREAM-IO NO-BOX. PAUSE 0.
        ASSIGN
            gn-prov.nomcli = x-nompro
            gn-prov.dircli = x-dirpro
            gn-prov.coddept = SUBSTRING(x-ubigeo,1,2)
            gn-prov.codprov = SUBSTRING(x-ubigeo,3,2)
            gn-prov.coddist = SUBSTRING(x-ubigeo,5,2).
        FIND FIRST gn-provd OF gn-prov WHERE gn-provd.sede = "@@@" EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE gn-provd THEN DO:
            CREATE gn-provd.
            BUFFER-COPY gn-prov TO gn-provd
                ASSIGN 
                gn-provD.DomFiscal = YES
                gn-provD.Sede = "@@@".   /* Valor que no se puede anular */
        END.
        ELSE ASSIGN
                gn-provD.CodDept = gn-prov.CodDept
                gn-provD.CodProv = gn-prov.CodProv 
                gn-provD.CodDist = gn-prov.CodDist
                gn-provD.DirCli  = gn-prov.DirCli.
        FIND TabDistr WHERE TabDistr.CodDepto = gn-provD.CodDept AND
            TabDistr.CodProvi = gn-provD.CodProv AND
            TabDistr.CodDistr = gn-provD.CodDist 
            NO-LOCK NO-ERROR.
        IF AVAILABLE TabDistr THEN gn-provD.Codpos = TabDistr.CodPos.
    END.
END.
INPUT CLOSE.
*/

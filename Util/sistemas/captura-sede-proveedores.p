DEFINE VARIABLE x-texto AS LONGCHAR NO-UNDO.
DEFINE VARIABLE x-url AS CHAR NO-UNDO.
DEFINE VARIABLE x-url-stock-cissac AS CHAR NO-UNDO.
DEFINE VARIABLE x-xml AS LONGCHAR NO-UNDO.
DEFINE VARIABLE x-pos1 AS INT NO-UNDO.
DEFINE VARIABLE x-pos2 AS INT NO-UNDO.

DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.
DEF VAR pValor AS CHAR NO-UNDO.
DEF VAR hDoc AS HANDLE NO-UNDO.
DEF VAR x-NroSedes AS INT NO-UNDO.
DEF VAR x-Item AS INT NO-UNDO.
DEF VAR x-Linea AS CHAR NO-UNDO.
DEF VAR x-Ubigeo AS CHAR NO-UNDO.
DEF VAR x-dirpro AS CHAR NO-UNDO.
DEF VAR x-Sede AS INT NO-UNDO.
DEF VAR x-codpro AS CHAR NO-UNDO.

DEF VAR k AS INT NO-UNDO.

CREATE X-DOCUMENT hDoc.
x-url-stock-cissac = "http://192.168.100.230:7000/BranchAddressConsult".
/* INPUT FROM d:\tmp\transporte.prn. */
/* INPUT FROM d:\tmp\unosolo.txt.    */
OUTPUT TO d:\tmp\prov-errores.txt.
FOR EACH gn-prov EXCLUSIVE-LOCK WHERE codcia = pv-codcia AND flgsit = "A"
    AND codpro > '10001749':
/* REPEAT:                         */
/*     IMPORT UNFORMATTED x-linea. */
/*     IF x-linea = '' THEN LEAVE. */
    k = k + 1.
/*     x-codpro = SUBSTRING(x-linea,1,11).                                    */
/*     FIND FIRST gn-prov WHERE gn-prov.codcia = cl-codcia AND                */
/*         gn-prov.ruc = x-codpro NO-LOCK NO-ERROR.                           */
/*     IF ERROR-STATUS:ERROR = YES THEN DO:                                   */
/*         /*MESSAGE 'Error en el código' x-codpro VIEW-AS ALERT-BOX ERROR.*/ */
/*         NEXT.                                                              */
/*     END.                                                                   */
/*     IF k > 1000 AND (k MODULO 1000 = 0) THEN DO:                  */
/*         DISPLAY gn-prov.codpro gn-prov.ruc WITH STREAM-IO NO-BOX. */
/*         PAUSE 0.                                                  */
/*     END.                                                          */
    x-linea = gn-prov.ruc.
    /* La URL del Webservice */
    x-url = TRIM(x-url-stock-cissac).
    x-url = x-url + "/" + TRIM(x-linea) + "/".
    hDoc:LOAD("FILE", x-url, FALSE) /*NO-ERROR*/.
    hDoc:SAVE("LONGCHAR",x-xml) /*NO-ERROR*/.
    /*x-texto = CAPS(STRING(x-xml)).*/
    x-Texto = CAPS(x-xml).
    IF INDEX(x-texto, 'NOT FOUND') > 0 THEN NEXT.
    /* Numero de Sedes */
    RUN Devuelve-Caracter ("itemsqty", OUTPUT pValor).
    IF TRUE <> (pValor > '') THEN NEXT.
    ASSIGN x-NroSedes = INTEGER(pValor) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    /* NO mas de 100 sedes */
    IF x-NroSedes > 100 THEN DO:
        PUT UNFORMATTED gn-prov.codpro ' ' x-nrosedes SKIP.
        NEXT.
    END.
    /* Buscamos */
    x-Sede = 0.
    REPETIR:
    DO x-Item = 1 TO x-NroSedes:
        x-dirpro = ''.
        x-Ubigeo = ''.
        /* Direccion */
        RUN Devuelve-Caracter ( "address" + TRIM(STRING(x-Item)), OUTPUT x-dirpro).
        /* Ubigeo */
        RUN Devuelve-Caracter ( "ubigeo" + TRIM(STRING(x-Item)), OUTPUT x-Ubigeo).
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
        /* Buscamos si existe una coinciden de direccion en la sede */
        DEF VAR dFactor AS DEC NO-UNDO.
        FOR EACH gn-provd OF gn-prov EXCLUSIVE-LOCK WHERE gn-provd.sede <> "@@@":
            RUN lib/_fuzzycmp.p (gn-provD.dirpro, x-dirpro, 2, OUTPUT dFactor).
            IF dFactor >= 0.85 THEN DO:
                /* Reemplazar */
                ASSIGN 
                    /*gn-provD.SwSedeSunat = "S"*/
                    gn-provD.DomFiscal = NO
                    gn-provD.CodDept = SUBSTRING(x-Ubigeo,1,2)
                    gn-provD.CodProv = SUBSTRING(x-Ubigeo,3,2)
                    gn-provD.CodDist = SUBSTRING(x-Ubigeo,5,2)
                    gn-provD.dirpro  = x-dirpro
                    gn-provD.FchModificacion = TODAY.
                NEXT REPETIR.
            END.
        END.
        /* Buscamos un código que no se repita */
        REPEAT:
            x-Sede = x-Sede + 1.
            IF NOT CAN-FIND(FIRST gn-provd OF gn-prov WHERE gn-provD.Sede = STRING(x-Sede,'999')
                            NO-LOCK) THEN LEAVE.
        END.
        CREATE gn-provd.
        BUFFER-COPY gn-prov TO gn-provd
            ASSIGN 
            /*gn-provD.SwSedeSunat = "S"*/
            gn-provD.DomFiscal = NO
            gn-provD.Sede = STRING(x-Sede, '999')
            gn-provD.CodDept = SUBSTRING(x-Ubigeo,1,2)
            gn-provD.CodProv = SUBSTRING(x-Ubigeo,3,2)
            gn-provD.CodDist = SUBSTRING(x-Ubigeo,5,2)
            gn-provD.dirpro  = x-dirpro
            gn-provD.FchCreacion = TODAY
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    END.
END.
OUTPUT CLOSE.

PROCEDURE Devuelve-Caracter:
/* ************************ */
    DEF INPUT PARAMETER pTipo AS CHAR.
    DEF OUTPUT PARAMETER pValor AS CHAR.

    pValor = ''.
    DEF VAR x-Marcador-1 AS CHAR NO-UNDO.
    DEF VAR x-Marcador-2 AS CHAR NO-UNDO.

    x-Marcador-1 = "<" + TRIM(pTipo) + ">".
    x-Marcador-2 = "</" + TRIM(pTipo) + ">".
    x-pos1 = INDEX(x-texto,x-Marcador-1).
    IF x-pos1 > 0 THEN DO:
        x-pos1 = x-pos1 + LENGTH(x-Marcador-1).
        x-pos2 = INDEX(x-texto,x-Marcador-2).
        pValor = TRIM(SUBSTRING(x-texto,x-pos1,x-pos2 - x-pos1)).
    END.


END PROCEDURE.

/* RHC 12/06/18 De cualquier cliente del grupo */
/* Línea adicional en la rama testing */
/* Línea nueva */

DEF BUFFER x-vtatabla FOR vtatabla.
/* Esta línea es del hotfix */

DEF VAR LocalMaster AS CHAR.
DEF VAR LocalRelacionados AS CHAR.
DEF VAR LocalAgrupados AS LOG.
DEF VAR LocalCliente AS CHAR NO-UNDO.
DEF VAR s-codcia AS INTE INIT 001.
DEF VAR x-tabla-cyc AS CHAR.
DEF VAR x-dias-tolerables AS INTE.
DEF VAR x-cliente-ubicacion AS CHAR.

OUTPUT TO d:\posibles.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'ped'
    AND fchped >= DATE(10,01,2020)
    AND index(libre_c04, 'IMPORTE DENTRO DE LA LINEA DE CREDITO') > 0:

    RUN ccb/p-cliente-master (Faccpedi.CodCli,
                              OUTPUT LocalMaster,
                              OUTPUT LocalRelacionados,
                              OUTPUT LocalAgrupados).

    IF LocalAgrupados = YES AND LocalRelacionados > '' THEN DO:
    END.
    ELSE DO:
        LocalRelacionados = Faccpedi.CodCli.
    END.

    x-tabla-cyc = "APR.PED|DOCMNTOS".

    /* Cliente de LIMA o PROVINCIA */
    RUN VTA_ubicacion-cliente(INPUT faccpedi.codcli, OUTPUT x-cliente-ubicacion).

    IF x-cliente-ubicacion = "" THEN x-cliente-ubicacion = "CUALQUIERCOSA".

    
    /* DOCUMENTOS CON DEUDA VENCIDA */
    FOR EACH factabla WHERE factabla.codcia = s-codcia AND factabla.tabla = x-tabla-cyc NO-LOCK:
        x-dias-tolerables = 0.
        IF faccpedi.fmapgo = '002' THEN DO:
            /* CONTADO ANTICIPADO */
            RUN VTA_tolerancia-dias-vctos(INPUT factabla.codigo, INPUT faccpedi.coddiv,
                                          INPUT faccpedi.codcli, INPUT x-cliente-ubicacion, /* x-ubicli = "" : que la rutina calcule la ubicacion del cliente*/
                                          OUTPUT x-dias-tolerables).
        END.
        ELSE DO:
            x-dias-tolerables = factabla.valor[1].
        END.                
        IF x-dias-tolerables < 0 THEN x-dias-tolerables = 0.

        FIND FIRST CcbCDocu WHERE CcbCDocu.CodCia = FacCPedi.CodCia
            AND  CAN-DO(LocalRelacionados, CcbCDocu.CodCli)
            AND  CcbCDocu.FlgEst = "P" 
            AND  CcbCDocu.CodDoc = factabla.codigo                  /* FAC,BOL,CHQ,LET,N/D */
            AND  (CcbCDocu.FchVto + x-dias-tolerables ) < TODAY    /* 8 dias + de plazo */
            NO-LOCK NO-ERROR. 
        IF AVAIL CcbCDocu THEN DO:
            DISPLAY faccpedi.fmapgo faccpedi.coddiv faccpedi.coddoc faccpedi.nroped faccpedi.fchped 
                faccpedi.fchven faccpedi.imptot faccpedi.flgest
                WITH STREAM-IO NO-BOX WIDTH 320.
            PAUSE 0.
        END.
    END.        

END.



PROCEDURE VTA_tolerancia-dias-vctos:


    DEFINE INPUT PARAMETER pTipoDoc AS CHAR.
    DEFINE INPUT PARAMETER pDiviVta AS CHAR.
    DEFINE INPUT PARAMETER pCodCli AS CHAR.
    DEFINE INPUT PARAMETER pUbiCli AS CHAR.                 /**/
    DEFINE OUTPUT PARAMETER pDiasTolerables AS INT INIT 0.

    DEFINE VAR x-ubiclie AS CHAR.

    x-ubiclie = pUbiCli.

    IF pUbiCli = "" THEN DO:
        /* Se debe buscar si el cliente es de LIMA o PROVINCIA */
    END.
    FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                                x-vtatabla.llave_c1 = pTipoDoc AND
                                x-vtatabla.llave_c2 = pDiviVta AND
                                x-vtatabla.llave_c3 = x-ubiclie AND
                                x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
    IF AVAILABLE x-vtatabla THEN DO:
        pDiasTolerables = x-vtatabla.rango_valor[1].
        RETURN.
    END.
    FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                                x-vtatabla.llave_c1 = pTipoDoc AND
                                x-vtatabla.llave_c2 = pDiviVta AND
                                x-vtatabla.llave_c3 = "*" AND           /* Cualquier ubicacion del cliente */
                                x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
    IF AVAILABLE x-vtatabla THEN DO:
        pDiasTolerables = x-vtatabla.rango_valor[1].
        RETURN.
    END.
    FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                                x-vtatabla.llave_c1 = pTipoDoc AND
                                x-vtatabla.llave_c2 = "*" AND           /* Cualquier DIVISION */
                                x-vtatabla.llave_c3 = x-ubiclie AND
                                x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
    IF AVAILABLE x-vtatabla THEN DO:
        pDiasTolerables = x-vtatabla.rango_valor[1].
        RETURN.
    END.
    FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                                x-vtatabla.llave_c1 = pTipoDoc AND
                                x-vtatabla.llave_c2 = "*" AND           /* Cualquier DIVISION */
                                x-vtatabla.llave_c3 = "*" AND           /* Cualquier UBICACION del cliente */
                                x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
    IF AVAILABLE x-vtatabla THEN DO:
        pDiasTolerables = x-vtatabla.rango_valor[1].
        RETURN.
    END.



END PROCEDURE.


PROCEDURE vta_ubicacion-cliente:


    DEFINE INPUT PARAMETER pCodCli AS CHAR.
    DEFINE OUTPUT PARAMETER pUbiClie AS CHAR.

    pUbiClie = ''.
    /* Cliente es de LIMA o PROVINCIA siempre y cuando le hayan registrado direccion fiscal */
    FIND FIRST gn-clieD WHERE gn-clieD.codcia = 0 AND
                                gn-clieD.codcli = pCodCli AND 
                                gn-clieD.sede = "@@@" NO-LOCK NO-ERROR.     /* @@@ : Direccion Fiscal */

    IF AVAILABLE gn-clieD THEN DO:
        pUbiClie = 'LIMA'.
        IF gn-clieD.codpos = 'P0' THEN pUbiClie = 'PROVINCIA'.
    END.


END PROCEDURE.

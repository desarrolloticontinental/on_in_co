DEF VAR s-codcia AS INT INIT 001.
DEF VAR cb-codcia AS INT INIT 000.
DEF VAR lCorrelativo AS LOG.
DEF VAR s-periodo AS INT INIT 2012.
DEF VAR s-nromes AS INT INIT 06.
DEF VAR x-moncta AS INT.
DEF VAR x-clfaux AS CHAR.
DEF VAR X-IMPORT AS DECIMAL EXTENT 2 INIT 0  NO-UNDO.

DEFINE BUFFER DETALLE FOR cb-dmov.
DEFINE TEMP-TABLE DMOV LIKE cb-dmov.

x-ClfAux = "@CL".
lCorrelativo = FALSE.
/* CARGAMOS LOS SALDOS POR DOCUMENTO */
FOR EACH cb-ctas NO-LOCK WHERE cb-ctas.codcia = cb-codcia
    AND cb-ctas.codcta BEGINS '12'
    AND LENGTH(cb-ctas.codcta) = 8
    AND cb-ctas.ClfAux = "@CL":
    FOR EACH DETALLE NO-LOCK WHERE DETALLE.CODCIA  = S-CODCIA       
        AND DETALLE.PERIODO = S-PERIODO      
        AND DETALLE.NROMES <= S-NROMES       
        AND LOOKUP(DETALLE.CODOPE, '062,063') > 0
        AND DETALLE.CODCTA  = cb-ctas.CODCTA,
        FIRST cb-cmov NO-LOCK WHERE cb-cmov.codcia = detalle.codcia 
        AND cb-cmov.periodo = detalle.periodo 
        AND cb-cmov.nromes = detalle.nromes 
        AND cb-cmov.codope = detalle.codope 
        AND cb-cmov.nroast = detalle.nroast 
        BREAK BY DETALLE.CODCIA   BY DETALLE.Periodo 
              BY DETALLE.CODCTA   BY DETALLE.CODAUX
              BY DETALLE.CODDOC   BY DETALLE.NRODOC
              BY DETALLE.NROMES   BY DETALLE.FCHDOC:  
        IF cb-ctas.Codmon <> 3 THEN X-MONCTA = cb-ctas.Codmon.
        ELSE X-MONCTA = DETALLE.CodMon.
        /* RHC 27.10.2011 por quiebre */
        IF FIRST-OF(DETALLE.codcia) 
            OR FIRST-OF(DETALLE.periodo)
            OR FIRST-OF(DETALLE.codcta)
            OR FIRST-OF(DETALLE.codaux)
            OR FIRST-OF(DETALLE.coddoc)
            OR FIRST-OF(DETALLE.nrodoc)
            THEN DO:
            X-IMPORT[1] = 0.
            X-IMPORT[2] = 0.
            FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.CodCia  = S-CODCIA        
                AND cb-dmov.Periodo = S-PERIODO       
                AND cb-dmov.Codcta  = DETALLE.CODCTA  
                AND cb-dmov.Codaux  = DETALLE.codaux  
                AND cb-dmov.CodDoc  = DETALLE.CodDoc  
                AND cb-dmov.NroDoc  = DETALLE.NroDoc:
                IF cb-dmov.TpoMov THEN 
                   ASSIGN X-IMPORT[1] = X-IMPORT[1] - cb-dmov.ImpMn1 
                          X-IMPORT[2] = X-IMPORT[2] - cb-dmov.ImpMn2.
                ELSE
                   ASSIGN X-IMPORT[1] = X-IMPORT[1] + cb-dmov.ImpMn1 
                          X-IMPORT[2] = X-IMPORT[2] + cb-dmov.ImpMn2.
            END.

            IF (X-MONCTA = 1 AND ABSOLUTE(X-IMPORT[1]) > 0) OR
               (X-MONCTA = 2 AND ABSOLUTE(X-IMPORT[2]) > 0) THEN DO:
               CREATE DMOV.
               ASSIGN DMOV.CODCIA = S-CODCIA
                      DMOV.NroAst = DETALLE.NroAst
                      DMOV.CodOpe = DETALLE.CodOpe
    /*ML01*/          DMOV.Periodo = DETALLE.Periodo
    /*ML01*/          DMOV.NroMes = DETALLE.NroMes
                      DMOV.cco    = DETALLE.cco   
                      DMOV.Clfaux = DETALLE.Clfaux
                      DMOV.CndCmp = DETALLE.CndCmp
                      DMOV.Codaux = DETALLE.Codaux
                      DMOV.Codcta = DETALLE.Codcta
                      DMOV.CodDiv = DETALLE.CodDiv
                      DMOV.Coddoc = DETALLE.Coddoc
                      DMOV.Codmon = X-MONCTA
                      DMOV.Codref = DETALLE.Codref
                      DMOV.DisCCo = DETALLE.DisCCo
                      DMOV.Fchdoc = DETALLE.Fchdoc
                      DMOV.Fchvto = DETALLE.Fchvto
                      DMOV.flgact = DETALLE.flgact
                      DMOV.Glodoc = DETALLE.Glodoc
                      DMOV.ImpMn1 = ABSOLUTE(X-IMPORT[1])
                      DMOV.ImpMn2 = ABSOLUTE(X-IMPORT[2])
                      DMOV.Nrodoc = DETALLE.Nrodoc
                      DMOV.Nroref = (DETALLE.CodOpe + "-" + DETALLE.NroAst)
                      DMOV.Nroruc = DETALLE.Nroruc
                      DMOV.OrdCmp = DETALLE.OrdCmp
                      DMOV.tm     = DETALLE.tm
                      DMOV.Tpocmb = DETALLE.Tpocmb
                      DMOV.TpoMov = NOT DETALLE.TpoMov
                      DMOV.CodBco = DETALLE.CodBco.
            IF DETALLE.Codcta BEGINS "422" THEN 
                DMOV.TpoMov = NOT ( X-IMPORT[1] < 0 OR X-IMPORT[2] < 0 ).
            END.
        END.
    END.
END.
FOR EACH Cp-tpro NO-LOCK  WHERE Cp-tpro.CODCIA = CB-CODCIA 
    AND Cp-tpro.CORRELATIVO = lCorrelativo:
END.

DEF VAR x-nroser AS CHAR.
DEF VAR x-nrodoc AS CHAR.
OUTPUT TO c:\tmp\saldoventasconti-xcontabilidad.txt.
FOR EACH dmov NO-LOCK:
    FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= dmov.fchdoc NO-LOCK.
    PUT UNFORMATTED
        dmov.codaux '|'
        '|'
        dmov.coddoc '|'
        dmov.nrodoc '|'
        dmov.fchdoc '|'
        dmov.codmon '|'
        '|'
        '2' '|'
        (IF dmov.codmon = 1 THEN dmov.impmn1 ELSE dmov.impmn2) '|'
        dmov.codcta '|'
        (IF dmov.tpomov = NO THEN 'C' ELSE 'A') '|'
        FILL('|', 9)
        (IF dmov.codmon = 1 THEN dmov.impmn1 ELSE dmov.impmn2) '|'
        dmov.codcta '|'
        (IF dmov.tpomov = YES THEN 'C' ELSE 'A') '|'
        dmov.fchvto '|'
        gn-tcmb.venta 
        SKIP.
END.
OUTPUT CLOSE.


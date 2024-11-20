
DEFINE VARIABLE mov AS CHARACTER format "xxx" LABEL "Mov".
DEFINE VARIABLE qty AS DECIMAL LABEL "Cantidad".

DEFINE TEMP-TABLE wrkp_prod
    FIELDS wrkp_par  LIKE Almdmov.codmat
    FIELDS wrkp_cat  LIKE Almmmatg.catconta[1]
    FIELDS wrkp_des  LIKE Almmmatg.desmat
    FIELDS wrkp_mov  LIKE mov
    FIELDS wrkp_fch  LIKE Almdmov.FchDoc
    FIELDS wrkp_umr  LIKE Almdmov.CodUnd
    FIELDS wrkp_qty  LIKE qty
    FIELDS wrkp_cost LIKE Almdmov.VctoMn1
    FIELDS wrkp_ref1 LIKE Almcmov.nrorf1 COLUMN-LABEL "Ref 1"
    FIELDS wrkp_ref2 LIKE Almcmov.nrorf2 COLUMN-LABEL "Ref 2"
    FIELDS wrkp_fcho LIKE pr-odpc.fchord
    FIELDS wrkp_fchV LIKE pr-odpc.fchVTO
    FIELDS wrkp_site AS CHARACTER COLUMN-LABEL "Alm"
    FIELDS wrkp_serie LIKE Almdmov.Nroser
    FIELDS wrkp_nrodo LIKE Almdmov.NroDoc
    INDEX idx01 wrkp_ref1.

DEFINE TEMP-TABLE wrkd_prod
    FIELDS wrkd_par  LIKE Almdmov.codmat
    FIELDS wrkd_cat  LIKE Almmmatg.catconta[1]
    FIELDS wrkd_des  LIKE Almmmatg.desmat
    FIELDS wrkd_mov  LIKE mov
    FIELDS wrkd_fch  LIKE Almdmov.FchDoc
    FIELDS wrkd_umr  LIKE Almdmov.CodUnd
    FIELDS wrkd_qty  LIKE qty
    FIELDS wrkd_cost LIKE Almdmov.VctoMn1
    FIELDS wrkd_ref1 LIKE Almcmov.nrorf1 COLUMN-LABEL "Ref 1"
    FIELDS wrkd_ref2 LIKE Almcmov.nrorf2 COLUMN-LABEL "Ref 2"
    FIELDS wrkd_fcho LIKE pr-odpc.fchord
    FIELDS wrkd_fchV LIKE pr-odpc.fchVTO
    FIELDS wrkd_site AS CHARACTER COLUMN-LABEL "Alm"
    FIELDS wrkd_serie LIKE Almdmov.Nroser
    FIELDS wrkd_nrodo LIKE Almdmov.NroDoc
    INDEX idx01 wrkd_ref1.

FOR EACH Almmmatg NO-LOCK WHERE
    Almmmatg.codcia = 1
    /*
    LOOKUP(Almmmatg.catconta[1],"MP,PP,E1,S1") > 0
    */,
    EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE
    Almdmov.CodCia = Almmmatg.codcia AND
    Almdmov.codmat = Almmmatg.CodMat AND
    Almdmov.FchDoc >= 09/01/08 AND
    Almdmov.FchDoc <= TODAY AND
    Almdmov.TipMov = "I" AND
    Almdmov.codmov = 50:

    FIND Almcmov WHERE
        Almcmov.CodCia = Almdmov.codcia AND
        Almcmov.CodAlm = Almdmov.codalm AND
        Almcmov.TipMov = Almdmov.tipmov AND
        Almcmov.CodMov = Almdmov.codmov AND
        Almcmov.NroDoc = Almdmov.nrodoc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almcmov THEN NEXT.

    /* Busca OT */
    FIND pr-odpc WHERE
        pr-odpc.codcia = Almcmov.codcia AND
        PR-ODPC.NumOrd = SUBSTRING(Almcmov.nrorf1,3) NO-LOCK NO-ERROR.

    mov = Almdmov.TipMov + STRING(Almdmov.CodMov,"99").
    qty = Almdmov.CanDes * Almdmov.Factor.

    CREATE wrkp_prod.
    ASSIGN
        wrkp_par  = Almdmov.codmat
        wrkp_cat  = Almmmatg.catconta[1]
        wrkp_des  = Almmmatg.desmat
        wrkp_mov  = mov
        wrkp_fch  = Almdmov.FchDoc
        wrkp_umr  = Almdmov.CodUnd
        wrkp_qty  = qty
        wrkp_ref1 = Almcmov.nrorf1
        wrkp_ref2 = Almcmov.nrorf2
        wrkp_fcho = IF AVAILABLE pr-odpc THEN pr-odpc.fchord ELSE ?
        wrkp_fchV = IF AVAILABLE pr-odpc THEN pr-odpc.fchVTO ELSE ?
        wrkp_site = almdmov.codalm
        wrkp_serie = Almdmov.Nroser
        wrkp_nrodo = Almdmov.NroDoc
        wrkp_cost = Almdmov.VctoMn1.

END.


FOR EACH Almmmatg NO-LOCK WHERE
    Almmmatg.codcia = 1
    /*
    LOOKUP(Almmmatg.catconta[1],"MP,PP,E1,S1") > 0
    */ ,
    EACH Almdmov NO-LOCK USE-INDEX ALMD02 WHERE
    Almdmov.CodCia = Almmmatg.codcia AND
    Almdmov.codmat = Almmmatg.CodMat AND
    Almdmov.FchDoc >= 09/01/08 AND
    Almdmov.TipMov = "S" AND
    Almdmov.codmov = 50:

    FIND Almcmov WHERE
        Almcmov.CodCia = Almdmov.codcia AND
        Almcmov.CodAlm = Almdmov.codalm AND
        Almcmov.TipMov = Almdmov.tipmov AND
        Almcmov.CodMov = Almdmov.codmov AND
        Almcmov.NroDoc = Almdmov.nrodoc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almcmov THEN NEXT.

    /* Busca OT */
    FIND pr-odpc WHERE
        pr-odpc.codcia = Almcmov.codcia AND
        PR-ODPC.NumOrd = SUBSTRING(Almcmov.nrorf1,3) NO-LOCK NO-ERROR.

    mov = Almdmov.TipMov + STRING(Almdmov.CodMov,"99").
    qty = Almdmov.CanDes * Almdmov.Factor.

    CREATE wrkd_prod.
    ASSIGN
        wrkd_par  = Almdmov.codmat
        wrkd_cat  = Almmmatg.catconta[1]
        wrkd_des  = Almmmatg.desmat
        wrkd_mov  = mov
        wrkd_fch  = Almdmov.FchDoc
        wrkd_umr  = Almdmov.CodUnd
        wrkd_qty  = qty
        wrkd_ref1 = Almcmov.nrorf1
        wrkd_ref2 = Almcmov.nrorf2
        wrkd_fcho = IF AVAILABLE pr-odpc THEN pr-odpc.fchord ELSE ?
        wrkd_fchV = IF AVAILABLE pr-odpc THEN pr-odpc.fchVTO ELSE ?
        wrkd_site = almdmov.codalm
        wrkd_serie = Almdmov.Nroser
        wrkd_nrodo = Almdmov.NroDoc
        wrkd_cost = Almdmov.VctoMn1.

END.

OUTPUT TO D:\SIE\SALIDAS_ENTRADAS_CISSAC.TXT.

    DEF VAR primero AS LOGICAL NO-UNDO.

    PUT UNFORMATTED
        "ALMACEN|"
        "ARTICULO|"
        "CATEGORIA|"
        "DESCRIPCION|"
        "MOV|"
        "FECHA|"
        "UM|"
        "CANTIDAD|"
        "COSTO|"
        "ORDEN PROD|"
        "REFERENCIA 2|"
        "FECHA ORDEN|"
        "FECHA VCTO|"
        "SERIE|"
        "NUMERO|"
        "ALMACEN|"
        "COMPONENTE|"
        "CATEGORIA|"
        "DESCRIPCION|"
        "MOV|"
        "FECHA|"
        "UM|"
        "CANTIDAD|"
        "COSTO|"
        "ORDEN PROD|"
        "REFERENCIA 2|"
        "FECHA ORDEN|"
        "FECHA VCTO|"
        "SERIE|"
        "NUMERO"
        SKIP.

    FOR EACH wrkp_prod NO-LOCK
        BREAK BY wrkp_ref1:
        PUT UNFORMATTED
            wrkp_site "|"
            wrkp_par "|"
            wrkp_cat "|"
            wrkp_des "|"
            wrkp_mov "|"
            wrkp_fch "|"
            wrkp_umr "|"
            wrkp_qty "|"
            wrkp_cost "|"
            wrkp_ref1 "|"
            wrkp_ref2 "|"
            wrkp_fcho "|"
            wrkp_fchV "|"
            wrkp_serie "|"
            wrkp_nrodo "|".
        primero = TRUE.
        IF LAST-OF(wrkp_ref1) THEN
            FOR EACH wrkd_prod WHERE
            wrkd_ref1 = wrkp_ref1 NO-LOCK:
            IF PRIMERO THEN
            PUT UNFORMATTED
                wrkd_site "|"
                wrkd_par "|"
                wrkd_cat "|"
                wrkd_des "|"
                wrkd_mov "|"
                wrkd_fch "|"
                wrkd_umr "|"
                wrkd_qty "|"
                wrkd_cost "|"
                wrkd_ref1 "|"
                wrkd_ref2 "|"
                wrkd_fcho "|"
                wrkd_fchV "|"
                wrkd_serie "|"
                wrkd_nrodo
                SKIP.
           ELSE
           PUT UNFORMATTED
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                wrkd_site "|"
                wrkd_par "|"
                wrkd_cat "|"
                wrkd_des "|"
                wrkd_mov "|"
                wrkd_fch "|"
                wrkd_umr "|"
                wrkd_qty "|"
                wrkd_cost "|"
                wrkd_ref1 "|"
                wrkd_ref2 "|"
                wrkd_fcho "|"
                wrkd_fchV "|"
                wrkd_serie "|"
                wrkd_nrodo
                SKIP.
            PRIMERO = FALSE.
        END.
        ELSE PUT UNFORMATTED
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            "|"
            SKIP.
    END.

OUTPUT CLOSE.

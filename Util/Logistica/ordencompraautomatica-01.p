/* GENERACION DE ORDENES DE COMPRA AUTOMATICAS */
DEF TEMP-TABLE T-CAB LIKE FacCPedi.
DEF TEMP-TABLE T-DET LIKE FacDPedi.
DEF TEMP-TABLE X-DET LIKE FacDPedi.
DEF TEMP-TABLE DCMP LIKE integral.LG-DOCMP.
DEF TEMP-TABLE T-DCMP LIKE DCMP.
DEF TEMP-TABLE detalle
    FIELD codmat AS CHAR FORMAT 'x(6)'
    FIELD alm11e AS DEC
    FIELD alm34e AS DEC
    FIELD alm35e AS DEC.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
DEF VAR s-CodMon AS INT INIT 1 NO-UNDO.
DEF VAR s-TpoCmb AS DEC NO-UNDO.
DEF VAR s-CodAlm AS CHAR NO-UNDO.
DEF VAR x-StkMin AS DEC NO-UNDO.
DEF VAR x-StkAct AS DEC NO-UNDO.
DEF VAR x-Dias   AS INT INIT 1 NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.
DEF VAR x-Linea  AS CHAR FORMAT 'x(20)' NO-UNDO.

DEF VAR S-USER-ID AS CHAR INIT 'ADMIN' NO-UNDO.
DEF VAR X-CUENTA-ITEMS AS INT INIT 0 NO-UNDO.
DEF VAR S-CODDIV AS CHAR INIT '00000' NO-UNDO.
  
/* 1) BARREMOS LAS ORDENES DE COMPRA */
INPUT FROM c:\tmp\compras.prn.
REPEAT:
    IMPORT UNFORMATTED x-Linea.
    IF x-Linea = '' THEN LEAVE.
    FIND FIRST detalle WHERE detalle.codmat = SUBSTRING(x-Linea,1,6)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codmat = SUBSTRING(x-Linea,1,6)
            detalle.alm11e = DECIMAL(SUBSTRING(x-linea,11,10))
            detalle.alm34e = DECIMAL(SUBSTRING(x-linea,21,10))
            detalle.alm35e = DECIMAL(SUBSTRING(x-linea,31,10)).
    END.
END.
INPUT CLOSE.
DEF VAR x-impnegativo AS DEC NO-UNDO.
DEF VAR x-aplicar AS DEC NO-UNDO.
DEF VAR x-alm11e AS DEC NO-UNDO.
DEF VAR x-alm34e AS DEC NO-UNDO.
DEF VAR x-alm35e AS DEC NO-UNDO.
FOR EACH detalle.
    x-impnegativo = 0.
    ASSIGN
        x-alm11e = 0
        x-alm34e = 0
        x-alm35e = 0.
    IF detalle.alm11e > 0 THEN x-alm11e = detalle.alm11e.
    IF detalle.alm34e > 0 THEN x-alm34e = detalle.alm34e.
    IF detalle.alm35e > 0 THEN x-alm35e = detalle.alm35e.
    IF detalle.alm11e < 0 THEN x-impnegativo = x-impnegativo + detalle.alm11e.
    IF detalle.alm34e < 0 THEN x-impnegativo = x-impnegativo + detalle.alm34e.
    IF detalle.alm35e < 0 THEN x-impnegativo = x-impnegativo + detalle.alm35e.
    x-impnegativo = ABS(x-impnegativo).
    REPEAT:
        IF x-alm11e > 0 THEN DO:
            x-aplicar = MINIMUM(x-alm11e, x-impnegativo).
            x-alm11e = x-alm11e - x-aplicar.
            x-impnegativo = x-impnegativo - x-aplicar.
        END.
        IF x-impnegativo <= 0 THEN LEAVE.
        IF x-alm34e > 0 THEN DO:
            x-aplicar = MINIMUM(x-alm34e, x-impnegativo).
            x-alm34e = x-alm34e - x-aplicar.
            x-impnegativo = x-impnegativo - x-aplicar.
        END.
        IF x-impnegativo <= 0 THEN LEAVE.
        IF x-alm35e > 0 THEN DO:
            x-aplicar = MINIMUM(x-alm35e, x-impnegativo).
            x-alm35e = x-alm35e - x-aplicar.
            x-impnegativo = x-impnegativo - x-aplicar.
        END.
        LEAVE.
    END.
    IF x-alm11e > 0 THEN DO:
        CREATE x-det.
        ASSIGN
            x-det.codcia = s-codcia
            x-det.codmat = detalle.codmat
            x-det.almdes = '11e'
            x-det.canped = x-alm11e.
    END.
    IF x-alm34e > 0 THEN DO:
        CREATE x-det.
        ASSIGN
            x-det.codcia = s-codcia
            x-det.codmat = detalle.codmat
            x-det.almdes = '34e'
            x-det.canped = x-alm34e.
    END.
    IF x-alm35e > 0 THEN DO:
        CREATE x-det.
        ASSIGN
            x-det.codcia = s-codcia
            x-det.codmat = detalle.codmat
            x-det.almdes = '35e'
            x-det.canped = x-alm35e.
    END.
END.
/* FOR EACH t-det BY t-det.codmat BY t-det.almdes.     */
/*     DISPLAY t-det.codmat t-det.almdes t-det.canped. */
/* END.                                                */
/* RETURN.                                             */

DEF VAR k AS INT NO-UNDO.
DO k = 1 TO 3:
    IF k = 1 THEN s-codalm = '11e'.
    IF k = 2 THEN s-codalm = '34e'.
    IF k = 3 THEN s-codalm = '35e'.
    EMPTY TEMP-TABLE T-DET.
    FOR EACH X-DET WHERE X-DET.almdes = s-codalm:
        CREATE T-DET.
        ASSIGN
            T-DET.codcia = s-codcia
            T-DET.codmat = X-DET.codmat
            T-DET.CanPed = X-DET.canped.
    END.
    RUN Genera-Ordenes.
END.


PROCEDURE Genera-Ordenes:
/* ********************* */

    FOR EACH T-DET, FIRST Almmmatg OF T-DET NO-LOCK:
        ASSIGN
            T-DET.UndVta = Almmmatg.UndBas
            T-DET.Factor = 1.
        IF T-DET.CanPed <= 0 THEN DELETE T-DET.
    END.
    
    ASSIGN
        s-TpoCmb = 1.
    FIND LAST GN-TCMB WHERE GN-TCMB.Fecha <= TODAY NO-LOCK NO-ERROR.
    IF AVAILABLE GN-TCMB THEN s-TpoCmb = INTEGRAL.GN-TCMB.Compra.
    
    /* 2) ACUMULAMOS LOS MATERIALES */  
    EMPTY TEMP-TABLE DCMP.
    FOR EACH T-DET WHERE T-DET.codcia = s-codcia
        AND T-DET.codmat <> '' NO-LOCK,
        FIRST INTEGRAL.Almmmatg OF T-DET NO-LOCK
        BREAK BY T-DET.CodMat:
        IF FIRST-OF(T-DET.CodMat)
        THEN ASSIGN
                x-StkMin = 0
                x-StkAct = 0.
        x-StkMin = x-StkMin + T-DET.CanPed * T-DET.Factor.
        IF LAST-OF(T-DET.CodMat)
        THEN DO:
            /* MOVIMIENTO DE COMPRA */
            F-FACTOR = 1.
            FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                AND  Almtconv.Codalter = Almmmatg.UndCmp
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtconv THEN NEXT.
            F-FACTOR = Almtconv.Equival.
            /***** Se Usara con lista de Precios Proveedor Original *****/
            FIND FIRST LG-dmatpr WHERE LG-dmatpr.CodCia = Almmmatg.codcia
                AND LG-dmatpr.codpro = Almmmatg.codpr1
                AND LG-dmatpr.codmat = Almmmatg.codmat  
                AND CAN-FIND(FIRST lg-cmatpr OF lg-dmatpr WHERE lg-cmatpr.flgest = "A" NO-LOCK)
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE LG-dmatpr THEN NEXT.
            CREATE DCMP. 
            ASSIGN
                DCMP.CodCia    = s-CodCia
                DCMP.CodMat    = Almmmatg.codmat
                DCMP.CanPedi   = (x-StkMin * x-Dias) - x-StkAct
                DCMP.Dsctos[1] = LG-dmatpr.Dsctos[1]
                DCMP.Dsctos[2] = LG-dmatpr.Dsctos[2]
                DCMP.Dsctos[3] = LG-dmatpr.Dsctos[3]
                DCMP.IgvMat    = LG-dmatpr.IgvMat
                DCMP.UndCmp    = Almmmatg.UndStk
                DCMP.ArtPro    = Almmmatg.ArtPro.
                /*DCMP.CanAten   = S-Codmon.*/
            IF S-CODMON = 1 THEN DO:
                IF LG-dmatpr.CodMon = 1 
                THEN DCMP.PreUni = LG-dmatpr.PreAct.
                ELSE DCMP.PreUni = ROUND(LG-dmatpr.PreAct * S-TPOCMB,4).
            END.
            ELSE DO:
                IF LG-dmatpr.CodMon = 2 
                THEN DCMP.PreUni = LG-dmatpr.PreAct.
                ELSE DCMP.PreUni = ROUND(LG-dmatpr.PreAct / S-TPOCMB,4).
            END.
            ASSIGN
                DCMP.ImpTot = ROUND(DCMP.CanPedi * ROUND(DCMP.PreUni * 
                                    (1 - (DCMP.Dsctos[1] / 100)) *
                                    (1 - (DCMP.Dsctos[2] / 100)) *
                                    (1 - (DCMP.Dsctos[3] / 100)) *
                                    (1 + (DCMP.IgvMat / 100)) , 4),2).
        END.
    END.
    
    /* REDONDEAMOS AL EMPAQUE */
    FOR EACH DCMP, FIRST Almmmatg OF DCMP NO-LOCK:
        IF Almmmatg.CanEmp <= 0 THEN NEXT.
        IF DCMP.CanPedi MODULO Almmmatg.CanEmp <> 0
            THEN DO:
            ASSIGN
                DCMP.CanPedi = ( TRUNCATE(DCMP.CanPedi / Almmmatg.CanEmp, 0) + 1 ) * Almmmatg.CanEmp
                DCMP.ImpTot    = ROUND(DCMP.CanPedi * ROUND(DCMP.PreUni * 
                                                            (1 - (DCMP.Dsctos[1] / 100)) *
                                                            (1 - (DCMP.Dsctos[2] / 100)) *
                                                            (1 - (DCMP.Dsctos[3] / 100)) *
                                                            (1 + (DCMP.IgvMat / 100)) , 4),2).
        END.
    END.
/*     MESSAGE 'LISTO PARA COMENZAR'.                 */
/*     FOR EACH dcmp.                                 */
/*         DISPLAY s-codalm dcmp.codmat dcmp.canpedi. */
/*     END.                                           */
/*     RETURN.                                        */

    /* 3) ORDENAMOS POR PROVEEDOR */
    EMPTY TEMP-TABLE T-DCMP.
    FOR EACH DCMP, FIRST Almmmatg WHERE almmmatg.codcia = s-codcia
        AND almmmatg.codmat = DCMP.codmat NO-LOCK
        BREAK BY Almmmatg.CodPr1 ON ERROR UNDO, RETURN:
        IF FIRST-OF(Almmmatg.codpr1) THEN x-Cuenta-Items = 0.
        CREATE T-DCMP.
        BUFFER-COPY DCMP TO T-DCMP.
        x-Cuenta-Items = x-Cuenta-Items + 1.
        IF LAST-OF(Almmmatg.CodPr1) OR x-Cuenta-Items > 16
        THEN DO:
            /* CREAMOS ORDEN DE COMPRA */
            FIND LG-CORR WHERE LG-CORR.CodCia = S-CODCIA 
                AND LG-CORR.CodDoc = "O/C" 
                AND LG-CORR.CodDiv = s-CodDiv
                EXCLUSIVE-LOCK NO-ERROR.
            CREATE LG-COCmp.
            ASSIGN 
                LG-COCmp.CodCia = S-CODCIA
                LG-COCmp.CodDiv = s-CodDiv
                LG-COCmp.TpoDoc = "N"
                LG-COCmp.NroDoc = LG-CORR.NroDoc
                LG-COCmp.CodPro = Almmmatg.CodPr1
                LG-COCmp.FlgSit = "G"               
                /*LG-COCmp.NroReq = I-NROREQ*/
                LG-COCmp.Userid-com = S-USER-ID
                LG-COCmp.FchDoc = TODAY
                LG-COCmp.TpoCmb = s-TpoCmb
                LG-COCmp.CodMon = s-CodMon
                LG-COCmp.CodAlm = s-CodAlm
                LG-COCmp.ModAdq = 'MC'.
            ASSIGN
                LG-CORR.NroDoc  = LG-CORR.NroDoc + 1.
            DISPLAY LG-COCmp.NroDoc.
            PAUSE 0.
            RELEASE LG-CORR.
            FIND gn-prov WHERE gn-prov.CodCia = 0 
                AND  gn-prov.CodPro = LG-COCmp.CodPro 
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN LG-COCmp.NomPro = gn-prov.NomPro.
            ASSIGN 
                LG-COCmp.ImpTot = 0
                LG-COCmp.ImpExo = 0.
            ASSIGN
                LG-COCmp.CndCmp = "100".    /* LETRAS CAMPAÑA */
    /*         FIND FIRST LG-cmatpr WHERE LG-cmatpr.CodCia = s-codcia */
    /*             AND LG-cmatpr.codpro = LG-COCmp.CodPro             */
    /*             AND  LG-cmatpr.FlgEst = "A"                        */
    /*             NO-LOCK NO-ERROR.                                  */
    /*         IF AVAILABLE LG-cmatpr                                 */
    /*         THEN ASSIGN                                            */
    /*                 LG-COCmp.CndCmp = LG-cmatpr.CndCmp.            */
    
            RUN Genera-Detalle.
    
            FIND LAST LG-CFGIGV NO-LOCK NO-ERROR.
            ASSIGN 
                LG-COCmp.ImpIgv = (LG-COCmp.ImpTot - LG-COCmp.ImpExo) - 
                                     ROUND((LG-COCmp.ImpTot - LG-COCmp.ImpExo) / (1 + LG-CFGIGV.PorIgv / 100),2)
                LG-COCmp.ImpBrt = LG-COCmp.ImpTot - LG-COCmp.ImpExo - LG-COCmp.ImpIgv
                LG-COCmp.ImpDto = 0
                LG-COCmp.ImpNet = LG-COCmp.ImpBrt - LG-COCmp.ImpDto.
    
    
            x-Cuenta-Items = 0.
            EMPTY TEMP-TABLE T-DCMP.
        END.
    END.

END PROCEDURE.


PROCEDURE Genera-Detalle:
/* ********************* */

FOR EACH T-DCMP NO-LOCK ON ERROR UNDO, RETURN:
     /* DETALLE */
     CREATE LG-DOCmp.
     BUFFER-COPY T-DCMP TO LG-DOCmp
         ASSIGN
         LG-DOCmp.codcia = LG-COCmp.codcia
         LG-DOCmp.tpodoc = LG-COCmp.tpodoc
         LG-DOCmp.nrodoc = LG-COCmp.nrodoc.
     /* TOTALES */            
     ASSIGN
         LG-COCmp.ImpTot = LG-COCmp.ImpTot + T-DCMP.ImpTot
         LG-COCmp.ImpExo = LG-COCmp.ImpExo + 
                            ( IF T-DCMP.IgvMat = 0 THEN T-DCMP.ImpTot ELSE 0).
END.
 
END PROCEDURE.





/* ***************************  Main Block  *************************** */
  DEF TEMP-TABLE T-CAB LIKE FacCPedi.
  DEF TEMP-TABLE T-DET LIKE FacDPedi.

 
  DEF TEMP-TABLE DCMP LIKE integral.LG-DOCMP.
  
  DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.
  DEF VAR s-CodMon AS INT INIT 1 NO-UNDO.
  DEF VAR s-TpoCmb AS DEC NO-UNDO.
  DEF VAR x-StkMin AS DEC NO-UNDO.
  DEF VAR x-StkAct AS DEC NO-UNDO.
  DEF VAR x-Dias   AS INT INIT 1 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR f-Factor AS DEC NO-UNDO.
  DEF VAR x-Linea  AS CHAR FORMAT 'x(20)' NO-UNDO.
  
  /* BARREMOS LAS ORDENES DE COMPRA */
  INPUT FROM m:\tmp\compras.prn.
  REPEAT:
    IMPORT UNFORMATTED x-Linea.
    IF substring(x-linea,1,6)  <> '' THEN DO:
        FIND FIRST t-det WHERE t-det.codmat = SUBSTRING(x-Linea,1,6)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-det THEN DO:
            CREATE T-DET.
            ASSIGN
                T-DET.codcia = s-codcia
                T-DET.codmat = SUBSTRING(x-Linea,1,6)
                T-DET.CanPed = DECIMAL(SUBSTRING(x-Linea,7,11)).
        END.
    END.
  END.
  INPUT CLOSE.
  FOR EACH T-DET, FIRST Almmmatg OF T-DET NO-LOCK:
    ASSIGN
        T-DET.UndVta = Almmmatg.UndBas
        T-DET.Factor = 1.
    IF T-DET.CanPed <= 0 THEN DELETE T-DET.
  END.

  s-TpoCmb = 1.
  FIND LAST INTEGRAL.GN-TCMB WHERE INTEGRAL.GN-TCMB.Fecha <= TODAY NO-LOCK NO-ERROR.
  IF AVAILABLE INTEGRAL.GN-TCMB
  THEN DO:
    s-TpoCmb = INTEGRAL.GN-TCMB.Compra.
  END.

  /* ACUMULAMOS LOS MATERIALES */  
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
        FIND INTEGRAL.Almtconv WHERE INTEGRAL.Almtconv.CodUnid = INTEGRAL.Almmmatg.UndBas
            AND  INTEGRAL.Almtconv.Codalter = INTEGRAL.Almmmatg.UndCmp
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE INTEGRAL.Almtconv THEN NEXT.
        F-FACTOR = INTEGRAL.Almtconv.Equival.
        /***** Se Usara con lista de Precios Proveedor Original *****/
        FIND FIRST INTEGRAL.LG-dmatpr WHERE INTEGRAL.LG-dmatpr.CodCia = INTEGRAL.Almmmatg.codcia
            AND  INTEGRAL.LG-dmatpr.codpro = INTEGRAL.Almmmatg.codpr1
            AND  INTEGRAL.LG-dmatpr.codmat = INTEGRAL.Almmmatg.codmat  
            AND  INTEGRAL.LG-dmatpr.FlgEst = "A" 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE INTEGRAL.LG-dmatpr THEN NEXT.
        CREATE DCMP. 
        ASSIGN
            DCMP.CodCia    = s-CodCia
            DCMP.CodMat    = INTEGRAL.Almmmatg.codmat
            DCMP.CanPedi   = (x-StkMin * x-Dias) - x-StkAct
            DCMP.Dsctos[1] = INTEGRAL.LG-dmatpr.Dsctos[1]
            DCMP.Dsctos[2] = INTEGRAL.LG-dmatpr.Dsctos[2]
            DCMP.Dsctos[3] = INTEGRAL.LG-dmatpr.Dsctos[3]
            DCMP.IgvMat    = INTEGRAL.LG-dmatpr.IgvMat
            DCMP.UndCmp    = INTEGRAL.Almmmatg.UndStk
            DCMP.ArtPro    = INTEGRAL.Almmmatg.ArtPro
            DCMP.CanAten   = S-Codmon.
        IF S-CODMON = 1 THEN DO:
            IF INTEGRAL.LG-dmatpr.CodMon = 1 
            THEN DCMP.PreUni = INTEGRAL.LG-dmatpr.PreAct.
            ELSE DCMP.PreUni = ROUND(INTEGRAL.LG-dmatpr.PreAct * S-TPOCMB,4).
        END.
        ELSE DO:
            IF INTEGRAL.LG-dmatpr.CodMon = 2 
            THEN DCMP.PreUni = INTEGRAL.LG-dmatpr.PreAct.
            ELSE DCMP.PreUni = ROUND(INTEGRAL.LG-dmatpr.PreAct / S-TPOCMB,4).
        END.
        ASSIGN
            DCMP.ImpTot    = ROUND(DCMP.CanPedi * ROUND(DCMP.PreUni * 
                                    (1 - (DCMP.Dsctos[1] / 100)) *
                                    (1 - (DCMP.Dsctos[2] / 100)) *
                                    (1 - (DCMP.Dsctos[3] / 100)) *
                                    (1 + (DCMP.IgvMat / 100)) , 4),2).

    END.
  END.

/*  /* REDONDEAMOS AL EMPAQUE */
 *   FOR EACH DCMP, FIRST Almmmatg OF DCMP NO-LOCK:
 *     IF Almmmatg.CanEmp <= 0 THEN NEXT.
 *     IF DCMP.CanPedi MODULO Almmmatg.CanEmp <> 0
 *     THEN DO:
 *         ASSIGN
 *             DCMP.CanPedi = ( TRUNCATE(DCMP.CanPedi / Almmmatg.CanEmp, 0) + 1 ) * Almmmatg.CanEmp
 *             DCMP.ImpTot    = ROUND(DCMP.CanPedi * ROUND(DCMP.PreUni * 
 *                                     (1 - (DCMP.Dsctos[1] / 100)) *
 *                                     (1 - (DCMP.Dsctos[2] / 100)) *
 *                                     (1 - (DCMP.Dsctos[3] / 100)) *
 *                                     (1 + (DCMP.IgvMat / 100)) , 4),2).
 *     END.
 *   END.
 * */
  MESSAGE 'LISTO PARA COMENZAR'.
  
  DEF VAR S-USER-ID AS CHAR INIT 'SISTEMAS' NO-UNDO.
  DEF VAR S-CODALM AS CHAR INIT '15' NO-UNDO.
  DEF VAR X-CUENTA-ITEMS AS INT INIT 0 NO-UNDO.
  DEF VAR S-CODDIV AS CHAR INIT '00000' NO-UNDO.
  
  /* ORDENAMOS POR PROVEEDOR */
  DEF TEMP-TABLE T-DCMP LIKE DCMP.
  FOR EACH DCMP,
        FIRST Almmmatg WHERE almmmatg.codcia = s-codcia
            AND almmmatg.codmat = DCMP.codmat NO-LOCK
        BREAK BY Almmmatg.CodPr1 ON ERROR UNDO, RETURN:
    IF FIRST-OF(Almmmatg.codpr1)
    THEN DO:
        x-Cuenta-Items = 0.
    END.
    CREATE T-DCMP.
    BUFFER-COPY DCMP TO T-DCMP.
    x-Cuenta-Items = x-Cuenta-Items + 1.
    IF LAST-OF(Almmmatg.CodPr1) OR x-Cuenta-Items > 13
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
        FIND FIRST LG-cmatpr WHERE LG-cmatpr.CodCia = s-codcia
            AND LG-cmatpr.codpro = LG-COCmp.CodPro
            AND  LG-cmatpr.FlgEst = "A" 
            NO-LOCK NO-ERROR.
        IF AVAILABLE LG-cmatpr
        THEN ASSIGN
                /*LG-COCmp.CndCmp = LG-cmatpr.CndCmp.    */
                LG-COCmp.CndCmp = '100'.

        RUN Genera-Detalle.

        FIND LAST LG-CFGIGV NO-LOCK NO-ERROR.
        ASSIGN 
            LG-COCmp.ImpIgv = (LG-COCmp.ImpTot - LG-COCmp.ImpExo) - 
                                 ROUND((LG-COCmp.ImpTot - LG-COCmp.ImpExo) / (1 + LG-CFGIGV.PorIgv / 100),2)
            LG-COCmp.ImpBrt = LG-COCmp.ImpTot - LG-COCmp.ImpExo - LG-COCmp.ImpIgv
            LG-COCmp.ImpDto = 0
            LG-COCmp.ImpNet = LG-COCmp.ImpBrt - LG-COCmp.ImpDto.


        x-Cuenta-Items = 0.
        FOR EACH T-DCMP:
            DELETE T-DCMP.
        END.
    END.
  END.



PROCEDURE Genera-Detalle:

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





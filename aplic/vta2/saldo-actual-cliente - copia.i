/* NOTA: Programas afectados:
    vta2/linea-de-credito-01.p
    ccb/v-consul-cctv3.w
    
*/

/* Sintaxis:
    {vta2/saldo-actual-cliente 
    &pCodCli=<Var CodCli>  
    &pSaldoDoc=<Var SaldoDoc> 
    &pDeuda=<Var Deuda>
    &pMonLCred=<Var MonLCred>
    &fSdoAct=<Func fSdoAct
*/

/* Suma Todos los Documentos Pendientes */
FOR EACH CcbCDocu NO-LOCK WHERE CcbCDocu.CodCia = S-CODCIA 
    AND CcbCDocu.CodCli = {&pCodCli} 
    AND CcbCDocu.FlgEst = "P",
    FIRST FacDocum OF CcbCDocu NO-LOCK WHERE FacDocum.TpoDoc <> ?:
    /* ******************************************************* */
    /* DOCUMENTOS QUE NO DEBEN AFECTAR LA LINEA DE CREDITO     */
    /* ******************************************************* */
    IF LOOKUP(CcbCDocu.CodDoc, 'FAC,BOL,N/C,N/D,BD,A/C,A/R,LET,CHQ,DCO,FAI,NCI,PAG') = 0 THEN NEXT.
/*     /* 1ro LETRAS ADELANTAS */                                              */
/*     IF Ccbcdocu.coddoc = 'LET' AND Ccbcdocu.codref = 'CLA' THEN NEXT.       */
/*     /* 2do. A/R por LETRAS ADELANTAS */                                     */
/*     IF Ccbcdocu.coddoc = 'A/R' AND Ccbcdocu.codref = 'CLA' THEN NEXT.       */
/*     /* 3ro. A/C POR FACTURAS ADELANTADAS */                                 */
/*     IF Ccbcdocu.coddoc = 'A/C' THEN NEXT.                                   */
/*     /* 4to. BD Boletas de Dep�sitoy A/R Anticipos de Efectivo en general */ */
/*     /* por qu�? Estos documentos pueden inflar la L�nea de Cr�dito */       */
/*     IF LOOKUP(Ccbcdocu.coddoc, 'BD,A/R') > 0 THEN NEXT.                     */
    /* *************************************************************** */
    /* RHC 07/12/17 Casos especiales: 00150 Atlas y 00070 Per� Compras */
    /* *************************************************************** */
    IF LOOKUP(s-CodDiv, '00150,00070') > 0 THEN DO:
        IF Ccbcdocu.DivOri <> s-CodDiv THEN NEXT.
    END.
    /* *************************************************************** */
    /* *************************************************************** */
    {&pSaldoDoc} = CcbCDocu.SdoAct.
    IF {&pMonLCred} = 1 THEN DO:
        IF CcbCDocu.CodMon = 2 THEN {&pSaldoDoc} = {&pSaldoDoc} * FacCfgGn.Tpocmb[1].
    END.
    ELSE DO:
        IF CcbCDocu.CodMon = 1 THEN {&pSaldoDoc} = {&pSaldoDoc} / FacCfgGn.Tpocmb[1].
    END.
    IF FacDocum.TpoDoc
    THEN {&pDeuda} = {&pDeuda} + {&pSaldoDoc}.
    ELSE {&pDeuda} = {&pDeuda} - {&pSaldoDoc}.
END.

/* Suma Pedidos Credito Pendientes Generar Guias */
FOR EACH FacCPedi NO-LOCK WHERE FacCPedi.CodCia = S-CODCIA 
    AND FacCPedi.CodDoc = "PED" 
    AND FacCPedi.CodCli = {&pCodCli} 
    AND LOOKUP(FacCPedi.FlgEst, "G,X,P,W,WX,WL") > 0:
    FIND FIRST gn-convt WHERE gn-convt.codig = Faccpedi.FmaPgo NO-LOCK.
    IF gn-convt.tipvta = "1" THEN NEXT.     /* NO CONTADOS */
    /* *************************************************************** */
    /* RHC 07/12/17 Casos especiales: 00150 Atlas y 00070 Per� Compras */
    /* *************************************************************** */
    IF LOOKUP(s-CodDiv, '00150,00070') > 0 THEN DO:
        IF Faccpedi.CodDiv <> s-CodDiv THEN NEXT.
    END.
    /* *************************************************************** */
    /* *************************************************************** */
    {&pSaldoDoc} = {&fSdoAct}().
    IF {&pMonLCred} = 1 THEN DO:
       IF FacCpedi.CodMon = 2 THEN {&pSaldoDoc} = {&pSaldoDoc} * FacCfgGn.Tpocmb[1].
    END.
    ELSE DO:
       IF FacCpedi.CodMon = 1 THEN {&pSaldoDoc} = {&pSaldoDoc} / FacCfgGn.Tpocmb[1].
    END.
    {&pDeuda} = {&pDeuda} + {&pSaldoDoc}.
END.

/* Suma Ordenes Despacho Pendientes Generar Guias */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH FacCPedi NO-LOCK WHERE FacCPedi.coddiv = gn-divi.coddiv
    AND FacCPedi.CodCia = S-CODCIA 
    AND FacCPedi.CodDoc = "O/D" 
    AND FacCPedi.CodCli = {&pCodCli} 
    AND FacCPedi.FlgEst = "P":
    FIND FIRST gn-convt WHERE gn-convt.codig = Faccpedi.FmaPgo NO-LOCK.
    IF gn-convt.tipvta = "1" THEN NEXT.     /* NO CONTADOS */
    /* *************************************************************** */
    /* RHC 07/12/17 Casos especiales: 00150 Atlas y 00070 Per� Compras */
    /* *************************************************************** */
    IF LOOKUP(s-CodDiv, '00150,00070') > 0 THEN DO:
        IF Faccpedi.CodDiv <> s-CodDiv THEN NEXT.
    END.
    /* *************************************************************** */
    /* *************************************************************** */
    {&pSaldoDoc} = {&fSdoAct}().
    IF {&pMonLCred} = 1 THEN DO:
       IF FacCpedi.CodMon = 2 THEN {&pSaldoDoc} = {&pSaldoDoc} * FacCfgGn.Tpocmb[1].
    END.
    ELSE DO:
       IF FacCpedi.CodMon = 1 THEN {&pSaldoDoc} = {&pSaldoDoc} / FacCfgGn.Tpocmb[1].
    END.
    {&pDeuda} = {&pDeuda} + {&pSaldoDoc}.
END.
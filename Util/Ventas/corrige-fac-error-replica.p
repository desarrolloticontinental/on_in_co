DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'fac' NO-UNDO.
DEF VAR s-nrodoc AS CHAR INIT '25100010940' NO-UNDO.
DEF VAR s-nroped AS CHAR INIT '124044936' NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00000' NO-UNDO.
DEF VAR iCountItem AS INT NO-UNDO.

DEFINE NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN'.

FIND faccfggn WHERE faccfggn.codcia = s-codcia NO-LOCK.

FIND faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = 'O/D'
    AND faccpedi.nroped = s-nroped
    NO-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'PED' s-nroped 'no encontrado' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = s-coddoc
    AND ccbcdocu.nrodoc = s-nrodoc
    EXCLUSIVE-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE s-coddoc s-nrodoc 'NO encontrado' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
MESSAGE 'documento:' SKIP
    ccbcdocu.coddoc ccbcdocu.nrodoc SKIP
    ccbcdocu.nomcli SKIP
    ccbcdocu.imptot SKIP
    'Continuamos?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.
FOR EACH ccbddocu OF ccbcdocu:
    DELETE ccbddocu.
END.

FOR EACH almcmov WHERE almcmov.codcia = ccbcdocu.codcia
    AND almcmov.codref = ccbcdocu.coddoc
    AND almcmov.nroref = ccbcdocu.nrodoc:
    DISPLAY almcmov.codalm almcmov.nrodoc.
    FOR EACH almdmov OF almcmov:
        DELETE almdmov.
    END.
    DELETE almcmov.
END.

ASSIGN
    CcbCDocu.CodCli = Faccpedi.codcli
    CcbCDocu.NomCli = Faccpedi.nomcli
    CcbCDocu.DirCli = Faccpedi.dircli
    CcbCDocu.RucCli = Faccpedi.ruccli
    Ccbcdocu.CodDiv = s-CodDiv
    Ccbcdocu.DivOri = FacCPedi.CodDiv    /* OJO: division de estadisticas */
    Ccbcdocu.CodAlm = FacCPedi.CodAlm   /* OJO: Almacén despacho */
    Ccbcdocu.CodRef = FacCPedi.CodDoc           /* CONTROL POR DEFECTO */
    Ccbcdocu.NroRef = FacCPedi.NroPed
    Ccbcdocu.Libre_c01 = FacCPedi.CodDoc        /* CONTROL ADICIONAL */
    Ccbcdocu.Libre_c02 = FacCPedi.NroPed
    Ccbcdocu.CodPed = FacCPedi.CodRef
    Ccbcdocu.NroPed = FacCPedi.NroRef
    Ccbcdocu.CodAnt = FacCPedi.Atencion     /* DNI */
    Ccbcdocu.TpoCmb = FacCfgGn.TpoCmb[1]
    Ccbcdocu.NroOrd = FacCPedi.ordcmp
    Ccbcdocu.FlgEst = "P"
    Ccbcdocu.TpoFac = "CR"                  /* CREDITO */
    Ccbcdocu.Tipo   = "CREDITO"  /*pOrigen*/
    Ccbcdocu.LugEnt2 = FacCPedi.LugEnt2
    Ccbcdocu.FlgCbd = FacCPedi.FlgIgv.
iCountItem = 1.
FOR EACH facdpedi OF faccpedi NO-LOCK, FIRST Almmmatg OF Facdpedi NO-LOCK:
    MESSAGE 'Producto:' facdpedi.codmat facdpedi.canped facdpedi.implin SKIP
        'Agregamos a la factura?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE rpta2 AS LOG.
    IF rpta2 = YES THEN DO:
        CREATE Ccbddocu.
        BUFFER-COPY Facdpedi TO Ccbddocu
        ASSIGN
            Ccbddocu.NroItm = iCountItem
            Ccbddocu.CodCia = Ccbcdocu.CodCia
            Ccbddocu.CodDiv = Ccbcdocu.CodDiv
            Ccbddocu.Coddoc = Ccbcdocu.Coddoc
            Ccbddocu.NroDoc = Ccbcdocu.NroDoc 
            Ccbddocu.FchDoc = Ccbcdocu.FchDoc
            Ccbddocu.CanDes = Facdpedi.CanAte
            Ccbddocu.impdcto_adelanto[4] = Facdpedi.Libre_d02.  /* Flete Unitario */
        ASSIGN
            Ccbddocu.Pesmat = Almmmatg.Pesmat * (Ccbddocu.Candes * Ccbddocu.Factor).
        /* CORREGIMOS IMPORTES */
        ASSIGN
            Ccbddocu.ImpLin = ROUND ( Ccbddocu.CanDes * Ccbddocu.PreUni * 
                                      ( 1 - Ccbddocu.Por_Dsctos[1] / 100 ) *
                                      ( 1 - Ccbddocu.Por_Dsctos[2] / 100 ) *
                                      ( 1 - Ccbddocu.Por_Dsctos[3] / 100 ), 2 ).
        IF Ccbddocu.Por_Dsctos[1] = 0 AND Ccbddocu.Por_Dsctos[2] = 0 AND Ccbddocu.Por_Dsctos[3] = 0 
            THEN Ccbddocu.ImpDto = 0.
        ELSE Ccbddocu.ImpDto = Ccbddocu.CanDes * Ccbddocu.PreUni - Ccbddocu.ImpLin.
        ASSIGN
            Ccbddocu.ImpLin = ROUND(Ccbddocu.ImpLin, 2)
            Ccbddocu.ImpDto = ROUND(Ccbddocu.ImpDto, 2).
        IF Ccbddocu.AftIsc 
            THEN Ccbddocu.ImpIsc = ROUND(Ccbddocu.PreBas * Ccbddocu.CanDes * (Almmmatg.PorIsc / 100),4).
        ELSE Ccbddocu.ImpIsc = 0.
        IF Ccbddocu.AftIgv 
            THEN Ccbddocu.ImpIgv = Ccbddocu.ImpLin - ROUND( Ccbddocu.ImpLin  / ( 1 + (Ccbcdocu.PorIgv / 100) ), 4 ).
        ELSE Ccbddocu.ImpIgv = 0.
        iCountItem = iCountItem + 1.
    END.
    IF iCountItem > 13 THEN LEAVE.
END.
RUN proc_GrabaTotales.
/* ACTUALIZAMOS ALMACENES */
DEF VAR pMensaje AS CHAR NO-UNDO.
RUN vta2/act_almv2 ( INPUT ROWID(CcbCDocu), OUTPUT pMensaje ).
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    MESSAGE "ERROR: NO se pudo actualizar el Kardex" VIEW-AS ALERT-BOX ERROR.
    UNDO, RETURN.
END.



PROCEDURE proc_GrabaTotales:
/* ************************ */
  DEFINE VARIABLE F-IGV LIKE Ccbcdocu.ImpIgv NO-UNDO.
  DEFINE VARIABLE F-ISC LIKE ccbcdocu.ImpIsc NO-UNDO.
  DEFINE VARIABLE F-ImpDtoAdelanto AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-IgvDtoAdelanto AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-ImpLin LIKE Ccbddocu.ImpLin NO-UNDO.

  ASSIGN
      Ccbcdocu.ImpDto = 0
      Ccbcdocu.ImpIgv = 0
      Ccbcdocu.ImpIsc = 0
      Ccbcdocu.ImpTot = 0
      Ccbcdocu.ImpExo = 0
      Ccbcdocu.ImpTot2= 0
      F-IGV = 0
      F-ISC = 0
      F-ImpDtoAdelanto = 0
      F-IgvDtoAdelanto = 0
      F-ImpLin = 0
      /*Ccbcdocu.Libre_d01 = 0*/    /* DESCUENTO LISTA EXPRESS WEB */
      Ccbcdocu.Libre_d02 = 0.
  /* RHC 14/03/2013 Nuevo cálculo */
  FOR EACH Ccbddocu OF Ccbcdocu:
      ASSIGN
          F-Igv = F-Igv + Ccbddocu.ImpIgv
          F-Isc = F-Isc + Ccbddocu.ImpIsc
          Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + Ccbddocu.ImpLin.
      /* Importe Inafecto o Exonerado */
      IF Ccbddocu.ImpIgv = 0 THEN Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2).
  END.
  ASSIGN
      Ccbcdocu.ImpTot = Ccbcdocu.ImpTot - Ccbcdocu.ImpDto2  /* <<< OJO: Descuento total Lista Express */
      Ccbcdocu.ImpIsc = ROUND(F-ISC,2)
      Ccbcdocu.ImpVta = ROUND( (Ccbcdocu.ImpTot - Ccbcdocu.ImpExo) / (1 + Ccbcdocu.PorIgv / 100), 2).
  IF Ccbcdocu.ImpExo = 0 THEN Ccbcdocu.ImpIgv = Ccbcdocu.ImpTot - Ccbcdocu.ImpVta.
  ELSE Ccbcdocu.ImpIgv = ROUND(Ccbcdocu.ImpVta * Ccbcdocu.PorIgv / 100, 2).
  ASSIGN
      Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta + Ccbcdocu.ImpDto + Ccbcdocu.Libre_d01  /* Dcto Lista Express SIN IGV */
      Ccbcdocu.SdoAct = Ccbcdocu.ImpTot.

  IF Ccbcdocu.PorIgv = 0.00     /* VENTA INAFECTA */
      THEN ASSIGN
          Ccbcdocu.ImpIgv = 0
          Ccbcdocu.ImpVta = Ccbcdocu.ImpExo
          Ccbcdocu.ImpBrt = Ccbcdocu.ImpExo.
  /* ************************************* */
  /* RHC 22/07/2016 TRANSFERENCIA GRATUITA */
  /* ************************************* */
  IF Ccbcdocu.FmaPgo = "900" THEN
      ASSIGN
      Ccbcdocu.ImpBrt = 0
      Ccbcdocu.ImpDto = 0
      Ccbcdocu.ImpExo = Ccbcdocu.ImpTot
      Ccbcdocu.ImpVta = 0
      Ccbcdocu.ImpIgv = 0.
  IF Ccbcdocu.FmaPgo = "899" THEN
      ASSIGN
      Ccbcdocu.ImpBrt = Ccbcdocu.ImpTot - Ccbcdocu.ImpIgv
      Ccbcdocu.ImpDto = 0
      Ccbcdocu.ImpExo = 0
      Ccbcdocu.ImpVta = 0
      Ccbcdocu.ImpTot = 0
      Ccbcdocu.SdoAct = 0.
  /* ************************************* */

END PROCEDURE.


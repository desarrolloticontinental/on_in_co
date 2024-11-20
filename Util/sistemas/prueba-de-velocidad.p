&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Banco de prueba de velocidad

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.23
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF faccorre.
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF almmmatg.

DEF STREAM reporte.

DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.

/* 2da. prueba: Solo Lectura */
OUTPUT STREAM reporte TO d:\newsie\on_in_co\util\sistemas\controlveloc.txt.
PUT STREAM reporte 'INICIO: ' TODAY STRING(TIME,'HH:MM:SS') SKIP.
FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia:
END.
PUT STREAM reporte 'FIN: ' TODAY STRING(TIME,'HH:MM:SS') SKIP.
OUTPUT STREAM reporte CLOSE.
RETURN.



DEF VAR s-coddoc AS CHAR INIT 'FAC' NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '11' NO-UNDO.
DEF VAR S-UNDVTA AS CHAR NO-UNDO.
DEF VAR pMensaje AS CHAR NO-UNDO.

DEF VAR x-Control AS INT NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.
DEF VAR F-PREBAS AS DEC DECIMALS 4.
DEF VAR F-PREVTA AS DEC DECIMALS 4.
DEF VAR F-DSCTOS AS DEC.       /* Descuento incluido en el precio unitario base */
DEF VAR Y-DSCTOS AS DEC.       /* Descuento por Volumen y/o Promocional */
DEF VAR Z-DSCTOS AS DEC.       /* Descuento por evento */
DEF VAR X-TIPDTO AS CHAR.      /* Tipo de descuento aplicado (PROM, VOL) */ 
DEF VAR f-FleteUnitario AS DEC.
DEF VAR s-TipVta AS CHAR.      /* Lista "A" o "B" */
DEF VAR PpError AS LOG.          /* Mostrar el error en pantalla */

DEF TEMP-TABLE T-CDOCU-1 LIKE Ccbcdocu.
DEF TEMP-TABLE T-CDOCU-2 LIKE Ccbcdocu.

EMPTY TEMP-TABLE T-CDOCU-1.
EMPTY TEMP-TABLE T-CDOCU-2.
OUTPUT STREAM reporte TO d:\newsie\on_in_co\util\sistemas\controlveloc.txt.
PUT STREAM reporte 'INICIO: ' TODAY STRING(TIME,'HH:MM:SS') SKIP.
DO x-Control = 1 TO 20:
    FIND FIRST Faccorre WHERE FacCorre.CodCia = s-codcia
        AND FacCorre.CodDiv = s-coddiv
        AND FacCorre.CodDoc = s-coddoc
        AND FacCorre.FlgEst = YES
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN LEAVE.
    CREATE ccbcdocu.
    ASSIGN
        CcbCDocu.CodCia = s-codcia
        CcbCDocu.CodDiv = s-coddiv
        CcbCDocu.CodDoc = s-coddoc
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.NroDoc = STRING(FacCorre.NroSer, '999') + STRING(FacCorre.Correlativo, '99999999')
        CcbCDocu.CodCli = '11111111111'
        CcbCDocu.FlgEst = 'P'
        CcbCDocu.CodAlm = s-codalm
        ccbcdocu.tpocmb = 3.2
        ccbcdocu.codmon = 1
        ccbcdocu.fmapgo = '000'
        ccbcdocu.horcie = STRING(TIME,'HH:MM:SS')
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error al grabar el comprobante' VIEW-AS ALERT-BOX WARNING.
        UNDO, LEAVE.
    END.
    ASSIGN FacCorre.Correlativo = FacCorre.Correlativo + 1.

    /* Grabamos productos linea 011 */
    FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = s-codcia
        AND almmmatg.codfam = '011':
        CREATE Ccbddocu.
        BUFFER-COPY Ccbcdocu TO Ccbddocu
            ASSIGN
            Ccbddocu.codmat = Almmmatg.codmat
            ccbddocu.aftisc = NO
            ccbddocu.candes = 1
            ccbddocu.undvta = Almmmatg.undbas
            ccbddocu.factor = 1.
        RUN util/sistemas/preciomayorista-cred-v2.p (
            INPUT "N",
            INPUT Ccbcdocu.CodDiv,
            INPUT Ccbcdocu.codcli,
            INPUT Ccbcdocu.CODMON,
            INPUT-OUTPUT S-UNDVTA,
            OUTPUT f-Factor,
            INPUT Ccbddocu.CODMAT,
            INPUT Ccbcdocu.fmapgo,
            INPUT Ccbddocu.candes,
            INPUT 4,
            OUTPUT  F-PREBAS,
            OUTPUT  F-PREVTA,
            OUTPUT  F-DSCTOS,
            OUTPUT  Y-DSCTOS,
            OUTPUT  Z-DSCTOS,
            OUTPUT  X-TIPDTO,
            OUTPUT  f-FleteUnitario,
            INPUT   "",
            INPUT PpError
            ).
        IF ppError = YES THEN UNDO, NEXT.
        ASSIGN
            Ccbddocu.undvta = s-undvta
            Ccbddocu.preuni = f-prevta
            Ccbddocu.por_dsctos[1] = y-dsctos
            Ccbddocu.por_dsctos[2] = z-dsctos.
        ASSIGN
            Ccbddocu.ImpLin = ROUND ( Ccbddocu.CanDes * Ccbddocu.PreUni * 
                          ( 1 - Ccbddocu.Por_Dsctos[1] / 100 ) *
                          ( 1 - Ccbddocu.Por_Dsctos[2] / 100 ) *
                          ( 1 - Ccbddocu.Por_Dsctos[3] / 100 ), 2 ).
        IF Ccbddocu.Por_Dsctos[1] = 0 AND Ccbddocu.Por_Dsctos[2] = 0 AND Ccbddocu.Por_Dsctos[3] = 0 
            THEN Ccbddocu.ImpDto = 0.
            ELSE Ccbddocu.ImpDto = Ccbddocu.CanDes * Ccbddocu.PreUni - Ccbddocu.ImpLin.
        IF f-FleteUnitario > 0 THEN DO:
            /* El flete afecta el monto final */
            IF Ccbddocu.ImpDto = 0 THEN DO:       /* NO tiene ningun descuento */
                ASSIGN
                    Ccbddocu.PreUni = ROUND(Ccbddocu.PreUni + f-FleteUnitario, 4)  /* Incrementamos el PreUni */
                    Ccbddocu.ImpLin = Ccbddocu.CanDes * Ccbddocu.PreUni.
            END.
            ELSE DO:      /* CON descuento promocional o volumen */
                ASSIGN
                    Ccbddocu.ImpLin = Ccbddocu.ImpLin + (Ccbddocu.CanDes * f-FleteUnitario)
                    Ccbddocu.PreUni = ROUND( (Ccbddocu.ImpLin + Ccbddocu.ImpDto) / Ccbddocu.CanDes, 4).
            END.
        END.
        ASSIGN
            Ccbddocu.ImpLin = ROUND(Ccbddocu.ImpLin, 2)
            Ccbddocu.ImpDto = ROUND(Ccbddocu.ImpDto, 2).
        IF Ccbddocu.AftIsc 
        THEN Ccbddocu.ImpIsc = ROUND(Ccbddocu.PreBas * Ccbddocu.CanDes * (Almmmatg.PorIsc / 100),4).
        ELSE Ccbddocu.ImpIsc = 0.
        IF Ccbddocu.AftIgv 
        THEN Ccbddocu.ImpIgv = Ccbddocu.ImpLin - ROUND( Ccbddocu.ImpLin  / ( 1 + (18 / 100) ), 4 ).
        ELSE Ccbddocu.ImpIgv = 0.
    END.
    RUN Graba-Totales.
    RUN util/sistemas/act_almv2.p ( 
        ROWID(Ccbcdocu),
        OUTPUT pMensaje
        ).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE pMensaje VIEW-AS ALERT-BOX WARNING.
        UNDO, LEAVE.
    END.
    CREATE T-CDOCU-1.
    BUFFER-COPY Ccbcdocu TO T-CDOCU-1 NO-ERROR.
END.
FOR EACH T-CDOCU-1:
    FIND Ccbcdocu OF T-CDOCU-1 NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcdocu THEN DO:
        CREATE T-CDOCU-2.
        BUFFER-COPY Ccbcdocu TO T-CDOCU-2 NO-ERROR.
    END.
END.
PUT STREAM reporte 'FIN: ' TODAY STRING(TIME,'HH:MM:SS') SKIP.
OUTPUT STREAM reporte CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Graba-Totales) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Totales Procedure 
PROCEDURE Graba-Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* graba totales de la factura */
  DEFINE VARIABLE F-IGV LIKE Ccbcdocu.ImpIgv NO-UNDO.
  DEFINE VARIABLE F-ISC LIKE Ccbcdocu.ImpIsc NO-UNDO.

  ASSIGN
      Ccbcdocu.ImpDto = 0
      Ccbcdocu.ImpIgv = 0
      Ccbcdocu.ImpIsc = 0
      Ccbcdocu.ImpTot = 0
      Ccbcdocu.ImpExo = 0
      F-IGV = 0
      F-ISC = 0.
  FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
      ASSIGN
          F-Igv = F-Igv + Ccbddocu.ImpIgv
          F-Isc = F-Isc + Ccbddocu.ImpIsc
          Ccbcdocu.ImpTot = Ccbcdocu.ImpTot + Ccbddocu.ImpLin.
      /* Importe Inafecto o Exonerado */
      IF Ccbddocu.ImpIgv = 0 THEN Ccbcdocu.ImpExo = Ccbcdocu.ImpExo + Ccbddocu.ImpLin.
  END.
  ASSIGN
      Ccbcdocu.ImpIgv = ROUND(F-IGV,2)
      Ccbcdocu.ImpIsc = ROUND(F-ISC,2)
      Ccbcdocu.ImpVta = Ccbcdocu.ImpTot - Ccbcdocu.ImpExo - Ccbcdocu.ImpIgv
      Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta + Ccbcdocu.ImpIsc + Ccbcdocu.ImpDto + Ccbcdocu.ImpExo
      Ccbcdocu.SdoAct = Ccbcdocu.ImpTot.
  /* ********************************************************************************************** */
  /* RHC 22/08/2016 Nuevo cálculo */
  /* ********************************************************************************************** */
  ASSIGN
      Ccbcdocu.ImpVta = ROUND ( (Ccbcdocu.ImpTot - Ccbcdocu.ImpExo) / (1 + Ccbcdocu.PorIgv / 100), 2 ).
  IF Ccbcdocu.ImpExo = 0 THEN Ccbcdocu.ImpIgv = Ccbcdocu.ImpTot - Ccbcdocu.ImpVta.
  ELSE Ccbcdocu.ImpIgv = ROUND(Ccbcdocu.ImpVta * Ccbcdocu.PorIgv / 100, 2).
  ASSIGN
      Ccbcdocu.ImpBrt = Ccbcdocu.ImpVta + Ccbcdocu.ImpDto + Ccbcdocu.ImpIsc
      Ccbcdocu.SdoAct = Ccbcdocu.ImpTot.
  /* ********************************************************************************************** */
  /* ********************************************************************************************** */
  IF Ccbcdocu.PorIgv = 0.00     /* VENTA INAFECTA */
      THEN ASSIGN
            Ccbcdocu.ImpIgv = 0
            Ccbcdocu.ImpVta = Ccbcdocu.ImpExo
            Ccbcdocu.ImpBrt = Ccbcdocu.ImpExo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


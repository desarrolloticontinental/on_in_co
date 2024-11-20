&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DISABLE TRIGGERS FOR LOAD OF dwh_compras_provmat.

Def var s-codcia    as inte init 1.
Def var x-signo1    as inte init 1.
Def var x-fin       as inte init 0.
Def var f-factor    as deci init 0.
Def var x-NroFchI   as inte init 0.
Def var x-NroFchF   as inte init 0.
Def var x-CodFchI   as date format '99/99/9999' init TODAY.
Def var x-CodFchF   as date format '99/99/9999' init TODAY.
Def var i           as inte init 0.
Def var x-TpoCmbCmp as deci init 1.
Def var x-TpoCmbVta as deci init 1.
Def var x-Day       as inte format '99'   init 1.
Def var x-Month     as inte format '99'   init 1.
Def var x-Year      as inte format '9999' init 1.
Def var x-coe       as deci init 0.
Def var x-can       as deci init 0.
def var x-fmapgo    as char.
def var x-canal     as char.

Def BUFFER B-CDOCU FOR CcbCdocu.
 
IF MONTH(TODAY - 1) = 01
THEN x-codfchi = DATE(12, 01, YEAR(TODAY - 1) - 1).
ELSE x-codfchi = DATE(MONTH(TODAY - 1) - 1, 01, YEAR(TODAY - 1)).

x-codfchf = TODAY - 1.

x-NroFchI = INTEGER(STRING(YEAR(x-CodFchI),"9999") + STRING(MONTH(x-CodFchI),"99")).      
x-NroFchF = INTEGER(STRING(YEAR(x-CodFchF),"9999") + STRING(MONTH(x-CodFchF),"99")).

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DISPLAY x-CodFchI x-CodFchF x-NroFchI x-NroFchF.
PAUSE 0.

/* RHC 18.08.04 AHORA SE VA A GENERAR PARA TODAS LAS DIVISIONES */
RUN Borra-Estadisticas.
FOR EACH Gn-Divi USE-INDEX IDX01 NO-LOCK WHERE Gn-Divi.Codcia = S-CodCia:
    FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia AND Almacen.coddiv = GN-DIVI.coddiv:
        FOR EACH Almdmov USE-INDEX ALMD04 NO-LOCK WHERE Almdmov.codcia = s-codcia
                AND Almdmov.codalm = Almacen.codalm
                AND Almdmov.fchdoc >= x-CodFchI
                AND Almdmov.fchdoc <= x-CodFchF
                AND Almdmov.tipmov = 'I' 
                AND ( Almdmov.codmov = 02 
                        OR Almdmov.codmov = 07 
                        OR Almdmov.codmov = 17 
                        OR Almdmov.codmov = 26 ),
                FIRST Almmmatg OF Almdmov NO-LOCK,
                FIRST Almcmov OF Almdmov NO-LOCK,
                FIRST Lg-cocmp WHERE Lg-cocmp.codcia = s-codcia
                    AND Lg-cocmp.codalm = almcmov.codalm
                    AND Lg-cocmp.tpodoc = 'N'
                    AND Lg-cocmp.nrodoc = INTEGER(Almcmov.nrorf1) NO-LOCK:
            RUN Carga-Estadistica.
        END.
    END.        
END.
FOR EACH Gn-Divi USE-INDEX IDX01 NO-LOCK WHERE Gn-Divi.Codcia = S-CodCia:
    FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia AND Almacen.coddiv = GN-DIVI.coddiv:
        FOR EACH Almdmov USE-INDEX ALMD04 NO-LOCK WHERE Almdmov.codcia = s-codcia
                AND Almdmov.codalm = Almacen.codalm
                AND Almdmov.fchdoc >= x-CodFchI
                AND Almdmov.fchdoc <= x-CodFchF
                AND Almdmov.tipmov = 'S'
                AND Almdmov.codmov = 26,
                FIRST Almmmatg OF Almdmov NO-LOCK,
                FIRST Almcmov OF Almdmov NO-LOCK,
                FIRST Lg-cocmp WHERE Lg-cocmp.codcia = s-codcia
                    AND Lg-cocmp.codalm = almcmov.codalm
                    AND Lg-cocmp.tpodoc = 'N'
                    AND Lg-cocmp.nrodoc = INTEGER(Almcmov.nrorf1) NO-LOCK:
            RUN Carga-Estadistica.
        END.
    END.        
END.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Estadisticas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Estadisticas Procedure 
PROCEDURE Borra-Estadisticas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Gn-Divi USE-INDEX IDX01 NO-LOCK WHERE Gn-Divi.Codcia = S-CodCia:
    FOR EACH dwh_compras_provmat WHERE dwh_compras_provmat.Codcia = S-CODCIA
                       AND dwh_compras_provmat.CodDiv = Gn-Divi.CodDiv
                       AND dwh_compras_provmat.NroFch >= x-NroFchI
                       AND dwh_compras_provmat.NroFch <= x-NroFchF
                       USE-INDEX llave02 :
        DELETE dwh_compras_provmat.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Estadistica) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Estadistica Procedure 
PROCEDURE Carga-Estadistica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    x-Signo1 = IF Almdmov.tipmov = 'I' THEN 1 ELSE -1.
    DISPLAY Almdmov.Codcia
            Almdmov.Codalm
            Almdmov.FchDoc 
            Almdmov.tipmov
            Almdmov.codmov
            Almdmov.NroDoc
            STRING(TIME,'HH:MM')
            TODAY WITH NO-LABELS NO-BOX.
    PAUSE 0.
    ASSIGN
        x-Day   = DAY(Almdmov.FchDoc)
        x-Month = MONTH(Almdmov.FchDoc)
        x-Year  = YEAR(Almdmov.FchDoc).
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= Almdmov.FchDoc
                      USE-INDEX Cmb01
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= Almdmov.FchDoc
                           USE-INDEX Cmb01
                           NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb 
    THEN ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.
    F-FACTOR  = 1. 
    IF Almdmov.factor > 0 THEN f-Factor = Almdmov.factor.
      
    FIND FIRST dwh_Compras_ProvMat WHERE dwh_Compras_ProvMat.codcia = almdmov.codcia
        AND dwh_Compras_ProvMat.coddiv = almacen.coddiv
        AND dwh_Compras_ProvMat.codprov = almcmov.codpro
        AND dwh_Compras_ProvMat.codmat = almdmov.codmat
        AND dwh_Compras_ProvMat.cndcmp = lg-cocmp.cndcmp
        AND dwh_Compras_ProvMat.nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99"))
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE dwh_Compras_ProvMat THEN DO:
         CREATE dwh_Compras_ProvMat.
         ASSIGN
         dwh_Compras_ProvMat.Codcia = Almdmov.Codcia 
         dwh_Compras_ProvMat.CodDiv = Almacen.Coddiv 
         dwh_Compras_ProvMat.codprov = almcmov.codpro
         dwh_Compras_ProvMat.codmat = almdmov.codmat
         dwh_Compras_ProvMat.cndcmp = lg-cocmp.cndcmp
         dwh_Compras_ProvMat.CodAno = x-Year 
         dwh_Compras_ProvMat.CodMes = x-Month
         dwh_Compras_ProvMat.Nrofch = INTEGER(STRING(x-Year,"9999") + STRING(x-Month,"99")) .
    END.                    
    IF Almcmov.CodMon = 1 THEN 
                ASSIGN
                dwh_Compras_ProvMat.CmpxDiaMn[x-Day] = dwh_Compras_ProvMat.CmpxDiaMn[x-Day] + ( x-signo1 * Almdmov.ImpCto)
                dwh_Compras_ProvMat.CmpxMesMn = dwh_Compras_ProvMat.CmpxMesMn + x-signo1 * Almdmov.ImpCto
                dwh_Compras_ProvMat.CmpxMesMe = dwh_Compras_ProvMat.CmpxMesMe + x-signo1 * Almdmov.ImpCto / x-TpoCmbCmp.
    IF Almcmov.CodMon = 2 THEN 
                ASSIGN
                dwh_Compras_ProvMat.CmpxDiaMe[x-Day] = dwh_Compras_ProvMat.CmpxDiaMe[x-Day] + ( x-signo1 * Almdmov.ImpCto)
                dwh_Compras_ProvMat.CmpxMesMn = dwh_Compras_ProvMat.CmpxMesMn + x-signo1 * Almdmov.ImpCto * x-TpoCmbVta
                dwh_Compras_ProvMat.CmpxMesMe = dwh_Compras_ProvMat.CmpxMesMe + x-signo1 * Almdmov.ImpCto.
    ASSIGN            
      dwh_Compras_ProvMat.CanxDia[x-Day] = dwh_Compras_ProvMat.CanxDia[x-Day] + ( x-signo1 * Almdmov.CanDes * F-FACTOR )
      dwh_Compras_ProvMat.CanxMes = dwh_Compras_ProvMat.CanxMes + ( x-signo1 * Almdmov.CanDes * F-FACTOR ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


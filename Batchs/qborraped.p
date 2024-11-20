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

/* ANULACION AUTOMATICA DE PEDIDOS */
/* ANULACION AUTOMATICA TRACKING DE PEDIDOS MOSTRADOR (P/M) */
/* TODOS MENOS ATE revisa qcierrepedvencido */

DISABLE TRIGGERS FOR LOAD OF faccpedm.
DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedm.
DISABLE TRIGGERS FOR LOAD OF Vtactrkped.
DISABLE TRIGGERS FOR LOAD OF Vtadtrkped.

DEFINE VARIABLE s-codcia AS INT INIT 001.

DEFINE BUFFER B-DPEDI FOR FacDPedi.
DEFINE BUFFER B-CPEDI FOR FacCPedi.
DEFINE VAR pFactor AS INT INIT -1.    /* +1 actualiza    -1 desactualiza */
DEFINE VARIABLE I-NRO AS INTEGER INIT 0 NO-UNDO.

/* RHC 19.05.2011 Borramos pedidos al contado (UTILEX) */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = gn-divi.coddiv
    AND faccpedi.coddoc = 'P/M'
    AND faccpedi.flgest <> 'C'
    AND faccpedi.fchped < (TODAY - 45):
    FOR EACH facdpedi OF faccpedi:
        DELETE facdpedi.
    END.
    DISPLAY 'PED CONTADO' faccpedi.coddoc faccpedi.nroped faccpedi.fchped faccpedi.flgest
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    DELETE faccpedi. 
END.

/* RHC 16.07.2012 RECHAZAR TODOS LOS PED VENCIDOS CON MAS DE 7 DIAS */
/* 14/06/2023: Rechazar ni bien esté vencido */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH FacCPedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = gn-divi.coddiv
    AND faccpedi.coddoc = 'PED'
    AND faccpedi.fchven < TODAY - 7
    AND LOOKUP(faccpedi.flgest, 'G,P,X,W,WX,WL') > 0:
    FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.canate > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Facdpedi THEN NEXT.
    ASSIGN
      FacCPedi.FlgEst = 'R'
      Faccpedi.UsrAprobacion = 'AUTO'
      Faccpedi.FchAprobacion = TODAY.
    FOR EACH FacDPedi OF FacCPedi:
        FIND FIRST B-DPedi WHERE B-DPedi.CodCia = FacCPedi.CodCia 
            AND B-DPedi.CodDiv = FacCPedi.CodDiv
            AND B-DPedi.CodDoc = FacCPedi.CodRef
            AND B-DPedi.NroPed = FacCPedi.NroRef   
            AND B-DPedi.CodMat = FacDPedi.CodMat 
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE B-DPedi 
        THEN ASSIGN
            B-DPEDI.FlgEst = 'P'
            B-DPEDI.CanAte = B-DPEDI.CanAte - (Facdpedi.CanPed - Facdpedi.CanAte).  /* <<<< OJO <<<< */
        ASSIGN
            Facdpedi.FlgEst = Faccpedi.FlgEst.   /* <<< OJO <<< */
    END.  
    FIND B-CPedi WHERE B-CPedi.CodCia = FacCPedi.CodCia
        AND  B-CPedi.CodDiv = FacCPedi.CodDiv
        AND  B-CPedi.CodDoc = FacCPedi.CodRef
        AND  B-CPedi.NroPed = FacCPedi.NroRef
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE B-CPedi THEN ASSIGN B-CPedi.FlgEst = "P".
    DISPLAY 'RECHAZADO' faccpedi.coddoc faccpedi.nroped faccpedi.fchped faccpedi.flgest
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
END.
    
/* RHC 29.11.2010 Borramos pre-pedidos vitrinas PPV */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = gn-divi.coddiv
    AND faccpedi.coddoc = 'PPV'
    AND faccpedi.fchped < TODAY:
    FOR EACH facdpedi OF faccpedi:
        DELETE facdpedi.
    END.
    DISPLAY 'VITRINAS' faccpedi.coddoc faccpedi.nroped faccpedi.fchped faccpedi.flgest
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    DELETE faccpedi. 
END.

/* RHC 11.06.2012 Borramos O/M (Ordenes de Despacho Mostrador ) Vencidas */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH Faccpedi WHERE Faccpedi.codcia = s-codcia
    AND Faccpedi.coddiv = gn-divi.coddiv
    AND Faccpedi.coddoc = "O/M"
    AND Faccpedi.fchven < TODAY - 7
    AND Faccpedi.flgest = "P":
    /* NO las que tiene aunque sea una atención */
    FIND FIRST facdpedi OF faccpedi WHERE facdpedi.canate > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE facdpedi THEN NEXT.
    FOR EACH facdpedi OF faccpedi:
        DELETE facdpedi.
    END.
    DISPLAY 'MOSTRADOR' faccpedi.coddoc faccpedi.nroped faccpedi.fchped faccpedi.flgest
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    DELETE faccpedi.
END.

/* RHC 23.07.2012 COTIZACIONES VENCIDAS SIN ATENCION */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH FacCPedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = gn-divi.coddiv
    AND faccpedi.coddoc = 'COT'
    AND faccpedi.fchven < TODAY - 7
    AND faccpedi.flgest = "P":
    FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.canate > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Facdpedi THEN NEXT.
    ASSIGN
      FacCPedi.FlgEst = 'V'
      Faccpedi.UsrAprobacion = 'AUTO'
      Faccpedi.FchAprobacion = TODAY.
    FOR EACH FacDPedi OF FacCPedi:
        Facdpedi.FlgEst = Faccpedi.FlgEst.   /* <<< OJO <<< */
    END.  
    DISPLAY 'COT VENCIDAS' faccpedi.coddoc faccpedi.nroped faccpedi.fchped faccpedi.flgest
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
END.

IF AVAILABLE(Faccpedi) THEN RELEASE Faccpedi.
IF AVAILABLE(Facdpedi) THEN RELEASE Facdpedi.
IF AVAILABLE(B-CPEDI)  THEN RELEASE B-CPEDI.
IF AVAILABLE(B-DPEDI)  THEN RELEASE B-DPEDI.

QUIT.

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
         HEIGHT             = 3.69
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



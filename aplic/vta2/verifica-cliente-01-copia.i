&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 3.96
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

DEFINE VAR OK AS LOGICAL NO-UNDO.
DEFINE VAR X-CREUSA AS DECIMAL NO-UNDO.
DEFINE VARIABLE I-ListPr     AS INTEGER NO-UNDO.
DEFINE VARIABLE F-MRGUTI     AS INTEGER NO-UNDO.
DEFINE VARIABLE dImpTot      AS DECIMAL NO-UNDO.
DEFINE VARIABLE T-SALDO      AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-totdias    AS INTEGER NO-UNDO.
DEFINE VARIABLE t-Resultado  AS CHAR    NO-UNDO.

IF FacCPedi.Flgest = 'G' THEN DO:
    OK = TRUE.
    LOOPCONTROL:
    DO:     /* BLOQUE DE CONTROL */
        /* CONTRATO MARCO NO NECESITAN APROBACION */
        FIND B-CPEDI WHERE B-CPEDI.codcia = Faccpedi.codcia
            AND B-CPEDI.coddoc = Faccpedi.codref
            AND B-CPEDI.nroped = Faccpedi.nroref
            NO-LOCK NO-ERROR.
        IF AVAILABLE B-CPEDI AND B-CPEDI.TpoPed = "M" 
            THEN DO:
                Faccpedi.FlgEst = "P".
                LEAVE LOOPCONTROL.
            END.
        /* POR LA LINEA DE CREDITO */
/*         RUN vtagn/p-linea-de-credito ( Faccpedi.CodCli, */
/*                                   Faccpedi.ImpTot,      */
/*                                   Faccpedi.CodMon,      */
/*                                   Faccpedi.FmaPgo,      */
/*                                   FALSE,                */
/*                                   OUTPUT t-Resultado).  */
        RUN vta2/linea-de-credito-01 ( Faccpedi.CodCli,
                                       Faccpedi.ImpTot,
                                       Faccpedi.CodMon,
                                       Faccpedi.FmaPgo,
                                       FALSE,
                                       OUTPUT t-Resultado).
        IF t-Resultado = 'ADM-ERROR' THEN OK = FALSE.
        IF OK = NO
        THEN DO:
            ASSIGN 
                 FacCPedi.Flgest = 'X'
                 FacCPedi.Glosa  = TRIM (FacCPedi.Glosa) + '//Linea Credito'
                 FacCPedi.Libre_c05 = 'SUPERA LA LINEA DE CREDITO'
                 FacCPedi.Libre_c04 = ' - SUPERA LA LINEA DE CREDITO'.
            LEAVE LOOPCONTROL.
        END.
        ELSE ASSIGN 
                 FacCPedi.Flgest = 'P'
                 FacCPedi.Libre_c04 = ' - NO SUPERA LA LINEA DE CREDITO'.
        /* DEUDA VENCIDA */
        FIND FIRST CcbCDocu WHERE CcbCDocu.CodCia = FacCPedi.CodCia
            AND  CcbCDocu.CodCli = FacCPedi.Codcli                         
            AND  LOOKUP(CcbCDocu.CodDoc, "FAC,BOL,CHQ,LET,N/D") > 0
            AND  CcbCDocu.FlgEst = "P" 
            AND  CcbCDocu.FchVto <= TODAY
            NO-LOCK NO-ERROR. 
        IF AVAIL CcbCDocu 
            THEN ASSIGN
                    FacCPedi.Flgest = 'X'
                    FacCPedi.Glosa  = TRIM (FacCPedi.Glosa) + '//Doc. Venc.'.

        /* POR LA CONDICION DE VENTA */
/*         FIND gn-convt WHERE gn-convt.Codig = gn-clie.cndvta NO-LOCK NO-ERROR.   */
/*         IF AVAILABLE gn-convt THEN F-totdias = gn-convt.totdias.                */
/*         FIND gn-convt WHERE gn-convt.Codig = FacCPedi.FmaPgo NO-LOCK NO-ERROR.  */
/*         IF AVAILABLE gn-convt                                                   */
/*         THEN IF gn-convt.totdias > F-totdias AND F-totdias > 0 THEN OK = FALSE. */
        IF LOOKUP(FacCPedi.fmapgo,"000") > 0 THEN OK = TRUE.
        /* SOLO CONTADO CONTRAENTREGA */
        IF LOOKUP(FacCPedi.fmaPgo,"001,002") > 0 THEN OK = FALSE.
        /*IF LOOKUP(FacCPedi.fmaPgo,"001") > 0 THEN OK = FALSE.*/
        /* RHC 15.10.04 Transferencias Gratuitas */
        IF FacCPedi.FmaPgo = '900' THEN OK = FALSE.
        IF OK = NO
        THEN DO:
            ASSIGN 
                FacCPedi.Flgest = 'X'
                FacCPedi.Glosa  = TRIM (FacCPedi.Glosa) + '//Cond.Cred.'
                FacCPedi.Libre_c05 = 'CONDICION DE VENTA'.
            LEAVE LOOPCONTROL.
        END.
        ELSE ASSIGN FacCPedi.Flgest = 'P'.          
        /* RHC 07.12.2009 PEDIDOS DE PROVINCIAS PASAN OBLIGADO POR APROBACION */
/*         IF Faccpedi.coddiv = '00018' THEN DO:                           */
/*             ASSIGN                                                      */
/*                 FacCPedi.Flgest = 'X'                                   */
/*                 FacCPedi.Glosa  = TRIM (FacCPedi.Glosa) + '//Provincia' */
/*                 FacCPedi.Libre_c05 = 'PROVINCIAS' + FacCPedi.Libre_c04. */
/*             LEAVE LOOPCONTROL.                                          */
/*         END.                                                            */
        /* RHC 08.05.2012 PEDIDOS DE SUPERMERCADOS PASAN OBLIGADO POR SEC GG GG*/
        IF Faccpedi.coddiv = '00017' THEN DO:
            ASSIGN
                FacCPedi.Flgest = 'W'
                FacCPedi.Glosa  = TRIM (FacCPedi.Glosa) + '//Supermercados'
                FacCPedi.Libre_c05 = 'SUPERMERCADOS' + FacCPedi.Libre_c04.
            LEAVE LOOPCONTROL.
        END.
        /* Aprobacion automatica en caso de pedidos contado-contraentrega */
        IF s-CodDiv = '00015'               /* Expolibreria */
            AND FacCPedi.FmaPgo = '001'      /* Contado contra-entrega */
            THEN FacCPedi.Flgest = 'P'.
        /* **************************************************************** */
        /* CONTROL FINAL SI ES QUE LA LINEA DE CREDITO ESTA AUTORIZADA O NO */
        /* **************************************************************** */
        IF FacCPedi.FlgEst = "P" AND LOOKUP(FacCPedi.fmapgo,"000") = 0 THEN DO:
            IF gn-clie.FlagAut = "" THEN    /* NO AUTORIZADA */
                ASSIGN
                    FacCPedi.Flgest = 'X'
                    FacCPedi.Libre_c05 = 'LINEA DE CREDITO NO AUTORIZADA'.
        END.
    END.
    /* ****************************************************************** */
    FOR EACH FacDPedi OF FacCPedi:
        ASSIGN  FacDPedi.Flgest = FacCPedi.Flgest.   /* <<< OJO <<< */
        RELEASE FacDPedi.
    END.
    IF FacCPedi.FlgEst = 'P' THEN DO:    /* APROBACION AUTOMATICA DEL PEDIDO */
       /* TRACKING */
        FIND Almacen OF Faccpedi NO-LOCK.
        RUN vtagn/pTracking-04 (s-CodCia,
                          Almacen.CodDiv,
                          Faccpedi.CodDoc,
                          Faccpedi.NroPed,
                          s-User-Id,
                          'ANP',
                          'P',
                          DATETIME(TODAY, MTIME),
                          DATETIME(TODAY, MTIME),
                          Faccpedi.coddoc,
                          Faccpedi.nroped,
                          Faccpedi.coddoc,
                          Faccpedi.nroped).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


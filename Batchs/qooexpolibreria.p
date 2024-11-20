&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Captura los pedidos de LISTA FACIL y los transforma en cotizaciones y pedidos
                    para despachar

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR s-codcia    AS INT  INIT 001        NO-UNDO.
DEF VAR cl-codcia   AS INT  INIT 000        NO-UNDO.
DEF VAR s-coddiv    AS CHAR INIT '00015'    NO-UNDO. 
DEF VAR pcoddiv     AS CHAR INIT '10015'    NO-UNDO.
DEF VAR s-codalm    AS CHAR                 NO-UNDO.
DEF VAR s-tpoped    AS CHAR INIT "E"        NO-UNDO.     /* Expolibreria */
DEF VAR s-porigv    AS DEC INIT 18          NO-UNDO.
DEF VAR s-user-id   AS CHAR INIT 'VTA-15'  NO-UNDO.
DEF VAR s-FlgEnv LIKE Faccpedi.flgenv INIT YES NO-UNDO.
DEF VAR pMensaje    AS CHAR                 NO-UNDO.
DEF VAR pMensajeFinal AS CHAR               NO-UNDO.

/* Control de Series */
FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddiv = s-coddiv
    AND Faccorre.coddoc = "COT"
    AND FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccorre THEN DO:
    DISPLAY "NO está configurado el correlativo para doc COT division " s-coddiv
        WITH STREAM-IO NO-BOX NO-UNDERLINE NO-LABELS.
    RETURN ERROR.
END.

DEF BUFFER B-CCPED FOR OpenCExpo.

DISABLE TRIGGERS FOR LOAD OF Faccpedi.
DISABLE TRIGGERS FOR LOAD OF Facdpedi.
DISABLE TRIGGERS FOR LOAD OF Faccorre.
DISABLE TRIGGERS FOR LOAD OF Gn-Clie.

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
         HEIGHT             = 8.38
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR s-codref LIKE Faccpedi.codref NO-UNDO.
DEF VAR s-nroref LIKE Faccpedi.nroref NO-UNDO.

REPEAT :
    /* Barremos la tabla intermedia */
    FOR EACH OpenCExpo NO-LOCK WHERE OpenCExpo.FlagMigracion = 'N':
        /* Cotizaciones */
        RUN Crea-Cotizaciones NO-ERROR. 
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE 'Devolvio un error'.
            NEXT.
        END.
    END.
    LEAVE.

    PAUSE 300.  /* Esperamos 5 minutos */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Crea-Cliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Cliente Procedure 
PROCEDURE Crea-Cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.codcli = OpenCExpo.codcli
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE gn-clie THEN RETURN.

CREATE gn-clie.
ASSIGN
    gn-clie.CodCia = cl-codcia
    gn-clie.CodCli = OpenCExpo.CodCli
    gn-clie.NomCli = OpenCExpo.NomCli
    gn-clie.DirCli = OpenCExpo.DirCli
    gn-clie.Ruc    = OpenCExpo.RucCli
    /*gn-clie.DNI    = OpenCExpo.CodAnt*/
    gn-clie.ApeMat = OpenCExpo.ApeMat
    gn-clie.ApePat = OpenCExpo.ApePat
    gn-clie.Nombre = OpenCExpo.Nombre
    gn-clie.clfCli = 'C'
    gn-clie.flgsit = "A"
    gn-clie.fching = TODAY
    gn-clie.usuario = OpenCExpo.usuario
    gn-clie.coddiv = OpenCExpo.coddiv
    gn-clie.canal = '099'
    gn-clie.CodDept = OpenCExpo.CodDept
    gn-clie.CodDist = OpenCExpo.CodDist
    gn-clie.CodProv = OpenCExpo.CodProv
    gn-clie.DirEnt  = OpenCExpo.LugEnt
    gn-clie.RucOld  = (IF OpenCExpo.AgeRet = 1 THEN "Si" ELSE "No")
    gn-clie.Libre_L01 = (IF OpenCExpo.AgePer = 1 THEN YES ELSE NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Crea-Cotizaciones) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Cotizaciones Procedure 
PROCEDURE Crea-Cotizaciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR s-coddoc AS CHAR INIT "COT" NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR i-nItem AS INT NO-UNDO.


DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    ASSIGN
        /*s-CodDiv = OpenCExpo.CodDiv
        pCodDiv  = OpenCExpo.LstPrec*/
        s-CodAlm = OpenCExpo.CodAlm.
    /* NO Almacenes de Remate */
    FOR EACH VtaAlmDiv NO-LOCK WHERE Vtaalmdiv.codcia = s-codcia
        AND Vtaalmdiv.coddiv = s-coddiv,
        FIRST Almacen OF Vtaalmdiv NO-LOCK WHERE Almacen.Campo-C[3] <> 'Si'
        BY VtaAlmDiv.Orden:
        IF s-CodAlm = "" THEN s-CodAlm = TRIM(VtaAlmDiv.CodAlm).
        ELSE s-CodAlm = s-CodAlm + "," + TRIM(VtaAlmDiv.CodAlm).
    END.

    FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
        AND Faccorre.coddiv = s-coddiv
        AND Faccorre.coddoc = "COT"
        AND FacCorre.FlgEst = YES
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccorre THEN RETURN ERROR.
    s-NroSer = FacCorre.NroSer.

    /* Trata de bloquear hasta 5 veces */
    DEF VAR LocalCounter AS INTEGER INITIAL 0 NO-UNDO.
    GetLock:
    REPEAT ON ERROR UNDO GetLock, LEAVE GetLock:
        FIND FacCorre WHERE FacCorre.CodCia = s-CodCia AND 
            FacCorre.CodDoc = s-coddoc AND
            FacCorre.NroSer = s-nroser
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE FacCorre THEN LEAVE.           /* Bloqeo Exitoso */
        IF AMBIGUOUS FacCorre THEN LEAVE GetLock.   /* Llave Duplicada */
        IF ERROR-STATUS:ERROR THEN LEAVE GetLock.   /* Registro no encontrado */
        LocalCounter = LocalCounter + 1.            /* Acumulamos Intentos */
        PAUSE 2 NO-MESSAGE.                         /* Dos segundos de espera */
        IF LocalCounter = 5 THEN LEAVE GetLock.    /* 5 intentos máximo */
    END.
    IF LocalCounter = 5 OR NOT AVAILABLE FacCorre THEN RETURN ERROR.
    IF FacCorre.FlgCic = NO THEN DO:
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            DISPLAY 'Se ha llegado al límite del correlativo:' FacCorre.NroFin SKIP
                'No se puede generar el documento' s-coddoc 'serie' s-nroser
                WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE.
            PAUSE 0.
            RETURN ERROR.
        END.
    END.
    IF FacCorre.FlgCic = YES THEN DO:
        /* REGRESAMOS AL NUMERO 1 */
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            IF FacCorre.NroIni > 0 THEN FacCorre.Correlativo = FacCorre.NroIni.
            ELSE FacCorre.Correlativo = 1.
        END.
    END.
    /* Buscamos un número válido */
    REPEAT:
        IF NOT CAN-FIND(FIRST FacCPedi WHERE FacCPedi.codcia = s-codcia
                    AND FacCPedi.coddiv = FacCorre.coddiv
                    AND FacCPedi.coddoc = FacCorre.coddoc
                    AND FacCPedi.nroped = STRING(FacCorre.nroser, '999') + 
                    STRING(FacCorre.correlativo, '999999')
                    NO-LOCK)
            THEN LEAVE.
        ASSIGN
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
    END.
    CREATE Faccpedi.
    BUFFER-COPY OpenCExpo
        TO FacCPedi
        ASSIGN 
        FacCPedi.CodCia = S-CODCIA
        FacCPedi.CodDiv = S-CODDIV
        FacCPedi.Libre_C01 = pCodDiv
        FacCPedi.CodDoc = s-coddoc 
        FacCPedi.CodAlm = s-CodAlm    /* Lista de Almacenes Válidos de Venta */
        FacCPedi.Atencion = OpenCExpo.RucCli
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.TpoPed = s-TpoPed
        FacCPedi.FlgEst = "P".    /* APROBADO */
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN 
        FacCPedi.NroRef = STRING(OpenCExpo.NroTrans)
        FacCPedi.Usuario = s-user-id
        FacCPedi.FmaPgo = (IF FacCPedi.FmaPgo = "" THEN "001" ELSE FacCPedi.FmaPgo)
        FacCPedi.FlgIgv = YES
        FacCPedi.Libre_d01 = 4
        FacCPedi.Libre_c01 = pCodDiv
        FacCPedi.ubigeo[2] = OpenCExpo.coddept
        FacCPedi.ubigeo[3] = OpenCExpo.codprov
        FacCPedi.ubigeo[4] = OpenCExpo.coddist.

    FOR EACH OpenDExpo WHERE OpenDExpo.NroTrans = OpenCExpo.NroTrans BY OpenDExpo.NroItm:
        I-NITEM = I-NITEM + 1.
        CREATE FacDPedi.
        BUFFER-COPY OpenDExpo
            TO FacDPedi
            ASSIGN
            FacDPedi.CodCia = FacCPedi.CodCia
            FacDPedi.CodDiv = FacCPedi.CodDiv
            FacDPedi.coddoc = FacCPedi.coddoc
            FacDPedi.NroPed = FacCPedi.NroPed
            FacDPedi.FchPed = FacCPedi.FchPed
            FacDPedi.Hora   = FacCPedi.Hora 
            FacDPedi.FlgEst = FacCPedi.FlgEst
            FacDPedi.NroItm = I-NITEM
            FacDPedi.Por_Dsctos[1] = OpenDExpo.PorDto1
            FacDPedi.Por_Dsctos[2] = OpenDExpo.PorDto2
            FacDPedi.Por_Dsctos[3] = OpenDExpo.PorDto3
            FacDPedi.AftIgv = (IF OpenDExpo.AftIgv = 1 THEN YES ELSE NO).
        /* Percepciones */
        ASSIGN
            Facdpedi.CanSol = OpenDExpo.PorPerc
            Facdpedi.CanApr = OpenDExpo.ImpPerc.
        FacDPedi.AftIsc = NO.
        FacDPedi.AlmDes = s-codalm.
    END.
    RUN Graba-Totales.
    RUN Crea-Cliente.
    ASSIGN
        s-CodRef = Faccpedi.coddoc
        s-NroRef = Faccpedi.nroped.

    /* MARCAMOS EL ORIGEN */
    FIND B-CCPED WHERE ROWID(B-CCPED) = ROWID(OpenCExpo) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CCPED THEN UNDO, RETURN ERROR.
    ASSIGN 
        B-CCPED.NroPed = FacCPedi.NroPed
        B-CCPED.FlagMigracion = 'S'
        B-CCPED.MigFecha = TODAY
        B-CCPED.MigHora = STRING(TIME, 'HH:MM:SS')
        B-CCPED.MigUsuario = s-user-id.
END.
IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
IF AVAILABLE(Faccpedi) THEN RELEASE Faccpedi.
IF AVAILABLE(Facdpedi) THEN RELEASE Facdpedi.
IF AVAILABLE(B-CCPED) THEN RELEASE B-CCPED.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-Totales) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Totales Procedure 
PROCEDURE Graba-Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.                                                           */
/*   DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.                                                           */
/*                                                                                                       */
/*   ASSIGN                                                                                              */
/*     FacCPedi.ImpDto = 0                                                                               */
/*     FacCPedi.ImpIgv = 0                                                                               */
/*     FacCPedi.ImpIsc = 0                                                                               */
/*     FacCPedi.ImpTot = 0                                                                               */
/*     FacCPedi.ImpExo = 0                                                                               */
/*     FacCPedi.Importe[3] = 0                                                                           */
/*     F-IGV = 0                                                                                         */
/*     F-ISC = 0.                                                                                        */
/*   /* VENTAS INAFECTAS A IGV */                                                                        */
/*   IF FacCPedi.FlgIgv = NO THEN DO:                                                                    */
/*       FacCPedi.PorIgv = 0.00.                                                                         */
/*       FOR EACH FacDPedi OF FacCPedi:                                                                  */
/*           ASSIGN                                                                                      */
/*               FacDPedi.AftIgv = NO                                                                    */
/*               FacDPedi.ImpIgv = 0.00.                                                                 */
/*       END.                                                                                            */
/*   END.                                                                                                */
/*   FOR EACH FacDPedi OF FacCPedi:                                                                      */
/*       ASSIGN                                                                                          */
/*           F-Igv = F-Igv + FacDPedi.ImpIgv                                                             */
/*           F-Isc = F-Isc + FacDPedi.ImpIsc                                                             */
/*           FacCPedi.ImpTot = FacCPedi.ImpTot + FacDPedi.ImpLin.                                        */
/*       /* Importe Inafecto o Exonerado */                                                              */
/*       IF FacDPedi.ImpIgv = 0 THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.                */
/*   END.                                                                                                */
/*   ASSIGN                                                                                              */
/*       FacCPedi.ImpIgv = ROUND(F-IGV,2)                                                                */
/*       FacCPedi.ImpIsc = ROUND(F-ISC,2)                                                                */
/*       FacCPedi.ImpVta = ROUND( (FacCPedi.ImpTot - FacCPedi.ImpExo) / (1 + FacCPedi.PorIgv / 100), 2)  */
/*       FacCPedi.ImpBrt = FacCPedi.ImpVta /*+ FacCPedi.ImpIsc*/ + FacCPedi.ImpDto /*+ FacCPedi.ImpExo*/ */
/*       FacCPedi.Importe[1] = FacCPedi.ImpTot.    /* Guardamos el importe original */                   */
/*   IF FacCPedi.FlgIgv = NO                                                                             */
/*       THEN ASSIGN                                                                                     */
/*           FacCPedi.ImpVta = FacCPedi.ImpExo                                                           */
/*           FacCPedi.ImpBrt = FacCPedi.ImpExo.                                                          */
/*                                                                                                       */
  /* *************************************** PERCEPCIONES ******************************** */
  DEF VAR s-PorPercepcion AS DEC NO-UNDO.
  DEF VAR pPercepcion AS DEC NO-UNDO.

  ASSIGN
      Faccpedi.AcuBon[4] = 0
      Faccpedi.AcuBon[5] = 0.
  FOR EACH Facdpedi OF Faccpedi NO-LOCK:
      IF Facdpedi.CanSol > 0 THEN s-PorPercepcion = Facdpedi.CanSol.
      ASSIGN
          pPercepcion = pPercepcion + Facdpedi.CanApr.
  END.
  ASSIGN
      Faccpedi.AcuBon[4] = s-PorPercepcion
      Faccpedi.AcuBon[5] = pPercepcion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


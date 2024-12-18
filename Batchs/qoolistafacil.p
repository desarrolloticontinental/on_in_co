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

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00506' NO-UNDO.      /* Ventas Web ListaFacil */
DEF VAR pcoddiv AS CHAR INIT '00506' NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '10' NO-UNDO.        /* Almac�n por defecto */
DEF VAR s-tpoped AS CHAR INIT "LF" NO-UNDO.     /* Lista Facil */
DEF VAR s-porigv AS DEC INIT 18 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'VTA-506' NO-UNDO.
DEF VAR s-FlgEnv LIKE Faccpedi.flgenv INIT YES NO-UNDO.
DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR pMensajeFinal AS CHAR NO-UNDO.

/* Control de Series */
FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddiv = s-coddiv
    AND Faccorre.coddoc = "COT"
    AND FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccorre THEN DO:
    DISPLAY "NO est� configurado el correlativo para doc COT division " s-coddiv
        WITH STREAM-IO NO-BOX NO-UNDERLINE NO-LABELS.
    RETURN ERROR.
END.
FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddiv = s-coddiv
    AND Faccorre.coddoc = "PED"
    AND FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccorre THEN DO:
    DISPLAY "NO est� configurado el correlativo para doc PED division " s-coddiv
        WITH STREAM-IO NO-BOX NO-UNDERLINE NO-LABELS.
    RETURN ERROR.
END.

DISABLE TRIGGERS FOR LOAD OF Faccpedi.
DISABLE TRIGGERS FOR LOAD OF Facdpedi.
DISABLE TRIGGERS FOR LOAD OF Faccorre.
DISABLE TRIGGERS FOR LOAD OF Gn-Clie.

DEF BUFFER B-CPEDI FOR Faccpedi.
DEF BUFFER B-DPEDI FOR Facdpedi.

DEF BUFFER B-CCPED FOR OpenCPedidos.

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

/* REPEAT : */
    /* Barremos la tabla intermedia */
DISPLAY 'Inicio:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
    FOR EACH OpenCPedidos NO-LOCK WHERE OpenCPedidos.FlagMigracion = '0':
        /* Cotizaciones */
        RUN Crea-Cotizaciones NO-ERROR. 
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE 'Devolvio un error'.
            NEXT.
        END.
        /* Pedidos */
        /*RUN Crea-Pedidos NO-ERROR.*/
    END.
    DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
    QUIT.
/*     PAUSE 300.  /* Esperamos 5 minutos */ */
/* END.                                      */

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
    AND gn-clie.codcli = OpenCPedidos.codcli
    NO-LOCK NO-ERROR NO-WAIT.
IF AVAILABLE gn-clie THEN RETURN.

CREATE gn-clie.
ASSIGN
    gn-clie.CodCia = cl-codcia
    gn-clie.CodCli = OpenCPedidos.CodCli
    gn-clie.NomCli = OpenCPedidos.NomCli
    gn-clie.DirCli = OpenCPedidos.DirCli
    gn-clie.Ruc    = OpenCPedidos.RucCli
    /*gn-clie.DNI    = OpenCPedidos.CodAnt*/
    /*gn-clie.ApeMat = OpenCPedidos.ApeMat*/
    /*gn-clie.ApePat = OpenCPedidos.ApePat*/
    /*gn-clie.Nombre = OpenCPedidos.Nombre*/
    gn-clie.clfCli = 'C'
    gn-clie.flgsit = "A"
    gn-clie.fching = TODAY
    gn-clie.usuario = 'LISTAFACIL'
    gn-clie.coddiv = s-coddiv
    gn-clie.canal = '099'
    gn-clie.CodDept = OpenCPedidos.CodDept
    gn-clie.CodDist = OpenCPedidos.CodDist
    gn-clie.CodProv = OpenCPedidos.CodProv
    gn-clie.DirEnt = OpenCPedidos.LugEnt.

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
        IF LocalCounter = 5 THEN LEAVE GetLock.    /* 5 intentos m�ximo */
    END.
    IF LocalCounter = 5 OR NOT AVAILABLE FacCorre THEN RETURN ERROR.
    IF FacCorre.FlgCic = NO THEN DO:
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            DISPLAY 'Se ha llegado al l�mite del correlativo:' FacCorre.NroFin SKIP
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
    /* Buscamos un n�mero v�lido */
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
    BUFFER-COPY OpenCPedidos
        TO FacCPedi
        ASSIGN 
        FacCPedi.CodCia = S-CODCIA
        FacCPedi.CodDiv = S-CODDIV
        FacCPedi.CodDoc = s-coddoc 
        FacCPedi.Cmpbnte = (IF OpenCPedidos.Cmpbnte = '1' THEN 'FAC' ELSE 'BOL')
        FacCPedi.CodAlm = s-CodAlm    /* Lista de Almacenes V�lidos de Venta */
        FacCPedi.FchPed = TODAY 
        FacCPedi.FchVen = TODAY 
        FacCPedi.Atencion = OpenCPedidos.DNICli
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.TpoPed = s-TpoPed
        FacCPedi.NroRef =  OpenCPedidos.NroPed
        FacCPedi.FlgEst = "P".    /* APROBADO */
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN 
        FacCPedi.FmaPgo = "000"
        FacCPedi.CodVen = "020"
        FacCPedi.FlgIgv = YES
        FacCPedi.PorIgv = s-PorIgv
        FacCPedi.Hora = STRING(TIME,"HH:MM")
        FacCPedi.Usuario = S-USER-ID
        FacCPedi.Libre_d01 = 4
        FacCPedi.Libre_c01 = pCodDiv
        FacCPedi.ubigeo[2] = OpenCPedidos.coddept
        FacCPedi.ubigeo[3] = OpenCPedidos.codprov
        FacCPedi.ubigeo[4] = OpenCPedidos.coddist.


    FOR EACH OpenDPedidos OF OpenCPedidos BY OpenDPedidos.NroItm:
        I-NITEM = I-NITEM + 1.
        CREATE FacDPedi.
        BUFFER-COPY OPenDPedidos
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
            FacDPedi.Por_Dsctos[3] = OpenDPedidos.PorDto
            FacDPedi.ImpLin = OpenDPedidos.ImpLin + OpenDPedidos.ImpIgv
            FacDPedi.PreUni = FacDPedi.ImpLin / FacDPedi.CanPed
            FacDPedi.Factor = 1.
        /* Percepciones */
/*         ASSIGN                                      */
/*             FacDPedi.CanApr = OpenDPedidos.ImpDto2. */
        /* ************ */
        FacDPedi.AftIgv = OpenDPedidos.ImpIgv > 0.
        FacDPedi.AftIsc = NO.
        FacDPedi.AlmDes = s-codalm.
    END.
    RUN Graba-Totales.
    IF FacCPedi.Cmpbnte = "FAC" THEN RUN Crea-Cliente.
    ELSE FacCPedi.CodCli = "11111111111".
    ASSIGN
        s-CodRef = Faccpedi.coddoc
        s-NroRef = Faccpedi.nroped.

    /* MARCAMOS EL ORIGEN */
    FIND B-CCPED WHERE ROWID(B-CCPED) = ROWID(OpenCPedidos) EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CCPED THEN UNDO, RETURN ERROR.
    ASSIGN 
        B-CCPED.FlagMigracion = '1'
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

&IF DEFINED(EXCLUDE-Crea-Pedidos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-Pedidos Procedure 
PROCEDURE Crea-Pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR s-coddoc AS CHAR INIT 'PED' NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR iLocalCounter AS INTEGER INITIAL 0 NO-UNDO.


DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
        AND Faccorre.coddiv = s-coddiv
        AND Faccorre.coddoc = "COT"
        AND FacCorre.FlgEst = YES
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccorre THEN UNDO, RETURN ERROR.
    s-NroSer = FacCorre.NroSer.

    /* RHC 21/01/2014 BLOQUEAMOS LA COTIZACION TAMBIEN */
    GetLock :
    DO ON STOP UNDO GetLock, RETRY GetLock:
        IF RETRY THEN DO:
            iLocalCounter = iLocalCounter + 1.
            IF iLocalCounter = 5 THEN LEAVE GetLock.
        END.
        FIND B-CPedi WHERE B-CPedi.CodCia = s-CodCia
            AND B-CPedi.CodDiv = s-CodDiv
            AND B-CPedi.CodDoc = s-CodRef
            AND B-CPedi.NroPed = s-NroRef EXCLUSIVE-LOCK NO-ERROR.
    END.
    IF iLocalCounter = 5 OR NOT AVAILABLE FacCorre THEN UNDO, RETURN ERROR.
    
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
        IF LocalCounter = 5 THEN LEAVE GetLock.    /* 5 intentos m�ximo */
    END.
    IF LocalCounter = 5 OR NOT AVAILABLE FacCorre THEN UNDO, RETURN ERROR.
    IF FacCorre.FlgCic = NO THEN DO:
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            DISPLAY 'Se ha llegado al l�mite del correlativo:' FacCorre.NroFin SKIP
                'No se puede generar el documento' s-coddoc 'serie' s-nroser
                WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE.
            PAUSE 0.
            UNDO, RETURN ERROR.
        END.
    END.
    IF FacCorre.FlgCic = YES THEN DO:
        /* REGRESAMOS AL NUMERO 1 */
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            IF FacCorre.NroIni > 0 THEN FacCorre.Correlativo = FacCorre.NroIni.
            ELSE FacCorre.Correlativo = 1.
        END.
    END.

    /* Buscamos un n�mero v�lido */
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
    BUFFER-COPY B-CPEDI
        TO Faccpedi
        ASSIGN 
        Faccpedi.CodCia = S-CODCIA
        Faccpedi.CodDiv = S-CODDIV
        Faccpedi.CodDoc = s-coddoc 
        Faccpedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        Faccpedi.CodRef = s-codref      /* COT */
        Faccpedi.NroRef = s-nroref      /* COT */
        Faccpedi.FchPed = TODAY 
        Faccpedi.FchVen = TODAY 
        Faccpedi.PorIgv = s-PorIgv 
        Faccpedi.FlgEst = "G"       /* FLAG TEMPORAL POR APROBAR */
        FacCPedi.TpoPed = s-TpoPed
        FacCPedi.FlgEnv = s-FlgEnv
        FacCPedi.Libre_c01 = s-User-id + '|' + STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM').
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN
        Faccpedi.Usuario = S-USER-ID
        Faccpedi.Hora   = STRING(TIME,"HH:MM").
    /* Division destino */
    FIND Almacen OF Faccpedi NO-LOCK NO-ERROR.
    IF AVAILABLE Almacen THEN FacCPedi.DivDes = Almacen.CodDiv.
    /* Detalle del Pedido */
    RUN Genera-Pedido.    /* Detalle del pedido */
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        DISPLAY 'NO se pudo generar el pedido' + CHR(10) +
            'NO hay stock suficiente en los almacenes'
            WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE.
        UNDO, RETURN ERROR.
    END.
    /* Grabamos Totales */
    RUN Graba-Totales.

    /* CALCULAMOS LA PERCEPCION */
    /*RUN vta2/percepcion-por-pedido ( ROWID(Faccpedi)).*/
    /*FIND CURRENT Faccpedi.*/

    RUN pactualizacotizacion ("C").
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN ERROR.
    /* *********************************************************** */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Pedido Procedure 
PROCEDURE Genera-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE I-NPEDI AS INTEGER NO-UNDO INIT 0.
  DEFINE VARIABLE f-Factor AS DEC NO-UNDO.
  DEFINE VARIABLE x-CanPed AS DEC NO-UNDO.
  DEFINE VARIABLE s-StkComprometido AS DEC.
  DEFINE VARIABLE s-StkDis AS DEC NO-UNDO.

  DEF VAR f-PreBas AS DEC NO-UNDO.
  DEF VAR f-PreVta AS DEC NO-UNDO.
  DEF VAR f-Dsctos AS DEC NO-UNDO.
  DEF VAR y-Dsctos AS DEC NO-UNDO.
  DEF VAR SW-LOG1  AS LOGI NO-UNDO.
  DEF VAR x-StkAct AS DEC NO-UNDO.

  /* POR CADA PEDI VOLVEMOS A VERIFICAR EL STOCK DISPONIBLE */
  DETALLE:
  FOR EACH B-DPEDI OF B-CPEDI, FIRST Almmmatg OF B-DPEDI NO-LOCK BY B-DPEDI.NroItm: 
      /* RUTINA QUE VERIFICA NUEVAMENTE EL STOCK DISPONIBLE Y AJUSTA LA CANTIDAD EN CASO NECESARIO */
      FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
          AND Almtconv.Codalter = B-DPEDI.UndVta
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Almtconv THEN DO:
          pMensaje = 'Se ha encontrado un problema con el producto ' + B-DPEDI.codmat + CHR(10) +
              'No se encuentra definido su factor de equivalencia' + CHR(10) +
              'Unidad base en el cat�logo: ' + Almmmatg.UndBas + CHR(10) +
              'Unidad de venta pedido:' + B-DPEDI.UndVta.
          RETURN 'ADM-ERROR'.
      END.
      f-Factor = Almtconv.Equival.
      FIND Almmmate WHERE Almmmate.codcia = s-codcia
          AND Almmmate.codalm = B-DPEDI.AlmDes
          AND Almmmate.codmat = B-DPEDI.CodMat
          NO-LOCK NO-ERROR .
      IF NOT AVAILABLE Almtconv THEN DO:
          pMensaje = 'Se ha encontrado un problema con el producto ' + B-DPEDI.codmat + CHR(10) +
              'No se encuentra asignado al almac�n ' + B-DPEDI.AlmDes + CHR(10).
          RETURN 'ADM-ERROR'.
      END.
      x-StkAct = Almmmate.StkAct.
      RUN Stock-Comprometido (B-DPEDI.CodMat, B-DPEDI.AlmDes, OUTPUT s-StkComprometido).
      s-StkDis = x-StkAct - s-StkComprometido.
      IF s-StkDis <= 0 THEN DO:
          pMensajeFinal = pMensajeFinal + 
              'NO hay stock suficiente para el producto ' + B-DPEDI.codmat + 
              'en el almac�n ' + B-DPEDI.AlmDes + CHR(10).
          RETURN 'ADM-ERROR'.
      END.
      /* **************************************************************************************** */
      x-CanPed = B-DPEDI.CanPed * f-Factor.
      IF s-StkDis < x-CanPed THEN DO:
          pMensajeFinal = pMensajeFinal + 
              'NO hay stock suficiente para el producto ' + B-DPEDI.codmat + 
              'en el almac�n ' + B-DPEDI.AlmDes + CHR(10).
          RETURN 'ADM-ERROR'.
      END.
      /* FIN DE CONTROL DE AJUSTES */
      ASSIGN
          B-DPEDI.ImpLin = ROUND ( B-DPEDI.CanPed * B-DPEDI.PreUni * 
                        ( 1 - B-DPEDI.Por_Dsctos[1] / 100 ) *
                        ( 1 - B-DPEDI.Por_Dsctos[2] / 100 ) *
                        ( 1 - B-DPEDI.Por_Dsctos[3] / 100 ), 2 ).
      IF B-DPEDI.Por_Dsctos[1] = 0 AND B-DPEDI.Por_Dsctos[2] = 0 AND B-DPEDI.Por_Dsctos[3] = 0 
      THEN B-DPEDI.ImpDto = 0.
      ELSE B-DPEDI.ImpDto = B-DPEDI.CanPed * B-DPEDI.PreUni - B-DPEDI.ImpLin.
      ASSIGN
          B-DPEDI.ImpLin = ROUND(B-DPEDI.ImpLin, 2)
          B-DPEDI.ImpDto = ROUND(B-DPEDI.ImpDto, 2).
      IF B-DPEDI.AftIsc 
          THEN B-DPEDI.ImpIsc = ROUND(B-DPEDI.PreBas * B-DPEDI.CanPed * (Almmmatg.PorIsc / 100),4).
      IF B-DPEDI.AftIgv 
          THEN B-DPEDI.ImpIgv = B-DPEDI.ImpLin - ROUND( B-DPEDI.ImpLin  / ( 1 + (s-PorIgv / 100) ), 4 ).
  END.
  /* RHC 21/10/2013 Deben despacharse completos los items que han tenido 
  descuento por volumen por lineas y sublineas agrupados.
  Si no se pueden cubrir entonces NO se despachan */
  /* AHORA S� GRABAMOS EL PEDIDO */
  FOR EACH B-DPEDI OF B-CPEDI, FIRST Almmmatg OF B-DPEDI NO-LOCK BY B-DPEDI.NroItm: 
      I-NPEDI = I-NPEDI + 1.
      CREATE Facdpedi.
      BUFFER-COPY B-DPEDI 
          TO Facdpedi
          ASSIGN
              Facdpedi.CodCia = Faccpedi.CodCia
              Facdpedi.CodDiv = Faccpedi.CodDiv
              Facdpedi.coddoc = Faccpedi.coddoc
              Facdpedi.NroPed = Faccpedi.NroPed
              Facdpedi.FchPed = Faccpedi.FchPed
              Facdpedi.Hora   = Faccpedi.Hora 
              Facdpedi.FlgEst = Faccpedi.FlgEst
              Facdpedi.NroItm = I-NPEDI.
  END.
  RETURN 'OK'.

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

  DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

  ASSIGN
    FacCPedi.ImpDto = 0
    FacCPedi.ImpIgv = 0
    FacCPedi.ImpIsc = 0
    FacCPedi.ImpTot = 0
    FacCPedi.ImpExo = 0
    FacCPedi.Importe[3] = 0
    F-IGV = 0
    F-ISC = 0.
  /* VENTAS INAFECTAS A IGV */
  IF FacCPedi.FlgIgv = NO THEN DO:
      FacCPedi.PorIgv = 0.00.
      FOR EACH FacDPedi OF FacCPedi:
          ASSIGN
              FacDPedi.AftIgv = NO
              FacDPedi.ImpIgv = 0.00.
      END.
  END.
  FOR EACH FacDPedi OF FacCPedi:
      ASSIGN
          F-Igv = F-Igv + FacDPedi.ImpIgv
          F-Isc = F-Isc + FacDPedi.ImpIsc
          FacCPedi.ImpTot = FacCPedi.ImpTot + FacDPedi.ImpLin.
      /* Importe Inafecto o Exonerado */
      IF FacDPedi.ImpIgv = 0 THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.
  END.
  ASSIGN
      FacCPedi.ImpIgv = ROUND(F-IGV,2)
      FacCPedi.ImpIsc = ROUND(F-ISC,2)
      FacCPedi.ImpVta = ROUND( (FacCPedi.ImpTot - FacCPedi.ImpExo) / (1 + FacCPedi.PorIgv / 100), 2)
      FacCPedi.ImpBrt = FacCPedi.ImpVta /*+ FacCPedi.ImpIsc*/ + FacCPedi.ImpDto /*+ FacCPedi.ImpExo*/
      FacCPedi.Importe[1] = FacCPedi.ImpTot.    /* Guardamos el importe original */
  IF FacCPedi.FlgIgv = NO 
      THEN ASSIGN
          FacCPedi.ImpVta = FacCPedi.ImpExo
          FacCPedi.ImpBrt = FacCPedi.ImpExo.

  /* *************************************** PERCEPCIONES ******************************** */
  DEF VAR s-PorPercepcion AS DEC NO-UNDO.
  DEF VAR pPercepcion AS DEC NO-UNDO.

  ASSIGN
      Faccpedi.AcuBon[4] = 0
      Faccpedi.AcuBon[5] = 0.
  FOR EACH Facdpedi OF Faccpedi NO-LOCK:
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

&IF DEFINED(EXCLUDE-pactualizacotizacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pactualizacotizacion Procedure 
PROCEDURE pactualizacotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pTipo  AS CHAR.     /* D: descarga(-)  C: carga(+) */

/* ACTUALIZA LA COTIZACION EN BASE AL PEDIDO AL CREDITO */
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      /* NO ACTUALIZAMOS LAS PROMOCIONES */
      FOR EACH Facdpedi OF Faccpedi NO-LOCK WHERE Facdpedi.Libre_c05 <> "OF",
          FIRST B-DPedi OF B-CPedi WHERE B-DPedi.CodMat = Facdpedi.CodMat:
          CASE pTipo:
              WHEN "D" THEN B-DPedi.CanAte = B-DPedi.CanAte - Facdpedi.CanPed.
              WHEN "C" THEN DO:
                  B-DPedi.CanAte = B-DPedi.CanAte + Facdpedi.CanPed.
                  /* CONTROL DE ATENCIONES */
                  IF B-DPEDI.CanAte > B-DPEDI.CanPed THEN DO:
                      DISPLAY 'Se ha detectado un error el el producto ' + B-DPEDI.codmat + CHR(10) +
                          'Los despachos superan a lo cotizado' + CHR(10) +
                          'Cant. cotizada: ' + STRING(B-DPEDI.CanPed, '->>>,>>9.99') + CHR(10) +
                          'Total pedidos : ' + STRING(B-DPEDI.CanAte, '->>>,>>9.99') + CHR(10) +
                          'FIN DEL PROCESO'
                          WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE.
                      UNDO, RETURN "ADM-ERROR".
                  END.
              END.
          END CASE.
      END.
      /* ACTUALIZAMOS FLAG DE LA COTIZACION */
      B-CPedi.FlgEst = "C".
      FIND FIRST B-DPedi OF B-CPedi WHERE B-DPedi.CanAte < B-DPedi.CanPed NO-LOCK NO-ERROR.
      IF AVAILABLE B-DPedi THEN B-CPedi.FlgEst = "P".
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Stock-Comprometido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stock-Comprometido Procedure 
PROCEDURE Stock-Comprometido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF OUTPUT PARAMETER pComprometido AS DEC.

/* CALCULO DEL STOCK COMPROMETIDO */
FIND FacCfgGn WHERE faccfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccfggn THEN RETURN.

/* Stock comprometido por PEDIDOS ACTIVOS MOSTRADOR */
DEF VAR TimeOut AS INTEGER NO-UNDO.
DEF VAR TimeNow AS INTEGER NO-UNDO.

/* Tiempo por defecto fuera de campa�a */
TimeOut = (FacCfgGn.Dias-Res * 24 * 3600) +
          (FacCfgGn.Hora-Res * 3600) + 
          (FacCfgGn.Minu-Res * 60).

pComprometido = 0.
DEF VAR i AS INT NO-UNDO.
DEF VAR x-CodAlm AS CHAR NO-UNDO.

DO i = 1 TO NUM-ENTRIES(pCodAlm):
    x-CodAlm = ENTRY(i, pCodAlm).
    /**********   Barremos para los PEDIDOS MOSTRADOR ***********************/ 
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.CodCia = s-codcia
        AND Facdpedi.AlmDes = x-CodAlm
        AND Facdpedi.codmat = pcodmat
        AND Facdpedi.coddoc = 'P/M'
        AND Facdpedi.FlgEst = "P" :
        FIND FIRST Faccpedi OF Facdpedi WHERE Faccpedi.FlgEst = "P" NO-LOCK NO-ERROR.
        IF NOT AVAIL Faccpedi THEN NEXT.

        TimeNow = (TODAY - FacCPedi.FchPed) * 24 * 3600.
        TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(FacCPedi.Hora, 1, 2)) * 3600) +
                  (INTEGER(SUBSTRING(FacCPedi.Hora, 4, 2)) * 60) ).
        IF TimeOut > 0 THEN DO:
            IF TimeNow <= TimeOut   /* Dentro de la valides */
            THEN DO:
                /* cantidad en reservacion */
                pComprometido = pComprometido + FacDPedi.Factor * FacDPedi.CanPed.
            END.
        END.
    END.
    /**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = x-CodAlm
            AND Facdpedi.codmat = pCodMat
            AND Facdpedi.coddoc = 'PED'
            AND Facdpedi.flgest = 'P':
        /* RHC 12.12.2011 agregamos los nuevos estados */
        FIND FIRST Faccpedi OF Facdpedi WHERE LOOKUP(Faccpedi.FlgEst, "G,X,P,W,WX,WL") > 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN NEXT.
        pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    END.
    /* ORDENES DE DESPACHO CREDITO */
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = x-CodAlm
            AND Facdpedi.codmat = pCodMat
            AND Facdpedi.coddoc = 'O/D'
            AND Facdpedi.flgest = 'P':
        FIND FIRST Faccpedi OF Facdpedi WHERE Faccpedi.flgest = 'P' NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN NEXT.
        pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    END.
    /* Stock Comprometido por Pedidos por Reposicion Automatica */
    FOR EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = s-codcia
        AND Almcrepo.TipMov = 'A'
        AND Almcrepo.AlmPed = x-CodAlm
        AND Almcrepo.FlgEst = 'P'
        AND Almcrepo.FlgSit = 'A',
        EACH Almdrepo OF Almcrepo NO-LOCK WHERE Almdrepo.codmat = pCodMat
        AND almdrepo.CanApro > almdrepo.CanAten:
        pComprometido = pComprometido + (Almdrepo.CanApro - Almdrepo.CanAten).
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : Transfiere las ventas de OpenOrange a Progress

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INTE INIT 000 NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR s-codcja AS CHAR INIT "I/C".
DEF VAR s-sercja AS INTE.
DEF VAR s-user-id AS CHAR INIT "ADMIN" NO-UNDO.
DEF VAR s-CodTer    LIKE ccbcterm.codter.
DEF VAR s-tipo   AS CHAR INIT "MOSTRADOR".
DEF VAR pNroDocCja AS CHAR NO-UNDO.
DEF VAR x-NroItm AS INT NO-UNDO.
DEF VAR x-NroDoc AS CHAR NO-UNDO.

DEF VAR FILL-IN_T_Compra AS DEC DECIMALS 4 NO-UNDO.
DEF VAR FILL-IN_T_Venta  AS DEC DECIMALS 4 NO-UNDO.

DEF VAR x-Rowid AS ROWID NO-UNDO.

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
         HEIGHT             = 4.5
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* CHEQUEO PREVIO */
FOR EACH openventas WHERE OpenVentas.FlagMigracion <> "S":   /*OpenVentas.FlagMigracion = "N"*/
    FIND Almmmate WHERE Almmmate.codcia = s-codcia
        AND Almmmate.codalm = OpenVentas.CodAlm
        AND Almmmate.codmat = OpenVentas.codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmate THEN DO:
        DISPLAY 
            'ERROR: Producto' OpenVentas.codmat
            'NO asignado al almacén' OpenVentas.CodAlm 
            WITH STREAM-IO NO-BOX NO-LABELS.
        RETURN.
    END.
END.

PRINCIPAL:
FOR EACH openventas WHERE OpenVentas.FlagMigracion <> "S"   /*OpenVentas.FlagMigracion = "N"*/
    AND OpenVentas.CodDoc <> ''
    AND OpenVentas.NroDoc <> ''
    BREAK BY OpenVentas.CodCaja 
    BY OpenVentas.usuario 
    BY OpenVentas.FchDoc 
    BY OpenVentas.HorCie
    BY OpenVentas.CodDoc 
    BY OpenVentas.NroDoc
    TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    /* Transformamos el Nro del documento SIN guiones */
    x-NroDoc = REPLACE(OpenVentas.NroDoc,'-', '').
    /* ********************************************** */
    IF FIRST-OF(OpenVentas.CodCaja)
        OR FIRST-OF(OpenVentas.usuario)
        OR FIRST-OF(OpenVentas.FchDoc)
        OR FIRST-OF(OpenVentas.HorCie)
        OR FIRST-OF(OpenVentas.CodDoc)
        OR FIRST-OF(OpenVentas.NroDoc)
        THEN DO:
        ASSIGN
            s-coddiv = OpenVentas.coddiv
            s-codter = Openventas.codcaja.
        FIND FIRST ccbdterm WHERE CcbDTerm.CodCia = s-codcia 
            AND CcbDTerm.CodDiv = s-coddiv 
            AND CcbDTerm.CodDoc = s-codcja 
            AND CcbDTerm.CodTer = s-codter NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbdterm THEN DO:
            DISPLAY
                "ERROR: El documento I/Caja no está configurado en el terminal" s-codter
                WITH STREAM-IO NO-BOX NO-LABELS.
            UNDO PRINCIPAL, RETURN.
        END.
        s-sercja = ccbdterm.nroser.

        /* CREAMOS LA CABECERA DEL INGRESO A CAJA */
        RUN Ingreso-a-Caja (OUTPUT pNroDocCja).
        IF RETURN-VALUE = "ADM-ERROR" THEN UNDO PRINCIPAL, RETURN.

        /* CREAMOS COMPROBANTE */
        FIND FIRST Ccbcdocu WHERE CcbCDocu.CodCia = s-codcia
            AND CcbCDocu.CodDoc = OpenVentas.coddoc
            AND CcbCDocu.NroDoc = x-NroDoc      /*OpenVentas.nrodoc*/
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbcdocu THEN DO:
            DISPLAY
                'ERROR: Comprobante ya registrado' ccbcdocu.coddoc ccbcdocu.nrodoc
                WITH STREAM-IO NO-BOX NO-LABELS.
            UNDO PRINCIPAL, RETURN.
        END.
        CREATE Ccbcdocu.
        ASSIGN
            CcbCDocu.CodCia = s-codcia
            CcbCDocu.CodDiv = s-coddiv
            CcbCDocu.CodDoc = OpenVentas.coddoc
            CcbCDocu.NroDoc = x-NroDoc      /*OpenVentas.nrodoc*/
            CcbCDocu.FchDoc = OpenVentas.fchdoc
            CcbCDocu.FchVto = OpenVentas.fchdoc
            CcbCDocu.CodAlm = OpenVentas.codalm
            CcbCDocu.CodCli = OpenVentas.codcli
            CcbCDocu.NomCli = OpenVentas.nomcli
            CcbCDocu.DirCli = OpenVentas.dircli
            CcbCDocu.RucCli = (IF OpenVentas.TpoDoc = "RUC" THEN OpenVentas.RucCli ELSE "")
            CcbCDocu.CodAnt = (IF OpenVentas.TpoDoc <> "RUC" THEN OpenVentas.RucCli ELSE "")
            CcbCDocu.FmaPgo = "000"
            CcbCDocu.Glosa  = OpenVentas.glosa
            CcbCDocu.Tipo   = s-tipo
            CcbCDocu.TipVta = "1"
            CcbCDocu.TpoFac = "C"
            CcbCDocu.CodMon = OpenVentas.codmon
            CcbCDocu.CodVen = OpenVentas.codven
            CcbCDocu.DivOri = s-coddiv
            CcbCDocu.FlgEst = OpenVentas.flgest
            CcbCDocu.ImpBrt = OpenVentas.impbrt
            CcbCDocu.ImpDto = OpenVentas.impdto
            CcbCDocu.ImpExo = OpenVentas.impexo
            CcbCDocu.ImpIgv = OpenVentas.impigv
            CcbCDocu.ImpTot = OpenVentas.imptot
            CcbCDocu.ImpVta = OpenVentas.impvta
            CcbCDocu.PorIgv = OpenVentas.porigv
            CcbCDocu.SdoAct = 0     /* OpenVentas.imptot */
            CcbCDocu.TpoCmb = FILL-IN_T_Compra  /* OpenVentas.tpocmb */
            CcbCDocu.UsuAnu = OpenVentas.usuanu
            CcbCDocu.usuario = OpenVentas.usuario
            CcbCDocu.ImpIsc = OpenVentas.impisc.
        ASSIGN
            CcbCDocu.Libre_c01 = s-user-id
            CcbCDocu.Libre_f01 = TODAY.
        /*MESSAGE ccbcdocu.codcia ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc.*/
        /* DETALLE DE CAJA */
        FIND Ccbccaja WHERE Ccbccaja.codcia = s-codcia
            AND Ccbccaja.coddiv = s-coddiv
            AND Ccbccaja.coddoc = s-codcja
            AND Ccbccaja.nrodoc = pNroDocCja
            NO-LOCK.
        CREATE Ccbdcaja.
        ASSIGN
            CcbDCaja.CodCia = Ccbccaja.CodCia
            CcbdCaja.CodDiv = Ccbccaja.CodDiv
            CcbDCaja.CodDoc = Ccbccaja.CodDoc
            CcbDCaja.NroDoc = Ccbccaja.NroDoc
            CcbDCaja.CodRef = CcbCDocu.CodDoc
            CcbDCaja.NroRef = CcbCDocu.NroDoc
            CcbDCaja.CodCli = CcbCDocu.CodCli
            CcbDCaja.CodMon = CcbCDocu.CodMon
            CcbDCaja.FchDoc = Ccbccaja.FchDoc
            CcbDCaja.ImpTot = CcbCDocu.ImpTot
            CcbDCaja.TpoCmb = Ccbccaja.TpoCmb.
        /* CANCELAMOS EL COMPROBANTE */
        ASSIGN
            CcbCDocu.FlgEst = "C"
            CcbCDocu.FchCan = TODAY
            CcbCDocu.SdoAct = 0
            x-NroItm = 0
            x-Rowid = ROWID(Ccbcdocu).
        /* CREAMOS CLIENTE */
        FIND gn-clie WHERE gn-clie.codcia = cl-codcia
            AND gn-clie.codcli = Ccbcdocu.codcli
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE gn-clie THEN DO:
            CREATE gn-clie.
            ASSIGN 
                gn-clie.CodCia     = cl-codcia
                gn-clie.CodCli     = Ccbcdocu.CodCli 
                gn-clie.NomCli     = Ccbcdocu.NomCli 
                gn-clie.DirCli     = Ccbcdocu.DirCli 
                gn-clie.Ruc        = Ccbcdocu.RucCli 
                gn-clie.DNI        = Ccbcdocu.CodAnt
                gn-clie.clfCli     = "C" 
                gn-clie.CodPais    = "01" 
                gn-clie.Fching     = TODAY 
                gn-clie.usuario    = Ccbcdocu.Usuario
                gn-clie.TpoCli     = "1"
                gn-clie.CodDiv     = Ccbcdocu.CODDIV.

             /* RHC 07.03.05 valores de linea de credito por defecto */
             ASSIGN
                 gn-clie.FlgSit = 'A'    /* Activo */
                 gn-clie.FlagAut = 'A'   /* Autorizado */
                 gn-clie.ClfCli = 'C'.   /* Regular / Malo */
        END.
    END.
    /* DETALLE DEL COMPROBANTE */
    FIND Ccbddocu WHERE CcbDDocu.CodCia = s-codcia
        AND CcbDDocu.CodDiv = s-coddiv
        AND CcbDDocu.CodDoc = OpenVentas.CodDoc 
        AND CcbDDocu.NroDoc = x-NroDoc      /*OpenVentas.NroDoc */
        AND CcbDDocu.codmat = OpenVentas.CodMat
        NO-ERROR.
    IF NOT AVAILABLE Ccbddocu THEN DO:
        CREATE Ccbddocu.
        x-NroItm = x-NroItm + 1.
    END.
    ASSIGN
        CcbDDocu.CodCia = s-codcia
        CcbDDocu.CodDiv = s-coddiv
        CcbDDocu.CodDoc = OpenVentas.CodDoc 
        CcbDDocu.NroDoc = x-NroDoc      /*OpenVentas.NroDoc */
        CcbDDocu.FchDoc = OpenVentas.FchDoc
        CcbDDocu.NroItm = x-NroItm
        CcbDDocu.CodCli = OpenVentas.CodCli
        CcbDDocu.AlmDes = OpenVentas.CodAlm
        CcbDDocu.codmat = OpenVentas.CodMat
        CcbDDocu.AftIgv = OpenVentas.AftIgv
        CcbDDocu.AftIsc = OpenVentas.AftIsc
        CcbDDocu.CanDes = CcbDDocu.CanDes + OpenVentas.CanDes
        CcbDDocu.Factor = OpenVentas.Factor
        CcbDDocu.ImpIgv = CcbDDocu.ImpIgv + OpenVentas.ImpIgvLin
        CcbDDocu.ImpLin = CcbDDocu.ImpLin + OpenVentas.ImpLin
        CcbDDocu.PreBas = Ccbddocu.ImpLin / Ccbddocu.CanDes
        CcbDDocu.PreUni = Ccbddocu.ImpLin / Ccbddocu.CanDes
        CcbDDocu.UndVta = OpenVentas.UndVta.
    IF LAST-OF(OpenVentas.CodCaja)
        OR LAST-OF(OpenVentas.usuario)
        OR LAST-OF(OpenVentas.FchDoc)
        OR LAST-OF(OpenVentas.HorCie)
        OR LAST-OF(OpenVentas.CodDoc)
        OR LAST-OF(OpenVentas.NroDoc)
        THEN DO:
        /* Descarga de Almacen */
        FIND Ccbcdocu WHERE ROWID(Ccbcdocu) = x-Rowid.
        IF Ccbcdocu.flgest <> "A" THEN DO:
            RUN vta2/act_alm (ROWID(CcbCDocu)).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO PRINCIPAL, RETURN.
        END.
    END.
    ASSIGN
        OpenVentas.FlagMigracion = "S"
        OpenVentas.FlagFechaHora = DATETIME(TODAY, MTIME)
        OpenVentas.FlagUsuario   = s-user-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-act_alm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE act_alm Procedure 
PROCEDURE act_alm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER X-ROWID AS ROWID.

FIND ccbcdocu WHERE ROWID(ccbcdocu) = X-ROWID NO-LOCK NO-ERROR.
IF NOT AVAILABLE ccbcdocu THEN RETURN "OK".

/* PEDIDO MOSTRADOR YA HA DESCARGADO STOCK EN CAJA */
IF CcbCDocu.CodDoc = "G/R" AND CcbCDocu.CodPed = "P/M" THEN RETURN "OK".

DEF VAR i AS INTEGER INITIAL 1 NO-UNDO.
DEF VAR LocalCounter AS INTEGER INITIAL 0 NO-UNDO.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK WHERE Ccbddocu.implin >= 0
        BREAK BY Ccbddocu.AlmDes:
        IF FIRST-OF(Ccbddocu.AlmDes) THEN DO:
            /* CABECERA */
            REPEAT:
                FIND Almacen WHERE Almacen.CodCia = Ccbddocu.codcia
                    AND Almacen.CodAlm = Ccbddocu.almdes       /* OJO */
                    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF NOT AVAILABLE Almacen THEN DO:
                    IF LOCKED Almacen THEN UNDO, RETRY.
                    ELSE DO:
                        DISPLAY
                            'ERROR: Almacén:' Ccbddocu.almdes '<< NO registrado'
                            'Se ha abortado la grabación' 
                            WITH STREAM-IO NO-BOX NO-LABELS.
                        UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
                    END.
                END.
                LEAVE.
            END.
            /* ************************* */
            CREATE almcmov.
            ASSIGN Almcmov.CodCia  = CcbDDocu.CodCia 
                   Almcmov.CodAlm  = CcbDDocu.AlmDes
                   Almcmov.TipMov  = "S"
                   Almcmov.CodMov  = CcbCDocu.CodMov
                   Almcmov.NroSer  = 0 /* INTEGER(SUBSTRING(ccbcdocu.NroDoc,1,3)) */
                   Almcmov.NroDoc  = Almacen.CorrSal 
                   Almacen.CorrSal = Almacen.CorrSal + 1
                   Almcmov.FchDoc  = CcbCDocu.FchDoc
                   Almcmov.HorSal  = STRING(TIME, "HH:MM:SS")
                   Almcmov.CodVen  = ccbcdocu.CodVen
                   Almcmov.CodCli  = ccbcdocu.CodCli
                   Almcmov.Nomref  = ccbcdocu.NomCli
                   Almcmov.CodRef  = ccbcdocu.CodDoc
                   Almcmov.NroRef  = ccbcdocu.nrodoc
                   Almcmov.NroRf1  = STRING(CcbCDocu.CodDoc, 'x(3)') + CcbCDocu.NroDoc
                   Almcmov.NroRf2  = CcbCDocu.CodPed + CcbCDocu.NroPed
                   Almcmov.usuario = s-user-id
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DISPLAY 
                    'ERROR: Ha ocurrido un error inesperado al grabar los movimientos de almacén' 
                    WITH STREAM-IO NO-BOX NO-LABELS.
                UNDO PRINCIPAL, RETURN "ADM-ERROR".
            END.
        END.
        /* DETALLE */
        CREATE Almdmov.
        ASSIGN Almdmov.CodCia = Almcmov.CodCia
               Almdmov.CodAlm = Almcmov.CodAlm
               Almdmov.CodMov = Almcmov.CodMov 
               Almdmov.NroSer = almcmov.nroser
               Almdmov.NroDoc = almcmov.nrodoc
               Almdmov.AftIgv = ccbddocu.aftigv
               Almdmov.AftIsc = ccbddocu.aftisc
               Almdmov.CanDes = ccbddocu.candes
               Almdmov.codmat = ccbddocu.codmat
               Almdmov.CodMon = ccbcdocu.codmon
               Almdmov.CodUnd = ccbddocu.undvta
               Almdmov.Factor = ccbddocu.factor
               Almdmov.FchDoc = CcbCDocu.FchDoc
               Almdmov.ImpDto = ccbddocu.impdto
               Almdmov.ImpIgv = ccbddocu.impigv
               Almdmov.ImpIsc = ccbddocu.impisc
               Almdmov.ImpLin = ccbddocu.implin
               Almdmov.NroItm = i
               Almdmov.PorDto = ccbddocu.pordto
               Almdmov.PreBas = ccbddocu.prebas
               Almdmov.PreUni = ccbddocu.preuni
               Almdmov.TipMov = "S"
               Almdmov.TpoCmb = ccbcdocu.tpocmb
               Almcmov.TotItm = i
               Almdmov.HraDoc = Almcmov.HorSal
               i = i + 1.
        /* EXISTE UNA RUTINA QUE ACTUALIZA LOS STOCKS DE LOS ALMACEN DE UTILEX */
/*         RUN alm/almdcstk (ROWID(almdmov)).                                     */
/*         IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO PRINCIPAL, RETURN 'ADM-ERROR'. */
/*         /* RHC 05.04.04 ACTIVAMOS KARDEX POR ALMACEN */                        */
/*         RUN alm/almacpr1 (ROWID(almdmov), 'U').                                */
/*         IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO PRINCIPAL, RETURN 'ADM-ERROR'. */
    END.
    IF AVAILABLE(Almacen) THEN RELEASE Almacen.
    IF AVAILABLE(Almcmov) THEN RELEASE Almcmov.
    IF AVAILABLE(Almdmov) THEN RELEASE Almdmov.
END.

RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Ingreso-a-Caja) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ingreso-a-Caja Procedure 
PROCEDURE Ingreso-a-Caja :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER para_nrodoccja LIKE Ccbccaja.NroDoc.

DEFINE VARIABLE x_NumDoc AS CHARACTER.

trloop:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* TIPO DE CAMBIO */
    FIND LAST Gn-tccja WHERE Gn-tccja.Fecha <= OpenVentas.FchDoc NO-LOCK NO-ERROR.
    IF AVAILABLE Gn-TcCja THEN DO:
        FILL-IN_T_Compra = Gn-Tccja.Compra.
        FILL-IN_T_Venta = Gn-Tccja.Venta.
    END.
    ELSE DO:
        FILL-IN_T_Compra = 0.
        FILL-IN_T_Venta = 0.
    END.

    FIND Faccorre WHERE
        FacCorre.CodCia = s-codcia AND
        FacCorre.CodDiv = s-coddiv AND
        FacCorre.CodDoc = s-codcja AND
        FacCorre.NroSer = s-sercja
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE FacCorre THEN DO:
        DISPLAY
            'ERROR: No se pudo bloquear el correlativo caja:' s-codcja 'serie:' s-sercja
            WITH STREAM-IO NO-BOX NO-LABELS.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* Crea Cabecera de Caja */
    CREATE Ccbccaja.
    ASSIGN
        Ccbccaja.CodCia     = s-CodCia
        Ccbccaja.CodDiv     = OpenVentas.CodDiv 
        Ccbccaja.CodDoc     = s-CodCja
        Ccbccaja.NroDoc     = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCorre.Correlativo = FacCorre.Correlativo + 1
        Ccbccaja.CodCaja    = OpenVentas.CodCaja
        Ccbccaja.usuario    = OpenVentas.usuario
        Ccbccaja.CodCli     = OpenVentas.codcli
        Ccbccaja.NomCli     = OpenVentas.NomCli
        Ccbccaja.CodMon     = OpenVentas.CodMon
        Ccbccaja.FchDoc     = OpenVentas.FchDoc
        Ccbccaja.ImpNac[1]  = OpenVentas.ImpNac1
        Ccbccaja.ImpNac[2]  = OpenVentas.ImpNac2
        Ccbccaja.ImpNac[3]  = OpenVentas.ImpNac3
        Ccbccaja.ImpNac[4]  = OpenVentas.ImpNac4
        Ccbccaja.ImpNac[5]  = OpenVentas.ImpNac5
        Ccbccaja.ImpNac[6]  = OpenVentas.ImpNac6
        Ccbccaja.ImpNac[7]  = OpenVentas.ImpNac7            
        Ccbccaja.ImpUsa[1]  = OpenVentas.ImpUsa1
        Ccbccaja.ImpUsa[2]  = OpenVentas.ImpUsa2
        Ccbccaja.ImpUsa[3]  = OpenVentas.ImpUsa3
        Ccbccaja.ImpUsa[4]  = OpenVentas.ImpUsa4 
        Ccbccaja.ImpUsa[5]  = OpenVentas.ImpUsa5
        Ccbccaja.ImpUsa[6]  = OpenVentas.ImpUsa6
        Ccbccaja.ImpUsa[7]  = OpenVentas.ImpUsa7
        Ccbccaja.Voucher[4] = OpenVentas.Voucher5   /* ó ¿7? */
        Ccbccaja.Voucher[5] = OpenVentas.Voucher8 
        Ccbccaja.Voucher[9] = OpenVentas.Voucher10
        Ccbccaja.CodBco[2]  = OpenVentas.Voucher2
        Ccbccaja.CodBco[3]  = OpenVentas.Voucher4
        Ccbccaja.CodBco[4]  = OpenVentas.Voucher6
        Ccbccaja.Tipo       = s-Tipo
        Ccbccaja.TpoCmb     = FILL-IN_T_Compra      /* OpenVentas.TpoCmb */
        Ccbccaja.VueNac     = OpenVentas.VueNac 
        Ccbccaja.VueUsa     = OpenVentas.VueUsa
        Ccbccaja.FLGEST     = OpenVentas.FlgEst.
    ASSIGN
        Ccbccaja.FlgCie = "C"
        Ccbccaja.FchCie = Ccbccaja.FchDoc.

    RELEASE faccorre.

    /* Captura Nro de Caja */
    para_nrodoccja = Ccbccaja.NroDoc.

END.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


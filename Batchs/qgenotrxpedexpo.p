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

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

DEF VAR s-coddoc AS CHAR INIT 'OTR' NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00021' NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '21' NO-UNDO.
DEF VAR s-nroser AS INT  INIT 000 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT "ADMIN" NO-UNDO.

FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
    FacCorre.CodDoc = S-CODDOC AND
    FacCorre.CodDiv = S-CODDIV AND
    FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
   MESSAGE "Codigo de Documento no configurado" VIEW-AS ALERT-BOX WARNING.
   RETURN ERROR.
END.
s-nroser = Faccorre.nroser.

FIND Almacen WHERE Almacen.codcia = s-codcia
    AND Almacen.codalm = "11e"
    NO-LOCK.

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
         HEIGHT             = 3.92
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.
DISABLE TRIGGERS FOR LOAD OF faccorre.

DEF BUFFER ORDENES FOR Faccpedi.
DEF TEMP-TABLE ITEM LIKE Facdpedi INDEX Llave01 AS PRIMARY CodMat.

DEF VAR F-CANPED AS DEC NO-UNDO.
DEF VAR T-CANPED AS DEC NO-UNDO.
DEF VAR i-NroItm AS INTEGER NO-UNDO.
DEF VAR s-FlgPicking LIKE GN-DIVI.FlgPicking.
DEF VAR s-FlgBarras LIKE GN-DIVI.FlgBarras.
DEF VAR x-NroRef AS CHAR NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    /* Bloqueamos el correlativo para controlar las actualizaciones multiusaurio */
    DEF VAR iLocalCounter AS INTEGER INITIAL 0 NO-UNDO.
    GetLock:
    DO ON STOP UNDO GetLock, RETRY GetLock:
        IF RETRY THEN DO:
            iLocalCounter = iLocalCounter + 1.
            IF iLocalCounter = 5 THEN LEAVE GetLock.
        END.
        FIND FacCorre WHERE FacCorre.CodCia = s-CodCia AND
            FacCorre.CodDoc = s-coddoc AND
            FacCorre.NroSer = s-nroser EXCLUSIVE-LOCK NO-ERROR.
    END.
    IF iLocalCounter = 5 OR NOT AVAILABLE FacCorre THEN UNDO, RETURN.

    /* Barremos los pedidos aprobados y sin O/D */
    EMPTY TEMP-TABLE ITEM.
    x-NroRef = ''.
    RLOOP:
    FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddiv = "00015"       /* Expolibreria */
            AND faccpedi.coddoc = "PED"
            AND faccpedi.codalm = s-codalm      /* Almacèn 21 Lurin */
            AND faccpedi.flgest = "C"           /* Aprobados y con O/D */
            AND CAN-FIND(FIRST ORDENES WHERE ORDENES.codcia = Faccpedi.codcia
                         AND ORDENES.coddiv = Faccpedi.coddiv
                         AND ORDENES.coddoc = "O/D"
                         AND ORDENES.codref = Faccpedi.coddoc
                         AND ORDENES.nroref = Faccpedi.nroped
                         AND ORDENES.fchped = TODAY
                         AND ORDENES.flgest = "P"
                         AND ORDENES.flgsit <> "C" NO-LOCK):
        /* PED sin O/D */
        FIND FIRST facdpedi OF faccpedi WHERE facdpedi.canate <> facdpedi.canped NO-LOCK NO-ERROR.
        IF AVAILABLE facdpedi THEN NEXT RLOOP.
        /* O/D sin despacho aparente */
        FOR EACH ORDENES WHERE ORDENES.codcia = Faccpedi.codcia
            AND ORDENES.coddiv = Faccpedi.coddiv
            AND ORDENES.coddoc = "O/D"
            AND ORDENES.codref = Faccpedi.coddoc
            AND ORDENES.nroref = Faccpedi.nroped
            AND ORDENES.fchped = TODAY
            AND ORDENES.flgest = "P"
            AND ORDENES.flgsit <> "C":
            FIND FIRST facdpedi OF ORDENES WHERE facdpedi.canate <> 0 NO-LOCK NO-ERROR.
            IF AVAILABLE facdpedi THEN NEXT RLOOP.
            ASSIGN
                ORDENES.FlgEst = "O".   /* OTR EN PROCESO */ 
                /*ORDENES.FlgSit = "O".   */
        END.
        /* Cambiamos estados */
        ASSIGN
            Faccpedi.flgest = "O".  /* OTR EN PROCESO */
        /* Control de PED agrupados en la OTR */
        IF LOOKUP(Faccpedi.NroPed, x-NroRef) = 0 
            THEN x-NroRef = x-NroRef + (IF TRUE <> (x-NroRef > "") THEN "" ELSE ",") + Faccpedi.nroped.
        /* Acumulamos productos */
        FOR EACH Facdpedi OF Faccpedi NO-LOCK:
            FIND ITEM WHERE ITEM.codmat = Facdpedi.codmat NO-ERROR.
            IF NOT AVAILABLE ITEM THEN CREATE ITEM.
            ASSIGN
                ITEM.codcia = s-codcia
                ITEM.codmat = Facdpedi.codmat
                ITEM.canped = ITEM.canped + Facdpedi.canped
                ITEM.factor = Facdpedi.factor
                ITEM.undvta = Facdpedi.undvta.
        END.
    END.
    FIND FIRST ITEM NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ITEM THEN LEAVE.   /* NO hay nada */
    /* Revisamos empaques */
    i-NroItm = 0.
    FOR EACH ITEM:
        FIND FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia
            AND Almmmatg.codmat = ITEM.codmat
            NO-LOCK.
        t-CanPed = ITEM.CanPed.
        f-CanPed = ITEM.CanPed.
        /* Si hay empaque redondeamos a la unidad superior */
        IF Almmmatg.DEC__03 > 0 THEN DO:
            f-CanPed = ( TRUNCATE((f-CanPed / Almmmatg.DEC__03),0) ) * Almmmatg.DEC__03.
            IF f-CanPed < t-CanPed THEN f-CanPed = f-CanPed + Almmmatg.DEC__03.
        END.
        i-NroItm = i-NroItm + 1.
        ASSIGN 
            ITEM.CodCia = s-codcia
            ITEM.CodDiv = s-coddiv
            ITEM.CodDoc = s-coddoc
            ITEM.NroPed = ''
            ITEM.ALMDES = s-codalm  /* *** OJO *** */
            ITEM.NroItm = i-NroItm
            ITEM.CanPed = f-CanPed    /* << OJO << */
            ITEM.CanAte = 0
            ITEM.Libre_d01 = t-CanPed
            ITEM.Libre_d02 = f-CanPed
            ITEM.Libre_c01 = '*'.
    END.
    /* Generamos OTR */
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    CREATE Faccpedi.
    ASSIGN 
        Faccpedi.CodCia = S-CODCIA
        Faccpedi.CodDoc = s-coddoc 
        Faccpedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        Faccpedi.CodRef = "PED"
        Faccpedi.NroRef = x-NroRef
        Faccpedi.FchPed = TODAY 
        Faccpedi.FchVen = TODAY + 7
        Faccpedi.CodDiv = S-CODDIV
        FacCPedi.DivDes = s-CodDiv
        Faccpedi.codalm = s-codalm  /* Almacèn salida */
        Faccpedi.codcli = "11e"     /* Almacen destino */
        Faccpedi.nomcli = Almacen.descripcion
        Faccpedi.dircli = Almacen.diralm
        Faccpedi.FlgEst = "P"       /* APROBADO */
        FacCPedi.TpoPed = ""
        FacCPedi.FlgEnv = YES
        FacCPedi.Libre_c01 = s-User-id + '|' + STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM')
        FacCPedi.Fchent = TODAY
        Faccpedi.Hora   = STRING(TIME,"HH:MM")
        Faccpedi.usuario = s-user-id.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    /* CONTROL DE OTROS PROCESOS POR DIVISION */
    FIND gn-divi WHERE gn-divi.codcia = Faccpedi.codcia
        AND gn-divi.coddiv = Faccpedi.divdes
        NO-LOCK.
    ASSIGN
        s-FlgPicking = GN-DIVI.FlgPicking
        s-FlgBarras  = GN-DIVI.FlgBarras.
    IF s-FlgPicking = YES THEN FacCPedi.FlgSit = "T".    /* Por Pickear */
    IF s-FlgPicking = NO AND s-FlgBarras = NO THEN FacCPedi.FlgSit = "C".    /* Barras OK */
    IF s-FlgPicking = NO AND s-FlgBarras = YES THEN FacCPedi.FlgSit = "P".   /* Picking OK */
    /* AHORA SÍ GRABAMOS EL ITEMDO */
    i-NroItm = 0.
    FOR EACH ITEM, FIRST Almmmatg OF ITEM NO-LOCK BY ITEM.NroItm: 
        i-NroItm = i-NroItm + 1.
        /*IF i-NroItm > 52 THEN LEAVE.*/
        CREATE Facdpedi.
        BUFFER-COPY ITEM 
            TO Facdpedi
            ASSIGN
            Facdpedi.CodCia = Faccpedi.CodCia
            Facdpedi.CodDiv = Faccpedi.CodDiv
            Facdpedi.coddoc = Faccpedi.coddoc
            Facdpedi.NroPed = Faccpedi.NroPed
            Facdpedi.FchPed = Faccpedi.FchPed
            Facdpedi.Hora   = Faccpedi.Hora 
            Facdpedi.FlgEst = Faccpedi.FlgEst
            FacDPedi.CanPick = FacDPedi.CanPed
            Facdpedi.NroItm = i-NroItm.
        DELETE ITEM.
    END.
END.
IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
IF AVAILABLE(Facdpedi) THEN RELEASE Facdpedi.
IF AVAILABLE(Faccpedi) THEN RELEASE Faccpedi.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



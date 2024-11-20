&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : Actualiza los costos de reposcicion y margenes de utilidad

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
         HEIGHT             = 9.58
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF NEW SHARED VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-NroDoc AS CHAR NO-UNDO.
DEF VAR x-NroRef AS CHAR NO-UNDO.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'OPENORANGE' NO-UNDO.
DEF VAR X-CTOUND AS DEC NO-UNDO.
DEF VAR F-PreVta-A AS DEC NO-UNDO.
DEF VAR F-MrgUti-A AS DEC NO-UNDO.
DEF VAR F-PreVta-B AS DEC NO-UNDO.
DEF VAR F-MrgUti-B AS DEC NO-UNDO.
DEF VAR F-PreVta-C AS DEC NO-UNDO.
DEF VAR F-MrgUti-C AS DEC NO-UNDO.
DEF VAR fMot LIKE Almmmatg.PreOfi.
DEF VAR MrgOfi LIKE Almmmatg.MrgUti-A.
DEF VAR f-Factor AS DEC NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF Almmmatg.
DISABLE TRIGGERS FOR LOAD OF VtaListaMinGn.
DISABLE TRIGGERS FOR LOAD OF VtaListaMay.
DISABLE TRIGGERS FOR LOAD OF VtaTabla.

/* RUTINA MODIFICADA SOLO PARA REPLICA EL CATALOGO DE MATERIALES */
PUT UNFORMATTED 'INICIO ' STRING(TIME, 'HH:MM:SS') SKIP.
FOR EACH OOCostos EXCLUSIVE-LOCK WHERE OOCostos.CodCia = s-codcia 
        AND OOCostos.FlagMigracion = 'N':
    FIND Almmmatg OF OOCostos NO-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    ASSIGN
        OOCostos.FlagMigracion = "S"
        OOCostos.MigFecha = TODAY
        OOCostos.MigHora = STRING(TIME, 'HH:MM:SS')
        OOCostos.MigUsuario = s-user-id.
    /* RHC 21/07/2020 Solo REPLICA si el costo es mayor que cero */
    IF Almmmatg.CtoTot <= 0 OR Almmmatg.CtoLis <= 0 THEN DO:
        CREATE LogTabla.
        ASSIGN
            logtabla.codcia = Almmmatg.CodCia
            logtabla.Dia = TODAY
            logtabla.Evento = "ERROR:COSTOCERO"
            logtabla.Hora = STRING(TIME, 'HH:MM:SS')
            logtabla.NumId = 0
            logtabla.Tabla = 'almmmatg'
            logtabla.Usuario = s-user-id
            logtabla.ValorLlave = STRING(Almmmatg.CodCia, '999') + STRING(Almmmatg.CodMat).
        NEXT.
    END.
    /* RHC 04/03/2019 Log General */
    CREATE LogTransactions.
    ASSIGN
        logtransactions.logdate = NOW
        logtransactions.tablename = "almmmatg"
        logtransactions.event = "WRITE"
        logtransactions.Usuario = s-user-id
        logtransactions.NumId = 0.
    RAW-TRANSFER Almmmatg TO logtransactions.datarecord.

    /* MIGRACION A LAS OTRAS TIENDAS */
    RUN Replica-Almmmatg.

/*     FIND VtaListaMinGn WHERE VtaListaMinGn.CodCia = Almmmatg.CodCia  */
/*         AND VtaListaMinGn.codmat = Almmmatg.CodMat NO-LOCK NO-ERROR. */
/*     IF AVAILABLE VtaListaMinGn THEN DO:                              */
/*         /* RHC 04/03/2019 Log General */                             */
/*         CREATE LogTransactions.                                      */
/*         ASSIGN                                                       */
/*             logtransactions.logdate = NOW                            */
/*             logtransactions.tablename = "vtalistamingn"              */
/*             logtransactions.event = "WRITE"                          */
/*             logtransactions.Usuario = s-user-id                      */
/*             logtransactions.NumId = 0.                               */
/*         RAW-TRANSFER Vtalistamingn TO logtransactions.datarecord.    */
/*         /* MIGRACION A LAS OTRAS TIENDAS */                          */
/*         RUN Replica-Vtalistamingn.                                   */
/*     END.                                                             */
END.
DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
QUIT.

/*
PRINCIPAL:
FOR EACH OOCostos WHERE OOCostos.CodCia = s-codcia AND OOCostos.FlagMigracion = 'N'
    TRANSACTION ON ERROR UNDO, NEXT ON STOP UNDO, NEXT:
    FIND Almmmatg OF OOCostos EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    CASE OOCostos.TipoCosto:
        WHEN 1 THEN DO:     /* NORMAL */
            IF (OOCostos.CtoLis + OOCostos.CtoTot) <= 0 THEN NEXT PRINCIPAL.
            RUN Recalcular NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO PRINCIPAL, NEXT PRINCIPAL.
        END.
        WHEN 2 THEN DO:     /* CONTRATO MARCO */
            IF (OOCostos.CtoLisMarco + OOCostos.CtoTotMarco) <= 0 THEN NEXT PRINCIPAL.
            RUN Recalcular-Marco NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO PRINCIPAL, NEXT PRINCIPAL.
        END.
    END CASE.
    /* LOG DE CONTROL */
    CREATE Logmmatg.
    BUFFER-COPY Almmmatg TO Logmmatg
        ASSIGN
            Logmmatg.LogDate = TODAY
            Logmmatg.LogTime = STRING(TIME, 'HH:MM:SS')
            Logmmatg.LogUser = s-user-id
            Logmmatg.FlagFechaHora = DATETIME(TODAY, MTIME)
            Logmmatg.FlagUsuario = s-user-id
            Logmmatg.flagestado = "C".

    ASSIGN
        OOCostos.FlagMigracion = "S"
        OOCostos.MigFecha = TODAY
        OOCostos.MigHora = STRING(TIME, 'HH:MM:SS')
        OOCostos.MigUsuario = "ADMIN".
END.
PUT UNFORMATTED 'PROCESO TERMINADO ' STRING(TIME, 'HH:MM:SS') SKIP.
QUIT.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Calcula-Margen-Utilidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcula-Margen-Utilidad Procedure 
PROCEDURE Calcula-Margen-Utilidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* RHC 08/09/2015 SOLO PARA LINEAS COMERCIALES */
FIND FIRST Almtfami OF Almmmatg NO-LOCK NO-ERROR.
IF NOT (AVAILABLE Almtfami AND Almtfami.SwComercial = YES) THEN RETURN.
    ASSIGN
        F-MrgUti-A = 0
        F-MrgUti-B = 0
        F-MrgUti-C = 0
        MrgOfi = 0.
    /****   MARGEN A   ****/
    ASSIGN
        F-PreVta-A = Almmmatg.Prevta[2]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1
        F-MrgUti-A = 0.    
    IF Almmmatg.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndA
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
            DISPLAY 'ERROR unidad A' Almmmatg.codmat Almmmatg.UndA WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            RETURN ERROR.
        END.
        F-FACTOR = Almtconv.Equival.
        F-MrgUti-A = ROUND(((((F-PreVta-A / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.
    ASSIGN
        F-PreVta-B = Almmmatg.Prevta[3]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1
        F-MrgUti-B = 0.   
    /****   MARGEN B   ****/
    IF Almmmatg.UndB <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndB
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
            DISPLAY 'ERROR unidad B' Almmmatg.codmat Almmmatg.UndB WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            RETURN ERROR.
        END.
        F-FACTOR = Almtconv.Equival.
        F-MrgUti-B = ROUND(((((F-PreVta-B / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.
    /****   MARGEN C   ****/
    ASSIGN
        F-PreVta-C = Almmmatg.Prevta[4]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1
        F-MrgUti-C = 0.
    IF Almmmatg.UndC <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndC
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
            DISPLAY 'ERROR unidad C' Almmmatg.codmat Almmmatg.UndC WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            RETURN ERROR.
        END.
        F-FACTOR = Almtconv.Equival.
        F-MrgUti-C = ROUND(((((F-PreVta-C / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.
    /**** MARGEN PRECIO DE OFICINA ****/
    /*MESSAGE 'margenes' f-mrguti-a f-mrguti-b f-mrguti-c.*/
    ASSIGN
        fMot   = 0
        MrgOfi = 0
        F-FACTOR = 1
        X-CTOUND = Almmmatg.CtoTot.
    CASE Almmmatg.Chr__02 :
        WHEN "T" THEN DO:        
            IF Almmmatg.Chr__01 <> "" THEN DO:
                FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                    AND  Almtconv.Codalter = Almmmatg.Chr__01
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Almtconv THEN DO:
                    DISPLAY 'ERROR unidad Oficina' Almmmatg.codmat Almmmatg.CHR__01 WITH STREAM-IO NO-BOX NO-LABELS.
                    PAUSE 0.
                    RETURN ERROR.
                END.
                F-FACTOR = Almtconv.Equival.
                fMot = Almmmatg.PreOfi / X-CTOUND / F-FACTOR.
                MrgOfi = ROUND((fMot - 1) * 100, 6).
            END.
        END.
        WHEN "P" THEN DO:
            MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
        END. 
    END.    
    ASSIGN
        Almmmatg.MrgUti-A  = F-MrgUti-A
        Almmmatg.MrgUti-B  = F-MrgUti-B
        Almmmatg.MrgUti-C  = F-MrgUti-C
        Almmmatg.MrgUti = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 

    /* MARGEN LISTA UTILEX */
    FIND VtaListaMinGn OF Almmmatg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR = YES AND LOCKED(VtaListaMinGn) THEN RETURN ERROR.
    IF AVAILABLE VtaListaMinGn THEN DO:
        ASSIGN
            X-CTOUND = Almmmatg.CtoTot
            F-FACTOR = 1.
        /****   Busca el Factor de conversion   ****/
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.CHR__01
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
            DISPLAY 'ERROR unidad Utilex' Almmmatg.codmat Almmmatg.CHR__01 WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            RETURN ERROR.
        END.
        F-FACTOR = Almtconv.Equival.
        VtaListaMinGn.Dec__01 = ROUND(((((VtaListaMinGn.PreOfi / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.

    /* MARGEN LISTA MAYORISTA POR DIVISION */
    FOR EACH VtaListaMay WHERE VtaListaMay.codcia = Almmmatg.codcia
        AND VtaListaMay.codmat = Almmmatg.codmat:
        ASSIGN
            X-CTOUND = Almmmatg.CtoTot
            F-FACTOR = 1.
        /****   Busca el Factor de conversion   ****/
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND Almtconv.Codalter = VtaListaMay.CHR__01
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
            DISPLAY 'ERROR unidad x División' Almmmatg.codmat VtaListaMay.CHR__01 WITH STREAM-IO NO-BOX NO-LABELS.
            PAUSE 0.
            RETURN ERROR.
        END.
        F-FACTOR = Almtconv.Equival.
        ASSIGN
            VtaListaMay.DEC__01 = ROUND(((((VtaListaMay.PreOfi / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Oficina) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Oficina Procedure 
PROCEDURE Precio-Oficina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR MaxCat AS DEC NO-UNDO.
DEF VAR MaxVta AS DEC NO-UNDO.
DEF VAR MrgMin AS DEC NO-UNDO.
DEF VAR Pre-Ofi AS DEC NO-UNDO.

MaxCat = 0.
MaxVta = 0.
fmot   = 0.
MrgMin = 5000.
MrgOfi = 0.
F-FACTOR = 1.
MaxCat = 4.
MaxVta = 3.

ASSIGN X-CTOUND = Almmmatg.CtoTot.

/****   Busca el Factor de conversion   ****/
IF Almmmatg.Chr__01 = "" THEN RETURN.
FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
    AND  Almtconv.Codalter = Almmmatg.Chr__01
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN RETURN ERROR.
F-FACTOR = Almtconv.Equival.
/*******************************************/
CASE Almmmatg.Chr__02 :
    WHEN "T" THEN DO:        
        /*  TERCEROS  */
        IF F-MrgUti-A < MrgMin AND F-MrgUti-A <> 0 THEN MrgMin = F-MrgUti-A.
        IF F-MrgUti-B < MrgMin AND F-MrgUti-B <> 0 THEN MrgMin = F-MrgUti-B.
        IF F-MrgUti-C < MrgMin AND F-MrgUti-C <> 0 THEN MrgMin = F-MrgUti-C.
        fmot = (1 + MrgMin / 100) / ((1 - MaxCat / 100) * (1 - MaxVta / 100)).
        pre-ofi = X-CTOUND * fmot * F-FACTOR .        
        MrgOfi = ROUND((fmot - 1) * 100, 6).
    END.
    WHEN "P" THEN DO:
        /* PROPIOS */
       pre-ofi = Almmmatg.Prevta[1] * F-FACTOR.
       MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
    END. 
END.    
ASSIGN
    Almmmatg.DEC__01 = MrgOfi
    Almmmatg.PreOfi = pre-ofi.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precios-Mayorista-Division) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precios-Mayorista-Division Procedure 
PROCEDURE Precios-Mayorista-Division :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-CtoTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.

FOR EACH VtaListaMay WHERE VtaListaMay.codcia = Almmmatg.codcia
    AND VtaListaMay.codmat = Almmmatg.codmat:
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = VtaListaMay.Chr__01
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN RETURN ERROR.
    F-FACTOR = Almtconv.Equival.
    ASSIGN
        VtaListaMay.PreOfi = ROUND(( Almmmatg.ctotot * f-Factor ) * (1 + VtaListaMay.Dec__01 / 100), 6).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precios-Mayorista-General) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precios-Mayorista-General Procedure 
PROCEDURE Precios-Mayorista-General :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN
        F-FACTOR = 1
        F-PreVta-A = 0
        F-PreVta-B = 0
        F-PreVta-C = 0
        X-CTOUND = Almmmatg.CtoTot.
    /****   Busca el Factor de conversion   ****/
    IF Almmmatg.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndA
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-PreVta-A = ROUND(( X-CTOUND * (1 + F-MrgUti-A / 100) ), 6) * F-FACTOR.
        END.
    END.
    IF Almmmatg.UndB <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndB
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-PreVta-B = ROUND(( X-CTOUND * (1 + F-MrgUti-B / 100) ), 6) * F-FACTOR.
        END.
    END.
    IF Almmmatg.UndC <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndC
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-PreVta-C = ROUND(( X-CTOUND * (1 + F-MrgUti-C / 100) ), 6) * F-FACTOR.
        END.
    END.
    ASSIGN
        Almmmatg.PreVta[1] = ROUND(( X-CTOUND * (1 + Almmmatg.MrgUti / 100) ), 6)
        Almmmatg.PreVta[2] = F-PreVta-A
        Almmmatg.PreVta[3] = F-PreVta-B
        Almmmatg.PreVta[4] = F-PreVta-C
        Almmmatg.INFOR[1]  = OOCostos.NroLista.
    RUN Precio-Oficina NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    /* Descuentos Promocionales */
    DEF BUFFER BTABLA FOR VtaTabla.
    DEF VAR F-PRECIO AS DEC NO-UNDO.

    F-PRECIO = Almmmatg.Prevta[1].
    IF Almmmatg.MonVta = 2 THEN F-PRECIO = Almmmatg.Prevta[1] * Almmmatg.TpoCmb.
    FOR EACH VtaTabla WHERE VtaTabla.CodCia = Almmmatg.CodCia
        AND VtaTabla.Llave_c1 = Almmmatg.codmat
        AND VtaTabla.Tabla = "DTOPROLIMA":
        f-Factor = 1.   /* x defecto */
        FIND BTABLA WHERE BTABLA.codcia = s-codcia
            AND BTABLA.llave_c1 = VtaTabla.Llave_c2
            AND BTABLA.tabla = "DIVFACXLIN"
            AND BTABLA.llave_c2 = Almmmatg.codfam
            NO-LOCK NO-ERROR.
        IF AVAILABLE BTABLA AND BTABLA.valor[1] > 0 THEN F-FACTOR = ( 1 + BTABLA.valor[1] / 100 ).
        ASSIGN
            VtaTabla.Valor[2] = ROUND( (F-PRECIO * F-FACTOR) * ( 1 - ( VtaTabla.Valor[1] / 100 ) ),4).
    END.
    /**/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precios-Utilex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precios-Utilex Procedure 
PROCEDURE Precios-Utilex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR x-CtoTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.

IF NOT AVAILABLE VtaListaMinGn THEN RETURN.

FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
    AND Almtconv.Codalter = VtaListaMinGn.Chr__01
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN RETURN ERROR.
F-FACTOR = Almtconv.Equival.
ASSIGN
    VtaListaMinGn.PreOfi = ROUND(( Almmmatg.ctotot * f-Factor ) * (1 + VtaListaMinGn.Dec__01 / 100), 6)
    Almmmatg.MrgAlt[1] = VtaListaMinGn.Dec__01.
    
/* MIGRACION A LAS OTRAS TIENDAS */
RUN Replica-Vtalistamingn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Recalcular) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recalcular Procedure 
PROCEDURE Recalcular :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       SE mantiene el margen de utilidad => Los precios se ajustan
------------------------------------------------------------------------------*/

    DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
        /* **************************************************************************** */
        /*************************** MARGEN PRECIO DE LISTA *****************************/
        /* **************************************************************************** */
        RUN Calcula-Margen-Utilidad NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.
        /* **************************************************************************** */
        /******************  RECALCULAMOS PRECIOS DE VENTA  *****************************/
        /* **************************************************************************** */
        /* Ahora sí actualizamos los costos de reposición */
        ASSIGN
            Almmmatg.CtoLis = OOCostos.CtoLis 
            Almmmatg.CtoTot = OOCostos.CtoTot
            X-CTOUND        = OOCostos.CtoTot.
        /* LISTA DE PRECIO MAYORISTA GENERAL (LIMA) */
        RUN Precios-Mayorista-General NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.
        /* LISTA DE PRECIOS UTILEX */
        RUN Precios-Utilex.
        IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.
        /* LISTA DE PRECIOS MAYORISTA POR DIVISION */
        /*RUN Precios-Mayorista-Division.*/
        /*IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR.*/
        /* MIGRACION A LAS OTRAS TIENDAS */
        RUN Replica-Almmmatg.
    END.

END PROCEDURE.

/*
DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:

    /**** MARGEN PRECIO DE LISTA ****/
    ASSIGN Almmmatg.MrgUti = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
    /****   MARGEN A   ****/
    ASSIGN
        F-PreVta-A = Almmmatg.Prevta[2]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1
        F-MrgUti-A = 0.    
    IF Almmmatg.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndA
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-MrgUti-A = ROUND(((((F-PreVta-A / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
        END.
    END.
    ASSIGN
        F-PreVta-B = Almmmatg.Prevta[3]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1
        F-MrgUti-B = 0.   
    /****   MARGEN B   ****/
    IF Almmmatg.UndB <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndB
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-MrgUti-B = ROUND(((((F-PreVta-B / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
        END.
    END.
    /****   MARGEN C   ****/
    ASSIGN
        F-PreVta-C = Almmmatg.Prevta[4]
        X-CTOUND = Almmmatg.CtoTot
        F-FACTOR = 1
        F-MrgUti-C = 0.
    IF Almmmatg.UndC <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
            AND  Almtconv.Codalter = Almmmatg.UndC
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival.
            F-MrgUti-C = ROUND(((((F-PreVta-C / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
        END.
    END.
    ASSIGN
        Almmmatg.MrgUti-A = f-MrgUti-A
        Almmmatg.MrgUti-B = f-MrgUti-B
        Almmmatg.MrgUti-C = f-MrgUti-C.

    /**** MARGEN PRECIO DE OFICINA ****/
    ASSIGN
        fMot   = 0
        MrgOfi = 0
        F-FACTOR = 1
        X-CTOUND = Almmmatg.CtoTot.
    CASE Almmmatg.Chr__02 :
        WHEN "T" THEN DO:        
            IF Almmmatg.Chr__01 <> "" THEN DO:
                FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                    AND  Almtconv.Codalter = Almmmatg.Chr__01
                    NO-LOCK NO-ERROR.
                IF AVAILABLE Almtconv THEN DO:
                    F-FACTOR = Almtconv.Equival.
                END.
            END.
            fMot = Almmmatg.PreOfi / X-CTOUND / F-FACTOR.
            MrgOfi = ROUND((fMot - 1) * 100, 6).
        END.
        WHEN "P" THEN DO:
            MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
        END. 
    END.    
    ASSIGN
        Almmmatg.DEC__01 = MrgOfi.

    /* LISTA MINORISTA POR DIVISION */
    DEF VAR x-CtoTot AS DEC NO-UNDO.

    /* RHC 26.05.2011 Margen Lista Minorista */
    FOR EACH vtalistamin WHERE vtalistamin.codcia = Almmmatg.CodCia AND vtalistamin.codmat = Almmmatg.codmat:
        IF Almmmatg.monvta = vtalistamin.monvta THEN x-CtoTot = Almmmatg.CtoTot.
        ELSE IF vtalistamin.monvta = 1 THEN x-CtoTot = Almmmatg.ctotot *  Almmmatg.tpocmb.
        ELSE x-CtoTot = Almmmatg.ctotot /  Almmmatg.tpocmb.
        f-Factor = 1.
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = vtalistamin.Chr__01
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.  
        ASSIGN
            vtalistamin.Dec__01 = ( (vtalistamin.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100. 
    END.
    /* LISTA MINORISTA GENERAL (UTILEX) */
    FOR EACH vtalistaminGn WHERE vtalistaminGn.codcia = Almmmatg.CodCia
        AND vtalistaminGn.codmat = Almmmatg.codmat:
        x-CtoTot = Almmmatg.ctotot.
        f-Factor = 1.
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = vtalistaminGn.Chr__01
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.  
        ASSIGN
            vtalistaminGn.Dec__01 = ( (vtalistaminGn.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100. 
    END.
    /* LISTA MAYORISTA POR DIVISION */
    /* RHC 06/11/2013 LA MONEDA Y TIPO DE CAMBIO ESTAN EL ALMMMATG */
    FOR EACH VtaListaMay WHERE VtaListaMay.codcia = Almmmatg.codcia
        AND VtaListaMay.codmat = Almmmatg.codmat:
        ASSIGN
            VtaListaMay.MonVta = Almmmatg.MonVta
            VtaListaMay.TpoCmb = Almmmatg.TpoCmb
            x-CtoTot           = Almmmatg.CtoTot
            f-Factor = 1.
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = VtaListaMay.Chr__01
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        ASSIGN
            VtaListaMay.Dec__01 = ( (VtaListaMay.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100. 
    END.
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Recalcular-Marco) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recalcular-Marco Procedure 
PROCEDURE Recalcular-Marco :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* El Contrato Marco SIEMPRE está en SOLES (Almmmatg.DEC__02 = 1) */
ASSIGN
    Almmmatg.CtoLisMarco = OOCostos.CtoLisMarco 
    Almmmatg.CtoTotMarco = OOCostos.CtoTotMarco.
FIND Almmmatp OF Almmmatg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF ERROR-STATUS:ERROR AND LOCKED(Almmmatp) THEN RETURN ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.
ASSIGN
    Almmmatp.CtoTot = OOCostos.CtoTotMarco.

DEF VAR x-CToTot AS DEC NO-UNDO.

IF Almmmatg.monvta = Almmmatp.monvta THEN x-CtoTot = Almmmatp.CtoTot.
ELSE IF Almmmatp.monvta = 1 THEN x-CtoTot = Almmmatp.ctotot *  Almmmatg.tpocmb.
ELSE x-CtoTot = Almmmatp.ctotot /  Almmmatg.tpocmb.

DEF VAR f-Factor AS DEC NO-UNDO.

FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
    AND Almtconv.Codalter = Almmmatp.Chr__01
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN DO:
    DISPLAY 'ERROR unidad Marco' Almmmatg.codmat Almmmatp.CHR__01 WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN ERROR.
END.
F-FACTOR = Almtconv.Equival.
ASSIGN
    Almmmatp.Dec__01 = ( (Almmmatp.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100
    Almmmatp.INFOR[1]  = OOCostos.NroLista.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-Almmmatg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-Almmmatg Procedure 
PROCEDURE Replica-Almmmatg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = almmmatg
    &Key    =  "string(almmmatg.codcia,'999') + string(almmmatg.codmat,'x(6)')"
    &Prg    = r-almmmatg
    &Event  = WRITE
    &FlgDB0 = YES          /* Replicas: Sede Remota -> Sede Principal */
    &FlgDB1 = YES    /* Plaza Lima Norte 00501 */
    &FlgDB2 = YES    /* Surquillo 00023 */
    &FlgDB3 = YES    /* Chorrillos 00027 */
    &FlgDB4 = YES    /* San Borja 00502 */
    &FlgDB5 = YES    /* La Molina 00503 */
    &FlgDB6 = YES    /* Beneficiencia 00504 */
    &FlgDB7 = YES    /* Feria Plaza Norte 00505 */
    &FlgDB8 = YES    /* La Rambla 00507 */
    &FlgDB9 = YES    /* San Isidro 00508 */
    &FlgDB10 = YES   /* Chiclayo 00065 */
    &FlgDB11 = YES   /* Atocongo 00510 */
    &FlgDB12 = YES   /* Angamos 00511 */
    &FlgDB13 = YES   /* Salaverry 00512 */
    &FlgDB14 = YES   /* Centro Civico 00513 */
    &FlgDB15 = YES   /* Primavera 00514 */
    &FlgDB16 = YES   /* Bellavista 00516 */
    &FlgDB17 = TRUE
    &FlgDB18 = YES   /* AREQUIPA */
    &FlgDB19 = YES   /* TRUJILLO */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL */
    &FlgDB30 = NO   /* SERVIDOR ATE */
    }


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Replica-Vtalistamingn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replica-Vtalistamingn Procedure 
PROCEDURE Replica-Vtalistamingn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = vtalistamingn
    &Key    =  "string(vtalistamingn.codcia,'999') + string(vtalistamingn.codmat,'x(6)')"
    &Prg    = r-vtalistamingn
    &Event  = WRITE
    &FlgDB0 = TRUE  /* Replicar de la sede remota a la base principal */
    &FlgDB1 = NO    /* Plaza Lima Norte 00501 */
    &FlgDB2 = NO    /* Surquillo 00023 */
    &FlgDB3 = NO    /* Chorrillos 00027 */
    &FlgDB4 = NO    /* San Borja 00502 */
    &FlgDB5 = NO    /* La Molina 00503 */
    &FlgDB6 = NO    /* Beneficiencia 00504 */
    &FlgDB7 = NO    /* Plaza Norte 00505 */
    &FlgDB8 = NO    /* La Rambla 00507 */
    &FlgDB9 = NO    /* San Isidro 00508 */
    &FlgDB10 = NO   /* Chiclayo 00065 */
    &FlgDB11 = TRUE
    &FlgDB12 = TRUE
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = NO   /* Jesus Obrero 00005 */
    &FlgDB19 = NO   /* CONTINENTAL PERU */
    &FlgDB20 = NO   /* EXPOLIBRERIA */
    &FlgDB30 = NO   /* EXPOLIBRERIA */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


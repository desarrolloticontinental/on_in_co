&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Chequear las ventas vs movimientos de almacén

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
         HEIGHT             = 4.12
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-fecha AS DATE NO-UNDO.
DEF VAR x-Comprobantes AS CHAR INIT 'FAC,BOL,TCK' NO-UNDO.
DEF VAR x-Error AS CHAR NO-UNDO.

x-Fecha = TODAY - 7.

/* 1ro comprobantes vs almacenes */
FOR EACH gn-divi NO-LOCK WHERE codcia = s-codcia:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND LOOKUP(ccbcdocu.coddoc, x-comprobantes) > 0
        AND ccbcdocu.flgest <> 'A'
        AND LOOKUP(CcbCdocu.TpoFac, 'A,S') = 0
        AND ccbcdocu.fchdoc >= x-fecha
        AND ccbcdocu.fchdoc < TODAY:
        FIND almcmov WHERE almcmov.codcia = s-codcia
            AND almcmov.tipmov = 's'
            AND almcmov.codmov = 02
            AND almcmov.codref = ccbcdocu.coddoc
            AND almcmov.nroref = ccbcdocu.nrodoc
            AND almcmov.flgest <> 'A'
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE almcmov THEN DO:
            IF AMBIGUOUS almcmov THEN x-Error = "COMPROBANTE REPETIDO EN ALMACEN".
            ELSE x-Error = "COMPROBANTE NO REGISTRADO EN ALMACEN".
            DISPLAY
                x-Error '|'
                ccbcdocu.coddiv '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.fchdoc
                WITH STREAM-IO NO-BOX WIDTH 200.
            PAUSE 0.
            /* CORREGIR ERROR */
            RUN Act_Alm.
        END.
        ELSE DO:
            FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
                FIND almdmov OF almcmov WHERE almdmov.codmat = ccbddocu.codmat NO-LOCK NO-ERROR.
                IF NOT AVAILABLE almdmov OR ccbddocu.candes <> almdmov.candes THEN DO:
                    x-Error = "PRODUCTO MAL REGISTRADO EN ALMACEN".
                    DISPLAY
                        x-Error '|'
                        ccbcdocu.coddiv '|'
                        ccbcdocu.coddoc '|'
                        ccbcdocu.nrodoc '|'
                        ccbcdocu.fchdoc '|'
                        ccbddocu.codmat 
                        WITH STREAM-IO NO-BOX WIDTH 200.
                    PAUSE 0.
                    /* CORREGIR ERROR */
                    RUN Act_Alm.
                    LEAVE.
                END.
            END.
        END.
    END.
END.

/* 2do almacenes vs comprobantes */
FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia:
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
        AND almcmov.codalm = almacen.codalm
        AND almcmov.tipmov = 's'
        AND almcmov.codmov = 02
        AND almcmov.flgest <> 'A'
        AND LOOKUP(almcmov.codref, x-comprobantes) > 0
        AND almcmov.fchdoc >= x-fecha
        AND almcmov.fchdoc < TODAY - 1:
        FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
            AND ccbcdocu.coddoc = almcmov.codref
            AND ccbcdocu.nrodoc = almcmov.nroref
            AND ccbcdocu.flgest <> 'A'
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu THEN DO:
            IF AMBIGUOUS ccbcdocu THEN x-Error = "COMPROBANTE REPETIDO EN VENTAS".
            ELSE x-Error = "COMBPROBANTE NO REGISTRADO EN VENTAS".
            DISPLAY
                x-Error '|'
                almcmov.codalm '|'
                almcmov.nroser '|'
                almcmov.nrodoc '|'
                almcmov.codref '|'
                almcmov.nroref '|'
                almcmov.fchdoc
                WITH STREAM-IO NO-BOX WIDTH 200.
            PAUSE 0.
        END.
        ELSE DO:
            FOR EACH almdmov OF almcmov NO-LOCK:
                FIND ccbddocu OF ccbcdocu WHERE ccbddocu.codmat = almdmov.codmat NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ccbddocu OR ccbddocu.candes <> almdmov.candes THEN DO:
                    x-Error = "PRODUCTO MAL REGISTRADO EN COMPROBANTES".
                    DISPLAY
                        x-Error '|'
                        ccbcdocu.coddiv '|'
                        ccbcdocu.coddoc '|'
                        ccbcdocu.nrodoc '|'
                        ccbcdocu.fchdoc '|'
                        ccbddocu.codmat 
                        WITH STREAM-IO NO-BOX WIDTH 200.
                    PAUSE 0.
                END.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Act_Alm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Act_Alm Procedure 
PROCEDURE Act_Alm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RETURN ''.

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.

/* 1ro borramos cualquier indicio */
FOR EACH almcmov WHERE almcmov.codcia = s-codcia
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 02
    AND almcmov.codref = ccbcdocu.coddoc
    AND almcmov.nroref = ccbcdocu.nrodoc
    AND almcmov.flgest <> 'A':
    FOR EACH almdmov OF almcmov:
        DELETE almdmov.
    END.
    DELETE almcmov.
END.

DEF VAR i AS INTEGER INITIAL 1 NO-UNDO.
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
                        MESSAGE 'Almacén' Ccbddocu.almdes 'NO registrado'
                            VIEW-AS ALERT-BOX ERROR.
                        UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
                    END.
                END.
                LEAVE.
            END.
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
                   Almcmov.usuario = "SISTEMAS".
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


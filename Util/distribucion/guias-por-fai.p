DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00021'.

DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR COMBO-NroSer-Guia AS CHAR INIT '228' NO-UNDO.

DEF BUFFER Reporte FOR Ccbcdocu.
DEF BUFFER B-CDOCU FOR Ccbcdocu.
DEF BUFFER B-DDOCU FOR Ccbddocu.

DEF VAR x-FormatoGUIA AS CHAR INIT '999-999999' NO-UNDO.

RUN sunat\p-formato-doc (INPUT "G/R", OUTPUT x-FormatoGUIA).

DEF NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN' NO-UNDO.

DEFINE TEMP-TABLE T-CcbADocu NO-UNDO LIKE CcbADocu.
DEFINE BUFFER B-ADocu FOR CcbADocu.

    DEFINE VARIABLE iCountItem AS INTEGER INITIAL 1 NO-UNDO.
    DEFINE VARIABLE lCreaHeader AS LOGICAL NO-UNDO.

    FIND FacCfgGn WHERE FacCfgGn.CodCia = s-CodCia NO-LOCK NO-ERROR.

    ASSIGN
        lCreaHeader = TRUE.
    pMensaje = "".
    trloop:
    DO TRANSACTION ON ERROR UNDO trloop, RETURN 'ADM-ERROR' ON STOP UNDO trloop, RETURN 'ADM-ERROR':
        /* Correlativo */
        {lib\lock-genericov21.i &Tabla="FacCorre" ~
            &Condicion="FacCorre.CodCia = s-CodCia ~
            AND FacCorre.CodDoc = 'G/R' ~
            AND FacCorre.CodDiv = s-CodDiv ~
            AND FacCorre.NroSer = INTEGER(COMBO-NroSer-Guia)" ~
            &Bloqueo= "EXCLUSIVE-LOCK" ~
            &Accion="RETRY" ~
            &Mensaje="NO" ~
            &TipoError="DO: ~
            pMensaje = 'Error en el control de correlativo G/R ' + s-CodDiv + ' ' + COMBO-NroSer-Guia. ~
            UNDO trloop, RETURN 'ADM-ERROR'. ~
            END"}

        FOR EACH Reporte NO-LOCK WHERE Reporte.codcia = s-codcia
            AND Reporte.coddiv = s-coddiv
            AND Reporte.coddoc = 'FAI'
            AND Reporte.nrodoc >= '021000884' AND Reporte.nrodoc <= '021001066',
            FIRST B-CDOCU OF Reporte NO-LOCK, 
            EACH B-DDOCU OF B-CDOCU NO-LOCK
            BREAK BY Reporte.CodCia BY Reporte.NroDoc BY B-DDOCU.NroItm:
            /* Cabecera */
            IF lCreaHeader THEN DO:
                CREATE CcbCDocu.
                BUFFER-COPY B-CDOCU
                    TO CcbCDocu
                    ASSIGN
                    CcbCDocu.CodDiv = s-CodDiv
                    CcbCDocu.CodDoc = "G/R"
                    CcbCDocu.NroDoc =  STRING(FacCorre.NroSer,ENTRY(1,x-FormatoGUIA,'-')) + 
                                        STRING(FacCorre.Correlativo,ENTRY(2,x-FormatoGUIA,'-')) 
                    CcbCDocu.FchDoc = TODAY
                    CcbCDocu.CodRef = B-CDOCU.CodDoc
                    CcbCDocu.NroRef = B-CDOCU.NroDoc
                    CcbCDocu.FlgEst = "F"   /* FACTURADO */
                    CcbCDocu.usuario = S-USER-ID
                    CcbCDocu.TpoFac = "A"     /* AUTOMATICA (No descarga stock) */
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    pMensaje = "Mal configurado el correlativo de la G/R o duplicado".
                    UNDO trloop, RETURN 'ADM-ERROR'.
                END.
                ASSIGN
                    FacCorre.Correlativo = FacCorre.Correlativo + 1.
                DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc.
                PAUSE 0.
                /* RHC 22/07/2015 COPIAMOS DATOS DEL TRANSPORTISTA */
                FIND FIRST T-CcbADocu NO-LOCK NO-ERROR.
                IF AVAILABLE T-CcbADocu THEN DO:
                    FIND FIRST B-ADOCU WHERE B-ADOCU.codcia = Ccbcdocu.codcia
                        AND B-ADOCU.coddiv = Ccbcdocu.coddiv
                        AND B-ADOCU.coddoc = Ccbcdocu.coddoc
                        AND B-ADOCU.nrodoc = Ccbcdocu.nrodoc
                        NO-ERROR.
                    IF NOT AVAILABLE B-ADOCU THEN CREATE B-ADOCU.
                    BUFFER-COPY T-CcbADocu 
                        TO B-ADOCU
                        ASSIGN
                            B-ADOCU.CodCia = Ccbcdocu.CodCia
                            B-ADOCU.CodDiv = Ccbcdocu.CodDiv
                            B-ADOCU.CodDoc = Ccbcdocu.CodDoc
                            B-ADOCU.NroDoc = Ccbcdocu.NroDoc.
                END.
                ASSIGN
                    lCreaHeader = FALSE.
            END.
            /* Detalle */
            CREATE Ccbddocu.
            BUFFER-COPY B-DDOCU 
                TO Ccbddocu
                ASSIGN
                    CcbDDocu.NroItm = iCountItem
                    Ccbddocu.coddiv = Ccbcdocu.coddiv
                    Ccbddocu.coddoc = Ccbcdocu.coddoc
                    Ccbddocu.nrodoc = Ccbcdocu.nrodoc.                        
            iCountItem = iCountItem + 1.
            IF iCountItem > FacCfgGn.Items_Guias OR LAST-OF(Reporte.CodCia) OR LAST-OF(Reporte.NroDoc)
                THEN DO:
                RUN proc_GrabaTotalesGR.
                iCountItem = 1.
                lCreaHeader = TRUE.
            END.
        END.
    END.
    RETURN "OK".


PROCEDURE proc_GrabaTotalesGR:

{vta2/graba-totales-factura-cred.i}


END PROCEDURE.

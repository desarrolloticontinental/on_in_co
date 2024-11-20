/* RHC 17/01/2017
   Fraccionar la cotización en dos:
   Solo línea 010
   Las demás lineas 
   */
   
DEF TEMP-TABLE t-cpedi LIKE faccpedi.
DEF TEMP-TABLE t-dpedi LIKE facdpedi.

DEF BUFFER B-CPEDI FOR faccpedi.
DEF BUFFER B-DPEDI FOR facdpedi.
DEF BUFFER B-ADOCU FOR Ccbadocu.

DEF VAR x-linea AS CHAR NO-UNDO.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.

INPUT FROM d:\tmp\expolibreria.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE t-cpedi.
    ASSIGN
        t-cpedi.codcia = s-codcia
        t-cpedi.coddoc = s-coddoc
        t-cpedi.coddiv = s-coddiv
        t-cpedi.nroped = x-linea.
END.
INPUT CLOSE.

FOR EACH t-cpedi:
    FIND B-CPEDI OF t-cpedi NO-LOCK NO-ERROR.
    IF AVAILABLE B-CPEDI THEN
        BUFFER-COPY B-CPEDI 
        EXCEPT B-CPEDI.codcia B-CPEDI.coddiv B-CPEDI.coddoc B-CPEDI.nroped
        TO t-cpedi.
    ELSE DELETE t-cpedi.
END.

DEF VAR k AS INT NO-UNDO.
LooGrabarData:
FOR EACH t-cpedi NO-LOCK:
    FIND B-CPEDI OF t-cpedi EXCLUSIVE-LOCK NO-ERROR.
    IF NOT (B-CPEDI.flgest = 'P' AND 
            NOT CAN-FIND(FIRST B-DPEDI OF B-CPEDI WHERE B-DPEDI.canate <> 0 NO-LOCK))
        THEN DO:
        MESSAGE t-cpedi.coddiv t-cpedi.coddoc t-cpedi.nroped t-cpedi.fchped t-cpedi.codcli t-cpedi.nomcli t-cpedi.flgest
            VIEW-AS ALERT-BOX.
        NEXT.
    END.
    /* ****************************************************************************** */
    /* Control de correlativo */
    /* ****************************************************************************** */
    ASSIGN
        s-nroser = INTEGER(SUBSTRING(B-CPEDI.nroped,1,3)).
    /* Trata de bloquear hasta 5 veces */
    DEF VAR LocalCounter AS INTEGER INITIAL 0 NO-UNDO.
    GetLock:
    DO ON STOP UNDO GetLock, RETRY GetLock:
        IF RETRY THEN DO:
            LocalCounter = LocalCounter + 1.
            IF LocalCounter = 5 THEN LEAVE GetLock.
        END.
        FIND Faccorre WHERE FacCorre.CodCia = s-CodCia 
            AND FacCorre.CodDoc = s-CodDoc
            AND FacCorre.NroSer = s-NroSer
            EXCLUSIVE-LOCK NO-ERROR.
    END.
    IF LocalCounter = 5 OR NOT AVAILABLE FacCorre 
        THEN UNDO LooGrabarData, RETURN "ADM-ERROR".
    IF FacCorre.FlgCic = NO THEN DO:
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            MESSAGE 'Se ha llegado al límite del correlativo:' FacCorre.NroFin SKIP
                'No se puede generar el documento' s-coddoc 'serie' s-nroser
                VIEW-AS ALERT-BOX ERROR.
            UNDO LooGrabarData, RETURN "ADM-ERROR".
        END.
    END.
    IF FacCorre.FlgCic = YES THEN DO:
        /* REGRESAMOS AL NUMERO 1 */
        IF FacCorre.NroFin > 0 AND FacCorre.Correlativo > FacCorre.NroFin THEN DO:
            IF FacCorre.NroIni > 0 THEN FacCorre.Correlativo = FacCorre.NroIni.
            ELSE FacCorre.Correlativo = 1.
        END.
    END.
    /* ****************************************************************************** */
    /* ****************************************************************************** */
    DO k = 1 TO 2:
        EMPTY TEMP-TABLE t-dpedi.
        CASE k:
            WHEN 1 THEN DO:     /* Solo linea 010 */
                FOR EACH B-DPEDI OF B-CPEDI NO-LOCK,
                    FIRST Almmmatg OF B-DPEDI NO-LOCK WHERE Almmmatg.codfam = '010':
                    CREATE t-dpedi.
                    BUFFER-COPY B-DPEDI TO t-dpedi.
                END.
            END.
            WHEN 2 THEN DO:     /* Las otras lineas */
                FOR EACH B-DPEDI OF B-CPEDI NO-LOCK,
                    FIRST Almmmatg OF B-DPEDI NO-LOCK WHERE Almmmatg.codfam <> '010':
                    CREATE t-dpedi.
                    BUFFER-COPY B-DPEDI TO t-dpedi.
                END.
            END.
        END CASE.
        FIND FIRST t-dpedi NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-dpedi THEN NEXT.
        CREATE FacCPedi.
        BUFFER-COPY B-CPEDI 
            TO FacCPedi
            ASSIGN 
                FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
                FacCPedi.CodRef = B-CPEDI.CodDoc
                facCPedi.NroRef = B-CPEDI.NroPed
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Correlativo de la Orden mal registrado o duplicado" VIEW-AS ALERT-BOX ERROR.
            UNDO LooGrabarData, LEAVE.
        END.
        /* TRACKING */
        RUN vtagn/pTracking-04 (Faccpedi.CodCia,
                                  Faccpedi.CodDiv,
                                  Faccpedi.CodRef,
                                  Faccpedi.NroRef,
                                  'ADMIN',
                                  'GOD',
                                  'P',
                                  DATETIME(TODAY, MTIME),
                                  DATETIME(TODAY, MTIME),
                                  Faccpedi.CodDoc,
                                  Faccpedi.NroPed,
                                  Faccpedi.CodRef,
                                  Faccpedi.NroRef)
            NO-ERROR.
        /* COPIAMOS DATOS DEL TRANSPORTISTA */
        FIND FIRST Ccbadocu WHERE Ccbadocu.codcia = B-CPEDI.codcia
            AND Ccbadocu.coddiv = B-CPEDI.coddiv
            AND Ccbadocu.coddoc = B-CPEDI.coddoc
            AND Ccbadocu.nrodoc = B-CPEDI.nroped
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbadocu THEN DO:
            FIND FIRST B-ADOCU WHERE B-ADOCU.codcia = Faccpedi.codcia
                AND B-ADOCU.coddiv = Faccpedi.coddiv
                AND B-ADOCU.coddoc = Faccpedi.coddoc
                AND B-ADOCU.nrodoc = Faccpedi.nroped
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE B-ADOCU THEN CREATE B-ADOCU.
            BUFFER-COPY Ccbadocu TO B-ADOCU
                ASSIGN
                    B-ADOCU.CodDiv = FacCPedi.CodDiv
                    B-ADOCU.CodDoc = FacCPedi.CodDoc
                    B-ADOCU.NroDoc = FacCPedi.NroPed.
        END.
        RUN Genera-Pedido.    /* Detalle del pedido */ 
        {vta2/graba-totales-cotizacion-cred.i}

        /* ******************************************************************** */
        ASSIGN 
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
    END.
    ASSIGN
        B-CPEDI.FlgEst = "A".
END.


      PROCEDURE Genera-Pedido:
      /* ******************** */

      FOR EACH t-dpedi NO-LOCK:
          CREATE Facdpedi.
          BUFFER-COPY t-dpedi
              TO Facdpedi
              ASSIGN
           FacDPedi.CodCia  = FacCPedi.CodCia 
           FacDPedi.coddiv  = FacCPedi.coddiv 
           FacDPedi.coddoc  = FacCPedi.coddoc 
           FacDPedi.NroPed  = FacCPedi.NroPed 
           FacDPedi.FchPed  = FacCPedi.FchPed
           FacDPedi.Hora    = FacCPedi.Hora .
END.

END PROCEDURE.

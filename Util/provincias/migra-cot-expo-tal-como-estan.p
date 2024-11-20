DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS' NO-UNDO.

DEF BUFFER PEDIDO FOR faccpedi.
DEF BUFFER B-CPEDI FOR faccpedi.
DEF BUFFER B-DPEDI FOR facdpedi.

/*  Backup de la informacion */
&SCOPED-DEFINE condicion1 codcia = s-codcia ~
    AND coddiv = '00015' ~
    AND coddoc = 'COT' ~
    AND fchped >= 09/01/2014 ~
    AND usuario = 'SAC-15'
    
&SCOPED-DEFINE condicion2 PEDIDO.codcia = s-codcia ~
        AND PEDIDO.coddoc = 'PED' ~
        AND PEDIDO.codref = faccpedi.coddoc ~
        AND PEDIDO.nroref = faccpedi.nroped
    
&SCOPED-DEFINE condicion3 ccbcdocu.codcia = s-codcia ~
            AND ccbcdocu.codped = PEDIDO.coddoc ~
            AND ccbcdocu.nroped = PEDIDO.nroped

/* BACKUP DE LA INFORMACION */
DEF STREAM cotizaciones.
DEF STREAM comprobantes.
OUTPUT STREAM cotizaciones TO c:\tmp\backupconti\faccpedi.d.
OUTPUT STREAM comprobantes TO c:\tmp\backupconti\ccbcdocu.d.

FOR EACH faccpedi NO-LOCK WHERE {&condicion1}:
    EXPORT STREAM cotizaciones faccpedi.
    FOR EACH PEDIDO NO-LOCK WHERE {&condicion2}:
        FOR EACH ccbcdocu NO-LOCK WHERE {&condicion3}:
            EXPORT STREAM comprobantes ccbcdocu.
        END.
    END.
END.
OUTPUT STREAM cotizaciones CLOSE.
OUTPUT STREAM comprobantes CLOSE.

/* MIGRACION DE LAS COTIZACIONES */
FOR EACH faccpedi WHERE {&condicion1}:
    RUN Migrar-a-Provincias.
    FOR EACH PEDIDO NO-LOCK WHERE {&condicion2}:
        FOR EACH ccbcdocu WHERE {&condicion3}:
            ASSIGN
                ccbcdocu.codope = ccbcdocu.divori
                ccbcdocu.divori = '00018'.  /* Cambiamos la division */
        END.
    END.
END.
MESSAGE 'Migración exitosa' VIEW-AS ALERT-BOX INFORMATION.

/* ************************** */
PROCEDURE Migrar-a-Provincias:
/* ************************** */

IF NOT AVAILABLE Faccpedi THEN RETURN.

CREATE B-CPEDI.
BUFFER-COPY FacCPedi TO B-CPEDI
    ASSIGN
        B-CPEDI.CodDiv = '00018'.
FOR EACH FacDPedi OF FacCPedi NO-LOCK:
    CREATE B-DPEDI.
    BUFFER-COPY FacDPedi TO B-DPEDI
        ASSIGN
            B-DPEDI.CodDiv = B-CPEDI.CodDiv.
END.
ASSIGN
    FacCPedi.Libre_C03 = FacCPedi.FlgEst
    FacCPedi.FlgEst = "A"       /* ANULADA */
    FacCPedi.FchAprobacion  = TODAY
    FacCPedi.UsrAprobacion = s-user-id.

END PROCEDURE.

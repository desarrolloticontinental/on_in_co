DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS' NO-UNDO.

DEF BUFFER ORDENES FOR faccpedi.
DEF BUFFER B-CPEDI FOR faccpedi.
DEF BUFFER B-DPEDI FOR facdpedi.

/*  Backup de la informacion */
&SCOPED-DEFINE condicion1 codcia = s-codcia ~
    AND coddiv = '00015' ~
    AND coddoc = 'PED' ~
    AND fchped >= 09/01/2014 ~
    AND usuario = 'SAC-15'
    
&SCOPED-DEFINE condicion2 ORDENES.codcia = s-codcia ~
        AND ORDENES.coddoc = 'O/D' ~
        AND ORDENES.codref = faccpedi.coddoc ~
        AND ORDENES.nroref = faccpedi.nroped
    
/* BACKUP DE LA INFORMACION */
/* DEF STREAM pedidos.                                    */
/* DEF STREAM ordenes.                                    */
/* OUTPUT STREAM pedidos TO c:\tmp\backupconti\pedidos.d. */
/* OUTPUT STREAM ordenes TO c:\tmp\backupconti\ordenes.d. */
/*                                                        */
/* FOR EACH faccpedi NO-LOCK WHERE {&condicion1}:         */
/*     EXPORT STREAM pedidos faccpedi.                    */
/*     FOR EACH ORDENES NO-LOCK WHERE {&condicion2}:      */
/*         EXPORT STREAM ordenes ORDENES.                 */
/*     END.                                               */
/* END.                                                   */
/* OUTPUT STREAM pedidos CLOSE.                           */
/* OUTPUT STREAM ordenes CLOSE.                           */

/* MIGRACION DE LAS COTIZACIONES */
FOR EACH faccpedi WHERE {&condicion1}:
    RUN Migrar-a-Provincias-Pedidos.
    FOR EACH ORDENES WHERE {&condicion2}:
        RUN Migrar-a-Provincias-Ordenes.
    END.
END.
MESSAGE 'Migración exitosa' VIEW-AS ALERT-BOX INFORMATION.

/* ************************** */
PROCEDURE Migrar-a-Provincias-Pedidos:
/* ************************** */

IF NOT AVAILABLE Faccpedi THEN RETURN.

CREATE B-CPEDI.
BUFFER-COPY FacCPedi TO B-CPEDI
    ASSIGN
        B-CPEDI.CodDiv = '00018'
        B-CPEDI.NroPed = '118' + SUBSTRING(Faccpedi.nroped, 4).
FOR EACH FacDPedi OF FacCPedi NO-LOCK:
    CREATE B-DPEDI.
    BUFFER-COPY FacDPedi TO B-DPEDI
        ASSIGN
            B-DPEDI.CodDiv = B-CPEDI.CodDiv
            B-DPEDI.NroPed = B-CPEDI.NroPed.
END.
ASSIGN
    FacCPedi.Libre_C03 = FacCPedi.FlgEst
    FacCPedi.FlgEst = "A"       /* ANULADA */
    FacCPedi.FchAprobacion  = TODAY
    FacCPedi.UsrAprobacion = s-user-id.

END PROCEDURE.

/* ************************** */
PROCEDURE Migrar-a-Provincias-Ordenes:
/* ************************** */

IF NOT AVAILABLE ORDENES THEN RETURN.

CREATE B-CPEDI.
BUFFER-COPY ORDENES TO B-CPEDI
    ASSIGN
        B-CPEDI.CodDiv = '00018'
        B-CPEDI.NroPed = '118' + SUBSTRING(ORDENES.NroPed,4).
FOR EACH FacDPedi OF ORDENES NO-LOCK:
    CREATE B-DPEDI.
    BUFFER-COPY FacDPedi TO B-DPEDI
        ASSIGN
            B-DPEDI.CodDiv = B-CPEDI.CodDiv
            B-DPEDI.NroPed = B-CPEDI.NroPed.
END.
ASSIGN
    ORDENES.Libre_C03 = ORDENES.FlgEst
    ORDENES.FlgEst = "A"       /* ANULADA */
    ORDENES.FchAprobacion  = TODAY
    ORDENES.UsrAprobacion = s-user-id.

END PROCEDURE.

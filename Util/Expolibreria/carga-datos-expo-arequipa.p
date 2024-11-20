/* CARGAMOS LA INFORMACION PARA EXPO AREQUIPA ENERO */

/* STOCK */
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF vtacatventa.
DISABLE TRIGGERS FOR LOAD OF almcatvtac.
DISABLE TRIGGERS FOR LOAD OF almcatvtad.
DISABLE TRIGGERS FOR LOAD OF expasist.

DISPLAY 'borrando almacenes' SKIP. PAUSE 0.
FOR EACH almmmate WHERE codcia = 1 AND codalm = '60':
    DELETE almmmate.
END.
DEF BUFFER bmate FOR almmmate.
DISPLAY 'cargando almacenes' SKIP. PAUSE 0.
FOR EACH almmmate NO-LOCK WHERE codcia = 1 AND codalm = '35':
    CREATE bmate.
    BUFFER-COPY almmmate
        TO bmate
        ASSIGN bmate.codalm = '60'.
END.

/* PEDIDOS DE LOS PROVEEDORES */
DISPLAY 'borrando pedidos' SKIP. PAUSE 0.
FOR EACH vtacatventa WHERE vtacatventa.codcia = 001 AND vtacatventa.coddiv = '10060':
    DELETE vtacatventa.
END.
FOR EACH almcatvtac WHERE almcatvtac.codcia = 001 AND almcatvtac.coddiv = '10060':
    DELETE almcatvtac.
END.
FOR EACH almcatvtad WHERE almcatvtad.codcia = 001 AND almcatvtad.coddiv = '10060':
    DELETE almcatvtad.
END.
FOR EACH expasist WHERE expasist.codcia = 001 AND expasist.coddiv = '10060':
    DELETE expasist.
END.

DEF BUFFER dventa FOR vtacatventa.
DEF BUFFER dcatc FOR almcatvtac.
DEF BUFFER dcatd FOR almcatvtad.
DEF BUFFER basist FOR expasist.

DISPLAY 'cargando pedidos' SKIP. PAUSE 0.
FOR EACH vtacatventa NO-LOCK WHERE vtacatventa.codcia = 001 AND vtacatventa.coddiv = '10015':
    CREATE dventa.
    BUFFER-COPY vtacatventa 
        TO dventa
        ASSIGN dventa.coddiv = '10060'.
END.
FOR EACH almcatvtac NO-LOCK WHERE almcatvtac.codcia = 001 AND almcatvtac.coddiv = '10015':
    CREATE dcatc.
    BUFFER-COPY almcatvtac 
        TO dcatc
        ASSIGN dcatc.coddiv = '10060'.
END.
FOR EACH almcatvtad NO-LOCK WHERE almcatvtad.codcia = 001 AND almcatvtad.coddiv = '10015':
    CREATE dcatd.
    BUFFER-COPY almcatvtad 
        TO dcatd
        ASSIGN dcatd.coddiv = '10060'.
END.

FOR EACH expasist NO-LOCK WHERE expasist.codcia = 001 AND expasist.coddiv = '00015'.
    CREATE basist.
    BUFFER-COPY expasist
        TO basist 
        ASSIGN basist.coddiv = '10060'.
END.

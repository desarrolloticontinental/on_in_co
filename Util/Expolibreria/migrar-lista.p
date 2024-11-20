/* migrar precios de una lista a otra */

DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR x-divold AS CHAR INIT '20015' NO-UNDO.
DEF VAR x-divnew AS CHAR INIT '20060' NO-UNDO.

/* Borramos datos de la lista nueva */
FOR EACH vtalistamay EXCLUSIVE-LOCK WHERE vtalistamay.codcia = s-codcia
    AND vtalistamay.coddiv = x-divnew:
    DELETE vtalistamay.
END.
/* Migramos datos de la lista base a la lista nueva */
DEF BUFFER b-lista FOR vtalistamay.
FOR EACH vtalistamay NO-LOCK WHERE vtalistamay.codcia = s-codcia
    AND vtalistamay.coddiv = x-divold:
    CREATE b-lista.
    BUFFER-COPY vtalistamay
        TO b-lista
        ASSIGN b-lista.coddiv = x-divnew.
END.
MESSAGE 'Lista de Precios Migrada'.
/* Migramos descuentos por volumen */
DEF VAR s-tabla AS CHAR INIT 'DVXDSF' NO-UNDO.
FOR EACH factabla EXCLUSIVE-LOCK WHERE factabla.codcia = s-codcia
    AND factabla.tabla = s-tabla
    AND factabla.codigo BEGINS x-divnew:
    DELETE factabla.
END.
DEF BUFFER b-tabla FOR factabla.
FOR EACH factabla NO-LOCK WHERE factabla.codcia = s-codcia
    AND factabla.tabla = s-tabla
    AND factabla.codigo BEGINS x-divold:
    CREATE b-tabla.
    BUFFER-COPY factabla
        TO b-tabla
        ASSIGN b-tabla.codigo = TRIM(x-divnew) + SUBSTRING(factabla.codigo,6).
END.
MESSAGE 'Descuento por Volumen Migrada'.
/* Migramos descuentos por volumen pos saldo */
s-Tabla = "EDVXSALDOC".
DEF VAR s-Tabla-1 AS CHAR NO-UNDO.
s-Tabla-1 = "EDVXSALDOD".
FOR EACH factabla EXCLUSIVE-LOCK WHERE factabla.codcia = s-codcia
    AND factabla.tabla = s-tabla
    AND factabla.codigo BEGINS x-divnew:
    DELETE factabla.
END.
FOR EACH factabla EXCLUSIVE-LOCK WHERE factabla.codcia = s-codcia
    AND factabla.tabla = s-tabla-1
    AND factabla.codigo BEGINS x-divnew:
    DELETE factabla.
END.
FOR EACH factabla NO-LOCK WHERE factabla.codcia = s-codcia
    AND factabla.tabla = s-tabla
    AND factabla.codigo BEGINS x-divold:
    CREATE b-tabla.
    BUFFER-COPY factabla
        TO b-tabla
        ASSIGN b-tabla.codigo = TRIM(x-divnew) + '|' + ENTRY(2,factabla.codigo,'|').
END.
FOR EACH factabla NO-LOCK WHERE factabla.codcia = s-codcia
    AND factabla.tabla = s-tabla-1
    AND factabla.codigo BEGINS x-divold:
    CREATE b-tabla.
    BUFFER-COPY factabla
        TO b-tabla
        ASSIGN b-tabla.codigo = TRIM(x-divnew) + '|' + ENTRY(2,factabla.codigo,'|') + '|' + ENTRY(3,factabla.codigo,'|').
END.
MESSAGE 'Descuento por Saldo Migrada'.


/*
DISABLE TRIGGERS FOR LOAD OF vtacatventa.
DISABLE TRIGGERS FOR LOAD OF almcatvtac.
DISABLE TRIGGERS FOR LOAD OF almcatvtad.

DEF BUFFER b-catventa FOR vtacatventa.
DEF BUFFER b-catventac FOR almcatvtac.
DEF BUFFER b-catventad FOR almcatvtad.
FOR EACH vtacatventa WHERE vtacatventa.codcia = s-codcia
    AND vtacatventa.coddiv = x-divnew:
    FOR EACH AlmCatVtaC OF VtaCatVenta:
        FOR EACH AlmCatVtaD OF AlmCatVtaC:
            DELETE Almcatvtad.
        END.
        DELETE Almcatvtac.
    END.
    DELETE vtacatventa.
END.
FOR EACH vtacatventa NO-LOCK WHERE vtacatventa.codcia = s-codcia
    AND vtacatventa.coddiv = x-divold:
    CREATE b-catventa.
    BUFFER-COPY vtacatventa
        TO b-catventa
        ASSIGN b-catventa.coddiv = x-divnew.
    FOR EACH AlmCatVtaC OF VtaCatVenta NO-LOCK:
        CREATE b-catventac.
        BUFFER-COPY almcatvtac
            TO b-catventac
            ASSIGN b-catventac.coddiv = x-divnew.
        FOR EACH AlmCatVtaD OF AlmCatVtaC NO-LOCK:
            CREATE b-catventad.
            BUFFER-COPY almcatvtad
                TO b-catventad
                ASSIGN b-catventad.coddiv = x-divnew.
        END.
    END.
END.


*/

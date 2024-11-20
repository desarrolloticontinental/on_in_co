/* Carga factores chen por division */
DEF VAR x-linea AS CHAR NO-UNDO.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

/* 1ro borramos tablas                          */
FOR EACH vtatabla WHERE vtatabla.codcia = s-codcia
    AND LOOKUP(vtatabla.llave_c1, "00060,00061,00062,00063,10060,20060") > 0
    AND vtatabla.tabla = "DIVFACXLIN":
    DELETE vtatabla.
END.
FOR EACH vtatabla WHERE vtatabla.codcia = s-codcia
    AND LOOKUP(vtatabla.llave_c1, "00060,00061,00062,00063,10060,20060") > 0
    AND vtatabla.tabla = "DIVFACXSLIN":
    DELETE vtatabla.
END.
/* ******************************************** */
/* 2do. cargamos tabla base division 00060      */
DEF VAR x-valor AS DEC DECIMALS 6 NO-UNDO.
INPUT FROM d:\tmp\factorarequipa.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    x-valor = DEC(SUBSTRING(x-linea,21)).
    /* cabecera */
    FIND vtatabla WHERE vtatabla.codcia = s-codcia
        AND vtatabla.tabla = "DIVFACXLIN"
        AND vtatabla.llave_c1 = "00060"
        AND vtatabla.llave_c2 = SUBSTRING(x-linea,1,10)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vtatabla THEN DO:
        CREATE vtatabla.
        ASSIGN
            vtatabla.codcia = s-codcia
            vtatabla.tabla = "DIVFACXLIN"
            vtatabla.llave_c1 = "00060"
            vtatabla.llave_c2 = SUBSTRING(x-linea,1,10).
    END.
    /* detalle */
    CREATE vtatabla.
    ASSIGN
        vtatabla.codcia = s-codcia
        vtatabla.tabla = "DIVFACXSLIN"
        vtatabla.llave_c1 = "00060"
        vtatabla.llave_c2 = SUBSTRING(x-linea,1,10)
        vtatabla.llave_c3 = SUBSTRING(x-linea,11,10)
        vtatabla.valor[1] = x-valor.
END.
/* ******************************************** */
/* 3ro cargamos a las demas divisiones relacionadas */
DEF VAR s-coddiv AS CHAR INIT '00061,00062,00063,10060,20060' NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF BUFFER b-tabla FOR vtatabla.

DO j = 1 TO NUM-ENTRIES(s-coddiv):
    FOR EACH vtatabla NO-LOCK WHERE vtatabla.codcia = s-codcia
        AND vtatabla.tabla = "DIVFACXLIN"
        AND vtatabla.llave_c1 = "00060":
        CREATE b-tabla.
        BUFFER-COPY vtatabla
            TO b-tabla
            ASSIGN b-tabla.llave_c1 = ENTRY(j,s-coddiv).
    END.
    FOR EACH vtatabla NO-LOCK WHERE vtatabla.codcia = s-codcia
        AND vtatabla.tabla = "DIVFACXSLIN"
        AND vtatabla.llave_c1 = "00060":
        CREATE b-tabla.
        BUFFER-COPY vtatabla
            TO b-tabla
            ASSIGN b-tabla.llave_c1 = ENTRY(j,s-coddiv).
    END.
END.
/* ******************************************** */

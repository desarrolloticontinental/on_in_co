DEFINE TEMP-TABLE t-Ventas_Cabecera NO-UNDO LIKE Ventas_Cabecera.
DEF VAR cDivisiones AS CHAR INIT '*'.
DEF VAR pdDesde AS DATE.
pdDesde = DATE(10,01,2022).
DEF VAR pdHasta AS DATE.
pdHasta = DATE(03,31,2023).
DEF VAR cCodDiv AS CHAR.
DEF VAR cCtgCtble AS CHAR.
DEF VAR cCategoriasContables AS CHAR INIT 'MC,MI'.
DEF VAR cCodMat AS CHAR.
cCodMat = '002751'.

DEF VAR X AS DEC.

DEF BUFFER NCREDITO FOR ventas_cabecera.

EMPTY TEMP-TABLE t-Ventas_Cabecera.
VENTAS:
FOR EACH ventas_detalle NO-LOCK WHERE
        ventas_detalle.datekey >= pdDesde AND 
        ventas_detalle.datekey <= pdHasta AND
        ventas_detalle.codmat = cCodMat,
    FIRST ventas_cabecera NO-LOCK WHERE 
        ventas_cabecera.CODdoc = ventas_detalle.coddoc AND 
        ventas_cabecera.nrodoc = ventas_detalle.nrodoc:
    IF LOOKUP(ventas_cabecera.coddoc, 'FAC,BOL') = 0 THEN NEXT.
    /* 1) Por las divisiones válidas */
    IF cDivisiones <> "*" THEN DO:
        cCodDiv = TRIM(ventas_detalle.coddiv).
        IF LOOKUP(cCodDiv, cDivisiones) = 0 THEN NEXT.        
    END.
    /* 2) Las N/C relacionadas por cada FAC */
    FOR EACH NCREDITO NO-LOCK WHERE NCREDITO.codref = Ventas_Cabecera.CodDoc AND
        NCREDITO.nroref = Ventas_Cabecera.NroDoc AND
        NCREDITO.coddoc = "N/C":
        FIND FIRST t-Ventas_Cabecera WHERE t-Ventas_Cabecera.coddoc = NCREDITO.coddoc AND
            t-Ventas_Cabecera.nrodoc = NCREDITO.nrodoc NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-Ventas_Cabecera THEN DO:
            CREATE t-Ventas_Cabecera.
            BUFFER-COPY NCREDITO TO t-Ventas_Cabecera.
        END.
    END.
    X = X + (Ventas_Detalle.ImpNacSIGV - Ventas_Detalle.PromNacSIGV).
END.
FOR EACH t-Ventas_Cabecera NO-LOCK,
    FIRST Ventas_Cabecera NO-LOCK WHERE Ventas_Cabecera.CodDoc = t-Ventas_Cabecera.CodDoc AND       /* N/C */
        Ventas_Cabecera.NroDoc = t-Ventas_Cabecera.NroDoc:
    FOR EACH Ventas_Detalle NO-LOCK WHERE Ventas_Detalle.CodDoc = Ventas_Cabecera.CodDoc AND
        Ventas_Detalle.NroDoc = Ventas_Cabecera.NroDoc AND
        Ventas_Detalle.codmat = cCodMat,
        FIRST DimProducto FIELDS(codmat tpoart CtgCtble) NO-LOCK WHERE DimProducto.codmat = Ventas_Detalle.codmat:
        IF DimProducto.tpoart <> 'A' THEN NEXT.
        cCtgCtble = DimProducto.CtgCtble.
        IF LOOKUP(cCtgCtble,cCategoriasContables) = 0 THEN NEXT.
        X = X + (Ventas_Detalle.ImpNacSIGV - Ventas_Detalle.PromNacSIGV).
    END.
END.
MESSAGE X.

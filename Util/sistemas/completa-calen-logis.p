DEF NEW SHARED VAR s-codcia AS INT INIT 001.

DEF VAR s-Tabla AS CHAR INIT 'ZGHR'.  /* Zona Geografia Hoja de Ruta */
DEF VAR s-Tabla-2 AS CHAR INIT "SZGHR" NO-UNDO.

DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.
DEF BUFFER B-DTabla FOR Vtadtabla.

FIND Vtactabla WHERE VTactabla.codcia = s-codcia
    AND Vtactabla.tabla = s-Tabla
    AND Vtactabla.llave = '02'
    NO-LOCK.
FIND VtaDTabla WHERE VtaDTabla.CodCia = VtaCTabla.CodCia
    AND VtaDTabla.Llave = VtaCTabla.Llave
    AND VtaDTabla.Tabla = s-Tabla-2
    AND VtaDTabla.LlaveDetalle = "C" 
    AND VtaDTabla.Tipo = '01'
    NO-LOCK.

FOR EACH TabDepto NO-LOCK,
    EACH TabProvi OF TabDepto NO-LOCK,
    EACH TabDistr OF TabProvi NO-LOCK:
    IF TabProvi.CodDepto = '15' AND TabProvi.CodProvi = '01' THEN NEXT.
    /* Buscamos si ya está registrado */
    FIND FIRST B-DTabla WHERE B-DTabla.codcia = s-codcia AND
        B-DTabla.tabla = s-Tabla-2 AND
        B-DTabla.libre_c01 = TabDistr.CodDepto AND
        B-DTabla.libre_c02 = TabDistr.CodProvi AND
        B-DTabla.libre_c03 = TabDistr.CodDistr
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-DTabla THEN NEXT.
    DISPLAY TabDistr.CodDepto TabDistr.CodProvi TabDistr.CodDistr.
    PAUSE 0.

    CREATE B-DTABLA.
    ASSIGN
        B-DTabla.CodCia = VtaDTabla.CodCia 
        B-DTabla.Tabla  = VtaDTabla.Tabla 
        B-DTabla.Llave  = VtaDTabla.Llave
        B-DTabla.Tipo   = VtaDTabla.Tipo
        B-DTabla.LlaveDetalle = "D"
        B-DTabla.Libre_c01 = TabDistr.CodDepto 
        B-DTabla.Libre_c02 = TabDistr.CodProvi 
        B-DTabla.Libre_c03 = TabDistr.CodDistr
        B-DTabla.FchCreacion = TODAY
        B-DTabla.UsrCreacion = s-user-id.
END.

/*
FOR EACH Vtactabla NO-LOCK WHERE VtaCTabla.CodCia = s-codcia 
    AND VtaCTabla.Tabla = s-tabla:
    FOR EACH VtaDTabla WHERE VtaDTabla.CodCia = VtaCTabla.CodCia
        AND VtaDTabla.Llave = VtaCTabla.Llave
        AND VtaDTabla.Tabla = s-Tabla-2
        AND VtaDTabla.LlaveDetalle = "C" NO-LOCK:
        FOR EACH B-DTabla WHERE B-DTabla.CodCia = VtaDTabla.CodCia
            AND B-DTabla.Tabla = VtaDTabla.Tabla
            AND B-DTabla.Llave = VtaDTabla.Llave
            AND B-DTabla.Tipo = VtaDTabla.Tipo
            AND B-DTabla.LlaveDetalle = "D" NO-LOCK:
        END.
    END.
END.
*/

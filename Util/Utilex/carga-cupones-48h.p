/* Rutina para generar CUPONES válidos para las tiendas UTILEX */
DEF VAR cEncarte AS CHAR INIT '2020' NO-UNDO.
DEF VAR cTabla   AS CHAR INIT 'UTILEX-CUPON' NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'ADMIN' NO-UNDO.

/* 1ro. Cabecera de Control */
FIND VtaCTabla WHERE VtaCTabla.CodCia = s-codcia
    AND VtaCTabla.Tabla = cTabla
    AND VtaCTabla.Llave = cEncarte
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VTactabla THEN DO:
    CREATE VTactabla.
    ASSIGN
        VtaCTabla.CodCia = s-codcia
        VtaCTabla.Tabla = cTabla
        VtaCTabla.Llave = cEncarte
        VtaCTabla.UsrCreacion = s-user-id
        VtaCTabla.FchCreacion= TODAY.
END.
/* 2do. generamos los cupones solo para UTILEX */
DEF VAR iControl AS INT NO-UNDO.
DEF VAR cCodigo AS CHAR NO-UNDO.
DEF VAR cCadena AS CHAR NO-UNDO.
DEF VAR iVerificador AS INT NO-UNDO.
FOR EACH gn-divi NO-LOCK WHERE codcia = 1 and canalventa = 'MIN' and campo-log[1] = no:
    /* Por cada UTILEX se van a generar 500 cupones */
    DO iControl = 1 TO 500:
        REPEAT:
            /* Número de 4 dígitos alfanumérico */
            RUN lib/_gen_password.p (4, OUTPUT cCodigo).
            /* Digito verificador */
            RUN lib/calcula-digito-verificador.p (cCodigo, OUTPUT iVerificador).
            IF iVerificador > 0 AND iVerificador < 10 THEN DO:
                IF INDEX(cCodigo, '_') = 0 AND INDEX(cCodigo, '0') = 0 THEN LEAVE.
            END.
        END.
        cCadena = CAPS(cCodigo) + STRING(iVerificador,'9').
        CREATE Vtadtabla.
        BUFFER-COPY Vtactabla TO Vtadtabla
            ASSIGN
            VtaDTabla.Tipo = gn-divi.CodDiv
            VtaDTabla.LlaveDetalle = cCadena
            VtaDTabla.FchCreacion = TODAY
            VtaDTabla.UsrCreacion = s-user-id
            VtaDTabla.Libre_d01 = iControl.
    END.
END.

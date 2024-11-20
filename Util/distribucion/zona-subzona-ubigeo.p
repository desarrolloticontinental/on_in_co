
DEF VAR s-codcia AS INT INIT 001.

DEF VAR s-Tabla1 AS CHAR INIT 'ZGHR'.  /* Zona Geografia Hoja de Ruta */
DEF VAR s-Tabla2 AS CHAR INIT "SZGHR" NO-UNDO.

&SCOPED-DEFINE Condicion VtaCTabla.CodCia = s-codcia AND VtaCTabla.Tabla = s-tabla1

DEFINE BUFFER B-DTabla FOR VtaDTabla.
DEFINE TEMP-TABLE T-DTabla LIKE VtaDTabla.

OUTPUT TO d:\tmp\zona-subzona-ubigeo.txt.
FOR EACH vtactabla NO-LOCK WHERE {&condicion}:
    FOR EACH VtaDTabla WHERE VtaDTabla.CodCia = VtaCTabla.CodCia
        AND VtaDTabla.Llave = VtaCTabla.Llave
        AND VtaDTabla.Tabla = s-Tabla2
        AND VtaDTabla.LlaveDetalle = "C" NO-LOCK:
        RUN carga-temporal.
        FOR EACH T-DTabla WHERE TRUE NO-LOCK,
            FIRST TabDepto WHERE TabDepto.CodDepto = T-DTabla.Libre_c01 NO-LOCK,
            FIRST TabProvi WHERE TabProvi.CodDepto = T-DTabla.Libre_c01
            AND TabProvi.CodProvi = T-DTabla.Libre_c02 NO-LOCK,
            FIRST TabDistr WHERE TabDistr.CodDepto = T-DTabla.Libre_c01
            AND TabDistr.CodProvi = T-DTabla.Libre_c02
            AND TabDistr.CodDistr = T-DTabla.Libre_c03 NO-LOCK:
            DISPLAY 
                VtaCTabla.Llave FORMAT 'x(2)' COLUMN-LABEL 'Zona'
                VtaCTabla.Descripcion FORMAT 'x(60)' COLUMN-LABEL 'Descripcion'
                VtaDTabla.Tipo  FORMAT 'x(3)'   COLUMN-LABEL 'SubZona'
                VtaDTabla.Libre_c02 FORMAT 'x(20)' COLUMN-LABEL 'Descripcion'
                T-DTabla.Libre_c01 FORMAT 'x(8)' COLUMN-LABEL 'Departamento'
                TabDepto.NomDepto
                T-DTabla.Libre_c02 FORMAT 'x(8)' COLUMN-LABEL 'Provincia'
                TabProvi.NomProvi
                T-DTabla.Libre_c03 FORMAT 'x(8)' COLUMN-LABEL 'Distrito'
                TabDistr.NomDistr
                WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.
                .
        END.
    END.
END.
OUTPUT CLOSE.

PROCEDURE carga-temporal:

    /*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE T-DTabla.
FOR EACH B-DTabla WHERE B-DTabla.CodCia = VtaDTabla.CodCia
    AND B-DTabla.Tabla = VtaDTabla.Tabla
    AND B-DTabla.Llave = VtaDTabla.Llave
    AND B-DTabla.Tipo = VtaDTabla.Tipo
    AND B-DTabla.LlaveDetalle = "D" NO-LOCK:
    CREATE T-DTabla.
    BUFFER-COPY B-DTabla TO T-DTabla.
END.


END PROCEDURE.


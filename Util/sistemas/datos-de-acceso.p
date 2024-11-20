DEF VAR x-seccion AS CHAR.
DEF VAR x-cargo AS CHAR.

OUTPUT TO d:\datos.txt.
FOR EACH logtabla NO-LOCK WHERE codcia = 1
    AND evento = 'run-program'
    AND index(valorllave, 'wconsreposmerca') > 0
    AND logtabla.Dia >= 01/01/2022 ,
    FIRST _user NO-LOCK WHERE _userid = logtabla.usuario:
    x-seccion = ''.
    x-cargo = ''.
    FIND LAST pl-flg-mes WHERE pl-flg-mes.codcia = 1
        AND pl-flg-mes.codper = _user._given_name NO-LOCK NO-ERROR.
    IF AVAILABLE pl-flg-mes THEN DO:
        x-seccion = pl-flg-mes.seccion.
        x-cargo = pl-flg-mes.cargo.
    END.
    PUT UNFORMATTED
        logtabla.dia ';'
        logtabla.hora ';'
        logtabla.usuario ';'
        logtabla.valorllave ';'
        _user._user-name ';'
        x-seccion ';'
        x-cargo
        SKIP.

END.

/*
DEFINE TEMP-TABLE tusuario
    FIELD   tcodigo     AS  CHAR FORMAT 'x(25)' COLUMN-LABEL "Codigo"
    FIELD   tnombre     AS  CHAR FORMAT 'x(80)' COLUMN-LABEL "Nombre"
    FIELD   tcodpla     AS  CHAR FORMAT 'x(10)' COLUMN-LABEL "Cod.Planilla"
    FIELD   tcargo     AS  CHAR FORMAT 'x(60)' COLUMN-LABEL "Cargo"
    FIELD   tarea     AS  CHAR FORMAT 'x(10)' COLUMN-LABEL "Area"
    .
    
DEFINE TEMP-TABLE taccesos
    FIELD   tcodigo     AS  CHAR FORMAT 'x(25)' COLUMN-LABEL "Codigo"
    FIELD   tcodapp     AS  CHAR FORMAT 'x(5)' COLUMN-LABEL "Cod.App"
    FIELD   tnomapp     AS  CHAR FORMAT 'x(50)' COLUMN-LABEL "Nombre App"
    FIELD   tfecha     AS  DATE COLUMN-LABEL "Fecha"
    FIELD   thora     AS  CHAR FORMAT 'x(15)' COLUMN-LABEL "Hora"
    FIELD   tprograma     AS  CHAR FORMAT 'x(50)' COLUMN-LABEL "Programa"
    FIELD   topcion     AS  CHAR FORMAT 'x(60)' COLUMN-LABEL "Opcion menu"
    .

FOR EACH _user WHERE _user._DISABLED = NO OR _user._DISABLED = ? NO-LOCK:
    CREATE tusuario.

    ASSIGN tusuario.tcodigo = _user._userid
            tusuario.tnombre = _user._user-name
            tusuario.tcodpla = _user._given_name
        .
    FIND LAST pl-flg-mes WHERE pl-flg-mes.codcia = 1 AND pl-flg-mes.codper = _user._given_name NO-LOCK NO-ERROR.
    IF AVAILABLE pl-flg-mes THEN DO:
        ASSIGN tusuario.tcargo = pl-flg-mes.cargos
                tusuario.tarea = pl-flg-mes.seccion.
    END.

END.
 
FOR EACH tusuario NO-LOCK:
    FOR EACH pf-g003 NO-LOCK :
        FOR EACH logtabla WHERE logtabla.codcia = 1 AND logtabla.evento = 'RUN-PROGRAM' AND 
                    logtabla.tabla = pf-g003.aplic-id AND logtabla.dia >= 11/01/2022 AND logtabla.usuario = tusuario.tcodigo NO-LOCK:
    
            FIND FIRST pf-g002 WHERE pf-g002.aplic-id = logtabla.tabla AND pf-g002.programa = logtabla.valorllave NO-LOCK NO-ERROR.
            
            CREATE taccesos.
                ASSIGN taccesos.tcodigo = tusuario.tcodigo
                        taccesos.tcodapp = logtabla.tabla
                        taccesos.tnomapp = pf-g003.detalle
                        taccesos.tfecha = logtabla.dia
                        taccesos.thora = logtabla.hora
                        taccesos.tprograma = logtabla.valorllave
                        taccesos.topcion = IF (AVAILABLE pf-g002) THEN pf-g002.etiqueta ELSE ""
                    .
        END.
    END.
END.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN lib\Tools-to-excel PERSISTENT SET hProc.

def var c-csv-file as char no-undo.
def var c-xls-file as char no-undo. /* will contain the XLS file path created */

c-xls-file = 'd:\xpciman\usuarios-conti.xlsx'.

run pi-crea-archivo-csv IN hProc (input  buffer tusuario:handle,
                        /input  session:temp-directory + "file"/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tusuario:handle,
                        input  c-csv-file,
                        output c-xls-file) .


c-xls-file = 'd:\xpciman\usuarios-accesos.xlsx'.

run pi-crea-archivo-csv IN hProc (input  buffer taccesos:handle,
                        /input  session:temp-directory + "file"/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer taccesos:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.
*/

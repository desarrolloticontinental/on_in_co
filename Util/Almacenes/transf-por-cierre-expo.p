/* GENERAMOS UN ARCHIVO EXCEL CON LAS CANTIDADES A TRANSFERIR
PARA DEJAR EL ALMACÉN EN CERO */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-Almacenes AS CHAR NO-UNDO.
DEF VAR x-CodAlm AS CHAR NO-UNDO.
DEF VAR k AS IN NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.

DEF STREAM REPORTE.
DEF TEMP-TABLE Detalle
    FIELD codmat LIKE almdmov.codmat
    FIELD candes LIKE almdmov.candes.

x-Almacenes = "10A".

DO k = 1 TO NUM-ENTRIES(x-Almacenes):
    x-CodAlm = ENTRY(k, x-Almacenes).
    DISPLAY x-codalm.
    RUN Carga-Temporal.
    RUN Genera-Excel.
    MESSAGE 'fin almacen' x-codalm.
END.

RETURN.

PROCEDURE Carga-Temporal:
/* ********************* */

EMPTY TEMP-TABLE Detalle.
FOR EACH Almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia
    AND Almmmate.codalm = x-CodAlm
    AND Almmmate.stkact > 0,
    FIRST Almmmatg OF Almmmate NO-LOCK:
    /* buscamos el stock comprometido */
    pComprometido = 0.
    RUN vta2/stock-comprometido (Almmmate.codmat,
                                 Almmmate.codalm,
                                 OUTPUT pComprometido).
    IF Almmmate.StkAct - pComprometido > 0 THEN DO:
        CREATE Detalle.
        ASSIGN
            detalle.codmat = Almmmate.codmat
            detalle.candes = Almmmate.StkAct - pComprometido.
    END.
END.
END PROCEDURE.


PROCEDURE Genera-Excel:
/* ******************* */

/* Generamos el archivo texto */
DEF VAR x-Archivo AS CHAR NO-UNDO.
DEF VAR x-Llave AS CHAR NO-UNDO.

x-Archivo = "c:\ciman\borrar\transf60e" + TRIM(x-codalm) + ".txt".
OUTPUT STREAM REPORTE TO VALUE (x-Archivo).
x-Llave = ''.
x-Llave = x-Llave + "Codigo" + CHR(9).
x-Llave = x-Llave + "Cantidad" + CHR(9).
x-Llave = x-Llave + "".
PUT STREAM REPORTE UNFORMATTED x-LLave SKIP.
FOR EACH Detalle:
    x-Llave = "".
    x-Llave = x-Llave + STRING(Detalle.codmat, "999999") + CHR(9).
    x-LLave = x-Llave + STRING(Detalle.candes, ">>>>>>>>9.99") + CHR(9).
    x-Llave = x-Llave + "".
    PUT STREAM REPORTE UNFORMATTED x-LLave SKIP.
END.            
OUTPUT STREAM REPORTE CLOSE.
/* CARGAMOS EL EXCEL */
RUN lib/filetext-to-excel(x-Archivo, 'Transferir', YES).

END PROCEDURE.

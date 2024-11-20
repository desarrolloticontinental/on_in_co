DEF TEMP-TABLE t-Almacen_Stocks LIKE Almacen_Stocks
    FIELD Fecha AS CHAR FORMAT 'x(10)' LABEL 'Fecha'
    FIELD Hora  AS CHAR FORMAT 'x(5)'  LABEL 'Hora'
    .

/* ********************************************************************************* */
/* ALMACEN_STOCKS */
/* ********************************************************************************* */
DEF VAR x-Archivo AS CHAR NO-UNDO.
DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR x-CodHora AS CHAR NO-UNDO.

x-CodFchI = TODAY.
x-CodHora = STRING(TIME, 'HH:MM').

/* ********************************************************************************* */
/* Cargamos el archivo texto */
/* 23/01/2023 Fecha y Hora solicitado por D.LL. para un control */
/* ********************************************************************************* */
EMPTY TEMP-TABLE t-Almacen_Stocks.
FOR EACH Almacen_Stocks NO-LOCK:
    CREATE t-Almacen_Stocks.
    BUFFER-COPY Almacen_Stocks TO t-Almacen_Stocks.
    ASSIGN
        t-Almacen_Stocks.Fecha = STRING(YEAR(x-CodFchI), '9999') + "-" + 
                                STRING(MONTH(x-CodFchI), '99') + "-" + 
                                STRING(DAY(x-CodFchI), '99')
        t-Almacen_Stocks.Hora = x-CodHora.
END.
/* ********************************************************************************* */
/* Borramos cualquier archivo generado la semana pasada */
/* ********************************************************************************* */
DEF VAR x-Comando AS CHAR NO-UNDO.

IF WEEKDAY(TODAY) = 2 THEN DO:      /* Lunes */
    x-Comando = "rm -f /u/backup/IN/ON_IN_CO/log/almacen_stocks*.txt".
    OS-COMMAND VALUE(x-Comando) SILENT NO-CONSOLE.
END.
/* ********************************************************************************* */
/* Pasamos el archivo texto */
/* ********************************************************************************* */
x-Archivo = "/v/IN/ON_IN_CO/dbs/" + "almacen_stocks.txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-Almacen_Stocks NO-LOCK:
    EXPORT delimiter "~029" t-Almacen_Stocks.
END.
OUTPUT CLOSE.
/* ******************************************************************************* */
/* COPIA DE CONTROL */
/* ******************************************************************************* */
DEF VAR x-Destino AS CHAR NO-UNDO.

x-Destino = "/u/backup/IN/ON_IN_CO/log/almacen_stocks" + ~
    STRING(YEAR(x-CodFchI), '9999') +  ~
    STRING(MONTH(x-CodFchI), '99') + ~
    STRING(DAY(x-CodFchI), '99') + ~
    REPLACE(x-CodHora,':','') +  ".txt".
OUTPUT TO VALUE(x-destino) KEEP-MESSAGES.
FOR EACH t-Almacen_Stocks NO-LOCK:
    EXPORT delimiter "~029" t-Almacen_Stocks.
END.
OUTPUT CLOSE.
/* x-Comando = "cp " + x-archivo + " " + x-destino. */
/* OS-COMMAND VALUE(x-Comando).                     */
QUIT.

